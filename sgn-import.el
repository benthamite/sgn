;;; sgn-import.el --- Import history from Signal Desktop  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; One-time import of message history from Signal Desktop's SQLCipher
;; database into sgn's SQLite database.  Requires `sqlcipher' CLI.

;;; Code:

(require 'cl-lib)
(require 'json)

(declare-function sgn--log "sgn")
(declare-function sgn-db-upsert-chat "sgn-db")
(declare-function sgn-db-insert-message "sgn-db")
(declare-function sgn-db-upsert-reaction "sgn-db")
(declare-function sgn-db-get-message "sgn-db")

(defvar sgn-account)
(defvar sgn-db--connection)

;;;; Configuration

(defconst sgn-import--desktop-db-path
  (expand-file-name "Library/Application Support/Signal/sql/db.sqlite"
                    (getenv "HOME"))
  "Path to Signal Desktop's SQLCipher database.")

(defconst sgn-import--desktop-config-path
  (expand-file-name "Library/Application Support/Signal/config.json"
                    (getenv "HOME"))
  "Path to Signal Desktop's config file (contains the DB key).")

;;;; Key extraction

(defun sgn-import--read-key ()
  "Read the SQLCipher key from Signal Desktop's config.json.
Handles both the legacy plain `key' field and the newer
`encryptedKey' field (Chromium safeStorage, decrypted via
macOS Keychain + PBKDF2 + AES-128-CBC)."
  (unless (file-exists-p sgn-import--desktop-config-path)
    (user-error "Signal Desktop config not found: %s"
                sgn-import--desktop-config-path))
  (let* ((json-object-type 'alist)
         (config (json-read-file sgn-import--desktop-config-path))
         (plain-key (alist-get 'key config))
         (encrypted-key (alist-get 'encryptedKey config)))
    (cond
     (plain-key plain-key)
     (encrypted-key (sgn-import--decrypt-safe-storage-key encrypted-key))
     (t (user-error "No key or encryptedKey found in Signal Desktop config")))))

(defun sgn-import--decrypt-safe-storage-key (encrypted-hex)
  "Decrypt Chromium safeStorage ENCRYPTED-HEX using macOS Keychain.
Returns the hex DB key string."
  (let* ((keychain-pass (string-trim
                         (shell-command-to-string
                          "security find-generic-password -s 'Signal Safe Storage' -w 2>/dev/null || security find-generic-password -s 'Signal' -w")))
         (script (format
                  "const crypto = require('crypto');\
const enc = Buffer.from('%s', 'hex');\
const key = crypto.pbkdf2Sync('%s', 'saltysalt', 1003, 16, 'sha1');\
const iv = Buffer.alloc(16, 32);\
const d = crypto.createDecipheriv('aes-128-cbc', key, iv);\
process.stdout.write(Buffer.concat([d.update(enc.subarray(3)), d.final()]).toString('utf8'));"
                  encrypted-hex keychain-pass))
         (result (with-temp-buffer
                   (call-process "node" nil t nil "-e" script)
                   (buffer-string))))
    (when (string-empty-p result)
      (user-error "Failed to decrypt Signal Desktop encryptedKey"))
    result))

;;;; Export via sqlcipher

(defun sgn-import--export-to-plain-db ()
  "Export Signal Desktop's encrypted DB to a temporary plain SQLite file.
Return the path to the temporary file."
  (unless (executable-find "sqlcipher")
    (user-error "sqlcipher not found; install with: brew install sqlcipher"))
  (unless (file-exists-p sgn-import--desktop-db-path)
    (user-error "Signal Desktop database not found: %s"
                sgn-import--desktop-db-path))
  (let* ((key (sgn-import--read-key))
         (tmp-db (make-temp-file "sgn-import-" nil ".db"))
         (sql (format "PRAGMA key = \"x'%s'\";
ATTACH DATABASE '%s' AS export KEY '';
CREATE TABLE export.conversations AS
  SELECT id, type, name, profileFullName, e164, serviceId, groupId, active_at
  FROM conversations;
CREATE TABLE export.messages AS
  SELECT rowid, sent_at, type, sourceServiceId, body, conversationId,
         received_at, expireTimer, expirationStartTimestamp, isErased,
         json
  FROM messages
  WHERE body IS NOT NULL AND body != '' AND isErased IS NOT 1;
CREATE TABLE export.reactions AS
  SELECT conversationId, emoji, fromId, targetAuthorAci,
         targetTimestamp, timestamp
  FROM reactions;
DETACH DATABASE export;" key tmp-db))
         (exit-code (call-process "sqlcipher" nil nil nil
                                  sgn-import--desktop-db-path
                                  sql)))
    (unless (zerop exit-code)
      (when (file-exists-p tmp-db) (delete-file tmp-db))
      (user-error "sqlcipher export failed (exit code %d)" exit-code))
    (sgn--log "sgn-import: exported to %s" tmp-db)
    tmp-db))

;;;; ID mapping

(defun sgn-import--build-mappings (export-db)
  "Build ID mappings from EXPORT-DB.
Return a plist with:
  :conv-to-chat — hash: Desktop conversation UUID → sgn chat ID
  :uuid-to-id   — hash: any UUID (serviceId or conv-id) → sgn sender ID"
  (let ((conv-to-chat (make-hash-table :test 'equal))
        (uuid-to-id (make-hash-table :test 'equal))
        (rows (sqlite-select export-db
                "SELECT id, type, name, profileFullName, e164, serviceId, groupId, active_at
                 FROM conversations")))
    (dolist (row rows)
      (let* ((conv-id (nth 0 row))
             (type (nth 1 row))
             (name (nth 2 row))
             (profile-name (nth 3 row))
             (e164 (nth 4 row))
             (service-id (nth 5 row))
             (group-id (nth 6 row))
             (active-at (nth 7 row))
             ;; Determine sgn chat ID
             (chat-id (cond
                       ;; Group: use groupId (base64)
                       ((and (equal type "group") group-id)
                        group-id)
                       ;; Private: prefer e164, fall back to serviceId
                       (e164 e164)
                       (service-id service-id)))
             ;; Sender ID for messages/reactions: prefer e164
             (sender-id (or e164 service-id))
             ;; Display name
             (display-name (or (and name (not (string-empty-p name)) name)
                               (and profile-name
                                    (not (string-empty-p profile-name))
                                    profile-name))))
        (when chat-id
          (puthash conv-id chat-id conv-to-chat)
          ;; Map both serviceId AND conv-id → sender ID
          ;; so reactions (which use conv-id as fromId) resolve too
          (when sender-id
            (when service-id
              (puthash service-id sender-id uuid-to-id))
            (puthash conv-id sender-id uuid-to-id))
          ;; Upsert chat into sgn DB
          (sgn-db-upsert-chat chat-id
                              :name (or display-name "")
                              :type (if (equal type "group") "group" "individual")
                              :last-msg-ts active-at)
          ;; Register name in contacts cache for rendering
          (when (and display-name sender-id)
            (sgn-contacts-set-name sender-id display-name)))))
    (list :conv-to-chat conv-to-chat
          :uuid-to-id uuid-to-id)))

;;;; Message import

(defun sgn-import--resolve-sender (msg-type source-uuid uuid-to-id)
  "Resolve the sender ID for a message.
MSG-TYPE is \"incoming\" or \"outgoing\".  SOURCE-UUID is the
sender's serviceId.  UUID-TO-ID maps UUIDs to phone numbers."
  (cond
   ((equal msg-type "outgoing") sgn-account)
   ((and source-uuid (gethash source-uuid uuid-to-id)))
   (source-uuid source-uuid)
   (t "unknown")))

(defun sgn-import--import-messages (export-db mappings)
  "Import messages from EXPORT-DB using MAPPINGS.
Return the number of messages imported."
  (let* ((conv-to-chat (plist-get mappings :conv-to-chat))
         (uuid-to-id (plist-get mappings :uuid-to-id))
         (rows (sqlite-select export-db
                 "SELECT sent_at, type, sourceServiceId, body, conversationId,
                         expireTimer, json
                  FROM messages
                  ORDER BY sent_at ASC"))
         (count 0))
    (dolist (row rows)
      (let* ((sent-at (nth 0 row))
             (msg-type (nth 1 row))
             (source-uuid (nth 2 row))
             (body (nth 3 row))
             (conv-id (nth 4 row))
             (expire-timer (or (nth 5 row) 0))
             (raw-json (nth 6 row))
             (chat-id (gethash conv-id conv-to-chat))
             (sender (sgn-import--resolve-sender msg-type source-uuid uuid-to-id))
             ;; Extract quote info from JSON if present
             (quote-info (sgn-import--extract-quote raw-json uuid-to-id))
             (sgn-type (if (equal msg-type "outgoing") "sync" "data")))
        (when (and chat-id body sender)
          (sgn-db-insert-message
           (list :chat-id chat-id
                 :sender sender
                 :timestamp sent-at
                 :body body
                 :type sgn-type
                 :quote-ts (plist-get quote-info :quote-ts)
                 :quote-author (plist-get quote-info :quote-author)
                 :quote-body (plist-get quote-info :quote-body)
                 :expires-in expire-timer
                 :raw-json raw-json))
          (cl-incf count))))
    count))

(defun sgn-import--extract-quote (raw-json uuid-to-id)
  "Extract quote/reply info from RAW-JSON.
Return plist with :quote-ts :quote-author :quote-body, or nil."
  (when raw-json
    (condition-case nil
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read-from-string raw-json))
               (quote-data (alist-get 'quote data)))
          (when quote-data
            (let* ((quote-id (alist-get 'id quote-data))
                   (quote-author-uuid (alist-get 'authorAci quote-data))
                   (quote-author (or (and quote-author-uuid
                                          (gethash quote-author-uuid uuid-to-id))
                                     quote-author-uuid))
                   (quote-text (alist-get 'text quote-data)))
              (when quote-id
                (list :quote-ts quote-id
                      :quote-author quote-author
                      :quote-body quote-text)))))
      (error nil))))

;;;; Reaction import

(defun sgn-import--import-reactions (export-db mappings)
  "Import reactions from EXPORT-DB using MAPPINGS.
Return the number of reactions imported."
  (let* ((conv-to-chat (plist-get mappings :conv-to-chat))
         (uuid-to-id (plist-get mappings :uuid-to-id))
         (rows (sqlite-select export-db
                 "SELECT conversationId, emoji, fromId, targetAuthorAci,
                         targetTimestamp
                  FROM reactions"))
         (count 0))
    (dolist (row rows)
      (let* ((conv-id (nth 0 row))
             (emoji (nth 1 row))
             (from-uuid (nth 2 row))
             (target-author-uuid (nth 3 row))
             (target-ts (nth 4 row))
             (chat-id (gethash conv-id conv-to-chat))
             (sender (or (gethash from-uuid uuid-to-id) from-uuid))
             (target-author (or (gethash target-author-uuid uuid-to-id)
                                target-author-uuid)))
        (when (and chat-id emoji sender target-author target-ts)
          ;; Find the message rowid
          (let ((msg (sgn-db-get-message chat-id target-author target-ts)))
            (when msg
              (sgn-db-upsert-reaction
               (list :message-rowid (plist-get msg :rowid)
                     :chat-id chat-id
                     :target-author target-author
                     :target-timestamp target-ts
                     :sender sender
                     :emoji emoji
                     :removed 0))
              (cl-incf count))))))
    count))

;;;; Top-level command

;;;###autoload
(defun sgn-import-from-desktop ()
  "Import message history from Signal Desktop into sgn's database.
Requires `sqlcipher' CLI and a running Signal Desktop installation."
  (interactive)
  (require 'sgn)
  (unless sgn-db--connection
    (user-error "sgn database not initialized; run M-x sgn-start first"))
  (message "Exporting Signal Desktop database...")
  (let ((tmp-db-path (sgn-import--export-to-plain-db)))
    (unwind-protect
        (let ((export-db (sqlite-open tmp-db-path)))
          (unwind-protect
              (progn
                (message "Building conversation mappings...")
                (let ((mappings (sgn-import--build-mappings export-db)))
                  (message "Importing messages...")
                  (let ((msg-count (sgn-import--import-messages export-db mappings)))
                    (message "Importing reactions...")
                    (let ((rxn-count (sgn-import--import-reactions export-db mappings)))
                      (sgn--log "sgn-import: imported %d messages, %d reactions"
                                msg-count rxn-count)
                      (message "Import complete: %d messages, %d reactions."
                               msg-count rxn-count)))))
            (sqlite-close export-db)))
      (when (file-exists-p tmp-db-path)
        (delete-file tmp-db-path)))))

(provide 'sgn-import)
;;; sgn-import.el ends here
