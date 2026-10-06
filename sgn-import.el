;;; sgn-import.el --- Import history from Signal Desktop  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Import of text message history from Signal Desktop's SQLCipher
;; database into sgn's database.  Requires the `sqlcipher' program,
;; and on macOS `node' to decrypt the database key Signal Desktop
;; keeps in the Keychain.
;;
;; Secrets never appear on a command line, where other processes
;; could read them: the key and the SQL that uses it are passed to
;; the helper programs on their standard input.
;;
;; Messages already stored are skipped, so importing again is safe.
;; Messages whose disappearing timer ran out are not imported, and
;; the others keep counting down.  Reactions come from each message's
;; own record, which holds its current reactions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'sgn-db)
(require 'sgn-format)

(declare-function sgn--log "sgn")
(declare-function sgn-ensure-running "sgn")
(declare-function sgn-store-self "sgn-store")
(declare-function sgn-store-changed "sgn-store")
(declare-function sgn-contacts-set-name "sgn-contacts")
(declare-function sgn-contacts-name-known-p "sgn-contacts")

(defvar sgn-account)

;;;; Configuration

(defconst sgn-import--desktop-directory
  (expand-file-name "Library/Application Support/Signal" (getenv "HOME"))
  "Directory of Signal Desktop's data.")

(defun sgn-import--desktop-db-path ()
  "Return the path of Signal Desktop's SQLCipher database."
  (expand-file-name "sql/db.sqlite" sgn-import--desktop-directory))

(defun sgn-import--desktop-config-path ()
  "Return the path of Signal Desktop's config file, which holds the key."
  (expand-file-name "config.json" sgn-import--desktop-directory))

;;;; Running helpers

(defun sgn-import--run (input program &rest args)
  "Run PROGRAM with ARGS, feeding it INPUT; return its standard output.
Signal an error, quoting its standard error, if it fails."
  (let ((stderr (make-temp-file "sgn-import-err-")))
    (unwind-protect
        (with-temp-buffer
          (insert input)
          (let ((status (apply #'call-process-region (point-min) (point-max)
                               program t (list t stderr) nil args)))
            (unless (eql status 0)
              (error "%s failed (%s): %s" program status
                     (with-temp-buffer
                       (insert-file-contents stderr)
                       (string-trim (buffer-string)))))
            (buffer-string)))
      (delete-file stderr))))

;;;; Key extraction

(defun sgn-import--read-key ()
  "Return Signal Desktop's database key, as 64 hex digits."
  (let* ((path (sgn-import--desktop-config-path))
         (config (if (file-readable-p path)
                     (json-parse-string (with-temp-buffer
                                          (insert-file-contents path)
                                          (buffer-string))
                                        :object-type 'alist)
                   (user-error "Signal Desktop config not found: %s" path)))
         (key (or (alist-get 'key config)
                  (when-let* ((encrypted (alist-get 'encryptedKey config)))
                    (sgn-import--decrypt-key encrypted))
                  (user-error "No key found in Signal Desktop's config"))))
    (unless (string-match-p "\\`[0-9a-fA-F]\\{64\\}\\'" key)
      (user-error "Signal Desktop's database key has an unexpected form"))
    key))

(defconst sgn-import--decrypt-script
  "const crypto = require('crypto');
let input = '';
process.stdin.on('data', d => input += d);
process.stdin.on('end', () => {
  const {encrypted, password} = JSON.parse(input);
  const enc = Buffer.from(encrypted, 'hex');
  const key = crypto.pbkdf2Sync(password, 'saltysalt', 1003, 16, 'sha1');
  const d = crypto.createDecipheriv('aes-128-cbc', key, Buffer.alloc(16, 32));
  process.stdout.write(Buffer.concat([d.update(enc.subarray(3)),
                                      d.final()]).toString('utf8'));
});"
  "Node script decrypting Chromium safeStorage data read from stdin.")

(defun sgn-import--decrypt-key (encrypted)
  "Decrypt the safeStorage-encrypted key ENCRYPTED with the Keychain password."
  (unless (eq system-type 'darwin)
    (user-error "Decrypting Signal Desktop's key is only supported on macOS"))
  (unless (string-match-p "\\`[0-9a-fA-F]+\\'" encrypted)
    (user-error "Signal Desktop's encrypted key has an unexpected form"))
  (let ((password (string-trim
                   (sgn-import--run "" "security" "find-generic-password"
                                    "-s" "Signal Safe Storage" "-w"))))
    (string-trim (sgn-import--run (json-encode `((encrypted . ,encrypted)
                                                 (password . ,password)))
                                  "node" "-e" sgn-import--decrypt-script))))

;;;; Export via sqlcipher

(defun sgn-import--sql-string (string)
  "Return STRING as an SQL string literal."
  (concat "'" (string-replace "'" "''" string) "'"))

(defun sgn-import--export ()
  "Export the data to import from Signal Desktop's database.
Return the path of a temporary plain SQLite database."
  (unless (executable-find "sqlcipher")
    (user-error "The sqlcipher program is needed to import from Signal Desktop"))
  (unless (file-exists-p (sgn-import--desktop-db-path))
    (user-error "Signal Desktop database not found: %s"
                (sgn-import--desktop-db-path)))
  (let ((key (sgn-import--read-key))
        (export (make-temp-file "sgn-import-" nil ".db")))
    (delete-file export)
    (condition-case err
        (sgn-import--run
         (format "PRAGMA key = \"x'%s'\";
ATTACH DATABASE %s AS export KEY '';
CREATE TABLE export.conversations AS
  SELECT id, type, name, profileFullName, e164, serviceId, groupId
  FROM conversations;
CREATE TABLE export.messages AS
  SELECT sent_at, type, sourceServiceId, body, conversationId, expireTimer,
         expirationStartTimestamp, json
  FROM messages
  WHERE type IN ('incoming', 'outgoing')
    AND body IS NOT NULL AND body != ''
    AND isErased IS NOT 1
    AND storyId IS NULL
    AND NOT (COALESCE(expireTimer, 0) > 0
             AND expirationStartTimestamp IS NOT NULL
             AND expirationStartTimestamp + expireTimer * 1000
                 <= CAST(strftime('%%s', 'now') AS INTEGER) * 1000);
DETACH DATABASE export;
" key (sgn-import--sql-string export))
         "sqlcipher" (sgn-import--desktop-db-path))
      (error
       (when (file-exists-p export) (delete-file export))
       (signal (car err) (cdr err))))
    export))

;;;; Import

(defun sgn-import--conversations (export-db)
  "Return a hash table mapping Desktop conversation IDs to plists.
Each plist has the :chat-id and :type sgn uses for the
conversation, and :person, the identifier of its member for
direct chats.  Read from EXPORT-DB."
  (let ((table (make-hash-table :test 'equal)))
    (pcase-dolist (`(,id ,type ,name ,profile-name ,e164 ,service-id ,group-id)
                   (sqlite-select export-db
                                  "SELECT id, type, name, profileFullName, e164,
                                          serviceId, groupId
                                   FROM conversations"))
      (let ((person (or service-id e164))
            (name (seq-find (lambda (s) (and s (not (string-empty-p s))))
                            (list name profile-name))))
        (cond
         ((and (equal type "group") group-id)
          (puthash id (list :chat-id group-id :type "group" :name name) table))
         (person
          (puthash id (list :chat-id person :type "individual" :name name
                            :person person :number e164)
                   table)))))
    table))

(defun sgn-import--message-json (json)
  "Return the message record JSON as an alist, or nil if it is malformed."
  (when json
    (ignore-errors
      (json-parse-string json :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil))))

(defun sgn-import--import (export-db)
  "Import the conversations and messages in EXPORT-DB.
Return (MESSAGES . REACTIONS), the numbers imported."
  (let ((conversations (sgn-import--conversations export-db))
        (self (sgn-store-self))
        (chats nil)
        (messages 0)
        (reactions 0))
    (sgn-db-with-transaction
      (pcase-dolist (`(,sent-at ,type ,source ,body ,conversation-id
                       ,expire-timer ,expire-start ,json)
                     (sqlite-select export-db
                                    "SELECT sent_at, type, sourceServiceId, body,
                                            conversationId, expireTimer,
                                            expirationStartTimestamp, json
                                     FROM messages ORDER BY sent_at"))
        (when-let* ((chat (gethash conversation-id conversations))
                    (sender (if (equal type "outgoing") self
                              (or source (plist-get chat :person)))))
          (let* ((chat-id (plist-get chat :chat-id))
                 (record (sgn-import--message-json json))
                 (expires-in (or expire-timer 0))
                 (rowid (progn
                          (sgn-import--ensure-chat chat)
                          (sgn-db-insert-message
                           (append
                            (list :chat-id chat-id :sender sender
                                  :timestamp sent-at
                                  :outgoing (if (equal type "outgoing") 1 0)
                                  :body body :read-at sent-at
                                  :expires-in expires-in)
                            (sgn-import--quote record)
                            (sgn-import--mentions record)
                            (when (and (> expires-in 0) expire-start)
                              (list :expire-started-at expire-start
                                    :expires-at (+ expire-start
                                                   (* 1000 expires-in)))))))))
            (when rowid
              (cl-incf messages)
              (sgn-db-touch-chat chat-id sent-at)
              (push chat-id chats))
            (cl-incf reactions (sgn-import--reactions
                                record chat-id sender sent-at conversations))))))
    (apply #'sgn-store-changed (delete-dups chats))
    (cons messages reactions)))

(defun sgn-import--ensure-chat (chat)
  "Create the sgn chat for CHAT, a conversation plist, if needed."
  (let ((id (plist-get chat :chat-id))
        (name (plist-get chat :name)))
    (sgn-db-ensure-chat id (plist-get chat :type))
    (when (and name (not (sgn-contacts-name-known-p id)))
      (if (equal (plist-get chat :type) "group")
          (sgn-db-update-chat id :name name)
        (sgn-db-upsert-recipient id (plist-get chat :number) name))
      (sgn-contacts-set-name id name))))

(defun sgn-import--quote (record)
  "Return the quote columns for the Desktop message RECORD."
  (when-let* ((quote-data (alist-get 'quote record))
              (id (alist-get 'id quote-data)))
    (list :quote-ts id
          :quote-author (or (alist-get 'authorAci quote-data)
                            (alist-get 'author quote-data))
          :quote-body (alist-get 'text quote-data))))

(defun sgn-import--mentions (record)
  "Return the mention column for the Desktop message RECORD.
Desktop keeps mentions in `bodyRanges', in UTF-16 units like sgn."
  (when-let* ((mentions
               (cl-loop for range in (alist-get 'bodyRanges record)
                        for person = (alist-get 'mentionAci range)
                        when person
                        collect (list person (alist-get 'start range)
                                      (alist-get 'length range)))))
    (list :mentions-json (sgn-format-ranges-to-json mentions))))

(defun sgn-import--reactions (record chat-id author sent-at conversations)
  "Import the reactions in the Desktop message RECORD; return their number.
The message is the one AUTHOR sent at SENT-AT in CHAT-ID.
CONVERSATIONS maps reactors' conversation IDs to their plists."
  (let ((count 0))
    (dolist (reaction (alist-get 'reactions record))
      (when-let* ((from (gethash (alist-get 'fromId reaction) conversations))
                  (sender (plist-get from :person))
                  (emoji (alist-get 'emoji reaction)))
        (when (sgn-db-set-reaction chat-id author sent-at sender emoji
                                   (or (alist-get 'timestamp reaction) 0))
          (cl-incf count))))
    count))

;;;; Command

(defun sgn-import-desktop-available-p ()
  "Return non-nil if a Signal Desktop database is available to import."
  (and (file-exists-p (sgn-import--desktop-db-path))
       (executable-find "sqlcipher")))

;;;###autoload
(defun sgn-import-from-desktop ()
  "Import text message history from Signal Desktop into sgn."
  (interactive)
  (sgn-ensure-running)
  (message "Exporting Signal Desktop's database...")
  (let ((export (sgn-import--export)))
    (unwind-protect
        (let ((db (sqlite-open export)))
          (unwind-protect
              (pcase-let ((`(,messages . ,reactions) (sgn-import--import db)))
                (sgn--log "sgn-import: %d messages, %d reactions"
                          messages reactions)
                (message "Imported %d new messages and %d reactions."
                         messages reactions))
            (sqlite-close db)))
      (when (file-exists-p export)
        (delete-file export)))))

(provide 'sgn-import)
;;; sgn-import.el ends here
