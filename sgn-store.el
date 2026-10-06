;;; sgn-store.el --- Apply Signal events to the sgn database  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; The store applies the events `sgn-event' produces to the database,
;; all events of one envelope in a single transaction, and then tells
;; the rest of sgn what changed:
;;
;; - `sgn-store-changed-functions' are called with the list of chat
;;   IDs whose stored state changed, so that their buffers and the
;;   dashboard can redraw;
;; - `sgn-store-new-message-functions' are called with the chat ID
;;   and rowid of each newly stored incoming message, for unread
;;   handling and notifications;
;; - `sgn-store-typing-functions' are called with the chat ID, the
;;   sender, and whether they started typing.
;;
;; Database writes and UI updates are thus separate: nothing here
;; touches a buffer, and nothing in the UI writes the database
;; behind the store's back.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'sgn-db)
(require 'sgn-format)

(declare-function sgn--log "sgn")
(declare-function sgn-media-delete-files "sgn-media")
(declare-function sgn-contacts-set-name "sgn-contacts")
(declare-function sgn-contacts-name-known-p "sgn-contacts")

(defvar sgn-account)

;;;; Hooks

(defvar sgn-store-changed-functions nil
  "Functions called with the list of chat IDs whose state changed.")

(defvar sgn-store-new-message-functions nil
  "Functions called with the chat ID and rowid of a new incoming message.")

(defvar sgn-store-typing-functions nil
  "Functions called with a chat ID, a sender, and whether they started typing.")

;;;; Self

(defvar sgn-store--self-uuid nil
  "This account's ACI, or nil while unknown.")

(defun sgn-store-load-self ()
  "Load this account's ACI from the database."
  (setq sgn-store--self-uuid (sgn-db-get-meta "self_uuid")))

(defun sgn-store-self ()
  "Return the identifier of this account: its ACI, or its number."
  (or sgn-store--self-uuid sgn-account))

(defun sgn-store-self-p (id)
  "Return non-nil if ID identifies this account."
  (and id (or (equal id sgn-account)
              (equal id sgn-store--self-uuid))))

(defun sgn-store-self-ids ()
  "Return every identifier this account's messages may be stored under."
  (delq nil (list sgn-store--self-uuid sgn-account)))

(defun sgn-store-learn-self (uuid)
  "Record UUID as this account's ACI, rewriting older data to use it."
  (unless (equal uuid sgn-store--self-uuid)
    (setq sgn-store--self-uuid uuid)
    (sgn-db-set-meta "self_uuid" uuid)
    (sgn-db-merge-identity sgn-account uuid)))

;;;; Applying events

(defvar sgn-store--changed nil
  "Chat IDs changed by the events being applied.")

(defvar sgn-store--new-messages nil
  "(CHAT-ID . ROWID) of incoming messages stored by the events being applied.")

(defvar sgn-store--typing nil
  "Typing events seen while applying events.")

(defvar sgn-store--applying nil
  "Non-nil while `sgn-store-apply' is storing events.")

(defun sgn-store-apply (events)
  "Store EVENTS, then run the hooks describing what changed."
  (let ((sgn-store--changed nil)
        (sgn-store--new-messages nil)
        (sgn-store--typing nil)
        (sgn-store--applying t))
    (sgn-db-with-transaction
      (dolist (event events)
        (sgn-store--apply-event event)))
    (setq sgn-store--applying nil)
    (sgn-store--notify (delete-dups sgn-store--changed)
                       (nreverse sgn-store--new-messages)
                       (nreverse sgn-store--typing))))

(defun sgn-store--notify (chats new-messages typing)
  "Run the change hooks for CHATS, NEW-MESSAGES and TYPING events."
  (when chats
    (run-hook-with-args 'sgn-store-changed-functions chats))
  (pcase-dolist (`(,chat-id . ,rowid) new-messages)
    (run-hook-with-args 'sgn-store-new-message-functions chat-id rowid))
  (dolist (event typing)
    (run-hook-with-args 'sgn-store-typing-functions
                        (plist-get event :chat-id) (plist-get event :sender)
                        (plist-get event :started))))

(defun sgn-store-changed (&rest chat-ids)
  "Report that the stored state of CHAT-IDS changed."
  (if sgn-store--applying
      (setq sgn-store--changed (append chat-ids sgn-store--changed))
    (run-hook-with-args 'sgn-store-changed-functions chat-ids)))

(defun sgn-store--apply-event (event)
  "Store EVENT."
  (pcase (plist-get event :kind)
    ('contact (sgn-store--contact event))
    ('group (sgn-store--group event))
    ('message (sgn-store--message event))
    ('edit (sgn-store--edit event))
    ('delete (sgn-store--delete event))
    ('reaction (sgn-store--reaction event))
    ('pin (sgn-store--pin event))
    ('timer (sgn-store--timer event))
    ('read (sgn-store--read event))
    ('receipt (sgn-store--receipt event))
    ('typing (push event sgn-store--typing))))

(defun sgn-store--contact (event)
  "Record what EVENT tells about a person."
  (let ((uuid (plist-get event :uuid))
        (number (plist-get event :number))
        (name (plist-get event :name)))
    (when (and number (equal number sgn-account))
      (sgn-store-learn-self uuid))
    ;; The envelope carries the nickname or profile name; keep an
    ;; existing name, which the contact list sets more reliably.
    (unless (sgn-contacts-name-known-p uuid)
      (when name
        (sgn-db-upsert-recipient uuid number name)
        (sgn-contacts-set-name uuid name)
        (sgn-store-changed uuid)))
    (when (and number (sgn-db-merge-identity number uuid))
      (sgn-store-changed uuid))))

(defun sgn-store--group (event)
  "Record the group name EVENT reports."
  (let ((chat-id (plist-get event :chat-id))
        (name (plist-get event :name)))
    (sgn-db-ensure-chat chat-id "group")
    (unless (equal name (plist-get (sgn-db-get-chat chat-id) :name))
      (sgn-db-update-chat chat-id :name name)
      (sgn-contacts-set-name chat-id name)
      (sgn-store-changed chat-id))))

(defun sgn-store--ensure-chat (event)
  "Create the chat EVENT belongs to, if needed."
  (sgn-db-ensure-chat (plist-get event :chat-id) (plist-get event :chat-type)))

(defun sgn-store--json (value)
  "Return VALUE encoded as JSON, or nil if VALUE is nil."
  (when value (json-encode value)))

(defun sgn-store--message (event)
  "Store the message EVENT describes."
  (sgn-store--ensure-chat event)
  (let* ((chat-id (plist-get event :chat-id))
         (outgoing (plist-get event :outgoing))
         (timestamp (plist-get event :timestamp))
         (now (sgn-db-now))
         (latest (plist-get (sgn-db-get-chat chat-id) :last-msg-ts))
         (rowid (sgn-db-insert-message
                 (sgn-store--message-columns event now))))
    ;; A message delivered late carries the timer of its time, which
    ;; may have changed since.
    (when (and rowid (or (null latest) (>= timestamp latest)))
      (sgn-store--sync-chat-timer chat-id (plist-get event :expires-in)))
    (when rowid
      (dolist (media (plist-get event :attachments))
        (sgn-db-insert-media rowid media))
      (when-let* ((sticker (plist-get event :sticker)))
        (sgn-db-insert-media rowid (list :content-type "image/webp"
                                         :sticker-pack-id (plist-get sticker :pack-id)
                                         :sticker-id (plist-get sticker :sticker-id))))
      (sgn-db-touch-chat chat-id timestamp)
      (sgn-store--apply-pending chat-id (plist-get event :sender) timestamp rowid)
      (unless outgoing
        (push (cons chat-id rowid) sgn-store--new-messages))
      (sgn-store-changed chat-id))))

(defun sgn-store--message-columns (event now)
  "Return the message columns for the message EVENT, received at NOW."
  (let* ((outgoing (plist-get event :outgoing))
         (expires-in (or (plist-get event :expires-in) 0))
         (quote-data (plist-get event :quote)))
    (append
     (list :chat-id (plist-get event :chat-id)
           :sender (plist-get event :sender)
           :timestamp (plist-get event :timestamp)
           :outgoing (if outgoing 1 0)
           :body (plist-get event :body)
           :styles-json (sgn-format-ranges-to-json (plist-get event :styles))
           :mentions-json (sgn-format-ranges-to-json (plist-get event :mentions))
           :quote-ts (plist-get quote-data :ts)
           :quote-author (plist-get quote-data :author)
           :quote-body (plist-get quote-data :body)
           :expires-in expires-in
           :raw-json (sgn-store--json (plist-get event :raw)))
     ;; A message sent from this account is read, and its timer runs
     ;; from now; an incoming one's timer starts when it is read.
     (when outgoing
       (append (list :read-at now)
               (when (> expires-in 0)
                 (list :expire-started-at now
                       :expires-at (+ now (* 1000 expires-in)))))))))

(defun sgn-store--sync-chat-timer (chat-id seconds)
  "Record SECONDS as CHAT-ID's disappearing timer, if it changed.
Every message carries the timer in force when it was sent."
  (when (and seconds
             (not (eql seconds (plist-get (sgn-db-get-chat chat-id) :expiration))))
    (sgn-db-update-chat chat-id :expiration seconds)
    (sgn-store-changed chat-id)))

(defun sgn-store--timer (event)
  "Record the disappearing timer change EVENT describes."
  (sgn-store--ensure-chat event)
  (sgn-store--sync-chat-timer (plist-get event :chat-id)
                              (plist-get event :seconds)))

;;;; Mutations of earlier messages

(defun sgn-store--target (event)
  "Return the stored message EVENT's mutation applies to, or nil.
The target is the message EVENT's sender sent at its :target-ts."
  (sgn-db-get-message (plist-get event :chat-id) (plist-get event :sender)
                      (plist-get event :target-ts)))

(defun sgn-store--defer (event kind payload)
  "Keep EVENT's KIND mutation, with PAYLOAD, until its target arrives."
  (sgn-db-add-pending (plist-get event :chat-id) (plist-get event :sender)
                      (plist-get event :target-ts) kind payload
                      (plist-get event :timestamp)))

(defun sgn-store--edit (event)
  "Apply the edit EVENT describes, or keep it until its target arrives."
  (sgn-store--ensure-chat event)
  (let ((content (list :body (plist-get event :body)
                       :styles-json (sgn-format-ranges-to-json
                                     (plist-get event :styles))
                       :mentions-json (sgn-format-ranges-to-json
                                       (plist-get event :mentions))
                       :timestamp (plist-get event :timestamp))))
    (if-let* ((message (sgn-store--target event)))
        (sgn-store--apply-edit message content)
      (sgn-store--defer event "edit" (json-encode content)))))

(defun sgn-store--apply-edit (message content)
  "Replace MESSAGE's text with the edit CONTENT, unless it is older.
CONTENT is a plist with :body, :styles-json, :mentions-json and
:timestamp."
  (let ((edited-at (plist-get message :edited-at))
        (timestamp (plist-get content :timestamp)))
    (when (and (zerop (plist-get message :deleted))
               (or (null edited-at) (> timestamp edited-at)))
      (sgn-db-update-message (plist-get message :rowid)
                             :body (plist-get content :body)
                             :styles-json (plist-get content :styles-json)
                             :mentions-json (plist-get content :mentions-json)
                             :edited-at timestamp)
      (sgn-store-changed (plist-get message :chat-id)))))

(defun sgn-store--delete (event)
  "Apply the remote delete EVENT, or keep it until its target arrives."
  (sgn-store--ensure-chat event)
  (if-let* ((message (sgn-store--target event)))
      (sgn-store-delete-content message)
    (sgn-store--defer event "delete" nil)))

(defun sgn-store-delete-content (message)
  "Delete MESSAGE's content and attachments, keeping a tombstone."
  (sgn-media-delete-files (sgn-db-mark-deleted (plist-get message :rowid)))
  (sgn-store-changed (plist-get message :chat-id)))

(defun sgn-store--apply-pending (chat-id sender timestamp rowid)
  "Apply mutations that arrived before message ROWID.
The message is the one SENDER sent at TIMESTAMP in CHAT-ID."
  (pcase-dolist (`(,kind ,payload ,ts)
                 (sgn-db-take-pending chat-id sender timestamp))
    (let ((message (sgn-db-get-message-by-rowid rowid)))
      (pcase kind
        ("delete" (sgn-store-delete-content message))
        ("edit" (sgn-store--apply-edit
                 message (sgn-store--read-json-plist payload)))
        ("read" (sgn-db-mark-read rowid ts))))))

(defun sgn-store--read-json-plist (json)
  "Return the JSON object string JSON as a plist with keyword keys."
  (json-parse-string json :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))

(defun sgn-store--reaction (event)
  "Record the reaction EVENT describes."
  (sgn-store--ensure-chat event)
  (sgn-db-set-reaction (plist-get event :chat-id)
                       (plist-get event :target-author)
                       (plist-get event :target-ts)
                       (plist-get event :sender)
                       (unless (plist-get event :remove) (plist-get event :emoji))
                       (plist-get event :timestamp))
  (sgn-store-changed (plist-get event :chat-id)))

(defun sgn-store--pin (event)
  "Record the pin or unpin EVENT describes."
  (sgn-store--ensure-chat event)
  (sgn-db-set-pin (plist-get event :chat-id)
                  (plist-get event :target-author)
                  (plist-get event :target-ts)
                  (unless (plist-get event :unpin) (plist-get event :sender))
                  (plist-get event :timestamp))
  (sgn-store-changed (plist-get event :chat-id)))

(defun sgn-store--read (event)
  "Mark read the messages that EVENT says were read on another device.
Messages not stored yet are marked read when they arrive."
  (let ((now (sgn-db-now)))
    (pcase-dolist (`(,sender ,ts) (plist-get event :entries))
      (if-let* ((row (car (sgn-db-select
                           "SELECT rowid, chat_id FROM messages
                            WHERE sender = ? AND timestamp = ?"
                           (list sender ts)))))
          (when (> (sgn-db-mark-read (car row) now) 0)
            (sgn-store-changed (cadr row)))
        (sgn-db-add-pending "*" sender ts "read" nil now)))))

(defun sgn-store--receipt (event)
  "Record the receipts EVENT describes for this account's messages."
  (let ((sender (plist-get event :sender))
        (type (plist-get event :type))
        (time (plist-get event :when)))
    (dolist (ts (plist-get event :timestamps))
      (sgn-db-add-receipt ts sender type time)
      (when-let* ((message (sgn-db-find-own-message (sgn-store-self-ids) ts)))
        (sgn-store-changed (plist-get message :chat-id))))))

;;;; Expiry

(defun sgn-store-purge-expired ()
  "Delete messages whose disappearing timer ran out."
  (let ((chats nil))
    (sgn-db-with-transaction
      (pcase-dolist (`(,rowid ,chat-id) (sgn-db-expired-messages (sgn-db-now)))
        (sgn-media-delete-files (sgn-db-delete-message rowid))
        (push chat-id chats)))
    (when chats
      (run-hook-with-args 'sgn-store-changed-functions (delete-dups chats)))))

(provide 'sgn-store)
;;; sgn-store.el ends here
