;;; sgn.el --- Signal client via signal-cli JSON-RPC  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>
;; URL: https://github.com/benthamite/sgn
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Sgn provides a full-featured Signal messenger client for Emacs,
;; communicating with a running `signal-cli' daemon via JSON-RPC.
;;
;; Features:
;; - Telega-style chat buffers with message grouping and text properties.
;; - SQLite persistence for message history and full-text search (FTS5).
;; - Inline image/sticker rendering, voice notes, link previews.
;; - Reactions, replies, message editing and deletion.
;; - Dashboard (chat list) with unread counts and pinned chats.
;; - Desktop notifications and modeline/tab-bar unread indicator.
;;
;; Prerequisites:
;; 1. Emacs 29.1+ compiled with SQLite support.
;; 2. signal-cli v0.14+ installed and in $PATH.
;; 3. A registered/linked Signal account.
;; 4. Optional: ImageMagick (`convert') for animated stickers.

;;; Code:

(require 'cl-lib)
(require 'json)

;;;; Custom group

(defgroup sgn nil
  "Signal client for Emacs using signal-cli."
  :group 'comm
  :prefix "sgn-")

;;;; Core customization

(defcustom sgn-account nil
  "The registered Signal phone number (e.g. +15550000000).
This must match the account registered with signal-cli."
  :type '(choice (const :tag "Not set" nil) string)
  :group 'sgn)

(defcustom sgn-cli-program (or (executable-find "signal-cli") "signal-cli")
  "Path to the signal-cli executable."
  :type 'file
  :group 'sgn)

(defcustom sgn-data-directory (expand-file-name "~/.local/share/signal-cli")
  "Directory where signal-cli stores data (attachments, stickers, etc.)."
  :type 'directory
  :group 'sgn)

(defcustom sgn-auto-open-buffer nil
  "If non-nil, auto-switch to chat buffer on incoming message."
  :type 'boolean
  :group 'sgn)

(defcustom sgn-send-read-receipts t
  "If non-nil, send focus-gated read receipts via `sendReceipt'."
  :type 'boolean
  :group 'sgn)

(defcustom sgn-cli-auto-read-receipts nil
  "If non-nil, pass `--send-read-receipts' to signal-cli.
When enabled, signal-cli sends read receipts for every incoming
data message regardless of Emacs focus."
  :type 'boolean
  :group 'sgn)

(defcustom sgn-send-typing t
  "If non-nil, send typing indicators."
  :type 'boolean
  :group 'sgn)

;;;; Logging

(defun sgn--log (fmt &rest args)
  "Log debug info to *sgn-log* using FMT and ARGS."
  (with-current-buffer (get-buffer-create "*sgn-log*")
    (goto-char (point-max))
    (insert (format-time-string "[%H:%M:%S] "))
    (insert (apply #'format fmt args))
    (insert "\n")))

;;;###autoload
(defun sgn-show-log ()
  "Display the debug log buffer."
  (interactive)
  (display-buffer (get-buffer-create "*sgn-log*")))

;;;; Require submodules (after sgn--log is defined)

(require 'sgn-db)
(require 'sgn-rpc)
(require 'sgn-contacts)
(require 'sgn-media)
(require 'sgn-format)
(require 'sgn-chat)
(require 'sgn-actions)
(require 'sgn-notify)
(require 'sgn-search)
(require 'sgn-import)
(require 'sgn-dashboard)

;;;; Utility

(defun sgn--ensure-list (value)
  "Coerce VALUE to a list if it is a vector."
  (if (vectorp value) (append value nil) value))

(defun sgn--is-group-id (id)
  "Return non-nil if ID looks like a Signal group ID."
  (not (or (string-prefix-p "+" id)
           (string-match-p "-" id))))

;;;; Receive dispatch

(defun sgn--handle-receive (params)
  "Handle incoming messages from signal-cli.
PARAMS is the parsed JSON params from the \"receive\" method."
  (let* ((envelope (alist-get 'envelope params))
         (source (or (alist-get 'sourceNumber envelope)
                     (alist-get 'source envelope)))
         (source-name (alist-get 'sourceName envelope))
         (data (alist-get 'dataMessage envelope))
         (sync (alist-get 'syncMessage envelope))
         (typing (alist-get 'typingMessage envelope))
         (receipt (alist-get 'receiptMessage envelope)))
    ;; Update contact name cache
    (when (and source source-name)
      (sgn-contacts-set-name source source-name))
    ;; Dispatch by message type
    (cond
     (data (sgn--handle-data-message envelope source data))
     (sync (sgn--handle-sync-message envelope sync))
     (typing (sgn--handle-typing envelope source typing))
     (receipt (sgn--handle-receipt-message envelope source receipt)))))

;;;; Data message handling

(defun sgn--handle-data-message (envelope source data)
  "Handle an incoming data message.
ENVELOPE is the full envelope, SOURCE is the sender, DATA is the dataMessage."
  (let* ((group-info (alist-get 'groupInfo data))
         (chat-id (if group-info (alist-get 'groupId group-info) source))
         (timestamp (alist-get 'timestamp data))
         (msg-text (alist-get 'message data))
         (attachments (alist-get 'attachments data))
         (sticker (alist-get 'sticker data))
         (reaction (alist-get 'reaction data))
         (remote-delete (alist-get 'remoteDelete data))
         (edit-ts (alist-get 'editTimestamp data))
         (quote-data (alist-get 'quote data))
         (styles (alist-get 'textStyles data))
         (mentions (alist-get 'mentions data))
         (poll-data (alist-get 'poll data))
         (pin-data (alist-get 'pinMessage data)))
    ;; Ensure chat exists in DB
    (sgn-db-upsert-chat chat-id
                        :type (if group-info "group" "individual")
                        :last-msg-ts timestamp)
    (cond
     ;; Reaction
     (reaction
      (sgn--handle-incoming-reaction chat-id source reaction))
     ;; Remote delete
     (remote-delete
      (sgn--handle-incoming-delete chat-id remote-delete))
     ;; Edit
     (edit-ts
      (sgn--handle-incoming-edit chat-id source data edit-ts))
     ;; Poll
     (poll-data
      (sgn--handle-incoming-poll chat-id source timestamp poll-data envelope))
     ;; Pin
     (pin-data
      (sgn--handle-incoming-pin chat-id source pin-data))
     ;; Normal message (text, media, sticker)
     ((or msg-text attachments sticker)
      (sgn--handle-incoming-message
       chat-id source timestamp msg-text attachments sticker
       quote-data styles mentions envelope)))
    ;; Update dashboard
    (sgn-dashboard-refresh)
    ;; Update unread & notifications
    (sgn--update-unread-for-message chat-id source msg-text sticker attachments)))

(defun sgn--handle-incoming-message (chat-id sender timestamp body
                                              attachments sticker
                                              quote-data styles mentions
                                              envelope)
  "Persist and display an incoming message."
  (let* ((quote-ts (and quote-data (alist-get 'id quote-data)))
         (quote-author (and quote-data (alist-get 'author quote-data)))
         (quote-body (and quote-data (alist-get 'text quote-data)))
         (styles-json (when styles (json-encode styles)))
         (raw-json (json-encode envelope))
         (rowid (sgn-db-insert-message
                 (list :chat-id chat-id
                       :sender sender
                       :timestamp timestamp
                       :body body
                       :type "data"
                       :quote-ts quote-ts
                       :quote-author quote-author
                       :quote-body quote-body
                       :styles-json styles-json
                       :raw-json raw-json))))
    ;; Store attachments
    (when (and rowid attachments)
      (dolist (att (sgn--ensure-list attachments))
        (sgn-db-insert-media
         (list :message-rowid rowid
               :chat-id chat-id
               :content-type (alist-get 'contentType att)
               :file-path (or (alist-get 'storedFilename att)
                              (let ((aid (alist-get 'id att)))
                                (when aid
                                  (expand-file-name
                                   (format "attachments/%s" aid)
                                   sgn-data-directory))))
               :file-name (alist-get 'filename att)
               :is-voice (if (alist-get 'voiceNote att) 1 0)
               :width (alist-get 'width att)
               :height (alist-get 'height att)))))
    ;; Store sticker as media
    (when (and rowid sticker)
      (let* ((pack-id (alist-get 'packId sticker))
             (sticker-id (alist-get 'stickerId sticker))
             (file (when (and pack-id sticker-id)
                     (sgn-media-find-sticker pack-id sticker-id))))
        (sgn-db-insert-media
         (list :message-rowid rowid
               :chat-id chat-id
               :content-type "image/png"
               :file-path file
               :is-sticker 1))))
    ;; Render in chat buffer
    (when rowid
      (let ((msg (sgn-db-get-message-by-rowid rowid)))
        (when msg
          (sgn-chat-insert-message msg))))
    ;; Auto-open buffer
    (when (and sgn-auto-open-buffer body)
      (display-buffer (sgn-chat-get-buffer chat-id)))))

(defun sgn--handle-incoming-reaction (chat-id sender reaction)
  "Handle an incoming REACTION in CHAT-ID from SENDER."
  (let* ((emoji (alist-get 'emoji reaction))
         (target-author (alist-get 'targetAuthor reaction))
         (target-ts (alist-get 'targetTimestamp reaction))
         (is-remove (alist-get 'isRemove reaction))
         (msg (sgn-db-get-message chat-id target-author target-ts)))
    (when msg
      (let ((rowid (plist-get msg :rowid)))
        (if is-remove
            (sgn-db-remove-reaction chat-id target-author target-ts sender)
          (sgn-db-upsert-reaction
           (list :message-rowid rowid
                 :chat-id chat-id
                 :target-author target-author
                 :target-timestamp target-ts
                 :sender sender
                 :emoji emoji
                 :removed 0)))
        ;; Re-render message
        (when-let* ((buf (get-buffer
                          (format "*Sgn: %s*"
                                  (sgn-contacts-get-name chat-id)))))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (sgn-chat-update-message rowid))))))))

(defun sgn--handle-incoming-delete (chat-id remote-delete)
  "Handle a remote delete in CHAT-ID."
  (let* ((target-ts (alist-get 'timestamp remote-delete))
         ;; Find the message by timestamp in this chat
         (msgs (sgn-db-get-messages chat-id 1000))
         (msg (cl-find-if (lambda (m)
                            (equal (plist-get m :timestamp) target-ts))
                          msgs)))
    (when msg
      (let ((rowid (plist-get msg :rowid)))
        (sgn-db-delete-message rowid)
        (when-let* ((buf (get-buffer
                          (format "*Sgn: %s*"
                                  (sgn-contacts-get-name chat-id)))))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (sgn-chat-update-message rowid))))))))

(defun sgn--handle-incoming-edit (chat-id sender data edit-ts)
  "Handle an incoming edit to a message at EDIT-TS in CHAT-ID from SENDER."
  (let* ((new-body (alist-get 'message data))
         (new-styles (alist-get 'textStyles data))
         (styles-json (when new-styles (json-encode new-styles)))
         ;; Find original message
         (msg (sgn-db-get-message chat-id sender edit-ts)))
    (when msg
      (let ((rowid (plist-get msg :rowid))
            (now-ms (truncate (* (float-time) 1000))))
        (sgn-db-update-message rowid
                               (list :body new-body
                                     :edited-at now-ms
                                     :styles-json styles-json))
        (when-let* ((buf (get-buffer
                          (format "*Sgn: %s*"
                                  (sgn-contacts-get-name chat-id)))))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (sgn-chat-update-message rowid))))))))

(defun sgn--handle-incoming-poll (chat-id sender timestamp poll-data envelope)
  "Handle an incoming poll in CHAT-ID from SENDER."
  (let* ((question (alist-get 'question poll-data))
         (options (alist-get 'options poll-data))
         (raw-json (json-encode envelope))
         (rowid (sgn-db-insert-message
                 (list :chat-id chat-id
                       :sender sender
                       :timestamp timestamp
                       :body (format "📊 %s" question)
                       :type "data"
                       :raw-json raw-json))))
    (when rowid
      (sgn-db-upsert-poll
       (list :message-rowid rowid
             :chat-id chat-id
             :poll-author sender
             :poll-timestamp timestamp
             :question question
             :options-json (json-encode options))))))

(defun sgn--handle-incoming-pin (chat-id sender pin-data)
  "Handle a pin/unpin message in CHAT-ID from SENDER."
  (let* ((target-author (alist-get 'targetAuthor pin-data))
         (target-ts (alist-get 'targetTimestamp pin-data))
         (is-unpin (alist-get 'isUnpin pin-data))
         (msg (sgn-db-get-message chat-id target-author target-ts)))
    (when msg
      (let ((rowid (plist-get msg :rowid)))
        (if is-unpin
            (sgn-db-remove-pin chat-id target-author target-ts)
          (let ((now-ms (truncate (* (float-time) 1000))))
            (sgn-db-insert-pin
             (list :message-rowid rowid
                   :chat-id chat-id
                   :target-author target-author
                   :target-timestamp target-ts
                   :pinned-by sender
                   :pinned-at now-ms))))))))

;;;; Sync message handling

(defun sgn--handle-sync-message (envelope sync)
  "Handle a sync message (our own sent messages).
ENVELOPE is the full envelope, SYNC is the syncMessage."
  (let* ((sent (alist-get 'sentMessage sync))
         (sync-group (and sent (alist-get 'groupInfo sent)))
         (chat-id (or (and sync-group (alist-get 'groupId sync-group))
                      (and sent (alist-get 'destinationNumber sent))))
         (msg-text (and sent (alist-get 'message sent)))
         (timestamp (and sent (alist-get 'timestamp sent)))
         (attachments (and sent (alist-get 'attachments sent)))
         (sticker (and sent (alist-get 'sticker sent)))
         (edit-ts (and sent (alist-get 'editTimestamp sent)))
         (quote-data (and sent (alist-get 'quote sent)))
         (styles (and sent (alist-get 'textStyles sent))))
    (when (and chat-id (or msg-text attachments sticker))
      ;; Ensure chat exists
      (sgn-db-upsert-chat chat-id
                          :type (if sync-group "group" "individual")
                          :last-msg-ts timestamp)
      (cond
       ;; Edit sync
       (edit-ts
        (sgn--handle-incoming-edit chat-id sgn-account sent edit-ts))
       ;; Normal sent message
       (t
        (sgn--handle-incoming-message
         chat-id sgn-account timestamp msg-text attachments sticker
         quote-data styles nil envelope)))
      (sgn-dashboard-refresh))))

;;;; Typing message handling

(defun sgn--handle-typing (envelope source typing)
  "Handle a typing indicator from SOURCE."
  (ignore envelope)
  (let* ((action (alist-get 'action typing))
         (group-info (alist-get 'groupId typing))
         (chat-id (or group-info source)))
    (when chat-id
      (if (equal action "STARTED")
          (sgn-chat-show-typing chat-id source)
        ;; STOPPED
        (when-let* ((buf (get-buffer
                          (format "*Sgn: %s*"
                                  (sgn-contacts-get-name chat-id)))))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (sgn-chat-clear-typing))))))))

;;;; Receipt handling

(defun sgn--handle-receipt-message (envelope source receipt)
  "Handle a receipt message from SOURCE."
  (ignore envelope)
  (let* ((type-str (alist-get 'type receipt))
         (timestamps (sgn--ensure-list (alist-get 'timestamps receipt)))
         (when-ts (alist-get 'when receipt)))
    (dolist (ts timestamps)
      ;; Find the message and store the receipt
      (let ((msgs (sgn-db-get-messages source 500)))
        (when-let* ((msg (cl-find-if
                          (lambda (m) (equal (plist-get m :timestamp) ts))
                          msgs)))
          (sgn-db-upsert-receipt
           (list :message-rowid (plist-get msg :rowid)
                 :chat-id (plist-get msg :chat-id)
                 :target-author sgn-account
                 :target-timestamp ts
                 :recipient source
                 :type (or type-str "delivered")
                 :received-at (or when-ts
                                  (truncate (* (float-time) 1000))))))))))

;;;; Unread tracking

(defun sgn--update-unread-for-message (chat-id source body sticker attachments)
  "Update unread count and send notification for an incoming message."
  ;; Only increment for messages from others
  (when (and source (not (equal source sgn-account)))
    ;; Check if chat buffer is focused
    (let* ((buf (get-buffer (format "*Sgn: %s*"
                                    (sgn-contacts-get-name chat-id))))
           (focused (and buf
                         (get-buffer-window buf)
                         (eq buf (window-buffer (selected-window)))
                         (frame-focus-state))))
      (unless focused
        (sgn-db-increment-unread chat-id)
        ;; Send notification
        (let ((notify-body (cond
                            (body body)
                            (sticker "[Sticker]")
                            (attachments "[Attachment]")
                            (t "New message"))))
          (sgn-notify-message chat-id source notify-body))
        (sgn-notify-update)))))

;;;; Expiration timer

(defvar sgn--expiration-timer nil
  "Timer for periodic message expiration cleanup.")

(defun sgn--start-expiration-timer ()
  "Start the periodic expiration cleanup timer."
  (sgn--stop-expiration-timer)
  (setq sgn--expiration-timer
        (run-at-time 60 60 #'sgn-db-purge-expired)))

(defun sgn--stop-expiration-timer ()
  "Stop the expiration timer."
  (when sgn--expiration-timer
    (cancel-timer sgn--expiration-timer)
    (setq sgn--expiration-timer nil)))

;;;; Top-level commands

;;;###autoload
(defun sgn-start ()
  "Start the sgn Signal client.
Initializes the database, starts the signal-cli JSON-RPC process,
refreshes contacts, and opens the dashboard."
  (interactive)
  ;; Validate requirements
  (unless (sqlite-available-p)
    (user-error "Emacs was not compiled with SQLite support (required for sgn)"))
  (unless sgn-account
    (user-error "Variable `sgn-account' is not set"))
  (unless (executable-find sgn-cli-program)
    (user-error "signal-cli not found: %s" sgn-cli-program))
  ;; Initialize database
  (sgn-db-init)
  ;; Load contacts from DB into cache
  (sgn-contacts-load-from-db)
  ;; Set up receive handler
  (setq sgn-rpc-receive-handler #'sgn--handle-receive)
  ;; Start RPC process
  (sgn-rpc-start)
  ;; Start periodic refresh
  (sgn-contacts-start-refresh-timer)
  ;; Start expiration cleanup
  (sgn--start-expiration-timer)
  ;; Subscribe to receive messages
  (run-at-time 1 nil #'sgn-rpc-subscribe-receive)
  ;; Refresh contacts from signal-cli
  (run-at-time 2 nil #'sgn-contacts-refresh)
  ;; Enable global indicator
  (sgn-global-mode 1)
  (message "Sgn started."))

;;;###autoload
(defun sgn-stop ()
  "Stop the sgn Signal client."
  (interactive)
  (sgn-rpc-stop)
  (sgn-contacts-stop-refresh-timer)
  (sgn--stop-expiration-timer)
  (sgn-db-close)
  (sgn-global-mode -1)
  (message "Sgn stopped."))

;;;###autoload
(defun sgn-chat (recipient)
  "Open a chat with RECIPIENT.
When called interactively, prompts with completing-read over all
contacts and groups, sorted by recency."
  (interactive (list (sgn-contacts-completing-read)))
  (sgn-chat-open recipient))

;;;###autoload
(defun sgn-attach-file (file-path)
  "Send FILE-PATH as an attachment to the current chat."
  (interactive "fAttachment: ")
  (unless sgn-chat-id
    (user-error "Not in a Signal chat buffer"))
  (let ((full-path (expand-file-name file-path)))
    (sgn-rpc-send-message sgn-chat-id nil
                          `((attachments . [,full-path])))))

;;;###autoload
(defun sgn-send-voice-note ()
  "Record and send a voice note to the current chat."
  (interactive)
  (unless sgn-chat-id
    (user-error "Not in a Signal chat buffer"))
  ;; TODO: implement recording via sox
  (user-error "Voice note recording is not yet implemented"))

;;;###autoload
(defun sgn-note-to-self ()
  "Open the Note to Self chat."
  (interactive)
  (unless sgn-account
    (user-error "Variable `sgn-account' is not set"))
  (sgn-chat-open sgn-account))

;;;; Group management

;;;###autoload
(defun sgn-create-group (name members)
  "Create a new Signal group with NAME and MEMBERS.
MEMBERS is a list of phone numbers."
  (interactive
   (let* ((name (read-string "Group name: "))
          (members nil)
          (member ""))
     (while (progn
              (setq member (read-string
                            (format "Member %d (+phone, empty to finish): "
                                    (1+ (length members)))))
              (not (string-empty-p member)))
       (push member members))
     (list name (nreverse members))))
  (sgn-rpc-send "updateGroup"
                `((name . ,name)
                  (member . ,(vconcat members)))
                (lambda (result)
                  (sgn--log "Group created: %s" result)
                  (sgn-contacts-refresh)
                  (message "Group \"%s\" created." name))))

;;;###autoload
(defun sgn-set-disappearing (seconds)
  "Set the disappearing message timer for the current chat.
SECONDS is the timer duration; 0 to disable."
  (interactive "nDisappearing timer (seconds, 0=off): ")
  (unless sgn-chat-id
    (user-error "Not in a Signal chat buffer"))
  (if (sgn--is-group-id sgn-chat-id)
      (sgn-rpc-send "updateGroup"
                    `((groupId . ,sgn-chat-id)
                      (expiration . ,seconds)))
    (sgn-rpc-send "updateContact"
                  `((recipient . ,sgn-chat-id)
                    (expiration . ,seconds))))
  (sgn-db-upsert-chat sgn-chat-id :expiration seconds)
  (message "Disappearing messages: %s"
           (if (zerop seconds) "off"
             (sgn-chat--format-duration seconds))))

(declare-function sgn-chat--format-duration "sgn-chat")

;;;###autoload
(defun sgn-block-contact (contact)
  "Block CONTACT."
  (interactive (list (sgn-contacts-completing-read "Block: ")))
  (when (y-or-n-p (format "Block %s? " (sgn-contacts-get-name contact)))
    (sgn-rpc-send "block" `((recipient . [,contact])))
    (message "Blocked %s." (sgn-contacts-get-name contact))))

;;;###autoload
(defun sgn-unblock-contact (contact)
  "Unblock CONTACT."
  (interactive (list (sgn-contacts-completing-read "Unblock: ")))
  (sgn-rpc-send "unblock" `((recipient . [,contact])))
  (message "Unblocked %s." (sgn-contacts-get-name contact)))

(provide 'sgn)
;;; sgn.el ends here
