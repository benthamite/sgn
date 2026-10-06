;;; sgn-actions.el --- Sending and message actions for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Everything sgn sends: messages, attachments, edits, reactions,
;; deletions, pins, forwards, and chat settings.
;;
;; A sent message is stored at once with send status "sending" and
;; the local time as its timestamp.  signal-cli's reply replaces the
;; timestamp with the one Signal assigned, which other clients use to
;; refer to the message, and sets the status to "sent", "partial" or
;; "failed".  Without a reply within `sgn-send-timeout' seconds, or if
;; signal-cli stops before replying, the status becomes "unconfirmed":
;; the message may or may not have gone out.
;;
;; Other actions are applied locally when signal-cli confirms them;
;; reactions are shown at once and rolled back if signal-cli rejects
;; them.  Actions refuse to target a message whose Signal timestamp is
;; not known, since other clients could not find it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'mailcap)
(require 'sgn-db)
(require 'sgn-format)
(require 'sgn-media)
(require 'sgn-contacts)

(declare-function sgn--log "sgn")
(declare-function sgn-ensure-running "sgn")
(declare-function sgn-rpc-address "sgn-rpc")
(declare-function sgn-rpc-send "sgn-rpc")
(declare-function sgn-rpc-send-message "sgn-rpc")
(declare-function sgn-rpc-send-reaction "sgn-rpc")
(declare-function sgn-rpc-remote-delete "sgn-rpc")
(declare-function sgn-rpc-send-pin "sgn-rpc")
(declare-function sgn-store-self "sgn-store")
(declare-function sgn-store-self-p "sgn-store")
(declare-function sgn-store-changed "sgn-store")
(declare-function sgn-store-delete-content "sgn-store")
(declare-function sgn-chat-message-at-point "sgn-chat")
(declare-function sgn-chat-start-reply "sgn-chat")
(declare-function sgn-chat-start-edit "sgn-chat")
(declare-function sgn-chat-open "sgn-chat")
(declare-function sgn-chat-format-duration "sgn-chat")

(defvar sgn-account)
(defvar sgn-chat-id)

;;;; Customization

(defcustom sgn-send-timeout 30
  "Seconds to wait for signal-cli to confirm a sent message.
A message still unconfirmed after this long is marked as such.
It is marked as sent if confirmation arrives later."
  :type 'integer
  :group 'sgn)

;;;; Addressing

(defun sgn-chat-address (chat-id)
  "Return the RPC parameters addressing CHAT-ID."
  (if (equal (sgn-db-chat-type chat-id) "group")
      (sgn-rpc-address chat-id t)
    (sgn-rpc-address (if (sgn-store-self-p chat-id) sgn-account chat-id) nil)))

(defun sgn-actions--text-style-param (styles)
  "Return STYLES as the strings signal-cli's `textStyle' expects.
STYLES is a list of (STYLE START LENGTH) in UTF-16 units; signal-cli
reads each as \"START:LENGTH:STYLE\"."
  (vconcat (mapcar (pcase-lambda (`(,style ,start ,length))
                     (format "%d:%d:%s" start length style))
                   styles)))

;;;; Sending messages

(defun sgn-send-text (chat-id text &optional reply-to)
  "Send TEXT, which may contain markup, to CHAT-ID.
REPLY-TO, a message plist, makes it a reply to that message."
  (let ((parsed (sgn-format-parse-markup text)))
    (sgn-actions--send chat-id (plist-get parsed :text)
                       :styles (plist-get parsed :styles)
                       :quote reply-to)))

(defun sgn-attach-file (file &optional caption)
  "Send FILE to the current chat, with CAPTION as its text."
  (interactive (list (read-file-name "Attachment: " nil nil t)
                     (let ((caption (read-string "Caption (optional): ")))
                       (unless (string-empty-p caption) caption))))
  (unless sgn-chat-id
    (user-error "Not in a Signal chat buffer"))
  (let ((path (expand-file-name file)))
    (unless (file-regular-p path)
      (user-error "Not a file: %s" path))
    (sgn-actions--send sgn-chat-id caption :files (list path))))

(cl-defun sgn-actions--send (chat-id body &key styles quote files)
  "Send BODY with STYLES, quoting QUOTE and attaching FILES, to CHAT-ID.
BODY may be nil when FILES are sent.  The message is stored and
shown at once, with send status \"sending\"."
  (sgn-ensure-running)
  (let* ((now (sgn-db-now))
         (rowid (sgn-actions--store-outgoing chat-id body styles quote files now))
         (extras (append
                  (when styles
                    `((textStyle . ,(sgn-actions--text-style-param styles))))
                  (when quote
                    `((quoteTimestamp . ,(plist-get quote :timestamp))
                      (quoteAuthor . ,(sgn-actions--wire-id (plist-get quote :sender)))
                      (quoteMessage . ,(or (plist-get quote :body) ""))))
                  (when files
                    `((attachments . ,(vconcat files)))))))
    (sgn-store-changed chat-id)
    (condition-case err
        (sgn-rpc-send-message
         (sgn-chat-address chat-id) body extras
         (lambda (result) (sgn-actions--sent rowid chat-id result))
         (lambda (error-obj) (sgn-actions--send-failed rowid chat-id error-obj)))
      (error
       (sgn-actions--set-status rowid chat-id "failed")
       (signal (car err) (cdr err))))
    (run-at-time sgn-send-timeout nil #'sgn-actions--time-out rowid chat-id)
    rowid))

(defun sgn-actions--wire-id (id)
  "Return ID as signal-cli should receive it: this account by number."
  (if (sgn-store-self-p id) sgn-account id))

(defun sgn-actions--store-outgoing (chat-id body styles quote files now)
  "Store a message being sent to CHAT-ID at NOW; return its rowid.
BODY, STYLES, QUOTE and FILES are as in `sgn-actions--send'.  The
message gets a timestamp no other message from us in the chat has,
so that two sent within the same millisecond are both kept."
  (sgn-db-with-transaction
    (sgn-db-ensure-chat chat-id "individual")
    (let* ((expires-in (or (plist-get (sgn-db-get-chat chat-id) :expiration) 0))
           (ts now)
           (rowid nil))
      (while (not (setq rowid
                        (sgn-db-insert-message
                         (append
                          (list :chat-id chat-id :sender (sgn-store-self)
                                :timestamp ts :outgoing 1 :body body
                                :styles-json (sgn-format-ranges-to-json styles)
                                :quote-ts (plist-get quote :timestamp)
                                :quote-author (plist-get quote :sender)
                                :quote-body (plist-get quote :body)
                                :expires-in expires-in
                                :send-status "sending" :read-at now)
                          (when (> expires-in 0)
                            (list :expire-started-at now
                                  :expires-at (+ now (* 1000 expires-in))))))))
        (cl-incf ts))
      (dolist (file files)
        (sgn-db-insert-media rowid
                             (list :local-path file
                                   :file-name (file-name-nondirectory file)
                                   :content-type (mailcap-file-name-to-mime-type
                                                  file))))
      (sgn-db-touch-chat chat-id now)
      rowid)))

(defun sgn-actions--set-status (rowid chat-id status &rest attrs)
  "Set the send STATUS and other ATTRS of message ROWID in CHAT-ID.
If ATTRS cannot be stored, for instance because another message
already has the timestamp they give, only STATUS is."
  (when (and (bound-and-true-p sgn-db--connection)
             (sgn-db-get-message-by-rowid rowid))
    (condition-case err
        (apply #'sgn-db-update-message rowid :send-status status attrs)
      (sqlite-error
       (sgn--log "Could not update message %s with %S: %S" rowid attrs err)
       (sgn-db-update-message rowid :send-status status)))
    (sgn-store-changed chat-id)))

(defun sgn-actions-result-status (result)
  "Return the send status for a signal-cli send RESULT.
RESULT lists the outcome for each recipient: the status is \"sent\"
when every recipient succeeded, \"failed\" when none did, and
\"partial\" otherwise."
  (let* ((types (mapcar (lambda (r) (alist-get 'type r))
                        (alist-get 'results result)))
         (successes (cl-count "SUCCESS" types :test #'equal)))
    (cond ((= successes (length types)) "sent")
          ((zerop successes) "failed")
          (t "partial"))))

(defun sgn-actions--sent (rowid chat-id result)
  "Record the outcome RESULT of sending message ROWID to CHAT-ID.
The message takes the timestamp Signal assigned."
  (let ((timestamp (alist-get 'timestamp result))
        (status (sgn-actions-result-status result)))
    (when (equal status "failed")
      (sgn-actions--report-failure result))
    (if timestamp
        (sgn-actions--set-status rowid chat-id status :timestamp timestamp)
      (sgn-actions--set-status rowid chat-id status))))

(defun sgn-actions--report-failure (result)
  "Show why the send with RESULT failed for its recipients."
  (message "sgn: message not sent: %s"
           (mapconcat (lambda (r) (or (alist-get 'type r) "error"))
                      (alist-get 'results result) ", ")))

(defun sgn-actions--send-failed (rowid chat-id error-obj)
  "Record that sending message ROWID to CHAT-ID failed with ERROR-OBJ.
A send abandoned when signal-cli stopped may still have gone out."
  (if (alist-get 'abandoned error-obj)
      (sgn-actions--set-status rowid chat-id "unconfirmed")
    (message "sgn: message not sent: %s" (alist-get 'message error-obj))
    (sgn-actions--set-status rowid chat-id "failed")))

(defun sgn-actions--time-out (rowid chat-id)
  "Mark message ROWID in CHAT-ID unconfirmed if it is still sending."
  (when (and (bound-and-true-p sgn-db--connection)
             (equal (plist-get (sgn-db-get-message-by-rowid rowid) :send-status)
                    "sending"))
    (sgn-actions--set-status rowid chat-id "unconfirmed")))

;;;; Targeting messages

(defun sgn-actions--message-at-point ()
  "Return the message at point, or signal an error."
  (or (sgn-chat-message-at-point)
      (user-error "No message at point")))

(defun sgn-actions--targetable (msg)
  "Return MSG if other clients can identify it, else signal an error.
A message sgn sent is identifiable once Signal confirmed its
timestamp."
  (when (eql (plist-get msg :deleted) 1)
    (user-error "This message was deleted"))
  (when (member (plist-get msg :send-status) '("sending" "unconfirmed" "failed"))
    (user-error "This message was not confirmed as sent"))
  msg)

(defun sgn-actions--own (msg)
  "Return MSG if this account sent it, else signal an error."
  (unless (eql (plist-get msg :outgoing) 1)
    (user-error "You can only do that to your own messages"))
  msg)

;;;; Replying and editing

;;;###autoload
(defun sgn-reply ()
  "Reply to the message at point."
  (interactive)
  (let ((msg (sgn-actions--targetable (sgn-actions--message-at-point))))
    (sgn-chat-start-reply msg)
    (message "Replying to %s; C-g to cancel"
             (sgn-contacts-display-sender (plist-get msg :sender)))))

;;;###autoload
(defun sgn-edit ()
  "Edit your message at point."
  (interactive)
  (let ((msg (sgn-actions--own (sgn-actions--targetable
                                (sgn-actions--message-at-point)))))
    (unless (plist-get msg :body)
      (user-error "This message has no text to edit"))
    (sgn-chat-start-edit msg (sgn-format-to-markup
                              (plist-get msg :body)
                              (sgn-format-read-ranges (plist-get msg :styles-json))))))

(defun sgn-send-edit (msg text)
  "Replace the text of MSG, a message we sent, with TEXT.
The new text is shown at once, with an edit status, and restored
if signal-cli rejects it.  The message's own send status is left
alone: the message was sent whatever becomes of the edit."
  (sgn-ensure-running)
  (let* ((parsed (sgn-format-parse-markup text))
         (body (plist-get parsed :text))
         (styles (plist-get parsed :styles))
         (rowid (plist-get msg :rowid))
         (chat-id (plist-get msg :chat-id))
         (old (list :body (plist-get msg :body)
                    :styles-json (plist-get msg :styles-json)
                    :edited-at (plist-get msg :edited-at)
                    :edit-status (plist-get msg :edit-status))))
    (sgn-db-update-message rowid :body body
                           :styles-json (sgn-format-ranges-to-json styles)
                           :edited-at (sgn-db-now)
                           :edit-status "sending")
    (sgn-store-changed chat-id)
    (sgn-rpc-send-message
     (sgn-chat-address chat-id) body
     (append `((editTimestamp . ,(plist-get msg :timestamp)))
             (when styles
               `((textStyle . ,(sgn-actions--text-style-param styles)))))
     (lambda (result)
       (let ((status (sgn-actions-result-status result)))
         (if (equal status "failed")
             (sgn-actions--restore rowid chat-id old "Edit not sent")
           (sgn-actions--set-edit-status
            rowid chat-id (unless (equal status "sent") status)
            :edited-at (alist-get 'timestamp result)))))
     (lambda (error-obj)
       (if (alist-get 'abandoned error-obj)
           (sgn-actions--set-edit-status rowid chat-id "unconfirmed")
         (sgn-actions--restore rowid chat-id old
                               (format "Edit not sent: %s"
                                       (alist-get 'message error-obj))))))
    (run-at-time sgn-send-timeout nil #'sgn-actions--edit-timed-out
                 rowid chat-id)))

(defun sgn-actions--set-edit-status (rowid chat-id status &rest attrs)
  "Set the edit STATUS and other ATTRS of message ROWID in CHAT-ID."
  (when (and (bound-and-true-p sgn-db--connection)
             (sgn-db-get-message-by-rowid rowid))
    (condition-case err
        (apply #'sgn-db-update-message rowid :edit-status status attrs)
      (sqlite-error
       (sgn--log "Could not update message %s with %S: %S" rowid attrs err)
       (sgn-db-update-message rowid :edit-status status)))
    (sgn-store-changed chat-id)))

(defun sgn-actions--edit-timed-out (rowid chat-id)
  "Mark the edit of message ROWID in CHAT-ID unconfirmed if still sending."
  (when (and (bound-and-true-p sgn-db--connection)
             (equal (plist-get (sgn-db-get-message-by-rowid rowid) :edit-status)
                    "sending"))
    (sgn-actions--set-edit-status rowid chat-id "unconfirmed")))

(defun sgn-actions--restore (rowid chat-id attrs explanation)
  "Restore ATTRS of message ROWID in CHAT-ID and show EXPLANATION."
  (apply #'sgn-db-update-message rowid attrs)
  (sgn-store-changed chat-id)
  (message "sgn: %s" explanation))

;;;; Reactions

(defconst sgn-actions--common-emoji
  '(("👍" . "thumbs up") ("👎" . "thumbs down") ("❤️" . "heart")
    ("😂" . "laughing") ("😮" . "surprised") ("😢" . "sad") ("😡" . "angry")
    ("🔥" . "fire") ("🎉" . "party") ("💯" . "100") ("🤔" . "thinking")
    ("👏" . "clap") ("👀" . "eyes") ("🙏" . "pray") ("👌" . "ok")
    ("👋" . "wave") ("✅" . "check") ("❌" . "cross") ("⭐" . "star")
    ("🚀" . "rocket"))
  "Emoji offered for reactions, with their names.")

(defun sgn-actions--read-emoji ()
  "Read a reaction emoji: a listed one, or any emoji typed in."
  (let* ((candidates (mapcar (lambda (e) (format "%s %s" (car e) (cdr e)))
                             sgn-actions--common-emoji))
         (choice (string-trim (completing-read "React: " candidates)))
         (listed (cl-position choice candidates :test #'equal)))
    (cond
     (listed (car (nth listed sgn-actions--common-emoji)))
     ((and (not (string-empty-p choice))
           (not (string-match-p "[[:alnum:][:space:]]" choice)))
      choice)
     (t (user-error "Not an emoji: %s" choice)))))

;;;###autoload
(defun sgn-react ()
  "React to the message at point, or remove your reaction to it."
  (interactive)
  (let* ((msg (sgn-actions--targetable (sgn-actions--message-at-point)))
         (chat-id (plist-get msg :chat-id))
         (author (plist-get msg :sender))
         (ts (plist-get msg :timestamp))
         (mine (cadr (cl-find-if (lambda (r) (sgn-store-self-p (car r)))
                                 (sgn-db-get-reactions chat-id author ts))))
         (emoji (or mine (sgn-actions--read-emoji))))
    (sgn-ensure-running)
    (sgn-db-set-reaction chat-id author ts (sgn-store-self)
                         (unless mine emoji) (sgn-db-now))
    (sgn-store-changed chat-id)
    (sgn-rpc-send-reaction
     (sgn-chat-address chat-id) emoji (sgn-actions--wire-id author) ts mine
     nil
     (lambda (error-obj)
       (unless (alist-get 'abandoned error-obj)
         (sgn-db-set-reaction chat-id author ts (sgn-store-self) mine
                              (sgn-db-now))
         (sgn-store-changed chat-id)
         (message "sgn: reaction not sent: %s" (alist-get 'message error-obj)))))))

;;;; Deleting and pinning

;;;###autoload
(defun sgn-delete (&optional locally)
  "Delete your message at point for everyone.
With prefix argument LOCALLY, or for a message that was not
confirmed as sent, delete it from sgn only."
  (interactive "P")
  (let ((msg (sgn-actions--message-at-point)))
    (if (or locally
            (member (plist-get msg :send-status) '("failed" "unconfirmed")))
        (sgn-actions--delete-locally msg)
      (sgn-actions--delete-for-everyone
       (sgn-actions--own (sgn-actions--targetable msg))))))

(defun sgn-actions--delete-locally (msg)
  "Delete MSG from sgn's database, after confirmation."
  (when (y-or-n-p "Delete this message from sgn only? ")
    (sgn-media-delete-files (sgn-db-delete-message (plist-get msg :rowid)))
    (sgn-store-changed (plist-get msg :chat-id))
    (message "Message deleted from sgn.")))

(defun sgn-actions--delete-for-everyone (msg)
  "Delete MSG, which we sent, for everyone, after confirmation."
  (when (y-or-n-p "Delete this message for everyone? ")
    (sgn-ensure-running)
    (sgn-rpc-remote-delete
     (sgn-chat-address (plist-get msg :chat-id)) (plist-get msg :timestamp)
     (lambda (_result)
       (sgn-store-delete-content msg)
       (message "Message deleted."))
     (lambda (error-obj)
       (message "sgn: message not deleted: %s"
                (alist-get 'message error-obj))))))

;;;###autoload
(defun sgn-toggle-pin ()
  "Pin the message at point, or unpin it."
  (interactive)
  (let* ((msg (sgn-actions--targetable (sgn-actions--message-at-point)))
         (chat-id (plist-get msg :chat-id))
         (author (plist-get msg :sender))
         (ts (plist-get msg :timestamp))
         (unpin (sgn-db-pinned-p chat-id author ts)))
    (sgn-ensure-running)
    (sgn-rpc-send-pin
     (sgn-chat-address chat-id) (sgn-actions--wire-id author) ts unpin
     (lambda (_result)
       (sgn-db-set-pin chat-id author ts (unless unpin (sgn-store-self))
                       (sgn-db-now))
       (sgn-store-changed chat-id)
       (message (if unpin "Message unpinned." "Message pinned.")))
     (lambda (error-obj)
       (message "sgn: %s failed: %s" (if unpin "unpin" "pin")
                (alist-get 'message error-obj))))))

;;;; Forwarding and copying

(defun sgn-actions--plain-text (msg)
  "Return MSG's text with mentions written as @names, or nil."
  (when-let* ((body (plist-get msg :body)))
    (substring-no-properties
     (sgn-format-render body nil
                        (sgn-format-read-ranges (plist-get msg :mentions-json))))))

;;;###autoload
(defun sgn-forward ()
  "Forward the message at point, with its attachments, to another chat."
  (interactive)
  (let* ((msg (sgn-actions--message-at-point))
         (files (delq nil (mapcar #'sgn-media-path
                                  (sgn-db-get-media (plist-get msg :rowid)))))
         (text (sgn-actions--plain-text msg))
         (styles (unless (plist-get msg :mentions-json)
                   (sgn-format-read-ranges (plist-get msg :styles-json)))))
    (when (eql (plist-get msg :deleted) 1)
      (user-error "This message was deleted"))
    (unless (or text files)
      (user-error "Nothing to forward"))
    (let ((target (sgn-contacts-completing-read "Forward to: ")))
      (sgn-actions--send target text :styles styles :files files)
      (message "Forwarded to %s" (sgn-contacts-get-name target)))))

;;;###autoload
(defun sgn-copy-text ()
  "Copy the text of the message at point."
  (interactive)
  (if-let* ((text (sgn-actions--plain-text (sgn-actions--message-at-point))))
      (progn (kill-new text)
             (message "Message text copied."))
    (user-error "This message has no text")))

;;;; Chat settings

;;;###autoload
(defun sgn-set-disappearing (seconds)
  "Make new messages in the current chat disappear after SECONDS; 0 disables."
  (interactive "nDisappearing timer (seconds, 0 to disable): ")
  (unless sgn-chat-id
    (user-error "Not in a Signal chat buffer"))
  (sgn-ensure-running)
  (let ((chat-id sgn-chat-id))
    (sgn-rpc-send (if (equal (sgn-db-chat-type chat-id) "group")
                      "updateGroup"
                    "updateContact")
                  (if (equal (sgn-db-chat-type chat-id) "group")
                      `((groupId . ,chat-id) (expiration . ,seconds))
                    `((recipient . ,(sgn-actions--wire-id chat-id))
                      (expiration . ,seconds)))
                  (lambda (_result)
                    (sgn-db-update-chat chat-id :expiration seconds)
                    (sgn-store-changed chat-id)
                    (message "Disappearing messages: %s"
                             (if (zerop seconds) "off"
                               (sgn-chat-format-duration seconds))))
                  (lambda (error-obj)
                    (message "sgn: timer not changed: %s"
                             (alist-get 'message error-obj))))))

;;;###autoload
(defun sgn-create-group (name members)
  "Create a Signal group called NAME with MEMBERS, a list of chat IDs."
  (interactive
   (let ((name (read-string "Group name: "))
         (members nil)
         (member nil))
     (while (setq member (sgn-contacts-completing-read
                          (format "Member %d (empty to finish): "
                                  (1+ (length members)))
                          t))
       (push member members))
     (list name (nreverse members))))
  (sgn-ensure-running)
  (sgn-rpc-send "updateGroup"
                `((name . ,name)
                  (member . ,(vconcat (mapcar #'sgn-actions--wire-id members))))
                (lambda (result)
                  (when-let* ((id (alist-get 'groupId result)))
                    (sgn-db-ensure-chat id "group")
                    (sgn-db-update-chat id :name name)
                    (sgn-contacts-set-name id name)
                    (sgn-chat-open id))
                  (message "Group \"%s\" created." name))
                (lambda (error-obj)
                  (message "sgn: group not created: %s"
                           (alist-get 'message error-obj)))))

(defun sgn-actions--block (chat-id unblock)
  "Block CHAT-ID, or with UNBLOCK non-nil unblock it."
  (sgn-ensure-running)
  (let ((name (sgn-contacts-get-name chat-id)))
    (sgn-rpc-send (if unblock "unblock" "block")
                  (if (equal (sgn-db-chat-type chat-id) "group")
                      `((groupId . ,(vector chat-id)))
                    `((recipient . ,(vector (sgn-actions--wire-id chat-id)))))
                  (lambda (_result)
                    (message "%s %s." (if unblock "Unblocked" "Blocked") name))
                  (lambda (error-obj)
                    (message "sgn: could not %s %s: %s"
                             (if unblock "unblock" "block") name
                             (alist-get 'message error-obj))))))

;;;###autoload
(defun sgn-block-contact (chat-id)
  "Block the contact or group CHAT-ID."
  (interactive (list (sgn-contacts-completing-read "Block: ")))
  (when (y-or-n-p (format "Block %s? " (sgn-contacts-get-name chat-id)))
    (sgn-actions--block chat-id nil)))

;;;###autoload
(defun sgn-unblock-contact (chat-id)
  "Unblock the contact or group CHAT-ID."
  (interactive (list (sgn-contacts-completing-read "Unblock: ")))
  (sgn-actions--block chat-id t))

(provide 'sgn-actions)
;;; sgn-actions.el ends here
