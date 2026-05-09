;;; sgn-actions.el --- Message actions for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Message actions: react, reply, edit, delete, forward, pin, copy,
;; mentions, and polls.

;;; Code:

(require 'cl-lib)

(declare-function sgn--log "sgn")
(declare-function sgn-db-get-message-by-rowid "sgn-db")
(declare-function sgn-db-upsert-reaction "sgn-db")
(declare-function sgn-db-remove-reaction "sgn-db")
(declare-function sgn-db-get-reactions "sgn-db")
(declare-function sgn-db-upsert-poll "sgn-db")
(declare-function sgn-db-get-poll "sgn-db")
(declare-function sgn-db-insert-pin "sgn-db")
(declare-function sgn-db-remove-pin "sgn-db")
(declare-function sgn-rpc-send-reaction "sgn-rpc")
(declare-function sgn-rpc-remote-delete "sgn-rpc")
(declare-function sgn-rpc-send-message "sgn-rpc")
(declare-function sgn-rpc-send-poll-create "sgn-rpc")
(declare-function sgn-rpc-send-poll-vote "sgn-rpc")
(declare-function sgn-rpc-send-pin "sgn-rpc")
(declare-function sgn-rpc-send-unpin "sgn-rpc")
(declare-function sgn-chat-message-at-point "sgn-chat")
(declare-function sgn-chat-update-message "sgn-chat")
(declare-function sgn-chat-open "sgn-chat")
(declare-function sgn-contacts-display-sender "sgn-contacts")
(declare-function sgn-contacts-completing-read "sgn-contacts")

(defvar sgn-account)
(defvar sgn-chat-id)
(defvar sgn-chat--reply-target)
(defvar sgn-chat--edit-target)
(defvar sgn-chat--input-marker)

;;;; Faces

(defface sgn-mention-face
  '((t :inherit highlight))
  "Face for @mentions in group chats."
  :group 'sgn)

(defface sgn-mention-self-face
  '((t :inherit match))
  "Face for @mentions of self."
  :group 'sgn)

;;;; Emoji data for reactions

(defconst sgn-actions--common-emoji
  '(("thumbs up" . "👍")
    ("thumbs down" . "👎")
    ("heart" . "❤️")
    ("laughing" . "😂")
    ("surprised" . "😮")
    ("sad" . "😢")
    ("angry" . "😡")
    ("fire" . "🔥")
    ("party" . "🎉")
    ("100" . "💯")
    ("thinking" . "🤔")
    ("clap" . "👏")
    ("eyes" . "👀")
    ("pray" . "🙏")
    ("ok" . "👌")
    ("wave" . "👋")
    ("check" . "✅")
    ("cross" . "❌")
    ("star" . "⭐")
    ("rocket" . "🚀"))
  "Common emoji for reaction completing-read.")

;;;; React

;;;###autoload
(defun sgn-react ()
  "React to the message at point.
If you already reacted, removes your reaction instead."
  (interactive)
  (require 'sgn)
  (let ((msg (sgn-chat-message-at-point)))
    (unless msg
      (user-error "No message at point"))
    (let* ((rowid (plist-get msg :rowid))
           (target-author (plist-get msg :target-author))
           (target-ts (plist-get msg :timestamp))
           (chat-id (plist-get msg :chat-id))
           (existing (sgn-actions--my-reaction rowid)))
      (if existing
          ;; Remove existing reaction
          (progn
            (sgn-rpc-send-reaction chat-id
                                   (plist-get existing :emoji)
                                   target-author target-ts t)
            (sgn-db-remove-reaction chat-id target-author target-ts sgn-account)
            (sgn-chat-update-message rowid)
            (message "Reaction removed."))
        ;; Add new reaction
        (let* ((candidates (mapcar (lambda (e)
                                     (format "%s %s" (cdr e) (car e)))
                                   sgn-actions--common-emoji))
               (choice (completing-read "React: " candidates nil nil))
               (emoji (or (cdr (cl-find-if
                                (lambda (e)
                                  (string-match-p (regexp-quote (cdr e)) choice))
                                sgn-actions--common-emoji))
                          ;; Allow direct emoji input
                          (string-trim choice))))
          (sgn-rpc-send-reaction chat-id emoji target-author target-ts)
          (sgn-db-upsert-reaction
           (list :message-rowid rowid
                 :chat-id chat-id
                 :target-author target-author
                 :target-timestamp target-ts
                 :sender sgn-account
                 :emoji emoji
                 :removed 0))
          (sgn-chat-update-message rowid)
          (message "Reacted with %s" emoji))))))

(defun sgn-actions--my-reaction (message-rowid)
  "Return the current user's non-removed reaction plist for MESSAGE-ROWID, or nil."
  (let ((reactions (sgn-db-get-reactions message-rowid)))
    (cl-find-if (lambda (r)
                  (equal (plist-get r :sender) sgn-account))
                reactions)))

;;;; Reply

;;;###autoload
(defun sgn-reply ()
  "Set up a reply to the message at point."
  (interactive)
  (require 'sgn)
  (let ((msg (sgn-chat-message-at-point)))
    (unless msg
      (user-error "No message at point"))
    (let ((rowid (plist-get msg :rowid)))
      (when rowid
        (let ((full-msg (sgn-db-get-message-by-rowid rowid)))
          (when full-msg
            (setq sgn-chat--reply-target
                  (list :timestamp (plist-get full-msg :timestamp)
                        :sender (plist-get full-msg :sender)
                        :body (plist-get full-msg :body)))
            (sgn-chat--redraw-prompt)
            (goto-char (point-max))
            (message "Replying to %s. C-g to cancel."
                     (sgn-contacts-display-sender
                      (plist-get full-msg :sender)))))))))

(declare-function sgn-chat--redraw-prompt "sgn-chat")

;;;; Edit

;;;###autoload
(defun sgn-edit ()
  "Edit your own message at point."
  (interactive)
  (require 'sgn)
  (let ((msg (sgn-chat-message-at-point)))
    (unless msg
      (user-error "No message at point"))
    (unless (equal (plist-get msg :sender) sgn-account)
      (user-error "You can only edit your own messages"))
    (let* ((rowid (plist-get msg :rowid))
           (full-msg (sgn-db-get-message-by-rowid rowid))
           (body (and full-msg (plist-get full-msg :body))))
      (unless body
        (user-error "No text to edit"))
      (setq sgn-chat--edit-target
            (list :timestamp (plist-get full-msg :timestamp)
                  :rowid rowid
                  :body body))
      (sgn-chat--redraw-prompt)
      ;; Pre-fill input with existing text
      (goto-char (point-max))
      (insert body)
      (message "Editing message. C-g to cancel."))))

;;;; Delete

;;;###autoload
(defun sgn-delete ()
  "Delete your own message at point (remote delete)."
  (interactive)
  (require 'sgn)
  (let ((msg (sgn-chat-message-at-point)))
    (unless msg
      (user-error "No message at point"))
    (unless (equal (plist-get msg :sender) sgn-account)
      (user-error "You can only delete your own messages"))
    (when (y-or-n-p "Delete this message for everyone? ")
      (let ((chat-id (plist-get msg :chat-id))
            (target-ts (plist-get msg :timestamp))
            (rowid (plist-get msg :rowid)))
        (sgn-rpc-remote-delete chat-id target-ts)
        ;; Mark locally
        (sgn-db-delete-message rowid)
        (sgn-chat-update-message rowid)
        (message "Message deleted.")))))

(declare-function sgn-db-delete-message "sgn-db")

;;;; Forward

;;;###autoload
(defun sgn-forward ()
  "Forward the message at point to another chat."
  (interactive)
  (require 'sgn)
  (let ((msg (sgn-chat-message-at-point)))
    (unless msg
      (user-error "No message at point"))
    (let* ((rowid (plist-get msg :rowid))
           (full-msg (sgn-db-get-message-by-rowid rowid))
           (body (and full-msg (plist-get full-msg :body))))
      (unless body
        (user-error "No text to forward"))
      (let ((target-id (sgn-contacts-completing-read "Forward to: ")))
        (sgn-rpc-send-message target-id body)
        (message "Message forwarded to %s"
                 (sgn-contacts-display-sender target-id))))))

;;;; Copy text

;;;###autoload
(defun sgn-copy-text ()
  "Copy the text of the message at point to the kill ring."
  (interactive)
  (require 'sgn)
  (let ((msg (sgn-chat-message-at-point)))
    (unless msg
      (user-error "No message at point"))
    (let* ((rowid (plist-get msg :rowid))
           (full-msg (sgn-db-get-message-by-rowid rowid))
           (body (and full-msg (plist-get full-msg :body))))
      (unless body
        (user-error "No text to copy"))
      (kill-new body)
      (message "Message text copied."))))

;;;; Pin/unpin

;;;###autoload
(defun sgn-toggle-pin ()
  "Pin or unpin the message at point."
  (interactive)
  (require 'sgn)
  (let ((msg (sgn-chat-message-at-point)))
    (unless msg
      (user-error "No message at point"))
    (let* ((chat-id (plist-get msg :chat-id))
           (target-author (plist-get msg :target-author))
           (target-ts (plist-get msg :timestamp))
           (rowid (plist-get msg :rowid))
           ;; Check if already pinned
           (pins (sgn-db-get-pins chat-id))
           (existing (cl-find-if
                      (lambda (p)
                        (and (equal (plist-get p :target-author) target-author)
                             (equal (plist-get p :target-timestamp) target-ts)))
                      pins)))
      (if existing
          ;; Unpin
          (progn
            (sgn-rpc-send-unpin chat-id target-author target-ts)
            (sgn-db-remove-pin chat-id target-author target-ts)
            (sgn-chat-update-message rowid)
            (message "Message unpinned."))
        ;; Pin
        (let ((now-ms (truncate (* (float-time) 1000))))
          (sgn-rpc-send-pin chat-id target-author target-ts)
          (sgn-db-insert-pin
           (list :message-rowid rowid
                 :chat-id chat-id
                 :target-author target-author
                 :target-timestamp target-ts
                 :pinned-by sgn-account
                 :pinned-at now-ms))
          (sgn-chat-update-message rowid)
          (message "Message pinned."))))))

(declare-function sgn-db-get-pins "sgn-db")

;;;; Mentions

(defun sgn-actions-apply-mentions (text mentions)
  "Apply MENTIONS highlighting to TEXT.
MENTIONS is a list of alists with `start', `length', `uuid' keys.
Return propertized TEXT."
  (when mentions
    (let ((result (copy-sequence text)))
      (dolist (m (if (vectorp mentions) (append mentions nil) mentions))
        (let* ((start (alist-get 'start m))
               (length (alist-get 'length m))
               (uuid (alist-get 'uuid m))
               (face (if (and sgn-account (equal uuid sgn-account))
                         'sgn-mention-self-face
                       'sgn-mention-face)))
          (when (and start length
                     (<= (+ start length) (length result)))
            (add-face-text-property start (+ start length) face nil result))))
      result)))

;;;; Polls

;;;###autoload
(defun sgn-create-poll ()
  "Create a new poll in the current chat."
  (interactive)
  (require 'sgn)
  (unless sgn-chat-id
    (user-error "Not in a chat buffer"))
  (let* ((question (read-string "Poll question: "))
         (options nil)
         (option ""))
    (while (progn
             (setq option (read-string
                           (format "Option %d (empty to finish): "
                                   (1+ (length options)))))
             (not (string-empty-p option)))
      (push option options))
    (unless (>= (length options) 2)
      (user-error "A poll needs at least 2 options"))
    (setq options (nreverse options))
    (sgn-rpc-send-poll-create sgn-chat-id question options)
    (message "Poll created.")))

;;;###autoload
(defun sgn-vote-poll ()
  "Vote in the poll at point."
  (interactive)
  (require 'sgn)
  (let ((msg (sgn-chat-message-at-point)))
    (unless msg
      (user-error "No message at point"))
    (let* ((rowid (plist-get msg :rowid))
           (poll (sgn-db-get-poll rowid)))
      (unless poll
        (user-error "No poll at point"))
      (when (plist-get poll :closed)
        (user-error "This poll is closed"))
      (let* ((options-json (plist-get poll :options-json))
             (options (json-read-from-string options-json))
             (options-list (if (vectorp options) (append options nil) options))
             (candidates (cl-loop for opt in options-list
                                  for i from 0
                                  collect (format "%d. %s" (1+ i) opt)))
             (choice (completing-read "Vote: " candidates nil t))
             (idx (1- (string-to-number choice)))
             (chat-id (plist-get poll :chat-id))
             (poll-author (plist-get poll :poll-author))
             (poll-ts (plist-get poll :poll-timestamp))
             (vote-count (1+ (or (plist-get poll :self-vote-count) 0))))
        (sgn-rpc-send-poll-vote chat-id poll-author poll-ts
                                (vector idx) vote-count)
        ;; Update local state
        (sgn-db-upsert-poll
         (list :message-rowid rowid
               :chat-id chat-id
               :poll-author poll-author
               :poll-timestamp poll-ts
               :question (plist-get poll :question)
               :options-json options-json
               :votes-json (plist-get poll :votes-json)
               :self-vote-count vote-count))
        (message "Vote recorded.")))))

;;;; Poll rendering helper

(defun sgn-actions-render-poll (poll)
  "Render POLL plist into the current buffer."
  (let* ((question (plist-get poll :question))
         (options-json (plist-get poll :options-json))
         (options (json-read-from-string options-json))
         (options-list (if (vectorp options) (append options nil) options))
         (votes-json (plist-get poll :votes-json))
         (votes (when votes-json (json-read-from-string votes-json)))
         (closed (plist-get poll :closed))
         (total-votes 0))
    ;; Count total votes
    (when votes
      (maphash (lambda (_k v)
                 (setq total-votes (+ total-votes (length v))))
               (if (hash-table-p votes) votes (make-hash-table))))
    (insert (format "  📊 %s\n" question))
    (cl-loop for opt in options-list
             for i from 0
             do (let* ((key (format "%d" i))
                       (voters (and votes (alist-get (intern key) votes)))
                       (voter-list (if (vectorp voters) (append voters nil) voters))
                       (count (length voter-list))
                       (bar-width 8)
                       (filled (if (> total-votes 0)
                                   (round (* bar-width (/ (float count) total-votes)))
                                 0))
                       (empty (- bar-width filled)))
                  (insert (format "    %d. %-16s %s%s %d vote%s\n"
                                  (1+ i)
                                  opt
                                  (make-string filled ?▓)
                                  (make-string empty ?░)
                                  count
                                  (if (= count 1) "" "s")))))
    (unless closed
      (insert "    [Vote: v]\n"))
    (when closed
      (insert (propertize "    [Poll closed]\n" 'face 'shadow)))))

(provide 'sgn-actions)
;;; sgn-actions.el ends here
