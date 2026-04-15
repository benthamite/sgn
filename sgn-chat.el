;;; sgn-chat.el --- Chat buffer mode for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Chat buffer mode with telega-style rendering: header + body grouping,
;; text properties for point-based commands, multi-line input, and draft
;; persistence.

;;; Code:

(require 'cl-lib)

(declare-function sgn--log "sgn")
(declare-function sgn-db-get-messages "sgn-db")
(declare-function sgn-db-get-chat "sgn-db")
(declare-function sgn-db-save-draft "sgn-db")
(declare-function sgn-db-get-draft "sgn-db")
(declare-function sgn-db-get-reactions "sgn-db")
(declare-function sgn-db-get-media "sgn-db")
(declare-function sgn-db-set-unread "sgn-db")
(declare-function sgn-db-get-pins "sgn-db")
(declare-function sgn-rpc-send-typing "sgn-rpc")
(declare-function sgn-rpc-send-receipt "sgn-rpc")
(declare-function sgn-rpc-alive-p "sgn-rpc")
(declare-function sgn-contacts-get-name "sgn-contacts")
(declare-function sgn-contacts-display-sender "sgn-contacts")
(declare-function sgn-media-insert "sgn-media")
(declare-function sgn-media-insert-inline-image "sgn-media")
(declare-function sgn-media-insert-voice-note "sgn-media")
(declare-function sgn-media-insert-link-preview "sgn-media")
(declare-function sgn-format-apply-styles "sgn-format")

(defvar sgn-account)
(defvar sgn-send-read-receipts)
(defvar sgn-send-typing)

;;;; Customization

(defcustom sgn-history-page-size 50
  "Number of messages loaded per page."
  :type 'integer
  :group 'sgn)

(defcustom sgn-prompt "> "
  "Input prompt string displayed in chat buffers."
  :type 'string
  :group 'sgn)

(defcustom sgn-message-grouping-interval 300
  "Seconds within which consecutive messages from the same sender are grouped."
  :type 'integer
  :group 'sgn)

(defcustom sgn-timestamp-format 'smart
  "How to format message timestamps.
`smart' uses relative for recent, absolute for older.
`absolute' always shows full date/time.
`relative' always shows relative time."
  :type '(choice (const :tag "Smart" smart)
                 (const :tag "Absolute" absolute)
                 (const :tag "Relative" relative))
  :group 'sgn)

;;;; Faces

(defface sgn-my-msg-face
  '((t :inherit font-lock-function-name-face))
  "Face for your own messages."
  :group 'sgn)

(defface sgn-other-msg-face
  '((t :inherit font-lock-variable-name-face))
  "Face for other people's messages."
  :group 'sgn)

(defface sgn-header-face
  '((t :inherit bold))
  "Face for message group headers."
  :group 'sgn)

(defface sgn-timestamp-face
  '((t :inherit shadow))
  "Face for timestamps."
  :group 'sgn)

(defface sgn-deleted-face
  '((t :inherit shadow))
  "Face for deleted message placeholders."
  :group 'sgn)

(defface sgn-error-face
  '((t :inherit error))
  "Face for error messages."
  :group 'sgn)

(defface sgn-receipt-face
  '((t :inherit shadow))
  "Face for delivery status checkmarks."
  :group 'sgn)

(defface sgn-quote-face
  '((t :inherit font-lock-comment-face))
  "Face for quoted message text."
  :group 'sgn)

;;;; Buffer-local state

(defvar-local sgn-chat-id nil
  "The Signal chat ID for this buffer (phone number or group ID).")

(defvar-local sgn-chat--input-marker nil
  "Marker at the start of the editable input area.")

(defvar-local sgn-chat--prompt-start nil
  "Marker at the very start of the prompt line (before the prompt string).")

(defvar-local sgn-chat--last-sender nil
  "Sender of the most recently rendered message, for grouping.")

(defvar-local sgn-chat--last-timestamp nil
  "Timestamp (ms) of the most recently rendered message, for grouping.")

(defvar-local sgn-chat--reply-target nil
  "Plist of the message being replied to, or nil.
Keys: :timestamp :sender :body")

(defvar-local sgn-chat--edit-target nil
  "Plist of the message being edited, or nil.
Keys: :timestamp :rowid :body")

(defvar-local sgn-chat--typing-timer nil
  "Timer for sending typing stop indicator.")

(defvar-local sgn-chat--typing-indicator nil
  "String currently shown as typing indicator, or nil.")

(defvar-local sgn-chat--oldest-timestamp nil
  "Timestamp of the oldest loaded message, for pagination.")

(defvar-local sgn-chat--all-loaded nil
  "Non-nil when all history has been loaded (no more older messages).")

;;;; Keymap

(defvar sgn-chat-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Input area
    (define-key map (kbd "RET") #'sgn-chat-send-input)
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "C-j") #'newline)
    (define-key map (kbd "C-c C-a") #'sgn-attach-file)
    (define-key map (kbd "C-c C-v") #'sgn-send-voice-note)
    (define-key map (kbd "C-g") #'sgn-chat-cancel-action)
    ;; Read-only message area (these work via post-command remapping)
    (define-key map (kbd "r") #'sgn-react)
    (define-key map (kbd "q") #'sgn-reply)
    (define-key map (kbd "e") #'sgn-edit)
    (define-key map (kbd "d") #'sgn-delete)
    (define-key map (kbd "f") #'sgn-forward)
    (define-key map (kbd "P") #'sgn-toggle-pin)
    (define-key map (kbd "c") #'sgn-copy-text)
    (define-key map (kbd "g") #'sgn-load-more-history)
    map)
  "Keymap for `sgn-chat-mode'.")

;;;; Major mode

(define-derived-mode sgn-chat-mode fundamental-mode "sgn Chat"
  "Major mode for Signal chat buffers."
  (setq-local sgn-chat--input-marker (make-marker))
  (setq-local sgn-chat--prompt-start (make-marker))
  (visual-line-mode 1)
  (add-hook 'after-change-functions #'sgn-chat--on-input-change nil t)
  (add-hook 'kill-buffer-hook #'sgn-chat--on-kill nil t))

;;;; Buffer lifecycle

(defun sgn-chat-get-buffer (chat-id)
  "Get or create the chat buffer for CHAT-ID."
  (let* ((display-name (sgn-contacts-get-name chat-id))
         (buf-name (format "*sgn: %s*" display-name))
         (buffer (get-buffer buf-name)))
    ;; Check for name collision with a different chat
    (when (and buffer
               (not (equal (buffer-local-value 'sgn-chat-id buffer) chat-id)))
      ;; Disambiguate
      (let ((suffix (if (string-prefix-p "+" chat-id)
                        chat-id
                      (substring chat-id 0 (min 8 (length chat-id))))))
        (setq buf-name (format "*sgn: %s <%s>*" display-name suffix))
        (setq buffer (get-buffer buf-name))))
    (unless buffer
      (setq buffer (get-buffer-create buf-name))
      (with-current-buffer buffer
        (sgn-chat-mode)
        (setq sgn-chat-id chat-id)
        (sgn-chat--draw-prompt)
        (sgn-chat--load-history)
        (sgn-chat--restore-draft)))
    buffer))

(defun sgn-chat-open (chat-id)
  "Open the chat buffer for CHAT-ID and switch to it."
  (let ((buf (sgn-chat-get-buffer chat-id)))
    (switch-to-buffer buf)
    (goto-char (point-max))
    (sgn-chat--mark-read)))

(defun sgn-chat--mark-read ()
  "Mark the current chat as read and send read receipts if enabled."
  (when sgn-chat-id
    (sgn-db-set-unread sgn-chat-id 0)
    ;; Send read receipts for visible messages
    (when (and sgn-send-read-receipts
               (sgn-rpc-alive-p)
               (frame-focus-state))
      ;; Collect timestamps of unread messages from other senders
      (let ((timestamps nil)
            (receipt-recipient nil))
        (save-excursion
          (goto-char (point-min))
          (while (< (point) (marker-position sgn-chat--prompt-start))
            (let ((sender (get-text-property (point) 'sgn-message-sender))
                  (ts (get-text-property (point) 'sgn-message-ts)))
              (when (and sender ts
                         (not (equal sender sgn-account)))
                (unless receipt-recipient
                  (setq receipt-recipient sender))
                (push ts timestamps)))
            (goto-char (or (next-single-property-change (point) 'sgn-message-ts)
                           (point-max)))))
        (when (and receipt-recipient timestamps)
          (sgn-rpc-send-receipt receipt-recipient
                                (vconcat (nreverse timestamps))))))))

;;;; Prompt management

(defun sgn-chat--draw-prompt ()
  "Draw the input prompt and update markers."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (set-marker sgn-chat--prompt-start (point))
    (let ((prompt-text (sgn-chat--build-prompt-text)))
      (insert (propertize prompt-text
                          'read-only t
                          'face 'minibuffer-prompt
                          'rear-nonsticky '(read-only face)
                          'front-sticky '(read-only face))))
    (set-marker sgn-chat--input-marker (point))))

(defun sgn-chat--build-prompt-text ()
  "Build the prompt text, including reply/edit context if active."
  (cond
   (sgn-chat--reply-target
    (let* ((sender (sgn-contacts-display-sender
                    (plist-get sgn-chat--reply-target :sender)))
           (body (or (plist-get sgn-chat--reply-target :body) ""))
           (preview (if (> (length body) 40)
                        (concat (substring body 0 40) "…")
                      body)))
      (format "┃ Replying to %s: %s\n%s" sender preview sgn-prompt)))
   (sgn-chat--edit-target
    (format "┃ Editing message\n%s" sgn-prompt))
   (t
    sgn-prompt)))

(defun sgn-chat--redraw-prompt ()
  "Redraw the prompt (e.g., after reply/edit state changes)."
  (let ((inhibit-read-only t)
        (input-text (sgn-chat--get-input-text)))
    ;; Remove old prompt + input
    (delete-region (marker-position sgn-chat--prompt-start) (point-max))
    ;; Redraw
    (sgn-chat--draw-prompt)
    ;; Restore input
    (goto-char (point-max))
    (insert input-text)))

;;;; Cursor guard — no-op
;; The message area is protected by `read-only' text properties
;; (set when messages are rendered).  The cursor moves freely so
;; that point-based commands (react, reply, etc.) work naturally.

;;;; Input handling

(defun sgn-chat--get-input-text ()
  "Return the current input text (after the prompt)."
  (if (and sgn-chat--input-marker
           (marker-position sgn-chat--input-marker))
      (buffer-substring-no-properties
       (marker-position sgn-chat--input-marker)
       (point-max))
    ""))

(defun sgn-chat-send-input ()
  "Send the input text to the current chat."
  (interactive)
  (let ((text (string-trim (sgn-chat--get-input-text))))
    (when (string-empty-p text)
      (user-error "Nothing to send"))
    ;; Clear input area
    (let ((inhibit-read-only t))
      (delete-region (marker-position sgn-chat--input-marker) (point-max)))
    ;; Determine what to send
    (cond
     (sgn-chat--edit-target
      ;; Editing a message
      (let ((edit-ts (plist-get sgn-chat--edit-target :timestamp)))
        (sgn-chat--do-send-edit sgn-chat-id text edit-ts)
        (setq sgn-chat--edit-target nil)
        (sgn-chat--redraw-prompt)))
     (sgn-chat--reply-target
      ;; Replying to a message
      (let ((quote-ts (plist-get sgn-chat--reply-target :timestamp))
            (quote-author (plist-get sgn-chat--reply-target :sender))
            (quote-body (plist-get sgn-chat--reply-target :body)))
        (sgn-chat--do-send-reply sgn-chat-id text quote-ts quote-author quote-body)
        (setq sgn-chat--reply-target nil)
        (sgn-chat--redraw-prompt)))
     (t
      ;; Normal message
      (sgn-chat--do-send sgn-chat-id text)))
    ;; Stop typing indicator
    (sgn-chat--stop-typing)))

(declare-function sgn-rpc-send-message "sgn-rpc")
(declare-function sgn-rpc-send-edit "sgn-rpc")
(declare-function sgn-format-parse-markup "sgn-format")
(declare-function sgn-format-styles-to-json "sgn-format")

(defun sgn-chat--do-send (chat-id text)
  "Send plain TEXT to CHAT-ID, with markup parsing."
  (let* ((parsed (sgn-format-parse-markup text))
         (plain (plist-get parsed :text))
         (styles (plist-get parsed :styles))
         (extras (when styles
                   `((textStyle . ,(vconcat styles))))))
    (sgn-rpc-send-message chat-id plain extras)))

(defun sgn-chat--do-send-reply (chat-id text quote-ts quote-author quote-body)
  "Send TEXT as a reply in CHAT-ID."
  (let* ((parsed (sgn-format-parse-markup text))
         (plain (plist-get parsed :text))
         (styles (plist-get parsed :styles))
         (extras `((quoteTimestamp . ,quote-ts)
                   (quoteAuthor . ,quote-author)
                   (quoteMessage . ,(or quote-body "")))))
    (when styles
      (push `(textStyle . ,(vconcat styles)) extras))
    (sgn-rpc-send-message chat-id plain extras)))

(defun sgn-chat--do-send-edit (chat-id text edit-ts)
  "Send edited TEXT for message at EDIT-TS in CHAT-ID."
  (let* ((parsed (sgn-format-parse-markup text))
         (plain (plist-get parsed :text))
         (styles (plist-get parsed :styles))
         (extras (when styles
                   `((textStyle . ,(vconcat styles))))))
    (sgn-rpc-send-edit chat-id plain edit-ts extras)))

;;;; Cancel reply/edit

(defun sgn-chat-cancel-action ()
  "Cancel the current reply or edit action."
  (interactive)
  (cond
   (sgn-chat--reply-target
    (setq sgn-chat--reply-target nil)
    (sgn-chat--redraw-prompt)
    (message "Reply cancelled."))
   (sgn-chat--edit-target
    (setq sgn-chat--edit-target nil)
    (let ((inhibit-read-only t))
      (delete-region (marker-position sgn-chat--input-marker) (point-max)))
    (sgn-chat--redraw-prompt)
    (message "Edit cancelled."))
   (t
    (keyboard-quit))))

;;;; Typing indicator

(defun sgn-chat--on-input-change (beg _end _len)
  "Handle changes in the input area starting at BEG.
Sends typing indicators."
  (when (and sgn-send-typing
             sgn-chat-id
             sgn-chat--input-marker
             (marker-position sgn-chat--input-marker)
             (>= beg (marker-position sgn-chat--input-marker))
             (sgn-rpc-alive-p))
    ;; Send typing start
    (sgn-rpc-send-typing sgn-chat-id)
    ;; Reset the stop timer
    (when sgn-chat--typing-timer
      (cancel-timer sgn-chat--typing-timer))
    (setq sgn-chat--typing-timer
          (run-at-time 5 nil #'sgn-chat--stop-typing))))

(defun sgn-chat--stop-typing ()
  "Send typing stop indicator."
  (when (and sgn-chat-id (sgn-rpc-alive-p))
    (sgn-rpc-send-typing sgn-chat-id t))
  (when sgn-chat--typing-timer
    (cancel-timer sgn-chat--typing-timer)
    (setq sgn-chat--typing-timer nil)))

;;;; Typing indicator display

(defun sgn-chat-show-typing (chat-id sender)
  "Show that SENDER is typing in CHAT-ID."
  (let* ((buf (get-buffer (format "*sgn: %s*" (sgn-contacts-get-name chat-id)))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((name (sgn-contacts-display-sender sender)))
          (setq sgn-chat--typing-indicator
                (format "%s is typing…" name))
          (setq header-line-format
                (list (sgn-chat--header-line-base)
                      " — " sgn-chat--typing-indicator))
          (force-mode-line-update))
        ;; Auto-clear after 10 seconds
        (run-at-time 10 nil
                     (lambda (b)
                       (when (buffer-live-p b)
                         (with-current-buffer b
                           (sgn-chat-clear-typing))))
                     buf)))))

(defun sgn-chat-clear-typing ()
  "Clear the typing indicator."
  (setq sgn-chat--typing-indicator nil)
  (setq header-line-format (sgn-chat--header-line-base))
  (force-mode-line-update))

(defun sgn-chat--header-line-base ()
  "Return the base header line string for the current chat."
  (let* ((name (sgn-contacts-get-name sgn-chat-id))
         (chat (sgn-db-get-chat sgn-chat-id))
         (expiration (and chat (plist-get chat :expiration))))
    (concat name
            (when (and expiration (> expiration 0))
              (format " ⏱ %s" (sgn-chat--format-duration expiration))))))

;;;; Draft persistence

(defun sgn-chat--save-draft ()
  "Save the current input as a draft."
  (when sgn-chat-id
    (let ((text (sgn-chat--get-input-text)))
      (sgn-db-save-draft sgn-chat-id
                         (if (string-empty-p (string-trim text)) nil text)))))

(defun sgn-chat--restore-draft ()
  "Restore a saved draft into the input area."
  (when sgn-chat-id
    (let ((draft (sgn-db-get-draft sgn-chat-id)))
      (when (and draft (not (string-empty-p draft)))
        (goto-char (point-max))
        (insert draft)))))

(defun sgn-chat--on-kill ()
  "Handle buffer kill: save draft, stop timers."
  (sgn-chat--save-draft)
  (sgn-chat--stop-typing))

;;;; History loading

(defun sgn-chat--load-history ()
  "Load the most recent messages from the database."
  (let ((messages (sgn-db-get-messages sgn-chat-id sgn-history-page-size)))
    (when messages
      (setq sgn-chat--oldest-timestamp
            (plist-get (car messages) :timestamp))
      (when (< (length messages) sgn-history-page-size)
        (setq sgn-chat--all-loaded t))
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-min))
          (dolist (msg messages)
            (sgn-chat--render-message msg)))))))

(defun sgn-load-more-history ()
  "Load older messages from the database."
  (interactive)
  (if sgn-chat--all-loaded
      (message "All history loaded.")
    (let* ((older (sgn-db-get-messages sgn-chat-id
                                       sgn-history-page-size
                                       sgn-history-page-size))
           ;; Actually we need messages older than the oldest loaded
           ;; This requires an offset-based or timestamp-based query
           ;; For now, use a simple approach
           )
      ;; TODO: implement proper pagination with timestamp cursor
      (message "Load more history: not yet implemented (Phase 2)"))))

;;;; Message rendering

(defun sgn-chat-insert-message (msg)
  "Insert a new incoming/sync message MSG into the chat buffer.
MSG is a message plist from the database."
  (let ((chat-id (plist-get msg :chat-id)))
    (when-let* ((buf (get-buffer
                      (format "*sgn: %s*"
                              (sgn-contacts-get-name chat-id)))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (save-excursion
              ;; Insert before the prompt
              (goto-char (marker-position sgn-chat--prompt-start))
              (sgn-chat--render-message msg)))
          ;; Scroll to bottom if window is visible
          (let ((win (get-buffer-window buf)))
            (when win
              (set-window-point win (point-max)))))))))

(defun sgn-chat-update-message (rowid)
  "Re-render the message with ROWID in the appropriate buffer.
Used after edits, deletes, or reaction changes."
  ;; Find and re-render the message region
  (save-excursion
    (goto-char (point-min))
    (let ((pos (text-property-search-forward 'sgn-message-rowid rowid t)))
      (when pos
        (let* ((start (prop-match-beginning pos))
               (end (or (next-single-property-change start 'sgn-message-rowid)
                        (marker-position sgn-chat--prompt-start)))
               (msg (sgn-db-get-message-by-rowid rowid))
               (inhibit-read-only t))
          (when msg
            ;; Check if this message had a header (not grouped)
            (let ((has-header (get-text-property start 'sgn-message-header)))
              (delete-region start end)
              (goto-char start)
              ;; Re-render with forced header if it had one
              (if has-header
                  (let ((sgn-chat--last-sender nil)
                        (sgn-chat--last-timestamp nil))
                    (sgn-chat--render-message msg))
                (sgn-chat--render-message-body msg)))))))))

(declare-function sgn-db-get-message-by-rowid "sgn-db")

(defun sgn-chat--render-message (msg)
  "Render a single message MSG at point.
Handles grouping, headers, body, quotes, reactions, and media."
  (let* ((sender (plist-get msg :sender))
         (timestamp (plist-get msg :timestamp))
         (deleted (plist-get msg :deleted))
         (rowid (plist-get msg :rowid))
         (need-header (sgn-chat--needs-header-p sender timestamp))
         (start (point)))
    ;; Header (if not grouped)
    (when need-header
      (sgn-chat--render-header sender timestamp)
      (put-text-property start (point) 'sgn-message-header t))
    ;; Body
    (sgn-chat--render-message-body msg)
    ;; Update grouping state
    (setq sgn-chat--last-sender sender)
    (setq sgn-chat--last-timestamp timestamp)))

(defun sgn-chat--needs-header-p (sender timestamp)
  "Return non-nil if a new header is needed for SENDER at TIMESTAMP."
  (or (null sgn-chat--last-sender)
      (not (equal sender sgn-chat--last-sender))
      (and sgn-chat--last-timestamp timestamp
           (> (abs (- timestamp sgn-chat--last-timestamp))
              (* sgn-message-grouping-interval 1000)))))

(defun sgn-chat--render-header (sender timestamp)
  "Render a message group header for SENDER at TIMESTAMP."
  (let* ((name (sgn-contacts-display-sender sender))
         (time-str (sgn-chat--format-timestamp timestamp))
         (is-me (and sgn-account (equal sender sgn-account)))
         (name-face (if is-me 'sgn-my-msg-face 'sgn-other-msg-face))
         (header (format "── %s · %s " name time-str))
         (fill (make-string (max 0 (- (window-width) (length header) 1)) ?─)))
    (insert (propertize (concat header fill "\n")
                        'face 'sgn-header-face
                        'sgn-header-sender sender))))

(defun sgn-chat--render-message-body (msg)
  "Render the body, quote, reactions, and media of MSG at point."
  (let* ((rowid (plist-get msg :rowid))
         (sender (plist-get msg :sender))
         (timestamp (plist-get msg :timestamp))
         (chat-id (plist-get msg :chat-id))
         (body (plist-get msg :body))
         (deleted (plist-get msg :deleted))
         (edited-at (plist-get msg :edited-at))
         (quote-ts (plist-get msg :quote-ts))
         (quote-author (plist-get msg :quote-author))
         (quote-body (plist-get msg :quote-body))
         (styles-json (plist-get msg :styles-json))
         (target-author sender)
         (start (point))
         (is-me (and sgn-account (equal sender sgn-account))))
    ;; Quote block
    (when (and quote-ts quote-author)
      (sgn-chat--render-quote quote-author quote-body))
    ;; Message body
    (cond
     ((and deleted (not (zerop deleted)))
      (insert (propertize "  [Message deleted]\n" 'face 'sgn-deleted-face)))
     (body
      (let ((styled-text (if styles-json
                             (sgn-format-apply-styles body styles-json)
                           body)))
        (insert "  " styled-text)
        (when (and edited-at (not (zerop edited-at)))
          (insert (propertize " (edited)" 'face 'sgn-timestamp-face)))
        (insert "\n")))
     (t
      ;; Media-only message, no text body
      nil))
    ;; Media
    (when rowid
      (let ((media-list (sgn-db-get-media rowid)))
        (when media-list
          (dolist (m media-list)
            (insert "  ")
            (let ((content-type (plist-get m :content-type))
                  (file-path (plist-get m :file-path))
                  (is-voice (plist-get m :is-voice))
                  (is-sticker (plist-get m :is-sticker)))
              (cond
               ((and is-voice (not (zerop is-voice)) file-path)
                (sgn-media-insert-voice-note file-path 0))
               ((and file-path content-type
                     (string-prefix-p "image/" content-type))
                (sgn-media-insert-inline-image file-path))
               (file-path
                (insert (propertize
                         (format "[File: %s]"
                                 (or (plist-get m :file-name) "attachment"))
                         'face 'link)))
               (t
                (insert (propertize "[Media not downloaded]"
                                    'face 'font-lock-comment-face)))))
            (insert "\n")))))
    ;; Reactions
    (when rowid
      (let ((reactions (sgn-db-get-reactions rowid)))
        (when reactions
          (sgn-chat--render-reactions reactions))))
    ;; Pin indicator
    (when rowid
      (let ((pins (sgn-db-get-pins chat-id)))
        (when (cl-find-if (lambda (p)
                            (equal (plist-get p :message-rowid) rowid))
                          pins)
          (insert "  📌\n"))))
    ;; Apply text properties to the entire message region
    (put-text-property start (point) 'sgn-message-rowid rowid)
    (put-text-property start (point) 'sgn-message-ts timestamp)
    (put-text-property start (point) 'sgn-message-sender sender)
    (put-text-property start (point) 'sgn-message-chat-id chat-id)
    (put-text-property start (point) 'sgn-message-target-author target-author)
    ;; Protect message area from editing (cursor still moves freely)
    (put-text-property start (point) 'read-only t)
    (put-text-property start (point) 'rear-nonsticky '(read-only))))

(defun sgn-chat--render-quote (author body)
  "Render a quote block for AUTHOR with BODY."
  (let* ((name (sgn-contacts-display-sender author))
         (preview (if (and body (> (length body) 60))
                      (concat (substring body 0 60) "…")
                    (or body ""))))
    (insert (propertize (format "  ┃ %s: %s\n" name preview)
                        'face 'sgn-quote-face))))

(defun sgn-chat--render-reactions (reactions)
  "Render REACTIONS below the message body."
  ;; Group by emoji
  (let ((groups (make-hash-table :test 'equal)))
    (dolist (r reactions)
      (let ((emoji (plist-get r :emoji))
            (sender (plist-get r :sender)))
        (push (sgn-contacts-display-sender sender)
              (gethash emoji groups))))
    (insert "  ")
    (maphash (lambda (emoji senders)
               (insert (format "%s %s  "
                               emoji
                               (string-join (nreverse senders) ", "))))
             groups)
    (insert "\n")))

;;;; Timestamp formatting

(defun sgn-chat--format-timestamp (timestamp-ms)
  "Format TIMESTAMP-MS according to `sgn-timestamp-format'."
  (let* ((time (seconds-to-time (/ timestamp-ms 1000.0)))
         (now (current-time))
         (diff (float-time (time-subtract now time))))
    (pcase sgn-timestamp-format
      ('relative (sgn-chat--format-relative diff))
      ('absolute (format-time-string "%b %d, %H:%M" time))
      ('smart
       (cond
        ((< diff 86400)  ; today
         (format-time-string "%H:%M" time))
        ((< diff 604800) ; this week
         (format-time-string "%a, %H:%M" time))
        (t
         (format-time-string "%b %d, %H:%M" time)))))))

(defun sgn-chat--format-relative (diff-seconds)
  "Format DIFF-SECONDS as a relative time string."
  (cond
   ((< diff-seconds 60) "now")
   ((< diff-seconds 3600) (format "%dm" (floor (/ diff-seconds 60))))
   ((< diff-seconds 86400) (format "%dh" (floor (/ diff-seconds 3600))))
   (t (format "%dd" (floor (/ diff-seconds 86400))))))

(defun sgn-chat--format-duration (seconds)
  "Format SECONDS as a human-readable duration (e.g., \"24h\", \"7d\")."
  (cond
   ((< seconds 60) (format "%ds" seconds))
   ((< seconds 3600) (format "%dm" (/ seconds 60)))
   ((< seconds 86400) (format "%dh" (/ seconds 3600)))
   (t (format "%dd" (/ seconds 86400)))))

;;;; System messages

(defun sgn-chat-insert-system-msg (chat-id text &optional face)
  "Insert a system message TEXT into the chat buffer for CHAT-ID.
FACE defaults to `sgn-error-face'."
  (when-let* ((buf (get-buffer
                    (format "*sgn: %s*"
                            (sgn-contacts-get-name chat-id)))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (marker-position sgn-chat--prompt-start))
            (insert (propertize (concat "*** " text "\n")
                                'face (or face 'sgn-error-face)))))))))

;;;; Message at point

(defun sgn-chat-message-at-point ()
  "Return a plist of the message at point, or nil."
  (let ((rowid (get-text-property (point) 'sgn-message-rowid)))
    (when rowid
      (list :rowid rowid
            :timestamp (get-text-property (point) 'sgn-message-ts)
            :sender (get-text-property (point) 'sgn-message-sender)
            :chat-id (get-text-property (point) 'sgn-message-chat-id)
            :target-author (get-text-property (point) 'sgn-message-target-author)))))

;;;; Open media/link at point

(defun sgn-open-at-point ()
  "Open media or link at point."
  (interactive)
  (let ((button (button-at (point))))
    (if button
        (push-button (point))
      (message "Nothing to open at point."))))

(provide 'sgn-chat)
;;; sgn-chat.el ends here
