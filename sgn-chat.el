;;; sgn-chat.el --- Chat buffer mode for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Chat buffers.  Each chat has at most one buffer, found through a
;; registry keyed by chat ID; the buffer's name only shows the chat's
;; display name and follows it when it changes.
;;
;; A chat buffer has two parts: the message history, which is
;; read-only, and below it the prompt and the input area.  The history
;; is always drawn from the database: whenever the store reports that
;; a chat changed, its buffer redraws the history, keeping the input,
;; its undo history, and each window's position.  Messages are drawn
;; in timestamp order, and only the latest `sgn-chat--limit' of them.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'sgn-db)
(require 'sgn-format)
(require 'sgn-media)
(require 'sgn-contacts)

(declare-function sgn--log "sgn")
(declare-function sgn-mark-chat-read "sgn")
(declare-function sgn-rpc-alive-p "sgn-rpc")
(declare-function sgn-rpc-send-typing "sgn-rpc")
(declare-function sgn-chat-address "sgn-actions")
(declare-function sgn-send-text "sgn-actions")
(declare-function sgn-send-edit "sgn-actions")
(declare-function sgn-react "sgn-actions")
(declare-function sgn-reply "sgn-actions")
(declare-function sgn-edit "sgn-actions")
(declare-function sgn-delete "sgn-actions")
(declare-function sgn-forward "sgn-actions")
(declare-function sgn-toggle-pin "sgn-actions")
(declare-function sgn-copy-text "sgn-actions")
(declare-function sgn-attach-file "sgn-actions")
(declare-function sgn-search-in-chat "sgn-search")
(declare-function sgn-store-self-p "sgn-store")

(defvar sgn-send-typing)

;;;; Customization

(defcustom sgn-history-page-size 50
  "Number of messages shown at first, and added by each `sgn-load-more-history'."
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
`smart' shows the time for today, the weekday for this week, and
the date for older messages.  `absolute' always shows the date."
  :type '(choice (const :tag "Smart" smart)
                 (const :tag "Absolute" absolute))
  :group 'sgn)

;;;; Faces

(defface sgn-header-face
  '((t :inherit bold))
  "Face for message group headers."
  :group 'sgn)

(defface sgn-timestamp-face
  '((t :inherit shadow))
  "Face for timestamps and minor annotations."
  :group 'sgn)

(defface sgn-deleted-face
  '((t :inherit shadow))
  "Face for deleted message placeholders."
  :group 'sgn)

(defface sgn-error-face
  '((t :inherit error))
  "Face for messages that were not sent."
  :group 'sgn)

(defface sgn-receipt-face
  '((t :inherit shadow))
  "Face for delivery and read marks."
  :group 'sgn)

(defface sgn-quote-face
  '((t :inherit font-lock-comment-face))
  "Face for quoted message text."
  :group 'sgn)

;;;; Buffer-local state

(defvar-local sgn-chat-id nil
  "The chat ID of this buffer.")

(defvar-local sgn-chat--prompt-start nil
  "Marker at the start of the prompt.")

(defvar-local sgn-chat--input-marker nil
  "Marker at the start of the input area.")

(defvar-local sgn-chat--limit nil
  "Number of latest messages shown.")

(defvar-local sgn-chat--reply-target nil
  "The message being replied to, as a message plist, or nil.")

(defvar-local sgn-chat--stashed-input nil
  "Input set aside while an edit is composed, or nil.")

(defvar-local sgn-chat--edit-target nil
  "The message being edited, as a message plist, or nil.")

(defvar-local sgn-chat--revealed nil
  "List of rowids of messages whose spoilers are revealed.")

(defvar-local sgn-chat--redraw-timer nil
  "Timer for a pending redraw, or nil.")

(defvar-local sgn-chat--typists nil
  "Alist of (SENDER . TIMER) for people shown as typing.")

(defvar-local sgn-chat--typing-sent-at nil
  "Time the last typing indicator was sent, or nil when not typing.")

(defvar-local sgn-chat--typing-stop-timer nil
  "Timer that sends the typing stop indicator.")

(defvar-local sgn-chat--draft-timer nil
  "Timer that saves the input as a draft.")

(defvar sgn-chat--programmatic nil
  "Non-nil while sgn itself changes the input, which is not typing.")

(defconst sgn-chat--typing-refresh-interval 10
  "Seconds between typing indicators sent while typing continues.")

(defconst sgn-chat--typing-display-timeout 15
  "Seconds a typing indicator is shown without being renewed.")

;;;; Buffer registry

(defvar sgn-chat--buffers (make-hash-table :test 'equal)
  "Map from chat ID to its live chat buffer.")

(defun sgn-chat-buffer (chat-id)
  "Return the live buffer of CHAT-ID, or nil."
  (let ((buf (gethash chat-id sgn-chat--buffers)))
    (if (buffer-live-p buf)
        buf
      (remhash chat-id sgn-chat--buffers)
      nil)))

(defun sgn-chat--buffer-name (chat-id)
  "Return the name a buffer for CHAT-ID should have."
  (format "*sgn: %s*" (sgn-contacts-get-name chat-id)))

(defun sgn-chat-get-buffer (chat-id)
  "Return the buffer of CHAT-ID, creating it if needed."
  (or (sgn-chat-buffer chat-id)
      (let ((buf (generate-new-buffer (sgn-chat--buffer-name chat-id))))
        (puthash chat-id buf sgn-chat--buffers)
        (with-current-buffer buf
          (sgn-chat-mode)
          (setq sgn-chat-id chat-id)
          (sgn-chat--draw-prompt)
          (sgn-chat--restore-draft)
          (sgn-chat--redraw)
          (goto-char (point-max)))
        buf)))

(defun sgn-chat-open (chat-id)
  "Show the chat CHAT-ID in the selected window."
  (sgn-db-ensure-chat chat-id (or (sgn-db-chat-type chat-id) "individual"))
  (switch-to-buffer (sgn-chat-get-buffer chat-id)))

(defun sgn-chat--unregister ()
  "Remove the current buffer from the registry."
  (when (eq (gethash sgn-chat-id sgn-chat--buffers) (current-buffer))
    (remhash sgn-chat-id sgn-chat--buffers)))

(defun sgn-chat-on-identity-merged (number uuid)
  "Move the buffer of chat NUMBER to chat UUID, which it was merged into.
If UUID already has a buffer, the NUMBER buffer's input moves there
when that buffer has none, and the NUMBER buffer is killed."
  (when-let* ((buf (sgn-chat-buffer number)))
    (remhash number sgn-chat--buffers)
    (if-let* ((other (sgn-chat-buffer uuid)))
        (let ((input (with-current-buffer buf (sgn-chat--input-text))))
          (with-current-buffer other
            (when (and (string-empty-p (sgn-chat--input-text))
                       (not (string-empty-p input)))
              (sgn-chat--set-input input)))
          (with-current-buffer buf
            (setq sgn-chat-id nil))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buf)))
      (puthash uuid buf sgn-chat--buffers)
      (with-current-buffer buf
        (setq sgn-chat-id uuid))
      (sgn-chat-schedule-redraw uuid))))

(defun sgn-chat-buffers ()
  "Return the live chat buffers."
  (cl-loop for buf being the hash-values of sgn-chat--buffers
           when (buffer-live-p buf) collect buf))

;;;; Keymaps

(defvar sgn-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'sgn-chat-return)
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "C-j") #'newline)
    (define-key map (kbd "C-c C-a") #'sgn-attach-file)
    (define-key map (kbd "C-c C-s") #'sgn-search-in-chat)
    (define-key map (kbd "C-c C-l") #'sgn-load-more-history)
    (define-key map (kbd "C-g") #'sgn-chat-cancel-action)
    map)
  "Keymap for `sgn-chat-mode'.")

(defvar sgn-chat-message-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'sgn-reply)
    (define-key map (kbd "R") #'sgn-react)
    (define-key map (kbd "e") #'sgn-edit)
    (define-key map (kbd "d") #'sgn-delete)
    (define-key map (kbd "f") #'sgn-forward)
    (define-key map (kbd "P") #'sgn-toggle-pin)
    (define-key map (kbd "c") #'sgn-copy-text)
    (define-key map (kbd "g") #'sgn-load-more-history)
    map)
  "Keymap active on messages, through the `keymap' text property.
Single-key commands live here rather than in `sgn-chat-mode-map',
so that they do not shadow typing in the input area.")

;;;; Major mode

(define-derived-mode sgn-chat-mode fundamental-mode "sgn Chat"
  "Major mode for Signal chat buffers.

\\{sgn-chat-mode-map}
On a message:
\\{sgn-chat-message-map}"
  (setq-local sgn-chat--prompt-start (make-marker))
  (setq-local sgn-chat--input-marker (make-marker))
  (setq-local sgn-chat--limit sgn-history-page-size)
  (visual-line-mode 1)
  (add-hook 'after-change-functions #'sgn-chat--on-input-change nil t)
  (add-hook 'kill-buffer-hook #'sgn-chat--on-kill nil t))

;;;; Redrawing

(defun sgn-chat-schedule-redraw (chat-id)
  "Redraw the buffer of CHAT-ID, if any, once the current command is done."
  (when-let* ((buf (sgn-chat-buffer chat-id)))
    (with-current-buffer buf
      (unless (timerp sgn-chat--redraw-timer)
        (setq sgn-chat--redraw-timer
              (run-at-time 0 nil #'sgn-chat--redraw-buffer buf))))))

(defun sgn-chat--redraw-buffer (buffer)
  "Redraw BUFFER if it is live and the database is open."
  (when (and (buffer-live-p buffer) sgn-db--connection)
    (with-current-buffer buffer
      (sgn-chat--redraw))))

(defun sgn-chat-on-store-changed (chat-ids)
  "Redraw the buffers of CHAT-IDS, as `sgn-store-changed-functions' asks."
  (dolist (chat-id chat-ids)
    (sgn-chat-schedule-redraw chat-id)))

(defun sgn-chat--redraw ()
  "Draw the current buffer's history again from the database.
Each window keeps showing the same message, point stays on the
same message or input position, and the input is untouched."
  (when (timerp sgn-chat--redraw-timer)
    (cancel-timer sgn-chat--redraw-timer))
  (setq sgn-chat--redraw-timer nil)
  (sgn-chat--update-buffer-name)
  (let* ((anchors (sgn-chat--save-positions))
         (old-end (marker-position sgn-chat--prompt-start))
         (messages (sgn-db-get-messages sgn-chat-id sgn-chat--limit)))
    (let ((inhibit-read-only t)
          (buffer-undo-list t))
      (save-excursion
        (delete-region (point-min) sgn-chat--prompt-start)
        (goto-char (point-min))
        ;; History goes before the prompt, which must move past it.
        ;; The marker does so only here: otherwise text typed at the
        ;; prompt start, as with an empty prompt, would join the
        ;; history.
        (set-marker-insertion-type sgn-chat--prompt-start t)
        (unwind-protect
            (sgn-chat--insert-history messages)
          (set-marker-insertion-type sgn-chat--prompt-start nil))
        (when (> (point) (point-min))
          (add-text-properties (point-min) (point)
                               '(read-only t front-sticky (read-only)
                                           rear-nonsticky (read-only))))))
    (sgn-chat--fix-input-marker)
    (sgn-chat--shift-undo (- sgn-chat--prompt-start old-end))
    (sgn-chat--restore-positions anchors)
    (setq header-line-format (sgn-chat--header-line))))

(defun sgn-chat--update-buffer-name ()
  "Rename the buffer if the chat's display name changed."
  (let ((name (sgn-chat--buffer-name sgn-chat-id)))
    ;; `rename-buffer' may have made the name unique with a <N> suffix.
    (unless (string-match-p (concat "\\`" (regexp-quote name) "\\(?:<[0-9]+>\\)?\\'")
                            (buffer-name))
      (rename-buffer name t))))

(defun sgn-chat--fix-input-marker ()
  "Put the input marker right after the prompt.
With an empty prompt the two markers coincide, and inserting the
history would otherwise leave the input marker before it."
  (set-marker sgn-chat--input-marker
              (max sgn-chat--input-marker
                   (+ sgn-chat--prompt-start
                      (length (sgn-chat--prompt-text))))))

(defun sgn-chat--shift-undo (delta)
  "Shift buffer positions in `buffer-undo-list' by DELTA.
Undo entries only concern the input, which redrawing the history
moved by DELTA."
  (unless (or (zerop delta) (eq buffer-undo-list t))
    (setq buffer-undo-list
          (mapcar (lambda (entry) (sgn-chat--shift-undo-entry entry delta))
                  buffer-undo-list))))

(defun sgn-chat--shift-undo-entry (entry delta)
  "Return the undo ENTRY with its buffer positions shifted by DELTA."
  (pcase entry
    ((pred integerp) (+ entry delta))
    (`(,(and beg (pred integerp)) . ,(and end (pred integerp)))
     (cons (+ beg delta) (+ end delta)))
    (`(,(and text (pred stringp)) . ,(and pos (pred integerp)))
     (cons text (if (< pos 0) (- pos delta) (+ pos delta))))
    (`(nil ,prop ,val ,beg . ,end)
     (cl-list* nil prop val (+ beg delta) (+ end delta)))
    (_ entry)))

(defun sgn-chat--save-positions ()
  "Return the positions of point and window starts, relative to messages."
  (cons (cons nil (sgn-chat--anchor (point)))
        (mapcar (lambda (win)
                  (list win (sgn-chat--anchor (window-start win))
                        (sgn-chat--anchor (window-point win))))
                (get-buffer-window-list nil nil t))))

(defun sgn-chat--anchor (pos)
  "Return POS as an anchor that survives a redraw."
  (cond
   ((>= pos sgn-chat--prompt-start)
    (list 'prompt (- pos sgn-chat--prompt-start)))
   ((get-text-property pos 'sgn-message-rowid)
    (let ((start (or (previous-single-property-change
                      (1+ pos) 'sgn-message-rowid)
                     (point-min))))
      (list 'message (get-text-property pos 'sgn-message-rowid)
            (- pos start))))
   (t (list 'top pos))))

(defun sgn-chat--resolve-anchor (anchor)
  "Return the buffer position ANCHOR stands for after a redraw."
  (pcase anchor
    (`(prompt ,offset) (min (point-max) (+ sgn-chat--prompt-start offset)))
    (`(message ,rowid ,offset)
     (if-let* ((match (save-excursion
                        (goto-char (point-min))
                        (text-property-search-forward 'sgn-message-rowid rowid t))))
         (min (+ (prop-match-beginning match) offset)
              (1- (prop-match-end match)))
       (point-min)))
    (`(top ,pos) (min pos sgn-chat--prompt-start))))

(defun sgn-chat--restore-positions (anchors)
  "Restore point and window positions from ANCHORS."
  (goto-char (sgn-chat--resolve-anchor (cdr (car anchors))))
  (pcase-dolist (`(,win ,start ,point) (cdr anchors))
    (when (window-live-p win)
      (set-window-start win (sgn-chat--resolve-anchor start) t)
      (set-window-point win (sgn-chat--resolve-anchor point)))))

;;;; Rendering messages

(defun sgn-chat--insert-history (messages)
  "Insert MESSAGES, grouped under headers by sender and time."
  (let ((last-sender nil) (last-ts nil))
    (dolist (msg messages)
      (let ((sender (plist-get msg :sender))
            (ts (plist-get msg :timestamp))
            (start (point)))
        (unless (and (equal sender last-sender) last-ts
                     (< (- ts last-ts) (* 1000 sgn-message-grouping-interval)))
          (sgn-chat--insert-header sender ts))
        (sgn-chat--insert-message msg)
        (add-text-properties start (point)
                             (list 'sgn-message-rowid (plist-get msg :rowid)
                                   'keymap sgn-chat-message-map))
        (setq last-sender sender last-ts ts)))))

(defun sgn-chat--insert-header (sender timestamp)
  "Insert a header for messages from SENDER starting at TIMESTAMP."
  (let* ((label (format "── %s · %s " (sgn-contacts-display-sender sender)
                        (sgn-chat--format-timestamp timestamp)))
         (width (if-let* ((win (get-buffer-window nil t)))
                    (window-body-width win)
                  80))
         (fill (max 0 (- width (string-width label) 1))))
    (insert (propertize (concat label (make-string fill ?─) "\n")
                        'face 'sgn-header-face))))

(defun sgn-chat--insert-message (msg)
  "Insert the quote, body, media, reactions and pin mark of MSG."
  (let ((rowid (plist-get msg :rowid)))
    (when (plist-get msg :quote-ts)
      (sgn-chat--insert-quote (plist-get msg :quote-author)
                              (plist-get msg :quote-body)))
    (if (eql (plist-get msg :deleted) 1)
        (insert (propertize "  [Message deleted]" 'face 'sgn-deleted-face)
                "\n")
      (when-let* ((body (plist-get msg :body)))
        (unless (string-empty-p body)
          (insert "  " (sgn-format-render
                        body
                        (sgn-format-read-ranges (plist-get msg :styles-json))
                        (sgn-format-read-ranges (plist-get msg :mentions-json))
                        (memq rowid sgn-chat--revealed)))
          (sgn-chat--insert-annotations msg)
          (insert "\n")))
      (dolist (media (sgn-db-get-media rowid))
        (sgn-media-render media))
      (unless (plist-get msg :body)
        (when (sgn-chat--annotations msg)
          (insert " ")
          (sgn-chat--insert-annotations msg)
          (insert "\n"))))
    (sgn-chat--insert-reactions msg)
    (when (sgn-db-pinned-p sgn-chat-id (plist-get msg :sender)
                           (plist-get msg :timestamp))
      (insert "  📌\n"))))

(defun sgn-chat--insert-quote (author body)
  "Insert a quote block for the message AUTHOR wrote, BODY."
  (let* ((text (replace-regexp-in-string "\n" " " (or body "")))
         (preview (truncate-string-to-width text 60 nil nil "…")))
    (insert (propertize (format "  ┃ %s: %s\n"
                                (if author (sgn-contacts-display-sender author) "?")
                                preview)
                        'face 'sgn-quote-face))))

(defconst sgn-chat--send-status-labels
  '(("sending" "sending…" sgn-timestamp-face)
    ("unconfirmed" "not confirmed: may not have been sent" sgn-error-face)
    ("failed" "not sent" sgn-error-face)
    ("partial" "not delivered to every recipient" sgn-error-face))
  "Labels shown after sent messages, by send status.
Each entry is (STATUS LABEL FACE).  Messages whose status is not
listed, such as delivered or received ones, show no label.")

(defconst sgn-chat--edit-status-labels
  '(("sending" "saving edit…" sgn-timestamp-face)
    ("unconfirmed" "edit not confirmed" sgn-error-face)
    ("partial" "edit not delivered to every recipient" sgn-error-face))
  "Labels shown after edited messages, by the status of the latest edit.")

(defun sgn-chat--annotations (msg)
  "Return the annotations to show after MSG's text, as propertized strings."
  (delq nil
        (list
         (when (plist-get msg :edited-at)
           (propertize "(edited)" 'face 'sgn-timestamp-face))
         (when-let* ((entry (assoc (plist-get msg :send-status)
                                   sgn-chat--send-status-labels)))
           (propertize (format "(%s)" (nth 1 entry)) 'face (nth 2 entry)))
         (when-let* ((entry (assoc (plist-get msg :edit-status)
                                   sgn-chat--edit-status-labels)))
           (propertize (format "(%s)" (nth 1 entry)) 'face (nth 2 entry)))
         (when (and (eql (plist-get msg :outgoing) 1)
                    (member (plist-get msg :send-status) '(nil "sent")))
           (sgn-chat--receipt-mark (plist-get msg :timestamp))))))

(defun sgn-chat--insert-annotations (msg)
  "Insert MSG's annotations, each preceded by a space."
  (dolist (annotation (sgn-chat--annotations msg))
    (insert " " annotation)))

(defun sgn-chat--receipt-mark (timestamp)
  "Return the delivery mark for our message sent at TIMESTAMP, or nil."
  (let ((types (sgn-db-receipt-types timestamp)))
    (cond
     ((or (member "read" types) (member "viewed" types))
      (propertize "✓✓" 'face 'sgn-receipt-face 'help-echo "Read"))
     ((member "delivered" types)
      (propertize "✓" 'face 'sgn-receipt-face 'help-echo "Delivered")))))

(defun sgn-chat--insert-reactions (msg)
  "Insert the reactions to MSG on one line, if any."
  (when-let* ((reactions (sgn-db-get-reactions sgn-chat-id
                                               (plist-get msg :sender)
                                               (plist-get msg :timestamp))))
    (let ((groups nil))
      (pcase-dolist (`(,sender ,emoji) reactions)
        (push (sgn-contacts-display-sender sender)
              (alist-get emoji groups nil nil #'equal)))
      (insert "  "
              (mapconcat (pcase-lambda (`(,emoji . ,senders))
                           (format "%s %s" emoji
                                   (string-join (nreverse senders) ", ")))
                         (nreverse groups) "  ")
              "\n"))))

(defun sgn-chat--format-timestamp (timestamp-ms)
  "Format TIMESTAMP-MS according to `sgn-timestamp-format'."
  (if (eq sgn-timestamp-format 'absolute)
      (format-time-string "%b %d, %H:%M" (/ timestamp-ms 1000.0))
    (sgn-contacts-format-time timestamp-ms t)))

(defun sgn-chat-format-duration (seconds)
  "Format SECONDS as a short duration, such as \"5m\" or \"7d\"."
  (cond
   ((< seconds 60) (format "%ds" seconds))
   ((< seconds 3600) (format "%dm" (/ seconds 60)))
   ((< seconds 86400) (format "%dh" (/ seconds 3600)))
   ((< seconds 604800) (format "%dd" (/ seconds 86400)))
   (t (format "%dw" (/ seconds 604800)))))

(defun sgn-chat--header-line ()
  "Return the header line: the chat's name, timer, and who is typing."
  (let ((expiration (or (plist-get (sgn-db-get-chat sgn-chat-id) :expiration) 0))
        (typists (mapcar (lambda (entry) (sgn-contacts-display-sender (car entry)))
                         sgn-chat--typists)))
    (concat (sgn-contacts-get-name sgn-chat-id)
            (when (> expiration 0)
              (format " ⏱ %s" (sgn-chat-format-duration expiration)))
            (when typists
              (format " — %s %s typing…" (string-join typists ", ")
                      (if (cdr typists) "are" "is"))))))

;;;; Prompt and input

(defun sgn-chat--prompt-text ()
  "Return the prompt, including the reply or edit being composed."
  (cond
   (sgn-chat--reply-target
    (format "┃ Replying to %s: %s\n%s"
            (sgn-contacts-display-sender (plist-get sgn-chat--reply-target :sender))
            (truncate-string-to-width
             (replace-regexp-in-string
              "\n" " " (or (plist-get sgn-chat--reply-target :body) ""))
             40 nil nil "…")
            sgn-prompt))
   (sgn-chat--edit-target
    (format "┃ Editing message (C-g to cancel)\n%s" sgn-prompt))
   (t sgn-prompt)))

(defun sgn-chat--draw-prompt ()
  "Draw the prompt at the end of the buffer and set the markers."
  (let ((inhibit-read-only t)
        (sgn-chat--programmatic t))
    (save-excursion
      (goto-char (point-max))
      (let ((start (point)))
        (insert (propertize (sgn-chat--prompt-text)
                            'read-only t 'face 'minibuffer-prompt
                            'front-sticky '(read-only)
                            'rear-nonsticky t))
        (set-marker sgn-chat--prompt-start start)
        (set-marker sgn-chat--input-marker (point))))))

(defun sgn-chat--redraw-prompt ()
  "Redraw the prompt, keeping the input and point's place in it."
  (let ((input (sgn-chat--input-text))
        (offset (and (>= (point) sgn-chat--prompt-start)
                     (max 0 (- (point) sgn-chat--input-marker))))
        (sgn-chat--programmatic t)
        (inhibit-read-only t))
    (save-excursion
      (delete-region sgn-chat--prompt-start (point-max))
      (sgn-chat--draw-prompt)
      (goto-char (point-max))
      (insert input))
    (when offset
      (goto-char (min (point-max) (+ sgn-chat--input-marker offset))))))

(defun sgn-chat--input-text ()
  "Return the text in the input area."
  (buffer-substring-no-properties sgn-chat--input-marker (point-max)))

(defun sgn-chat--set-input (text)
  "Replace the input with TEXT."
  (let ((sgn-chat--programmatic t)
        (inhibit-read-only t))
    (delete-region sgn-chat--input-marker (point-max))
    (save-excursion
      (goto-char (point-max))
      (insert text))))

(defun sgn-chat-return ()
  "Send the input, or act on the message at point.
In the history, open the media, link or spoiler at point."
  (interactive)
  (if (>= (point) sgn-chat--input-marker)
      (sgn-chat-send-input)
    (sgn-open-at-point)))

(defun sgn-chat-send-input ()
  "Send the input to the chat, as a reply or edit if one is being composed.
The input is cleared only once the message has been handed over."
  (interactive)
  (let ((text (string-trim-right (sgn-chat--input-text))))
    (when (string-blank-p text)
      (user-error "Nothing to send"))
    (if sgn-chat--edit-target
        (sgn-send-edit sgn-chat--edit-target text)
      (sgn-send-text sgn-chat-id text sgn-chat--reply-target))
    (let ((editing sgn-chat--edit-target))
      (setq sgn-chat--reply-target nil
            sgn-chat--edit-target nil)
      (sgn-chat--set-input (if editing (sgn-chat--take-stash) ""))
      (sgn-chat--redraw-prompt)
      (unless editing
        (sgn-chat--stop-typing)
        (sgn-db-save-draft sgn-chat-id nil)))
    (goto-char (point-max))))

(defun sgn-chat--take-stash ()
  "Return the input set aside for an edit, and forget it."
  (prog1 (or sgn-chat--stashed-input "")
    (setq sgn-chat--stashed-input nil)))

(defun sgn-chat-start-reply (msg)
  "Compose a reply to MSG in the current buffer."
  (setq sgn-chat--reply-target msg
        sgn-chat--edit-target nil)
  (sgn-chat--redraw-prompt)
  (goto-char (point-max)))

(defun sgn-chat-start-edit (msg text)
  "Edit MSG, starting from TEXT, in the current buffer.
The input being composed is set aside until the edit is done."
  (unless sgn-chat--edit-target
    (setq sgn-chat--stashed-input (sgn-chat--input-text)))
  (setq sgn-chat--edit-target msg
        sgn-chat--reply-target nil)
  (sgn-chat--redraw-prompt)
  (sgn-chat--set-input text)
  (goto-char (point-max)))

(defun sgn-chat-cancel-action ()
  "Cancel the reply or edit being composed, or quit."
  (interactive)
  (cond
   (sgn-chat--reply-target
    (setq sgn-chat--reply-target nil)
    (sgn-chat--redraw-prompt)
    (message "Reply cancelled."))
   (sgn-chat--edit-target
    (setq sgn-chat--edit-target nil)
    (sgn-chat--set-input (sgn-chat--take-stash))
    (sgn-chat--redraw-prompt)
    (goto-char (point-max))
    (message "Edit cancelled."))
   (t (keyboard-quit))))

(defun sgn-load-more-history ()
  "Show `sgn-history-page-size' more of the chat's older messages."
  (interactive)
  (if (>= sgn-chat--limit (sgn-db-count-messages sgn-chat-id))
      (message "All history loaded.")
    (setq sgn-chat--limit (+ sgn-chat--limit sgn-history-page-size))
    (sgn-chat--redraw)))

(defun sgn-chat-show-message (rowid)
  "Move point to message ROWID, loading older history if needed."
  (when-let* ((msg (sgn-db-get-message-by-rowid rowid)))
    (let ((newer (sgn-db-select-value
                  "SELECT count(*) FROM messages WHERE chat_id = ?1
                     AND (timestamp > ?2 OR (timestamp = ?2 AND rowid >= ?3))
                     AND (expires_at IS NULL OR expires_at > ?4)"
                  (list sgn-chat-id (plist-get msg :timestamp) rowid
                        (sgn-db-now)))))
      (when (> newer sgn-chat--limit)
        (setq sgn-chat--limit (+ newer sgn-history-page-size))
        (sgn-chat--redraw))
      (when-let* ((match (save-excursion
                           (goto-char (point-min))
                           (text-property-search-forward 'sgn-message-rowid
                                                         rowid t))))
        (goto-char (prop-match-beginning match))
        (recenter)))))

;;;; Typing indicators sent

(defun sgn-chat--on-input-change (beg _end _len)
  "Send a typing indicator when the user changes the input at BEG."
  (when (and (not sgn-chat--programmatic)
             (>= beg sgn-chat--input-marker))
    (sgn-chat--schedule-draft-save)
    (when (and sgn-send-typing (sgn-rpc-alive-p)
               (not (sgn-store-self-p sgn-chat-id)))
      (if (string-blank-p (sgn-chat--input-text))
          (sgn-chat--stop-typing)
        (sgn-chat--start-typing)))))

(defun sgn-chat--start-typing ()
  "Tell the chat that we are typing, at most once per refresh interval."
  (when (or (null sgn-chat--typing-sent-at)
            (>= (float-time (time-since sgn-chat--typing-sent-at))
                sgn-chat--typing-refresh-interval))
    (sgn-rpc-send-typing (sgn-chat-address sgn-chat-id))
    (setq sgn-chat--typing-sent-at (current-time)))
  (when (timerp sgn-chat--typing-stop-timer)
    (cancel-timer sgn-chat--typing-stop-timer))
  (setq sgn-chat--typing-stop-timer
        (run-at-time 5 nil #'sgn-chat--stop-typing-in (current-buffer))))

(defun sgn-chat--stop-typing-in (buffer)
  "Send the typing stop indicator for the chat in BUFFER, if live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (sgn-chat--stop-typing))))

(defun sgn-chat--stop-typing ()
  "Tell the chat that we stopped typing, if we said we were."
  (when (and sgn-chat--typing-sent-at (sgn-rpc-alive-p))
    (sgn-rpc-send-typing (sgn-chat-address sgn-chat-id) t))
  (setq sgn-chat--typing-sent-at nil)
  (when (timerp sgn-chat--typing-stop-timer)
    (cancel-timer sgn-chat--typing-stop-timer))
  (setq sgn-chat--typing-stop-timer nil))

;;;; Typing indicators received

(defun sgn-chat-on-typing (chat-id sender started)
  "Show or hide that SENDER is typing in CHAT-ID, as STARTED says."
  (when-let* ((buf (sgn-chat-buffer chat-id))
              ((bound-and-true-p sgn-db--connection)))
    (with-current-buffer buf
      (when-let* ((entry (assoc sender sgn-chat--typists)))
        (cancel-timer (cdr entry))
        (setq sgn-chat--typists (delq entry sgn-chat--typists)))
      (when started
        (push (cons sender (run-at-time sgn-chat--typing-display-timeout nil
                                        #'sgn-chat-on-typing chat-id sender nil))
              sgn-chat--typists))
      (setq header-line-format (sgn-chat--header-line))
      (force-mode-line-update))))

;;;; Drafts

(defun sgn-chat--schedule-draft-save ()
  "Save the input as a draft once typing pauses."
  (when (timerp sgn-chat--draft-timer)
    (cancel-timer sgn-chat--draft-timer))
  (setq sgn-chat--draft-timer
        (run-with-idle-timer 2 nil #'sgn-chat--save-draft-in (current-buffer))))

(defun sgn-chat--save-draft-in (buffer)
  "Save the input of BUFFER as its chat's draft, if BUFFER is live."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (sgn-chat--save-draft))))

(defun sgn-chat--save-draft ()
  "Save the input as the chat's draft.
Text being edited into an existing message is not a draft."
  (when (and sgn-chat-id (bound-and-true-p sgn-db--connection))
    (sgn-db-save-draft sgn-chat-id (if sgn-chat--edit-target
                                       sgn-chat--stashed-input
                                     (sgn-chat--input-text)))))

(defun sgn-chat-save-all-drafts ()
  "Save the drafts of all chat buffers."
  (dolist (buf (sgn-chat-buffers))
    (with-current-buffer buf
      (sgn-chat--save-draft))))

(defun sgn-chat--restore-draft ()
  "Put the chat's saved draft in the input area."
  (when-let* ((draft (sgn-db-get-draft sgn-chat-id)))
    (sgn-chat--set-input draft)))

(defun sgn-chat--on-kill ()
  "Save the draft and stop timers when the buffer is killed."
  (ignore-errors (sgn-chat--save-draft))
  (ignore-errors (sgn-chat--stop-typing))
  (dolist (timer (list sgn-chat--redraw-timer sgn-chat--draft-timer
                       sgn-chat--typing-stop-timer))
    (when (timerp timer) (cancel-timer timer)))
  (dolist (entry sgn-chat--typists)
    (cancel-timer (cdr entry)))
  (sgn-chat--unregister))

;;;; Reading

(defun sgn-chat-visible-p (chat-id)
  "Return non-nil if CHAT-ID is in the selected window of a focused frame."
  (let ((buf (sgn-chat-buffer chat-id)))
    (and buf
         (eq buf (window-buffer (selected-window)))
         (frame-focus-state))))

(defun sgn-chat--mark-selected-read (&rest _)
  "Mark read the chat in the selected window, if its frame has focus."
  (let ((buf (window-buffer (selected-window))))
    (when (and (eq (buffer-local-value 'major-mode buf) 'sgn-chat-mode)
               (frame-focus-state)
               (bound-and-true-p sgn-db--connection))
      (sgn-mark-chat-read (buffer-local-value 'sgn-chat-id buf)))))

;;;; Message at point

(defun sgn-chat-message-at-point ()
  "Return the message at point as a plist, or nil."
  (when-let* ((rowid (get-text-property (point) 'sgn-message-rowid)))
    (sgn-db-get-message-by-rowid rowid)))

(defun sgn-open-at-point ()
  "Open what is at point: reveal a spoiler, play a voice note, open media or a URL."
  (interactive)
  (cond
   ((get-text-property (point) 'sgn-spoiler)
    (push (get-text-property (point) 'sgn-message-rowid) sgn-chat--revealed)
    (sgn-chat--redraw))
   ((get-text-property (point) 'sgn-voice-note)
    (sgn-media-play-audio (get-text-property (point) 'sgn-voice-note)))
   ((get-text-property (point) 'sgn-media-path)
    (sgn-media-open (get-text-property (point) 'sgn-media-path)))
   ((thing-at-point 'url)
    (browse-url (thing-at-point 'url)))
   (t (message "Nothing to open here"))))

(provide 'sgn-chat)
;;; sgn-chat.el ends here
