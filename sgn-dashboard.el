;;; sgn-dashboard.el --- Chat list buffer for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; The dashboard lists chats with messages, pinned chats first and
;; then by latest activity, with a preview of each chat's latest
;; message and its unread count.  It redraws whenever the store
;; reports a change, keeping each window on the same chat.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'color)
(require 'tabulated-list)
(require 'sgn-db)
(require 'sgn-format)
(require 'sgn-contacts)

(declare-function sgn-chat "sgn")
(declare-function sgn-chat-open "sgn-chat")
(declare-function sgn-mark-chat-read "sgn")
(declare-function sgn-search "sgn-search")
(declare-function sgn-start "sgn")
(declare-function sgn-rpc-alive-p "sgn-rpc")
(declare-function sgn-rpc-failure-advice "sgn-rpc")
(declare-function sgn-store-changed "sgn-store")

(defvar sgn-rpc--failure)

;;;; Buffer name

(defconst sgn-dashboard--buffer-name "*sgn*"
  "Name of the dashboard buffer.")

;;;; Keymap

(defvar sgn-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'sgn-dashboard-open)
    (define-key map (kbd "c") #'sgn-chat)
    (define-key map (kbd "s") #'sgn-search)
    (define-key map (kbd "g") #'sgn-dashboard-refresh)
    (define-key map (kbd "d") #'sgn-dashboard-mark-read)
    (define-key map (kbd "M") #'sgn-dashboard-toggle-mute)
    (define-key map (kbd "P") #'sgn-dashboard-toggle-pin)
    map)
  "Keymap for `sgn-dashboard-mode'.")

;;;; Faces

(defface sgn-dashboard-name-face
  '((t :inherit font-lock-keyword-face))
  "Face for chat names."
  :group 'sgn)

(defface sgn-dashboard-name-unread-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for chat names with unread messages."
  :group 'sgn)

(defface sgn-dashboard-preview-face
  '((t :inherit font-lock-comment-face))
  "Face for message preview text."
  :group 'sgn)

(defface sgn-dashboard-time-face
  '((t :inherit font-lock-type-face))
  "Face for timestamp column."
  :group 'sgn)

(defface sgn-dashboard-unread-face
  '((t :inherit warning :weight bold))
  "Face for unread count."
  :group 'sgn)

(defface sgn-dashboard-muted-face
  '((t :inherit shadow))
  "Face for muted indicator."
  :group 'sgn)

(defface sgn-dashboard-failure-face
  '((t :inherit error))
  "Face for the banner shown when signal-cli has failed."
  :group 'sgn)

;;;; Fade-out truncation (adapted from spofy-ui)

(defun sgn-dashboard--truncate (string max-width &optional face)
  "Truncate STRING to MAX-WIDTH with fade effect.
When FACE is non-nil, apply it.  If truncated, mark the last
three characters with `sgn-fade' property for post-render
gradient blending."
  (let* ((truncated (> (string-width string) max-width))
         (result (if truncated
                     (truncate-string-to-width string max-width)
                   string)))
    (when face
      (setq result (propertize result 'face face)))
    (when truncated
      (let ((len (length result)))
        (when (>= len 3)
          (dotimes (i 3)
            (put-text-property (+ (- len 3) i) (+ (- len 3) i 1)
                               'sgn-fade (1+ i) result)))))
    result))

(defun sgn-dashboard--blend-color (fg bg ratio)
  "Blend FG toward BG by RATIO (0.0 = pure FG, 1.0 = pure BG).
Return a hex color string, or nil if either color is unresolvable."
  (let ((fv (color-values fg))
        (bv (color-values bg)))
    (when (and fv bv)
      (format "#%02x%02x%02x"
              (ash (round (+ (* (- 1.0 ratio) (nth 0 fv))
                             (* ratio (nth 0 bv))))
                   -8)
              (ash (round (+ (* (- 1.0 ratio) (nth 1 fv))
                             (* ratio (nth 1 bv))))
                   -8)
              (ash (round (+ (* (- 1.0 ratio) (nth 2 fv))
                             (* ratio (nth 2 bv))))
                   -8)))))

(defun sgn-dashboard--resolve-foreground (face-val)
  "Resolve the effective foreground color from FACE-VAL."
  (cond
   ((symbolp face-val)
    (face-foreground face-val nil t))
   ((consp face-val)
    (cl-some (lambda (f)
               (and (facep f) (face-foreground f nil t)))
             face-val))))

(defvar-local sgn-dashboard--fade-overlays nil
  "Overlays for the truncation fade effect.")

(defun sgn-dashboard--apply-fades ()
  "Create fade overlays for characters marked with `sgn-fade'.
Levels 1/2/3 blend foreground toward background at 25%/50%/75%."
  (mapc #'delete-overlay sgn-dashboard--fade-overlays)
  (setq sgn-dashboard--fade-overlays nil)
  (let ((bg (face-background 'default nil t)))
    (when bg
      (save-excursion
        (goto-char (point-min))
        (let ((pos (point-min)))
          (while (< pos (point-max))
            (let ((level (get-text-property pos 'sgn-fade)))
              (if (not level)
                  (setq pos (or (next-single-property-change
                                 pos 'sgn-fade nil (point-max))
                                (point-max)))
                (let* ((face-val (get-text-property pos 'face))
                       (fg (or (sgn-dashboard--resolve-foreground face-val)
                               (face-foreground 'default nil t)))
                       (ratio (* 0.25 level))
                       (blended (when fg
                                  (sgn-dashboard--blend-color fg bg ratio))))
                  (when blended
                    (let ((ov (make-overlay pos (1+ pos))))
                      (overlay-put ov 'face (list :foreground blended))
                      (overlay-put ov 'sgn-fade t)
                      (push ov sgn-dashboard--fade-overlays))))
                (setq pos (1+ pos))))))))))

;;;; Column layout

(defconst sgn-dashboard--time-width 7
  "Width of the time column.")

(defconst sgn-dashboard--unread-width 5
  "Width of the unread count column.")

(defvar-local sgn-dashboard--name-width 38
  "Width of the chat name column, set from the window width.")

(defvar-local sgn-dashboard--preview-width 42
  "Width of the preview column, set from the window width.")

(defun sgn-dashboard--column-widths (window-width)
  "Return (NAME-WIDTH PREVIEW-WIDTH) filling WINDOW-WIDTH columns.
The name column takes about a third of the flexible space, within
20 to 40 columns; the preview takes the rest."
  ;; Padding, one separator after each of the first three columns, the
  ;; fixed columns, and one spare column so rows never wrap.
  (let* ((fixed (+ 1 3 sgn-dashboard--time-width sgn-dashboard--unread-width 1))
         (flexible (max 40 (- window-width fixed)))
         (name (min 40 (max 20 (/ flexible 3)))))
    (list name (- flexible name))))

(defun sgn-dashboard--set-columns (window-width)
  "Size the columns to fill WINDOW-WIDTH.  Return non-nil if they changed."
  (pcase-let ((`(,name ,preview) (sgn-dashboard--column-widths window-width)))
    (unless (and (= name sgn-dashboard--name-width)
                 (= preview sgn-dashboard--preview-width)
                 tabulated-list-format)
      (setq sgn-dashboard--name-width name
            sgn-dashboard--preview-width preview)
      (setq tabulated-list-format
            (vector (list "Chat" name nil)
                    (list "Last message" preview nil)
                    (list "Time" sgn-dashboard--time-width nil :right-align t)
                    (list "" sgn-dashboard--unread-width nil)))
      (tabulated-list-init-header)
      t)))

(defun sgn-dashboard--window-width ()
  "Return the body width of the window showing the dashboard."
  (window-body-width (or (get-buffer-window (current-buffer) t)
                         (selected-window))))

(defun sgn-dashboard--on-window-change (window)
  "Refill the columns if WINDOW's width no longer matches them."
  (with-current-buffer (window-buffer window)
    (when (sgn-dashboard--set-columns (window-body-width window))
      (sgn-dashboard--populate))))


;;;; Major mode

(define-derived-mode sgn-dashboard-mode tabulated-list-mode "sgn"
  "Major mode for the Signal chat list.

\\{sgn-dashboard-mode-map}"
  (setq tabulated-list-format nil)
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (setq-local truncate-lines t)
  (setq-local truncate-string-ellipsis "")
  (setq-local revert-buffer-function #'sgn-dashboard--revert)
  (add-hook 'window-size-change-functions #'sgn-dashboard--on-window-change nil t)
  (add-hook 'window-buffer-change-functions #'sgn-dashboard--on-window-change nil t)
  (sgn-dashboard--set-columns (sgn-dashboard--window-width)))

(defun sgn-dashboard--revert (_ignore-auto _noconfirm)
  "Revert function for the dashboard buffer."
  (sgn-dashboard--populate))

;;;; Entries

(defun sgn-dashboard--entries ()
  "Return the tabulated-list entries for chats with messages."
  (let ((latest (sgn-db-latest-messages)))
    (mapcar (lambda (chat)
              (sgn-dashboard--entry chat (gethash (plist-get chat :id) latest)))
            (sgn-db-get-chats))))

(defun sgn-dashboard--chat-name (chat)
  "Return the name to show for CHAT."
  (let ((id (plist-get chat :id)))
    (if (and (equal (plist-get chat :type) "group")
             (not (sgn-contacts-name-known-p id)))
        "Unnamed group"
      (sgn-contacts-get-name id))))

(defun sgn-dashboard--entry (chat latest)
  "Return the entry for CHAT, whose latest message is LATEST."
  (let ((unread (plist-get chat :unread))
        (muted (eql (plist-get chat :muted) 1))
        (pinned (eql (plist-get chat :pinned) 1)))
    (list (plist-get chat :id)
          (vector (sgn-dashboard--make-name (sgn-dashboard--chat-name chat)
                                            (> unread 0) pinned)
                  (sgn-dashboard--make-preview (sgn-dashboard--preview latest)
                                               muted)
                  (propertize (if-let* ((ts (plist-get chat :last-msg-ts)))
                                  (sgn-contacts-format-time ts)
                                "")
                              'face 'sgn-dashboard-time-face)
                  (if (> unread 0)
                      (propertize (format "(%d)" unread)
                                  'face 'sgn-dashboard-unread-face)
                    "")))))

(defun sgn-dashboard--make-name (name has-unread pinned)
  "Return the name column for NAME, bold if HAS-UNREAD, marked if PINNED."
  (sgn-dashboard--truncate (if pinned (concat "📌 " name) name)
                           (1- sgn-dashboard--name-width)
                           (if has-unread
                               'sgn-dashboard-name-unread-face
                             'sgn-dashboard-name-face)))

(defun sgn-dashboard--make-preview (preview muted)
  "Return the preview column for PREVIEW, marked if MUTED."
  (concat (if preview
              (sgn-dashboard--truncate preview (1- sgn-dashboard--preview-width)
                                       'sgn-dashboard-preview-face)
            "")
          (when muted
            (propertize " 🔇" 'face 'sgn-dashboard-muted-face))))

(defun sgn-dashboard--preview (msg)
  "Return the preview line for MSG, or nil."
  (when msg
    (format "%s: %s"
            (sgn-contacts-display-sender (plist-get msg :sender))
            (cond ((eql (plist-get msg :deleted) 1) "[deleted]")
                  ((sgn-format-preview msg))
                  (t "[media]")))))

;;;; Drawing

(defvar sgn-dashboard--refresh-timer nil
  "Timer for a pending dashboard refresh.")

(defun sgn-dashboard-on-store-changed (_chat-ids)
  "Refresh the dashboard soon, as `sgn-store-changed-functions' asks."
  (unless (timerp sgn-dashboard--refresh-timer)
    (setq sgn-dashboard--refresh-timer
          (run-at-time 0 nil #'sgn-dashboard-refresh))))

(defun sgn-dashboard-refresh ()
  "Redraw the dashboard, if it exists."
  (interactive)
  (when (timerp sgn-dashboard--refresh-timer)
    (cancel-timer sgn-dashboard--refresh-timer))
  (setq sgn-dashboard--refresh-timer nil)
  (when-let* ((buf (get-buffer sgn-dashboard--buffer-name)))
    (with-current-buffer buf
      (sgn-dashboard--populate))))

(defun sgn-dashboard--populate ()
  "Fill the dashboard, keeping each window on the same chat."
  (sgn-dashboard--set-columns (sgn-dashboard--window-width))
  (let ((positions (mapcar (lambda (win)
                             (list win (sgn-dashboard--id-at (window-point win))
                                   (count-lines (window-start win)
                                                (window-point win))))
                           (get-buffer-window-list nil nil t)))
        (here (sgn-dashboard--id-at (point))))
    (setq tabulated-list-entries
          (and (bound-and-true-p sgn-db--connection) (sgn-dashboard--entries)))
    (tabulated-list-print)
    (sgn-dashboard--goto-id here)
    (pcase-dolist (`(,win ,id ,lines) positions)
      (when (window-live-p win)
        (with-selected-window win
          (sgn-dashboard--goto-id id)
          (set-window-point win (point))
          (recenter lines))))
    (sgn-dashboard--apply-fades)
    (sgn-dashboard--update-failure-banner)))

(defun sgn-dashboard--id-at (pos)
  "Return the chat ID of the row at POS, or nil."
  (save-excursion
    (goto-char pos)
    (sgn-dashboard--chat-id-at-point)))

(defun sgn-dashboard--goto-id (id)
  "Move point to the row of chat ID, if there is one."
  (when id
    (goto-char (point-min))
    (while (and (not (eobp)) (not (equal (tabulated-list-get-id) id)))
      (forward-line 1))
    (when (eobp)
      (goto-char (point-min)))))

(defvar-local sgn-dashboard--failure-overlay nil
  "Overlay showing the signal-cli failure above the chat list.")

(defun sgn-dashboard--update-failure-banner ()
  "Show the signal-cli failure above the chat list, or remove it.
The banner is an overlay string, so it does not become a row."
  (when sgn-dashboard--failure-overlay
    (delete-overlay sgn-dashboard--failure-overlay)
    (setq sgn-dashboard--failure-overlay nil))
  (when-let* ((advice (sgn-rpc-failure-advice)))
    (setq sgn-dashboard--failure-overlay (make-overlay (point-min) (point-min)))
    (overlay-put sgn-dashboard--failure-overlay 'before-string
                 (propertize (format "No new messages: %s\n\n" advice)
                             'face 'sgn-dashboard-failure-face))))

;;;; Commands

;;;###autoload
(defun sgn-dashboard ()
  "Open the sgn dashboard, starting sgn if it is not running.
If signal-cli has already failed, do not restart it: the
dashboard shows the failure instead."
  (interactive)
  (require 'sgn)
  (unless (or (sgn-rpc-alive-p) sgn-rpc--failure)
    (sgn-start))
  (let ((buf (get-buffer-create sgn-dashboard--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'sgn-dashboard-mode)
        (sgn-dashboard-mode))
      (sgn-dashboard--populate))
    (switch-to-buffer buf)))

(defun sgn-dashboard--chat-id-at-point ()
  "Return the chat ID of the row at point, or nil."
  (or (tabulated-list-get-id)
      (let ((end (line-end-position))
            (id nil))
        (save-excursion
          (goto-char (line-beginning-position))
          (while (and (< (point) end) (not id))
            (setq id (get-text-property (point) 'tabulated-list-id))
            (unless id
              (goto-char (or (next-single-property-change
                              (point) 'tabulated-list-id nil end)
                             end)))))
        id)))

(defun sgn-dashboard--chat-at-point ()
  "Return the chat ID of the row at point, or signal an error."
  (or (sgn-dashboard--chat-id-at-point)
      (user-error "No chat at point")))

(defun sgn-dashboard-open ()
  "Open the chat at point."
  (interactive)
  (sgn-chat-open (sgn-dashboard--chat-at-point)))

(defun sgn-dashboard-mark-read ()
  "Mark the chat at point read, sending read receipts."
  (interactive)
  (sgn-mark-chat-read (sgn-dashboard--chat-at-point))
  (message "Marked as read."))

(defun sgn-dashboard--toggle (column)
  "Toggle the 0/1 COLUMN keyword of the chat at point; return its new value."
  (let* ((id (sgn-dashboard--chat-at-point))
         (value (if (eql (plist-get (sgn-db-get-chat id) column) 1) 0 1)))
    (sgn-db-update-chat id column value)
    (sgn-store-changed id)
    value))

(defun sgn-dashboard-toggle-mute ()
  "Mute or unmute the chat at point."
  (interactive)
  (message (if (eql (sgn-dashboard--toggle :muted) 1) "Muted." "Unmuted.")))

(defun sgn-dashboard-toggle-pin ()
  "Pin or unpin the chat at point."
  (interactive)
  (message (if (eql (sgn-dashboard--toggle :pinned) 1) "Pinned." "Unpinned.")))

(provide 'sgn-dashboard)
;;; sgn-dashboard.el ends here
