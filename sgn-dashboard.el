;;; sgn-dashboard.el --- Chat list buffer for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Telega-style root buffer showing all conversations using
;; `tabulated-list-mode'.  Per-column faces, fade-out truncation
;; (spofy-style gradient instead of ellipsis), pinned chats on top,
;; unread badges, and keyboard navigation.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'tabulated-list)

(declare-function sgn--log "sgn")
(declare-function sgn-db-get-chats "sgn-db")
(declare-function sgn-db-get-messages "sgn-db")
(declare-function sgn-db-set-unread "sgn-db")
(declare-function sgn-db-get-chat "sgn-db")
(declare-function sgn-db-upsert-chat "sgn-db")
(declare-function sgn-chat-open "sgn-chat")
(declare-function sgn-contacts-get-name "sgn-contacts")
(declare-function sgn-contacts-display-sender "sgn-contacts")
(declare-function sgn-notify-update "sgn-notify")
(declare-function sgn-rpc-failure-advice "sgn-rpc")

(defvar sgn-account)

;;;; Buffer name

(defconst sgn-dashboard--buffer-name "*sgn*"
  "Name of the dashboard buffer.")

;;;; Keymap

(defvar sgn-dashboard-mode-map nil
  "Keymap for `sgn-dashboard-mode'.")
(setq sgn-dashboard-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map (kbd "RET") #'sgn-dashboard-open)
        (define-key map (kbd "c") #'sgn-chat)
        (define-key map (kbd "s") #'sgn-search)
        (define-key map (kbd "g") #'sgn-dashboard-refresh)
        (define-key map (kbd "d") #'sgn-dashboard-mark-read)
        (define-key map (kbd "M") #'sgn-dashboard-toggle-mute)
        (define-key map (kbd "P") #'sgn-dashboard-toggle-pin)
        map))

(declare-function sgn-chat "sgn")
(declare-function sgn-search "sgn-search")

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
            (vector (list "Chat" name t)
                    (list "Last message" preview t)
                    (list "Time" sgn-dashboard--time-width t :right-align t)
                    (list "" sgn-dashboard--unread-width t)))
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
  (sgn-dashboard-refresh))

;;;; Sorting

(defun sgn-dashboard--entry-sort-key (entry)
  "Return a sort key for ENTRY: (PINNED-P HAS-TS-P TIMESTAMP NAME)."
  (let* ((vec (cadr entry))
         (name-str (elt vec 0))
         (time-str (elt vec 2))
         (pinned (get-text-property 0 'sgn-pinned name-str))
         (ts (get-text-property 0 'sgn-timestamp time-str))
         (name-lower (downcase (substring-no-properties name-str))))
    (list pinned (and ts (> ts 0)) (or ts 0) name-lower)))

;;;; Building entries

(defun sgn-dashboard--build-entries ()
  "Build the tabulated-list entries from the database.
Only include chats that have at least one stored message."
  (let ((chats (sgn-db-get-chats)))
    (cl-loop for chat in chats
             for name = (or (plist-get chat :name)
                            (sgn-contacts-get-name (plist-get chat :id)))
             when (and name (not (string-empty-p (string-trim name))))
             collect (sgn-dashboard--chat-to-entry chat))))

(defun sgn-dashboard--chat-to-entry (chat)
  "Convert a CHAT plist to a tabulated-list entry."
  (let* ((id (plist-get chat :id))
         (name (or (plist-get chat :name)
                   (sgn-contacts-get-name id)))
         (unread (or (plist-get chat :unread) 0))
         (muted (and (plist-get chat :muted)
                     (not (zerop (plist-get chat :muted)))))
         (pinned (and (plist-get chat :pinned)
                      (not (zerop (plist-get chat :pinned)))))
         (last-ts (plist-get chat :last-msg-ts))
         (preview (sgn-dashboard--get-preview id))
         (has-unread (> unread 0))
         (col-name (sgn-dashboard--make-name name has-unread pinned))
         (col-preview (sgn-dashboard--make-preview preview muted))
         (col-time (sgn-dashboard--make-time last-ts))
         (col-unread (sgn-dashboard--make-unread unread)))
    (list id (vector col-name col-preview col-time col-unread))))

(defun sgn-dashboard--make-name (name has-unread pinned)
  "Build NAME column with face and fade truncation."
  (let* ((display (if pinned (concat "📌 " name) name))
         (face (if has-unread
                   'sgn-dashboard-name-unread-face
                 'sgn-dashboard-name-face)))
    (propertize (sgn-dashboard--truncate
                 display (1- sgn-dashboard--name-width) face)
                'sgn-pinned pinned)))

(defun sgn-dashboard--make-preview (preview muted)
  "Build PREVIEW column with face and fade truncation."
  (let ((text (if preview
                  (sgn-dashboard--truncate
                   preview (1- sgn-dashboard--preview-width)
                   'sgn-dashboard-preview-face)
                "")))
    (if muted
        (concat text (propertize " 🔇" 'face 'sgn-dashboard-muted-face))
      text)))

(defun sgn-dashboard--make-time (timestamp-ms)
  "Build time column from TIMESTAMP-MS."
  (let ((str (if timestamp-ms
                 (sgn-dashboard--format-time timestamp-ms)
               "")))
    (propertize str
                'face 'sgn-dashboard-time-face
                'sgn-timestamp (or timestamp-ms 0))))

(defun sgn-dashboard--make-unread (count)
  "Build unread COUNT column."
  (if (> count 0)
      (propertize (format "(%d)" count) 'face 'sgn-dashboard-unread-face)
    ""))

;;;; Preview and time formatting

(defun sgn-dashboard--get-preview (chat-id)
  "Get last message preview for CHAT-ID."
  (let ((messages (sgn-db-get-messages chat-id 1)))
    (when messages
      (let* ((msg (car messages))
             (sender (plist-get msg :sender))
             (body (plist-get msg :body))
             (deleted (plist-get msg :deleted))
             (sender-name (sgn-contacts-display-sender sender)))
        (cond
         ((and deleted (not (zerop deleted)))
          "[deleted]")
         (body
          (format "%s: %s"
                  sender-name
                  (replace-regexp-in-string "\n" " " body)))
         (t
          (format "%s: [media]" sender-name)))))))

(defun sgn-dashboard--format-time (timestamp-ms)
  "Format TIMESTAMP-MS for the dashboard."
  (when timestamp-ms
    (let* ((time (seconds-to-time (/ timestamp-ms 1000.0)))
           (now (current-time))
           (diff (float-time (time-subtract now time))))
      (cond
       ((< diff 86400)
        (format-time-string "%H:%M" time))
       ((< diff 604800)
        (format-time-string "%a" time))
       (t
        (format-time-string "%b %d" time))))))

;;;; Commands

;;;###autoload
(defun sgn-dashboard ()
  "Open the sgn dashboard."
  (interactive)
  (require 'sgn)
  (let ((buf (get-buffer-create sgn-dashboard--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'sgn-dashboard-mode)
        (sgn-dashboard-mode))
      (sgn-dashboard--populate))
    (switch-to-buffer buf)))

(defun sgn-dashboard--populate ()
  "Populate the dashboard with current data."
  (sgn-dashboard--set-columns (sgn-dashboard--window-width))
  (let ((entries (sgn-dashboard--build-entries)))
    (setq entries
          (sort entries
                (lambda (a b)
                  (let ((ka (sgn-dashboard--entry-sort-key a))
                        (kb (sgn-dashboard--entry-sort-key b)))
                    (cond
                     ((and (nth 0 ka) (not (nth 0 kb))) t)
                     ((and (nth 0 kb) (not (nth 0 ka))) nil)
                     ((and (nth 1 ka) (not (nth 1 kb))) t)
                     ((and (nth 1 kb) (not (nth 1 ka))) nil)
                     ((and (nth 1 ka) (nth 1 kb))
                      (> (nth 2 ka) (nth 2 kb)))
                     (t (string< (nth 3 ka) (nth 3 kb))))))))
    (setq tabulated-list-entries entries)
    (tabulated-list-print t)
    (sgn-dashboard--apply-fades)
    (sgn-dashboard--update-failure-banner)))

(defvar-local sgn-dashboard--failure-overlay nil
  "Overlay showing the signal-cli failure above the chat list.")

(defun sgn-dashboard--update-failure-banner ()
  "Show the signal-cli failure above the chat list, or remove it.
The banner is an overlay string, so it does not become a row and
does not affect row lookups."
  (when sgn-dashboard--failure-overlay
    (delete-overlay sgn-dashboard--failure-overlay)
    (setq sgn-dashboard--failure-overlay nil))
  (when-let* ((advice (sgn-rpc-failure-advice)))
    (setq sgn-dashboard--failure-overlay (make-overlay (point-min) (point-min)))
    (overlay-put sgn-dashboard--failure-overlay 'before-string
                 (propertize (format "No new messages: %s\n\n" advice)
                             'face 'sgn-dashboard-failure-face))))

(defun sgn-dashboard-refresh ()
  "Refresh the dashboard."
  (interactive)
  (when-let* ((buf (get-buffer sgn-dashboard--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (sgn-dashboard--populate)))))

(defun sgn-dashboard--chat-id-at-point ()
  "Return the chat ID for the entry at point, or nil."
  (or (tabulated-list-get-id)
      (sgn-dashboard--chat-id-on-current-line)))

(defun sgn-dashboard--chat-id-on-current-line ()
  "Return the chat ID for the entry on the current line, or nil."
  (let ((end (line-end-position))
        id)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (and (< (point) end) (not id))
        (setq id (get-text-property (point) 'tabulated-list-id))
        (unless id
          (goto-char (or (next-single-property-change
                          (point) 'tabulated-list-id nil end)
                         end)))))
    id))

(defun sgn-dashboard-open ()
  "Open the chat at point."
  (interactive)
  (let ((chat-id (sgn-dashboard--chat-id-at-point)))
    (if chat-id
        (sgn-chat-open chat-id)
      (user-error "No chat at point"))))

(defun sgn-dashboard-mark-read ()
  "Mark the chat at point as read."
  (interactive)
  (let ((chat-id (sgn-dashboard--chat-id-at-point)))
    (unless chat-id
      (user-error "No chat at point"))
    (sgn-db-set-unread chat-id 0)
    (sgn-notify-update)
    (sgn-dashboard--populate)
    (message "Marked as read.")))

(defun sgn-dashboard-toggle-mute ()
  "Toggle mute on the chat at point."
  (interactive)
  (let ((chat-id (sgn-dashboard--chat-id-at-point)))
    (unless chat-id
      (user-error "No chat at point"))
    (let* ((chat (sgn-db-get-chat chat-id))
           (currently-muted (and chat (plist-get chat :muted)
                                (not (zerop (plist-get chat :muted)))))
           (new-muted (if currently-muted 0 1)))
      (sgn-db-upsert-chat chat-id :muted new-muted)
      (sgn-dashboard--populate)
      (message (if (zerop new-muted) "Unmuted." "Muted.")))))

(defun sgn-dashboard-toggle-pin ()
  "Toggle pin on the chat at point."
  (interactive)
  (let ((chat-id (sgn-dashboard--chat-id-at-point)))
    (unless chat-id
      (user-error "No chat at point"))
    (let* ((chat (sgn-db-get-chat chat-id))
           (currently-pinned (and chat (plist-get chat :pinned)
                                 (not (zerop (plist-get chat :pinned)))))
           (new-pinned (if currently-pinned 0 1)))
      (sgn-db-upsert-chat chat-id :pinned new-pinned)
      (sgn-dashboard--populate)
      (message (if (zerop new-pinned) "Unpinned." "Pinned.")))))

(provide 'sgn-dashboard)
;;; sgn-dashboard.el ends here
