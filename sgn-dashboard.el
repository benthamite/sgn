;;; sgn-dashboard.el --- Chat list buffer for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Telega-style root buffer showing all conversations using
;; `tabulated-list-mode'.  Pinned chats on top, unread badges, last
;; message preview, mute indicators, and keyboard navigation.

;;; Code:

(require 'cl-lib)
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

(defvar sgn-account)

;;;; Buffer name

(defconst sgn-dashboard--buffer-name "*Sgn*"
  "Name of the dashboard buffer.")

;;;; Keymap

;; Use `setq' (not `defvar') so reloading this file always installs
;; the correct bindings, even when an older keymap is already bound.
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

(defface sgn-dashboard-unread-face
  '((t :inherit warning :weight bold))
  "Face for unread count in the dashboard."
  :group 'sgn)

(defface sgn-dashboard-name-unread-face
  '((t :weight bold))
  "Face for chat names with unread messages."
  :group 'sgn)

(defface sgn-dashboard-preview-face
  '((t :inherit shadow))
  "Face for message preview text."
  :group 'sgn)

(defface sgn-dashboard-time-face
  '((t :inherit shadow))
  "Face for timestamp column."
  :group 'sgn)

(defface sgn-dashboard-muted-face
  '((t :inherit shadow))
  "Face for muted indicator."
  :group 'sgn)

;;;; Major mode

(define-derived-mode sgn-dashboard-mode tabulated-list-mode "Sgn"
  "Major mode for the Signal chat list.

\\{sgn-dashboard-mode-map}"
  (setq tabulated-list-format
        [("Chat" 38 t)                    ; chat name
         ("Last message" 42 t)            ; preview
         ("Time" 7 t :right-align t)      ; timestamp
         ("" 5 t)])                       ; unread count
  (setq tabulated-list-padding 0)
  (setq tabulated-list-sort-key nil)      ; we sort ourselves
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'sgn-dashboard--revert)
  (tabulated-list-init-header))

(defun sgn-dashboard--revert (_ignore-auto _noconfirm)
  "Revert function for the dashboard buffer."
  (sgn-dashboard-refresh))

;;;; Sorting

(defun sgn-dashboard--entry-sort-key (entry)
  "Return a sort key for ENTRY: (PINNED-P HAS-TS-P TIMESTAMP NAME).
Pinned first, then chats with timestamps by recency, then the
rest alphabetically."
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
Skip chats with empty or missing names."
  (let ((chats (sgn-db-get-chats t)))
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
         ;; Column values
         (col-name (sgn-dashboard--make-name name has-unread pinned))
         (col-preview (sgn-dashboard--make-preview preview muted))
         (col-time (sgn-dashboard--make-time last-ts))
         (col-unread (sgn-dashboard--make-unread unread)))
    (list id (vector col-name col-preview col-time col-unread))))

(defun sgn-dashboard--make-name (name has-unread pinned)
  "Build NAME column.  Bold if HAS-UNREAD, pin prefix if PINNED."
  (let ((display (if pinned (concat "📌 " name) name)))
    (propertize display
                'face (when has-unread 'sgn-dashboard-name-unread-face)
                'sgn-pinned pinned)))

(defun sgn-dashboard--make-preview (preview muted)
  "Build PREVIEW column.  Append mute icon if MUTED."
  (let ((text (if preview
                  (propertize preview 'face 'sgn-dashboard-preview-face)
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
          (let ((text (format "%s: %s"
                              sender-name
                              (replace-regexp-in-string "\n" " " body))))
            (if (> (length text) 60)
                (concat (substring text 0 57) "…")
              text)))
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
  "Open the Sgn dashboard."
  (interactive)
  (let ((buf (get-buffer-create sgn-dashboard--buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'sgn-dashboard-mode)
        (sgn-dashboard-mode))
      (sgn-dashboard--populate))
    (switch-to-buffer buf)))

(defun sgn-dashboard--populate ()
  "Populate the dashboard with current data.
Sort order: pinned first, then chats with messages by recency,
then remaining chats alphabetically."
  (let ((entries (sgn-dashboard--build-entries)))
    (setq entries
          (sort entries
                (lambda (a b)
                  (let ((ka (sgn-dashboard--entry-sort-key a))
                        (kb (sgn-dashboard--entry-sort-key b)))
                    ;; Compare: (pinned has-ts timestamp name)
                    (cond
                     ;; Pinned first
                     ((and (nth 0 ka) (not (nth 0 kb))) t)
                     ((and (nth 0 kb) (not (nth 0 ka))) nil)
                     ;; Has-timestamp before no-timestamp
                     ((and (nth 1 ka) (not (nth 1 kb))) t)
                     ((and (nth 1 kb) (not (nth 1 ka))) nil)
                     ;; Both have timestamps: most recent first
                     ((and (nth 1 ka) (nth 1 kb))
                      (> (nth 2 ka) (nth 2 kb)))
                     ;; Neither has timestamps: alphabetical
                     (t (string< (nth 3 ka) (nth 3 kb))))))))
    (setq tabulated-list-entries entries)
    (tabulated-list-print t)))

(defun sgn-dashboard-refresh ()
  "Refresh the dashboard."
  (interactive)
  (when-let* ((buf (get-buffer sgn-dashboard--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (sgn-dashboard--populate)))))

(defun sgn-dashboard--chat-id-at-point ()
  "Return the chat ID for the entry at point, or nil."
  (tabulated-list-get-id))

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
