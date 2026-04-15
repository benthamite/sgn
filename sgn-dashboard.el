;;; sgn-dashboard.el --- Chat list buffer for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Telega-style root buffer showing all conversations sorted by
;; recency, with pinned chats on top, unread badges, and last message
;; preview.

;;; Code:

(require 'cl-lib)

(declare-function sgn--log "sgn")
(declare-function sgn-db-get-chats "sgn-db")
(declare-function sgn-db-get-messages "sgn-db")
(declare-function sgn-db-set-unread "sgn-db")
(declare-function sgn-db-get-chat "sgn-db")
(declare-function sgn-db-upsert-chat "sgn-db")
(declare-function sgn-chat-open "sgn-chat")
(declare-function sgn-contacts-get-name "sgn-contacts")
(declare-function sgn-contacts-display-sender "sgn-contacts")
(declare-function sgn-contacts-completing-read "sgn-contacts")
(declare-function sgn-notify-update "sgn-notify")

(defvar sgn-account)

;;;; Faces (inherit from sgn-notify for consistency)

(defvar sgn-unread-face)

;;;; Buffer name

(defconst sgn-dashboard--buffer-name "*Sgn*"
  "Name of the dashboard buffer.")

;;;; Keymap

(defvar sgn-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'sgn-dashboard-open)
    (define-key map (kbd "c") #'sgn-chat)
    (define-key map (kbd "s") #'sgn-search)
    (define-key map (kbd "g") #'sgn-dashboard-refresh)
    (define-key map (kbd "d") #'sgn-dashboard-mark-read)
    (define-key map (kbd "M") #'sgn-dashboard-toggle-mute)
    (define-key map (kbd "P") #'sgn-dashboard-toggle-pin)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    map)
  "Keymap for `sgn-dashboard-mode'.")

(declare-function sgn-chat "sgn")
(declare-function sgn-search "sgn-search")

;;;; Major mode

(define-derived-mode sgn-dashboard-mode special-mode "Sgn"
  "Major mode for the Signal chat list."
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'sgn-dashboard--revert))

(defun sgn-dashboard--revert (_ignore-auto _noconfirm)
  "Revert function for the dashboard buffer."
  (sgn-dashboard-refresh))

;;;; Drawing

(defun sgn-dashboard--draw ()
  "Draw the dashboard content."
  (let* ((inhibit-read-only t)
         (chats (sgn-db-get-chats))
         (line (line-number-at-pos))
         (col (current-column)))
    (erase-buffer)
    (insert (propertize "*Sgn*\n\n" 'face 'bold))
    (if (null chats)
        (insert (propertize "  No conversations yet.\n" 'face 'shadow))
      (dolist (chat chats)
        (sgn-dashboard--draw-chat chat)))
    ;; Restore position
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column col)))

(defun sgn-dashboard--draw-chat (chat)
  "Draw a single CHAT entry."
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
         (time-str (if last-ts (sgn-dashboard--format-time last-ts) ""))
         (start (point))
         ;; Layout: icon + name (padded) + preview (truncated) + time + unread
         (max-name-width 24)
         (max-preview-width 30)
         (padded-name (sgn-dashboard--pad-or-truncate name max-name-width))
         (padded-preview (sgn-dashboard--pad-or-truncate
                          (or preview "") max-preview-width))
         (unread-str (if (> unread 0) (format "(%d)" unread) ""))
         (icon (cond (pinned "📌 ")
                     ((> unread 0) " ● ")
                     (t "   "))))
    ;; Draw line
    (insert icon)
    (if (> unread 0)
        (insert (propertize padded-name 'face 'bold))
      (insert padded-name))
    (insert "  ")
    (insert (propertize padded-preview 'face 'shadow))
    (insert "  ")
    (insert (propertize time-str 'face 'shadow))
    (when (> unread 0)
      (insert "  " (propertize unread-str 'face 'sgn-unread-face)))
    (when muted
      (insert (propertize " 🔇" 'face 'shadow)))
    (insert "\n")
    ;; Properties for commands
    (put-text-property start (point) 'sgn-dashboard-chat-id id)))

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
          (let ((text (format "%s: %s" sender-name body)))
            (if (> (length text) 50)
                (concat (substring text 0 47) "...")
              text)))
         (t
          (format "%s: [media]" sender-name)))))))

(defun sgn-dashboard--pad-or-truncate (str width)
  "Pad STR to WIDTH or truncate with ellipsis."
  (let ((len (length str)))
    (cond
     ((> len width)
      (concat (substring str 0 (- width 3)) "..."))
     ((< len width)
      (concat str (make-string (- width len) ?\s)))
     (t str))))

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
      (sgn-dashboard--draw))
    (switch-to-buffer buf)))

(defun sgn-dashboard-refresh ()
  "Refresh the dashboard."
  (interactive)
  (when (get-buffer sgn-dashboard--buffer-name)
    (with-current-buffer sgn-dashboard--buffer-name
      (sgn-dashboard--draw))))

(defun sgn-dashboard-open ()
  "Open the chat at point."
  (interactive)
  (let ((chat-id (get-text-property (point) 'sgn-dashboard-chat-id)))
    (if chat-id
        (sgn-chat-open chat-id)
      (user-error "No chat at point"))))

(defun sgn-dashboard-mark-read ()
  "Mark the chat at point as read."
  (interactive)
  (let ((chat-id (get-text-property (point) 'sgn-dashboard-chat-id)))
    (unless chat-id
      (user-error "No chat at point"))
    (sgn-db-set-unread chat-id 0)
    (sgn-notify-update)
    (sgn-dashboard--draw)
    (message "Marked as read.")))

(defun sgn-dashboard-toggle-mute ()
  "Toggle mute on the chat at point."
  (interactive)
  (let ((chat-id (get-text-property (point) 'sgn-dashboard-chat-id)))
    (unless chat-id
      (user-error "No chat at point"))
    (let* ((chat (sgn-db-get-chat chat-id))
           (currently-muted (and chat (plist-get chat :muted)
                                (not (zerop (plist-get chat :muted)))))
           (new-muted (if currently-muted 0 1)))
      (sgn-db-upsert-chat chat-id :muted new-muted)
      (sgn-dashboard--draw)
      (message (if (zerop new-muted) "Unmuted." "Muted.")))))

(defun sgn-dashboard-toggle-pin ()
  "Toggle pin on the chat at point."
  (interactive)
  (let ((chat-id (get-text-property (point) 'sgn-dashboard-chat-id)))
    (unless chat-id
      (user-error "No chat at point"))
    (let* ((chat (sgn-db-get-chat chat-id))
           (currently-pinned (and chat (plist-get chat :pinned)
                                 (not (zerop (plist-get chat :pinned)))))
           (new-pinned (if currently-pinned 0 1)))
      (sgn-db-upsert-chat chat-id :pinned new-pinned)
      (sgn-dashboard--draw)
      (message (if (zerop new-pinned) "Unpinned." "Pinned.")))))

(provide 'sgn-dashboard)
;;; sgn-dashboard.el ends here
