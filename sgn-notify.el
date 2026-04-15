;;; sgn-notify.el --- Notifications for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Notification system: modeline/tab-bar unread indicator and desktop
;; notifications (OS-native).

;;; Code:

(require 'cl-lib)

(declare-function sgn--log "sgn")
(declare-function sgn-db-get-chats "sgn-db")
(declare-function sgn-db-get-chat "sgn-db")
(declare-function sgn-chat-open "sgn-chat")
(declare-function sgn-contacts-get-name "sgn-contacts")

(defvar sgn-account)

;;;; Customization

(defcustom sgn-notification-style 'modeline
  "Where to display the unread count indicator.
`modeline' adds to the global mode line; `tab-bar' adds to the tab bar."
  :type '(choice (const :tag "Mode line" modeline)
                 (const :tag "Tab bar" tab-bar))
  :group 'sgn)

(defcustom sgn-desktop-notifications t
  "If non-nil, show desktop notifications for incoming messages."
  :type 'boolean
  :group 'sgn)

;;;; Faces

(defface sgn-unread-face
  '((t :inherit warning))
  "Face for unread count indicators."
  :group 'sgn)

;;;; Internal state

(defvar sgn-notify--global-unread 0
  "Total unread message count across all chats.")

(defvar sgn-notify--modeline-string ""
  "Current modeline indicator string.")

(defvar sgn-notify--tab-bar-item nil
  "Tab bar item for the unread indicator.")

;;;; Modeline indicator

(defun sgn-notify--update-modeline ()
  "Update the modeline indicator string."
  (setq sgn-notify--modeline-string
        (if (> sgn-notify--global-unread 0)
            (propertize (format " [Sgn:%d]" sgn-notify--global-unread)
                        'face 'sgn-unread-face)
          "")))

(defun sgn-notify--install-modeline ()
  "Add the sgn indicator to `global-mode-string'."
  (unless (memq 'sgn-notify--modeline-string global-mode-string)
    (setq global-mode-string
          (append global-mode-string '(sgn-notify--modeline-string)))))

(defun sgn-notify--remove-modeline ()
  "Remove the sgn indicator from `global-mode-string'."
  (setq global-mode-string
        (delq 'sgn-notify--modeline-string global-mode-string)))

;;;; Tab-bar indicator

(defun sgn-notify--tab-bar-item ()
  "Return a tab-bar item showing the unread count."
  (when (> sgn-notify--global-unread 0)
    (propertize (format " Sgn:%d " sgn-notify--global-unread)
                'face 'sgn-unread-face)))

(defun sgn-notify--install-tab-bar ()
  "Add the sgn indicator to the tab bar."
  (when (and (boundp 'tab-bar-format) (fboundp 'tab-bar-mode))
    (unless (memq 'sgn-notify--tab-bar-format tab-bar-format)
      (setq tab-bar-format
            (append tab-bar-format '(sgn-notify--tab-bar-format))))))

(defun sgn-notify--remove-tab-bar ()
  "Remove the sgn indicator from the tab bar."
  (when (boundp 'tab-bar-format)
    (setq tab-bar-format
          (delq 'sgn-notify--tab-bar-format tab-bar-format))))

(defun sgn-notify--tab-bar-format ()
  "Tab bar format function for sgn unread count."
  (when (> sgn-notify--global-unread 0)
    `((sgn-unread menu-item
                  ,(format " Sgn:%d " sgn-notify--global-unread)
                  ignore
                  :help "Unread Signal messages"))))

;;;; Update unread counts

(defun sgn-notify-update ()
  "Recalculate global unread count and update indicators."
  (let ((chats (sgn-db-get-chats))
        (total 0))
    (dolist (chat chats)
      (let ((unread (plist-get chat :unread))
            (muted (plist-get chat :muted)))
        (when (and unread (> unread 0)
                   (or (null muted) (zerop muted)))
          (setq total (+ total unread)))))
    (setq sgn-notify--global-unread total)
    (sgn-notify--update-modeline)
    (force-mode-line-update t)))

;;;; Desktop notifications

(defun sgn-notify-message (chat-id sender body)
  "Show a desktop notification for a new message.
CHAT-ID is the chat, SENDER is the sender name/ID, BODY is the message text.
Suppressed for muted chats."
  (when sgn-desktop-notifications
    (let ((chat (sgn-db-get-chat chat-id)))
      (unless (and chat (not (zerop (or (plist-get chat :muted) 0))))
        (let* ((title (format "Sgn: %s" (sgn-contacts-get-name
                                          (or sender chat-id))))
               (preview (cond
                         (body (if (> (length body) 100)
                                   (concat (substring body 0 100) "…")
                                 body))
                         (t "New message"))))
          (sgn-notify--desktop-notify title preview chat-id))))))

(defun sgn-notify--desktop-notify (title body chat-id)
  "Send a desktop notification with TITLE and BODY.
CHAT-ID is used for click-to-open."
  (cond
   ;; macOS
   ((eq system-type 'darwin)
    (sgn-notify--macos-notify title body chat-id))
   ;; Linux (D-Bus notifications)
   ((and (eq system-type 'gnu/linux)
         (fboundp 'notifications-notify))
    (require 'notifications)
    (notifications-notify
     :title title
     :body body
     :app-name "Sgn"
     :actions '("open" "Open Chat")
     :on-action (lambda (_id _key)
                  (sgn-chat-open chat-id))))
   ;; Fallback: message
   (t
    (message "%s: %s" title body))))

(defun sgn-notify--macos-notify (title body chat-id)
  "Send macOS notification via osascript.
TITLE and BODY are the notification content.  CHAT-ID is unused
here (AppleScript notifications don't support click callbacks
reliably)."
  (ignore chat-id)
  (let ((script (format
                 "display notification %s with title %s"
                 (sgn-notify--applescript-quote body)
                 (sgn-notify--applescript-quote title))))
    (start-process "sgn-notify" nil "osascript" "-e" script)))

(defun sgn-notify--applescript-quote (str)
  "Quote STR for use in AppleScript."
  (format "\"%s\"" (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" str)))

;;;; Global minor mode

(defvar sgn-global-mode-map (make-sparse-keymap)
  "Keymap for `sgn-global-mode'.")

;;;###autoload
(define-minor-mode sgn-global-mode
  "Global minor mode that shows Signal unread count indicator."
  :global t
  :lighter nil
  :keymap sgn-global-mode-map
  (if sgn-global-mode
      (progn
        (pcase sgn-notification-style
          ('modeline (sgn-notify--install-modeline))
          ('tab-bar (sgn-notify--install-tab-bar)))
        (sgn-notify-update))
    (sgn-notify--remove-modeline)
    (sgn-notify--remove-tab-bar)
    (setq sgn-notify--global-unread 0)
    (sgn-notify--update-modeline)
    (force-mode-line-update t)))

(provide 'sgn-notify)
;;; sgn-notify.el ends here
