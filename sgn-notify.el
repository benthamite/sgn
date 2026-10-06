;;; sgn-notify.el --- Notifications for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; The unread indicator in the mode line or tab bar, and desktop
;; notifications for incoming messages.  Notifications for a chat are
;; collected for a moment and shown together, so that a backlog
;; delivered after a reconnect produces one notification per chat.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sgn-db)
(require 'sgn-format)
(require 'sgn-contacts)

(declare-function sgn-chat-open "sgn-chat")
(declare-function notifications-notify "notifications")
(declare-function sgn-rpc-failure-advice "sgn-rpc")

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

(defface sgn-offline-face
  '((t :inherit error))
  "Face for the indicator shown when signal-cli has failed."
  :group 'sgn)

;;;; Indicator

(defvar sgn-notify--global-unread 0
  "Total unread message count across chats that are not muted.")

(defvar sgn-notify--modeline-string ""
  "Current mode line indicator string.")
;; The mode line only shows text properties of risky variables.
(put 'sgn-notify--modeline-string 'risky-local-variable t)

(defun sgn-notify--indicator-label ()
  "Return the propertized indicator text, or nil when there is nothing to show.
A signal-cli failure takes precedence over the unread count, since
no new messages arrive until it is fixed."
  (cond
   ((sgn-rpc-failure-advice)
    (propertize "sgn:offline" 'face 'sgn-offline-face
                'help-echo (sgn-rpc-failure-advice)))
   ((> sgn-notify--global-unread 0)
    (propertize (format "sgn:%d" sgn-notify--global-unread)
                'face 'sgn-unread-face
                'help-echo "Unread Signal messages"))))

(defun sgn-notify-update ()
  "Recount unread messages and update the indicator."
  (setq sgn-notify--global-unread
        (if (bound-and-true-p sgn-db--connection) (sgn-db-total-unread) 0))
  (setq sgn-notify--modeline-string
        (if-let* ((label (sgn-notify--indicator-label)))
            (concat " [" label "]")
          ""))
  (force-mode-line-update t))

(defun sgn-notify-on-store-changed (_chat-ids)
  "Update the indicator, as `sgn-store-changed-functions' asks."
  (sgn-notify-update))

(defun sgn-notify--tab-bar-format ()
  "Tab bar format function for the sgn indicator."
  (when-let* ((label (sgn-notify--indicator-label)))
    `((sgn-unread menu-item ,(concat " " label " ") ignore
                  :help ,(or (sgn-rpc-failure-advice)
                             "Unread Signal messages")))))

(defun sgn-notify--install ()
  "Add the indicator where `sgn-notification-style' says."
  (pcase sgn-notification-style
    ('modeline
     (unless (memq 'sgn-notify--modeline-string global-mode-string)
       (setq global-mode-string
             (append global-mode-string '(sgn-notify--modeline-string)))))
    ('tab-bar
     (when (boundp 'tab-bar-format)
       (unless (memq 'sgn-notify--tab-bar-format tab-bar-format)
         (setq tab-bar-format
               (append tab-bar-format '(sgn-notify--tab-bar-format))))))))

(defun sgn-notify--uninstall ()
  "Remove the indicator."
  (setq global-mode-string
        (delq 'sgn-notify--modeline-string global-mode-string))
  (when (boundp 'tab-bar-format)
    (setq tab-bar-format (delq 'sgn-notify--tab-bar-format tab-bar-format))))

;;;; Desktop notifications

(defvar sgn-notify--queued nil
  "Alist of (CHAT-ID . ROWIDS) of messages waiting to be announced.")

(defvar sgn-notify--timer nil
  "Timer that shows the queued notifications.")

(defconst sgn-notify--delay 1
  "Seconds to collect a chat's messages before announcing them.")

(defun sgn-notify-message (chat-id rowid)
  "Announce message ROWID in CHAT-ID, unless the chat is muted."
  (when (and sgn-desktop-notifications
             (eql 0 (plist-get (sgn-db-get-chat chat-id) :muted)))
    (push rowid (alist-get chat-id sgn-notify--queued nil nil #'equal))
    (unless (timerp sgn-notify--timer)
      (setq sgn-notify--timer
            (run-at-time sgn-notify--delay nil #'sgn-notify--flush)))))

(defun sgn-notify--flush ()
  "Show one notification for each chat with queued messages."
  (let ((queued (nreverse sgn-notify--queued)))
    (setq sgn-notify--queued nil
          sgn-notify--timer nil)
    (when (bound-and-true-p sgn-db--connection)
      (pcase-dolist (`(,chat-id . ,rowids) queued)
        (let ((messages (cl-remove-if
                         (lambda (msg) (or (null msg) (plist-get msg :read-at)))
                         (mapcar #'sgn-db-get-message-by-rowid
                                 (reverse rowids)))))
          (when messages
            (sgn-notify--show chat-id messages)))))))

(defun sgn-notify--show (chat-id messages)
  "Show a notification for MESSAGES, new in CHAT-ID."
  (let* ((last (car (last messages)))
         (sender (sgn-contacts-get-name (plist-get last :sender)))
         (group (equal (sgn-db-chat-type chat-id) "group"))
         (title (if group
                    (format "%s in %s" sender (sgn-contacts-get-name chat-id))
                  sender))
         (body (if (cdr messages)
                   (format "%d new messages" (length messages))
                 (sgn-notify--preview last))))
    (sgn-notify--desktop-notify title body chat-id)))

(defun sgn-notify--preview (msg)
  "Return a short preview of MSG, with spoilers concealed."
  (if-let* ((text (sgn-format-preview msg)))
      (truncate-string-to-width text 100 nil nil "…")
    "[Attachment]"))

(defun sgn-notify--desktop-notify (title body chat-id)
  "Show a desktop notification with TITLE and BODY for CHAT-ID."
  (cond
   ((eq system-type 'darwin)
    (sgn-notify--macos-notify title body))
   ((and (eq system-type 'gnu/linux) (require 'notifications nil t))
    (notifications-notify
     :title title
     :body (sgn-notify--escape-markup body)
     :app-name "sgn"
     :actions '("default" "Open chat")
     :on-action (lambda (_id _key) (sgn-chat-open chat-id))))
   (t
    (message "%s: %s" title body))))

(defun sgn-notify--escape-markup (text)
  "Escape TEXT for notification daemons that interpret markup."
  (replace-regexp-in-string
   "[&<>]" (lambda (c) (pcase c ("&" "&amp;") ("<" "&lt;") (">" "&gt;")))
   text t t))

(defun sgn-notify--macos-notify (title body)
  "Show a macOS notification with TITLE and BODY via osascript."
  (start-process "sgn-notify" nil "osascript" "-e"
                 (format "display notification %s with title %s"
                         (sgn-notify--applescript-quote body)
                         (sgn-notify--applescript-quote title))))

(defun sgn-notify--applescript-quote (str)
  "Return STR as an AppleScript string literal."
  (format "\"%s\"" (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" str)))

;;;; Global minor mode

;;;###autoload
(define-minor-mode sgn-global-mode
  "Global minor mode that shows the Signal unread count indicator."
  :global t
  :lighter nil
  :group 'sgn
  (if sgn-global-mode
      (progn (sgn-notify--install)
             (sgn-notify-update))
    (sgn-notify--uninstall)
    (setq sgn-notify--global-unread 0
          sgn-notify--modeline-string "")
    (force-mode-line-update t)))

(provide 'sgn-notify)
;;; sgn-notify.el ends here
