;;; sgn.el --- Signal client via signal-cli JSON-RPC  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>
;; URL: https://github.com/benthamite/sgn
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; sgn is a Signal messenger client for Emacs built on signal-cli's
;; JSON-RPC mode, with SQLite persistence and full-text search.
;;
;; How the parts fit together:
;;
;; - `sgn-rpc' runs signal-cli and exchanges JSON-RPC messages with it.
;; - `sgn-event' turns each envelope signal-cli receives into events.
;; - `sgn-store' applies events to the database (`sgn-db') and reports
;;   which chats changed.
;; - Chat buffers (`sgn-chat'), the dashboard (`sgn-dashboard') and
;;   notifications (`sgn-notify') redraw from the database when told.
;; - `sgn-actions' sends messages and performs actions on them.
;;
;; This file holds the user options, the session lifecycle, and the
;; glue between those parts.
;;
;; Prerequisites:
;; 1. Emacs 29.1+ compiled with SQLite support.
;; 2. signal-cli 0.14+ installed and in $PATH.
;; 3. A Signal account linked to signal-cli (see `sgn-link').

;;; Code:

(require 'cl-lib)
(require 'json)

;;;; Custom group

(defgroup sgn nil
  "Signal client for Emacs using signal-cli."
  :group 'comm
  :prefix "sgn-")

;;;; Core customization

(defcustom sgn-account nil
  "The Signal account's phone number, such as \"+15550000000\".
This must match the account linked to signal-cli."
  :type '(choice (const :tag "Not set" nil) string)
  :group 'sgn)

(defcustom sgn-cli-program (or (executable-find "signal-cli") "signal-cli")
  "Path to the signal-cli executable."
  :type 'file
  :group 'sgn)

(defcustom sgn-data-directory
  (expand-file-name "signal-cli" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Directory where signal-cli stores its data, attachments and stickers."
  :type 'directory
  :group 'sgn)

(defcustom sgn-auto-open-buffer nil
  "If non-nil, display a chat's buffer when a message arrives in it."
  :type 'boolean
  :group 'sgn)

(defcustom sgn-send-read-receipts t
  "If non-nil, send read receipts for messages you read in sgn.
A chat's messages count as read when it is shown in the selected
window of a focused frame."
  :type 'boolean
  :group 'sgn)

(defcustom sgn-send-typing t
  "If non-nil, send typing indicators while you compose a message."
  :type 'boolean
  :group 'sgn)

;;;; Logging

(defconst sgn--log-max-size (* 4 1024 1024)
  "Size in characters beyond which the oldest half of the log is dropped.")

(defun sgn--log (fmt &rest args)
  "Log the message FMT formatted with ARGS to *sgn-log*."
  (with-current-buffer (get-buffer-create "*sgn-log*")
    (save-excursion
      (goto-char (point-max))
      (insert (format-time-string "[%H:%M:%S] ")
              (apply #'format fmt args) "\n")
      (when (> (buffer-size) sgn--log-max-size)
        (goto-char (/ (buffer-size) 2))
        (delete-region (point-min) (line-beginning-position 2))))))

;;;###autoload
(defun sgn-show-log ()
  "Display the debug log buffer."
  (interactive)
  (display-buffer (get-buffer-create "*sgn-log*")))

;;;; Submodules (after `sgn--log' is defined)

(require 'sgn-db)
(require 'sgn-rpc)
(require 'sgn-event)
(require 'sgn-store)
(require 'sgn-contacts)
(require 'sgn-media)
(require 'sgn-format)
(require 'sgn-chat)
(require 'sgn-actions)
(require 'sgn-notify)
(require 'sgn-search)
(require 'sgn-import)
(require 'sgn-dashboard)
(require 'sgn-link)

;;;; Receiving

(defun sgn--handle-receive (params)
  "Store what the signal-cli receive notification PARAMS carries."
  (sgn-store-apply (sgn-event-from-envelope params (sgn-store-self))))

(defun sgn--on-new-message (chat-id rowid)
  "Handle the new incoming message ROWID in CHAT-ID.
A chat on screen is marked read at once; otherwise the message
is announced, unless the chat is muted."
  (cond
   ((sgn-chat-visible-p chat-id)
    (sgn-mark-chat-read chat-id))
   (t
    (sgn-notify-message chat-id rowid)
    (when sgn-auto-open-buffer
      (display-buffer (sgn-chat-get-buffer chat-id))))))

;;;; Reading

(defun sgn-mark-chat-read (chat-id)
  "Mark the unread messages of CHAT-ID read and send read receipts."
  (when-let* ((unread (sgn-db-unread-messages chat-id)))
    (let ((now (sgn-db-now))
          (by-sender nil))
      (sgn-db-with-transaction
        (pcase-dolist (`(,sender ,ts ,rowid) unread)
          (sgn-db-mark-read rowid now)
          (push ts (alist-get sender by-sender nil nil #'equal))))
      (when (and sgn-send-read-receipts (sgn-rpc-alive-p))
        (pcase-dolist (`(,sender . ,timestamps) by-sender)
          (sgn-rpc-send-receipt sender timestamps)))
      (sgn-store-changed chat-id))))

;;;; Expiry

(defvar sgn--expiry-timer nil
  "Timer that deletes messages whose disappearing timer ran out.")

(defun sgn--purge-expired ()
  "Delete expired messages, if the database is open."
  (when sgn-db--connection
    (sgn-store-purge-expired)))

;;;; Lifecycle

(defun sgn-running-p ()
  "Return non-nil if sgn is started."
  (and sgn-db--connection (sgn-rpc-alive-p)))

(defun sgn-ensure-running ()
  "Start sgn unless it is running.
If signal-cli failed, signal that failure instead: starting again
would fail the same way."
  (unless (sgn-rpc-alive-p)
    (when sgn-rpc--failure
      (user-error "%s" (sgn-rpc-failure-advice)))
    (sgn-start)
    (unless (sgn-rpc-alive-p)
      (user-error "sgn could not start signal-cli"))))

;;;###autoload
(defun sgn-start ()
  "Start sgn: open the database and start signal-cli."
  (interactive)
  (unless sgn-account
    (user-error "Variable `sgn-account' is not set"))
  (when (sgn-link-in-progress-p)
    (user-error "Linking is in progress; sgn starts once it completes"))
  (if (sgn-running-p)
      (message "sgn is already running.")
    (sgn-db-init)
    (sgn-store-load-self)
    (sgn-contacts-load-from-db)
    (sgn-db-settle-interrupted-sends)
    (sgn-db-purge-pending (- (sgn-db-now) (* 30 24 3600 1000)))
    (setq sgn-rpc-receive-handler #'sgn--handle-receive)
    (sgn-rpc-start)
    (sgn-contacts-start-refresh-timer)
    (when (timerp sgn--expiry-timer)
      (cancel-timer sgn--expiry-timer))
    (setq sgn--expiry-timer (run-at-time 10 10 #'sgn--purge-expired))
    (sgn-global-mode 1)
    (sgn-dashboard-refresh)
    (message "sgn started.")))

;;;###autoload
(defun sgn-stop ()
  "Stop sgn: stop signal-cli and close the database."
  (interactive)
  (when sgn-db--connection
    (sgn-chat-save-all-drafts))
  (when (timerp sgn--expiry-timer)
    (cancel-timer sgn--expiry-timer))
  (setq sgn--expiry-timer nil)
  (sgn-contacts-stop-refresh-timer)
  (sgn-rpc-stop)
  (sgn-db-close)
  (sgn-global-mode -1)
  (message "sgn stopped."))

(defun sgn--on-failure-change ()
  "Update indicators after signal-cli fails or recovers."
  (sgn-notify-update)
  (sgn-dashboard-refresh))

(defun sgn--save-drafts-on-exit ()
  "Save drafts before Emacs exits."
  (when sgn-db--connection
    (ignore-errors (sgn-chat-save-all-drafts))))

;;;; Wiring

(add-hook 'sgn-store-changed-functions #'sgn-chat-on-store-changed)
(add-hook 'sgn-store-changed-functions #'sgn-dashboard-on-store-changed)
(add-hook 'sgn-store-changed-functions #'sgn-notify-on-store-changed)
(add-hook 'sgn-store-new-message-functions #'sgn--on-new-message)
(add-hook 'sgn-store-typing-functions #'sgn-chat-on-typing)
(add-hook 'sgn-db-merge-functions #'sgn-chat-on-identity-merged)
(add-hook 'sgn-rpc-failure-change-hook #'sgn--on-failure-change)
(add-hook 'kill-emacs-hook #'sgn--save-drafts-on-exit)
(add-hook 'window-selection-change-functions #'sgn-chat--mark-selected-read)
(add-hook 'window-buffer-change-functions #'sgn-chat--mark-selected-read)
(add-function :after after-focus-change-function #'sgn-chat--mark-selected-read)

;;;; Commands

;;;###autoload
(defun sgn-chat (chat-id)
  "Open the chat CHAT-ID, read with completion."
  (interactive (progn (sgn-ensure-running)
                      (list (sgn-contacts-completing-read))))
  (sgn-chat-open chat-id))

;;;###autoload
(defun sgn-note-to-self ()
  "Open the Note to Self chat."
  (interactive)
  (sgn-ensure-running)
  (sgn-chat-open (sgn-store-self)))

(provide 'sgn)
;;; sgn.el ends here
