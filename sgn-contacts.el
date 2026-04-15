;;; sgn-contacts.el --- Contact and group management for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Contact and group cache backed by SQLite, with completing-read for
;; chat selection.  Contacts are fetched from signal-cli on startup and
;; periodically, then persisted in the `chats' table.

;;; Code:

(require 'cl-lib)

(declare-function sgn--log "sgn")
(declare-function sgn-db-upsert-chat "sgn-db")
(declare-function sgn-db-get-chat "sgn-db")
(declare-function sgn-db-get-chats "sgn-db")
(declare-function sgn-rpc-list-contacts "sgn-rpc")
(declare-function sgn-rpc-list-groups "sgn-rpc")

(defvar sgn-account)

;;;; Internal state

(defvar sgn-contacts--cache (make-hash-table :test 'equal)
  "Cache of Signal IDs to display names.
Keys are phone numbers or base64 group IDs; values are display name strings.")

(defvar sgn-contacts--refresh-timer nil
  "Timer for periodic contact refresh.")

(defconst sgn-contacts--refresh-interval 300
  "Seconds between automatic contact refreshes.")

(defconst sgn-contacts--initial-delay 2
  "Seconds to wait after process start before first contact refresh.")

;;;; Name resolution

(defun sgn-contacts-get-name (id)
  "Return display name for signal ID, or ID itself as fallback."
  (or (gethash id sgn-contacts--cache)
      id))

(defun sgn-contacts-set-name (id name)
  "Set display NAME for signal ID in cache."
  (when (and id name (not (string-empty-p name)))
    (puthash id name sgn-contacts--cache)))

(defun sgn-contacts-display-sender (sender)
  "Return display string for SENDER.
If SENDER matches `sgn-account', return \"You\"; otherwise resolve name."
  (if (and sgn-account (equal sender sgn-account))
      "You"
    (sgn-contacts-get-name sender)))

;;;; Refresh from signal-cli

(defun sgn-contacts-refresh ()
  "Fetch contacts and groups from signal-cli, update cache and database."
  (interactive)
  (sgn-rpc-list-contacts #'sgn-contacts--populate-contacts)
  (sgn-rpc-list-groups #'sgn-contacts--populate-groups))

(defun sgn-contacts--populate-contacts (result)
  "Populate cache and DB from a listContacts RESULT."
  (let ((contacts (if (vectorp result) (append result nil) result)))
    (dolist (contact contacts)
      (let ((number (alist-get 'number contact))
            (name (alist-get 'name contact)))
        (when (and number name (not (string-empty-p name)))
          (sgn-contacts-set-name number name)
          (sgn-db-upsert-chat number :name name :type "individual")))))
  (sgn--log "Contacts refreshed: %d entries" (hash-table-count sgn-contacts--cache)))

(defun sgn-contacts--populate-groups (result)
  "Populate cache and DB from a listGroups RESULT."
  (let ((groups (if (vectorp result) (append result nil) result)))
    (dolist (group groups)
      (let ((group-id (alist-get 'id group))
            (name (alist-get 'name group)))
        (when group-id
          (when (and name (not (string-empty-p name)))
            (sgn-contacts-set-name group-id name))
          (sgn-db-upsert-chat group-id
                              :name (or name "")
                              :type "group")))))
  (sgn--log "Groups refreshed"))

;;;; Periodic refresh

(defun sgn-contacts-start-refresh-timer ()
  "Start the periodic contact refresh timer."
  (sgn-contacts-stop-refresh-timer)
  (setq sgn-contacts--refresh-timer
        (run-at-time sgn-contacts--initial-delay
                     sgn-contacts--refresh-interval
                     #'sgn-contacts-refresh)))

(defun sgn-contacts-stop-refresh-timer ()
  "Stop the periodic contact refresh timer."
  (when sgn-contacts--refresh-timer
    (cancel-timer sgn-contacts--refresh-timer)
    (setq sgn-contacts--refresh-timer nil)))

;;;; Completing-read interface

(defun sgn-contacts--is-group-id (id)
  "Return non-nil if ID looks like a Signal group ID.
Phone numbers start with \"+\", UUIDs contain \"-\"; anything else
is assumed to be a base64 group ID."
  (not (or (string-prefix-p "+" id)
           (string-match-p "-" id))))

(defun sgn-contacts-completing-read (&optional prompt)
  "Read a chat ID via `completing-read' with annotations.
PROMPT defaults to \"Chat: \".  Returns the selected chat ID (not the
display name).  Candidates are sorted by recency (most recent
conversation first), with duplicate names disambiguated."
  (let* ((chats (sgn-db-get-chats t))
         (candidates (sgn-contacts--build-candidates chats))
         (selected (completing-read
                    (or prompt "Chat: ")
                    (sgn-contacts--completion-table candidates)
                    nil nil nil nil nil)))
    (or (cdr (assoc selected candidates))
        ;; If no match, treat input as a raw phone number
        selected)))

(defun sgn-contacts--build-candidates (chats)
  "Build alist of (display-string . chat-id) from CHATS plists.
Disambiguate duplicate display names."
  (let ((name-counts (make-hash-table :test 'equal))
        (raw-entries nil))
    ;; First pass: count name occurrences for disambiguation
    (dolist (chat chats)
      (let* ((id (plist-get chat :id))
             (name (or (plist-get chat :name)
                       (sgn-contacts-get-name id))))
        (push (cons name id) raw-entries)
        (puthash name (1+ (or (gethash name name-counts) 0)) name-counts)))
    (setq raw-entries (nreverse raw-entries))
    ;; Second pass: disambiguate duplicates
    (mapcar
     (lambda (entry)
       (let ((name (car entry))
             (id (cdr entry)))
         (if (> (gethash name name-counts 0) 1)
             ;; Disambiguate: append short ID hint
             (let ((suffix (if (sgn-contacts--is-group-id id)
                               (concat " <" (substring id 0 (min 8 (length id))) "…>")
                             (concat " <" id ">"))))
               (cons (concat name suffix) id))
           (cons name id))))
     raw-entries)))

(defun sgn-contacts--completion-table (candidates)
  "Build a completion table from CANDIDATES with annotations.
CANDIDATES is an alist of (display-string . chat-id)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (cand candidates)
      (puthash (car cand) (cdr cand) table))
    (lambda (string pred action)
      (cond
       ((eq action 'metadata)
        `(metadata
          (annotation-function . ,(sgn-contacts--annotation-fn))
          (category . sgn-chat)))
       (t
        (complete-with-action action table string pred))))))

(defun sgn-contacts--annotation-fn ()
  "Return an annotation function for chat candidates.
Shows last message preview and timestamp."
  (lambda (candidate)
    (let* ((chats (sgn-db-get-chats t))
           (chat (cl-find-if (lambda (c)
                               (let ((name (or (plist-get c :name)
                                               (sgn-contacts-get-name (plist-get c :id)))))
                                 (string-prefix-p name candidate)))
                             chats))
           (ts (and chat (plist-get chat :last-msg-ts)))
           (unread (and chat (plist-get chat :unread))))
      (concat
       (when (and unread (> unread 0))
         (format " (%d)" unread))
       (when ts
         (format " · %s" (sgn-contacts--format-relative-time ts)))))))

(defun sgn-contacts--format-relative-time (timestamp-ms)
  "Format TIMESTAMP-MS (milliseconds) as a relative time string."
  (let* ((now (float-time))
         (then (/ timestamp-ms 1000.0))
         (diff (- now then)))
    (cond
     ((< diff 60) "now")
     ((< diff 3600) (format "%dm ago" (floor (/ diff 60))))
     ((< diff 86400) (format "%dh ago" (floor (/ diff 3600))))
     ((< diff 604800) (format "%dd ago" (floor (/ diff 86400))))
     (t (format-time-string "%b %d" (seconds-to-time then))))))

;;;; Warm cache from database on startup

(defun sgn-contacts-load-from-db ()
  "Load contact names from the database into the in-memory cache."
  (let ((chats (sgn-db-get-chats t)))
    (dolist (chat chats)
      (let ((id (plist-get chat :id))
            (name (plist-get chat :name)))
        (when (and id name (not (string-empty-p name)))
          (puthash id name sgn-contacts--cache))))))

(provide 'sgn-contacts)
;;; sgn-contacts.el ends here
