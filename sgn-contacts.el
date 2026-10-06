;;; sgn-contacts.el --- Contact and group management for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Display names for people and groups, kept in an in-memory cache
;; backed by the `recipients' and `chats' tables, the periodic refresh
;; of contacts and groups from signal-cli, and `completing-read' over
;; chats.  Names are only for display: chats and buffers are always
;; identified by chat ID.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sgn-db)

(declare-function sgn--log "sgn")
(declare-function sgn-rpc-list-contacts "sgn-rpc")
(declare-function sgn-rpc-list-groups "sgn-rpc")
(declare-function sgn-rpc-alive-p "sgn-rpc")
(declare-function sgn-store-self-p "sgn-store")
(declare-function sgn-store-learn-self "sgn-store")
(declare-function sgn-store-changed "sgn-store")

(defvar sgn-account)

;;;; Name cache

(defvar sgn-contacts--cache (make-hash-table :test 'equal)
  "Map from a person's or group's identifier to its display name.")

(defvar sgn-contacts--numbers (make-hash-table :test 'equal)
  "Map from a person's ACI to their phone number.")

(defun sgn-contacts-load-from-db ()
  "Fill the name cache from the database."
  (clrhash sgn-contacts--cache)
  (clrhash sgn-contacts--numbers)
  (pcase-dolist (`(,uuid ,number ,name) (sgn-db-get-recipients))
    (when number
      (puthash uuid number sgn-contacts--numbers))
    (when name
      (puthash uuid name sgn-contacts--cache)
      (when number
        (puthash number name sgn-contacts--cache))))
  (dolist (chat (sgn-db-get-chats t))
    (when-let* ((name (plist-get chat :name)))
      (unless (string-empty-p name)
        (puthash (plist-get chat :id) name sgn-contacts--cache)))))

(defun sgn-contacts-set-name (id name)
  "Set display NAME for identifier ID."
  (when (and id name (not (string-empty-p name)))
    (puthash id name sgn-contacts--cache)))

(defun sgn-contacts-name-known-p (id)
  "Return non-nil if a display name is known for ID."
  (gethash id sgn-contacts--cache))

(defun sgn-contacts-get-name (id)
  "Return the display name for the person or group ID."
  (cond
   ((sgn-store-self-p id) "Note to Self")
   ((gethash id sgn-contacts--cache))
   ((gethash id sgn-contacts--numbers))
   ((string-match-p "\\`[0-9a-f]\\{8\\}-" id)
    (format "Unknown (%s)" (substring id 0 8)))
   (t id)))

(defun sgn-contacts-display-sender (sender)
  "Return the display string for message SENDER: \"You\" for this account."
  (if (sgn-store-self-p sender)
      "You"
    (sgn-contacts-get-name sender)))

;;;; Refresh from signal-cli

(defvar sgn-contacts--refresh-timer nil
  "Timer for periodic contact refresh.")

(defconst sgn-contacts--refresh-interval 300
  "Seconds between automatic contact refreshes.")

(defun sgn-contacts-refresh ()
  "Fetch contacts and groups from signal-cli and record them."
  (interactive)
  (sgn-rpc-list-contacts #'sgn-contacts--record-contacts)
  (sgn-rpc-list-groups #'sgn-contacts--record-groups))

(defun sgn-contacts--refresh-if-running ()
  "Refresh contacts if signal-cli is running."
  (when (sgn-rpc-alive-p)
    (sgn-contacts-refresh)))

(defun sgn-contacts-start-refresh-timer ()
  "Refresh contacts now and then periodically."
  (sgn-contacts-stop-refresh-timer)
  (setq sgn-contacts--refresh-timer
        (run-at-time 0 sgn-contacts--refresh-interval
                     #'sgn-contacts--refresh-if-running)))

(defun sgn-contacts-stop-refresh-timer ()
  "Stop the periodic contact refresh."
  (when sgn-contacts--refresh-timer
    (cancel-timer sgn-contacts--refresh-timer)
    (setq sgn-contacts--refresh-timer nil)))

(defun sgn-contacts--record-contacts (contacts)
  "Record the people in CONTACTS, a listContacts result."
  (let ((changed nil))
    (sgn-db-with-transaction
      (dolist (contact contacts)
        (let ((uuid (sgn-contacts--non-empty (alist-get 'uuid contact)))
              (number (sgn-contacts--non-empty (alist-get 'number contact)))
              (name (sgn-contacts--display-name contact)))
          (when uuid
            (when (equal number sgn-account)
              (sgn-store-learn-self uuid))
            (unless (equal name (gethash uuid sgn-contacts--cache))
              (push uuid changed))
            (sgn-db-upsert-recipient uuid number name)
            (when number
              (puthash uuid number sgn-contacts--numbers)
              (when (sgn-db-merge-identity number uuid)
                (push uuid changed)))
            (when name
              (sgn-contacts-set-name uuid name)
              (when number
                (sgn-contacts-set-name number name)))))))
    (sgn--log "Contacts refreshed: %d" (length contacts))
    (when changed
      (apply #'sgn-store-changed (delete-dups changed)))))

(defun sgn-contacts--display-name (contact)
  "Return the best display name for CONTACT, a listContacts entry, or nil."
  (or (sgn-contacts--non-empty (alist-get 'nickName contact))
      (sgn-contacts--non-empty (alist-get 'name contact))
      (sgn-contacts--profile-name (alist-get 'profile contact))
      (sgn-contacts--non-empty (alist-get 'username contact))))

(defun sgn-contacts--profile-name (profile)
  "Return the full name in PROFILE, or nil."
  (when profile
    (sgn-contacts--non-empty
     (string-trim (concat (or (alist-get 'givenName profile) "") " "
                          (or (alist-get 'familyName profile) ""))))))

(defun sgn-contacts--non-empty (s)
  "Return S if it is a non-empty string, else nil."
  (and (stringp s) (not (string-empty-p s)) s))

(defun sgn-contacts--record-groups (groups)
  "Record the groups in GROUPS, a listGroups result.
Members' numbers are merged into their ACIs, which covers people
who are not in the contact list."
  (let ((changed nil))
    (sgn-db-with-transaction
      (dolist (group groups)
        (when-let* ((id (alist-get 'id group)))
          (let ((name (sgn-contacts--non-empty (alist-get 'name group))))
            (sgn-db-ensure-chat id "group")
            (unless (equal name (plist-get (sgn-db-get-chat id) :name))
              (sgn-db-update-chat id :name name)
              (push id changed))
            (sgn-contacts-set-name id name)
            (dolist (member (alist-get 'members group))
              (let ((uuid (sgn-contacts--non-empty (alist-get 'uuid member)))
                    (number (sgn-contacts--non-empty (alist-get 'number member))))
                (when (and uuid number)
                  (if (equal number sgn-account)
                      (sgn-store-learn-self uuid)
                    (puthash uuid number sgn-contacts--numbers)
                    (when (sgn-db-merge-identity number uuid)
                      (push uuid changed))))))))))
    (when changed
      (apply #'sgn-store-changed (delete-dups changed)))))

;;;; Completing-read

(defun sgn-contacts-completing-read (&optional prompt allow-empty)
  "Read a chat with `completing-read' and return its ID.
PROMPT defaults to \"Chat: \".  Candidates are chats, most recent
first, and contacts without a chat.  A phone number not among
them is also accepted.  With ALLOW-EMPTY, empty input returns nil."
  (let* ((candidates (sgn-contacts--candidates))
         (table (make-hash-table :test 'equal)))
    (dolist (candidate candidates)
      (puthash (car candidate) (cdr candidate) table))
    (let ((choice (completing-read (or prompt "Chat: ")
                                   (sgn-contacts--completion-table
                                    (mapcar #'car candidates) table)
                                   nil 'confirm)))
      (cond
       ((plist-get (gethash choice table) :id))
       ((and allow-empty (string-empty-p choice)) nil)
       (t (sgn-contacts--read-number choice))))))

(defun sgn-contacts--read-number (input)
  "Return the chat ID for phone number INPUT, or signal an error."
  (let ((number (replace-regexp-in-string "[ ()-]" "" input)))
    (unless (string-match-p "\\`\\+[0-9]\\{6,15\\}\\'" number)
      (user-error "No chat named %s (phone numbers need a leading +)" input))
    (or (sgn-db-uuid-for-number number) number)))

(defun sgn-contacts--candidates ()
  "Return (DISPLAY . PLIST) for chats and contacts, most recent first.
PLIST has the chat's :id, :type, :unread and :last-msg-ts."
  (let* ((chats (sgn-db-get-chats t))
         (chat-ids (make-hash-table :test 'equal))
         (entries nil))
    (dolist (chat chats)
      (puthash (plist-get chat :id) t chat-ids)
      (push chat entries))
    (pcase-dolist (`(,uuid ,_number ,_name) (sgn-db-get-recipients))
      (unless (gethash uuid chat-ids)
        (push (list :id uuid :type "individual") entries)))
    (sgn-contacts--disambiguate
     (sort (nreverse entries)
           (lambda (a b) (> (or (plist-get a :last-msg-ts) 0)
                            (or (plist-get b :last-msg-ts) 0)))))))

(defun sgn-contacts--disambiguate (entries)
  "Return (DISPLAY . ENTRY) for ENTRIES, with unique DISPLAY strings."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (cl-incf (gethash (sgn-contacts-get-name (plist-get entry :id)) counts 0)))
    (mapcar (lambda (entry)
              (let* ((id (plist-get entry :id))
                     (name (sgn-contacts-get-name id)))
                (cons (if (> (gethash name counts) 1)
                          (format "%s <%s>" name
                                  (or (gethash id sgn-contacts--numbers)
                                      (substring id 0 (min 8 (length id)))))
                        name)
                      entry)))
            entries)))

(defun sgn-contacts--completion-table (names table)
  "Return a completion table over NAMES, keeping their order.
TABLE maps each name to its entry, for annotations."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata
          (category . sgn-chat)
          (display-sort-function . identity)
          (annotation-function
           . ,(lambda (candidate)
                (sgn-contacts--annotation (gethash candidate table)))))
      (complete-with-action action names string pred))))

(defun sgn-contacts--annotation (entry)
  "Return the completion annotation for chat ENTRY."
  (let ((unread (or (plist-get entry :unread) 0))
        (ts (plist-get entry :last-msg-ts)))
    (concat (when (> unread 0) (format " (%d)" unread))
            (when ts (format " · %s" (sgn-contacts-format-time ts))))))

(defun sgn-contacts-format-time (timestamp-ms &optional with-time)
  "Return TIMESTAMP-MS formatted for lists of chats or messages.
Today's times show the hour, this week's the weekday, older ones
the date.  With WITH-TIME, weekdays and dates include the hour."
  (let* ((time (/ timestamp-ms 1000.0))
         (age (- (float-time) time)))
    (format-time-string
     (cond ((< age 86400) "%H:%M")
           ((< age 604800) (if with-time "%a %H:%M" "%a"))
           (t (if with-time "%b %d, %H:%M" "%b %d")))
     time)))

(provide 'sgn-contacts)
;;; sgn-contacts.el ends here
