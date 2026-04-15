;;; sgn-search.el --- Full-text search for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; FTS5-powered full-text search across Signal conversations, with a
;; results buffer and jump-to-message support.

;;; Code:

(require 'cl-lib)

(declare-function sgn--log "sgn")
(declare-function sgn-db-search "sgn-db")
(declare-function sgn-db-get-chat "sgn-db")
(declare-function sgn-chat-open "sgn-chat")
(declare-function sgn-contacts-get-name "sgn-contacts")
(declare-function sgn-contacts-display-sender "sgn-contacts")

(defvar sgn-chat-id)

;;;; Internal state

(defvar sgn-search--last-query nil
  "The most recent search query string.")

;;;; Keymap

(defvar sgn-search-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'sgn-search-goto-result)
    (define-key map (kbd "n") #'sgn-search-next-result)
    (define-key map (kbd "p") #'sgn-search-prev-result)
    (define-key map (kbd "s") #'sgn-search)
    (define-key map (kbd "g") #'sgn-search-refresh)
    map)
  "Keymap for `sgn-search-mode'.")

;;;; Major mode

(define-derived-mode sgn-search-mode special-mode "sgn Search"
  "Major mode for Signal message search results.")

;;;; Search commands

;;;###autoload
(defun sgn-search (query)
  "Search all conversations for QUERY using FTS5.
Display results in the *sgn Search* buffer."
  (interactive "sSearch: ")
  (setq sgn-search--last-query query)
  (let ((results (sgn-db-search query nil 100)))
    (sgn-search--display query results)))

;;;###autoload
(defun sgn-search-in-chat (query)
  "Search within the current chat for QUERY."
  (interactive "sSearch in chat: ")
  (unless sgn-chat-id
    (user-error "Not in a chat buffer"))
  (setq sgn-search--last-query query)
  (let ((results (sgn-db-search query sgn-chat-id 100)))
    (sgn-search--display query results)))

(defun sgn-search-refresh ()
  "Re-run the last search."
  (interactive)
  (if sgn-search--last-query
      (sgn-search sgn-search--last-query)
    (call-interactively #'sgn-search)))

;;;; Results display

(defun sgn-search--display (query results)
  "Display search RESULTS for QUERY in the *sgn Search* buffer."
  (let ((buf (get-buffer-create "*sgn Search*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (sgn-search-mode)
        (insert (propertize (format "Search: %s\n\n" query)
                            'face 'bold))
        (if (null results)
            (insert (propertize "No results found.\n" 'face 'shadow))
          (dolist (msg results)
            (sgn-search--render-result msg))
          (insert (propertize
                   (format "\n%d result%s"
                           (length results)
                           (if (= (length results) 1) "" "s"))
                   'face 'shadow)))
        (goto-char (point-min))))
    (switch-to-buffer buf)))

(defun sgn-search--render-result (msg)
  "Render a single search result MSG."
  (let* ((chat-id (plist-get msg :chat-id))
         (sender (plist-get msg :sender))
         (timestamp (plist-get msg :timestamp))
         (snippet (or (plist-get msg :snippet)
                      (plist-get msg :body)
                      ""))
         (rowid (plist-get msg :rowid))
         (chat-name (sgn-contacts-get-name chat-id))
         (sender-name (sgn-contacts-display-sender sender))
         (time-str (sgn-search--format-time timestamp))
         (start (point)))
    ;; Header line
    (insert (propertize (format "── %s · %s " sender-name time-str)
                        'face 'bold))
    (insert (propertize (make-string
                         (max 0 (- (window-width) (current-column) 1))
                         ?─)
                        'face 'shadow))
    (insert "\n")
    ;; Snippet with search term highlighting
    (insert "  " snippet "\n")
    ;; Chat link
    (insert (propertize (format "  → *sgn: %s*" chat-name)
                        'face 'link))
    (insert "\n\n")
    ;; Properties for navigation
    (put-text-property start (point) 'sgn-search-chat-id chat-id)
    (put-text-property start (point) 'sgn-search-rowid rowid)
    (put-text-property start (point) 'sgn-search-timestamp timestamp)))

(defun sgn-search--format-time (timestamp-ms)
  "Format TIMESTAMP-MS for search results."
  (when timestamp-ms
    (let* ((time (seconds-to-time (/ timestamp-ms 1000.0)))
           (now (current-time))
           (diff (float-time (time-subtract now time))))
      (cond
       ((< diff 86400)
        (format-time-string "%H:%M" time))
       ((< diff 604800)
        (format-time-string "%a, %H:%M" time))
       (t
        (format-time-string "%b %d, %H:%M" time))))))

;;;; Navigation

(defun sgn-search-goto-result ()
  "Jump to the message at point in its chat buffer."
  (interactive)
  (let ((chat-id (get-text-property (point) 'sgn-search-chat-id)))
    (if chat-id
        (sgn-chat-open chat-id)
      (user-error "No search result at point"))))

(defun sgn-search-next-result ()
  "Move to the next search result."
  (interactive)
  (let ((pos (next-single-property-change (point) 'sgn-search-rowid)))
    (when pos
      (goto-char pos)
      ;; Skip to the actual result start
      (unless (get-text-property (point) 'sgn-search-rowid)
        (let ((next (next-single-property-change (point) 'sgn-search-rowid)))
          (when next (goto-char next)))))))

(defun sgn-search-prev-result ()
  "Move to the previous search result."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'sgn-search-rowid)))
    (when pos
      (goto-char pos)
      (let ((start (previous-single-property-change (point) 'sgn-search-rowid)))
        (when start (goto-char start))))))

(provide 'sgn-search)
;;; sgn-search.el ends here
