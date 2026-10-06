;;; sgn-search.el --- Full-text search for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Full-text search across Signal conversations, with a results
;; buffer from which RET jumps to the message in its chat.  The words
;; of a query are matched literally; all of them must occur.

;;; Code:

(require 'cl-lib)
(require 'sgn-db)
(require 'sgn-format)
(require 'sgn-contacts)

(declare-function sgn-ensure-running "sgn")
(declare-function sgn-chat-open "sgn-chat")
(declare-function sgn-chat-show-message "sgn-chat")

(defvar sgn-chat-id)

;;;; State

(defvar sgn-search--last nil
  "The last search, as (QUERY . CHAT-ID); CHAT-ID is nil for all chats.")

;;;; Keymap and mode

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

(define-derived-mode sgn-search-mode special-mode "sgn Search"
  "Major mode for Signal message search results.")

;;;; Commands

;;;###autoload
(defun sgn-search (query)
  "Search all chats for the words of QUERY."
  (interactive "sSearch: ")
  (sgn-ensure-running)
  (sgn-search--run query nil))

;;;###autoload
(defun sgn-search-in-chat (query)
  "Search the current chat for the words of QUERY."
  (interactive "sSearch in chat: ")
  (unless sgn-chat-id
    (user-error "Not in a chat buffer"))
  (sgn-search--run query sgn-chat-id))

(defun sgn-search-refresh ()
  "Run the last search again."
  (interactive)
  (if sgn-search--last
      (sgn-search--run (car sgn-search--last) (cdr sgn-search--last))
    (call-interactively #'sgn-search)))

(defun sgn-search--run (query chat-id)
  "Show the messages matching QUERY, in CHAT-ID or in all chats if nil."
  (setq sgn-search--last (cons query chat-id))
  (sgn-search--display query chat-id (sgn-db-search query chat-id 100)))

;;;; Results

(defun sgn-search--display (query chat-id results)
  "Show RESULTS for QUERY, searched in CHAT-ID or all chats."
  (with-current-buffer (get-buffer-create "*sgn Search*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (sgn-search-mode)
      (insert (propertize (format "Search: %s%s\n\n" query
                                  (if chat-id
                                      (format " in %s" (sgn-contacts-get-name chat-id))
                                    ""))
                          'face 'bold))
      (if (null results)
          (insert (propertize "No results found.\n" 'face 'shadow))
        (dolist (msg results)
          (sgn-search--insert-result msg))
        (insert (propertize (format "%d result%s" (length results)
                                    (if (cdr results) "s" ""))
                            'face 'shadow)))
      (goto-char (point-min))
      (sgn-search-next-result))
    (switch-to-buffer (current-buffer))))

(defun sgn-search--insert-result (msg)
  "Insert the search result MSG."
  (let ((start (point)))
    (insert (propertize (format "── %s in %s · %s\n"
                                (sgn-contacts-display-sender (plist-get msg :sender))
                                (sgn-contacts-get-name (plist-get msg :chat-id))
                                (sgn-contacts-format-time (plist-get msg :timestamp) t))
                        'face 'bold)
            "  " (replace-regexp-in-string "\n" " " (or (plist-get msg :snippet) ""))
            "\n\n")
    (add-text-properties start (point)
                         (list 'sgn-search-chat-id (plist-get msg :chat-id)
                               'sgn-search-rowid (plist-get msg :rowid)))))

;;;; Navigation

(defun sgn-search-goto-result ()
  "Show the message at point in its chat."
  (interactive)
  (let ((chat-id (get-text-property (point) 'sgn-search-chat-id))
        (rowid (get-text-property (point) 'sgn-search-rowid)))
    (unless chat-id
      (user-error "No search result at point"))
    (sgn-chat-open chat-id)
    (sgn-chat-show-message rowid)))

(defun sgn-search-next-result ()
  "Move to the start of the next search result."
  (interactive)
  (let ((pos (point)))
    (when (get-text-property pos 'sgn-search-rowid)
      (setq pos (or (next-single-property-change pos 'sgn-search-rowid)
                    (point-max))))
    (when-let* ((next (if (get-text-property pos 'sgn-search-rowid)
                          pos
                        (next-single-property-change pos 'sgn-search-rowid))))
      (goto-char next))))

(defun sgn-search-prev-result ()
  "Move to the start of the previous search result."
  (interactive)
  (let* ((pos (point))
         ;; Results are adjacent, so the run before the current
         ;; result's start is the previous result.
         (start (if (get-text-property pos 'sgn-search-rowid)
                    (or (previous-single-property-change (1+ pos) 'sgn-search-rowid)
                        (point-min))
                  pos))
         (prev (and (> start (point-min))
                    (previous-single-property-change start 'sgn-search-rowid))))
    (when (and prev (get-text-property prev 'sgn-search-rowid))
      (goto-char prev))))

(provide 'sgn-search)
;;; sgn-search.el ends here
