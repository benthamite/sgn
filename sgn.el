;;; sgn.el --- Signal client via signal-cli JSON-RPC  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>
;; URL: https://github.com/benthamite/sgn
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

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

;; Sgn.el provides a lightweight, text-based interface for Signal. It
;; communicates with a running `signal-cli' daemon via JSON-RPC.
;;
;; Features:
;; - Strict chat buffers (read-only history, guarded prompt).
;; - Inline image rendering.
;; - Sticker support (APNG->GIF conversion for animation).
;; - Auto-refreshing dashboard of active chats.
;; - Native Emacs desktop notifications.
;;
;; Prerequisites:
;; 1. signal-cli installed and in $PATH.
;; 2. A registered/linked Signal account.
;; 3. For animated stickers: `imagemagick' (specifically the `convert' command)
;;    must be available in your PATH.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'notifications)
(require 'button)
(require 'image)

;;; Configuration

(defgroup sgn nil
  "Signal client for Emacs using signal-cli."
  :group 'comm
  :prefix "sgn-")

(defcustom sgn-account nil
  "The registered Signal phone number (e.g. +15550000000).
This must match the account registered with signal-cli."
  :type '(choice (const :tag "Not Set" nil) string))

(defcustom sgn-cli-program (or (executable-find "signal-cli") "signal-cli")
  "Path to the signal-cli executable.
This program is a runtime dependency.  If it is in your variable `exec-path',
this is automatically set.  Otherwise, you must provide the absolute path."
  :type 'file)

(defcustom sgn-data-directory (expand-file-name "~/.local/share/signal-cli")
  "Directory where signal-cli stores data (attachments, stickers, etc.).
Default on Linux is typically ~/.local/share/signal-cli."
  :type 'directory)

(defcustom sgn-enable-animation t
  "If non-nil, animate stickers and GIFs in the chat buffer.
When nil, only the first frame of animated media will be displayed."
  :type 'boolean)

(defcustom sgn-prompt "> "
  "The prompt string displayed in chat buffers."
  :type 'string)

(defcustom sgn-auto-open-buffer t
  "If non-nil, automatically display the chat buffer when a message arrives."
  :type 'boolean)

;;; Faces

(defface sgn-my-msg-face
  '((t :inherit font-lock-function-name-face))
  "Face applied to your own messages.")

(defface sgn-other-msg-face
  '((t :inherit font-lock-variable-name-face))
  "Face applied to messages from other users.")

(defface sgn-timestamp-face
  '((t :inherit shadow))
  "Face for message timestamps.")

(defface sgn-error-face
  '((t :inherit error))
  "Face for error messages.")

;;; Internal State

(defconst sgn--process-name "signal-rpc"
  "Internal name for the signal-cli process.")

(defconst sgn--max-partial-line-length 100000
  "Maximum bytes to buffer before discarding an incomplete JSON line.
Prevents unbounded memory growth if signal-cli emits a very long
line without a terminating newline.")

(defconst sgn--contact-refresh-delay 2
  "Seconds to wait after process start before refreshing contacts.
Gives signal-cli time to initialize its JSON-RPC interface.")

(defconst sgn--sticker-max-width 150
  "Max pixel width for sticker images (smaller than general media).")

(defconst sgn--image-max-width 400
  "Default max pixel width for inline images.")

(defconst sgn--temp-file-cleanup-delay "5 sec"
  "Delay before deleting temporary converted sticker files.
Gives Emacs time to read and render the image before removal.")

(defconst sgn--sticker-fallback-emoji "🧩"
  "Emoji shown when a sticker lacks its own emoji metadata.")

(defconst sgn--buffer-stderr " *sgn-stderr*"
  "Name of the hidden buffer used for process stderr.")

(defvar sgn--rpc-id-counter 0
  "Counter for JSON-RPC request IDs.")

(defvar sgn--request-buffer-map (make-hash-table :test 'equal)
  "Mapping of RPC ID to buffer name for error reporting.")

(defvar sgn--contact-map (make-hash-table :test 'equal)
  "Cache of Signal IDs to display names.
Keys are phone numbers (for contacts) or base64 group IDs
\(for groups).  Values are human-readable names.")

(defvar sgn--active-chats (make-hash-table :test 'equal)
  "Hash table used as a set of currently active chat IDs.
Keys are Signal IDs; values are always t.")

(defvar sgn--partial-line ""
  "Buffer string for incomplete JSON lines received from the process.")

(defun sgn--ensure-list (value)
  "Coerce VALUE to a list if it is a vector.
`json-read' returns JSON arrays as vectors; this normalizes them
for `dolist' iteration."
  (if (vectorp value) (append value nil) value))

(defvar sgn--pending-callbacks (make-hash-table :test 'equal)
  "Mapping of RPC ID to callback function for handling responses.")

;;; Logging

(defun sgn--log (fmt &rest args)
  "Log debug info to *sgn-log* using FMT and ARGS."
  (with-current-buffer (get-buffer-create "*sgn-log*")
    (goto-char (point-max))
    (insert (format-time-string "[%H:%M:%S] "))
    (insert (apply #'format fmt args))
    (insert "\n")))

(defun sgn-show-log ()
  "Display the debug log buffer."
  (interactive)
  (display-buffer (get-buffer-create "*sgn-log*")))

;;; Process Infrastructure

;;;###autoload
(defun sgn-start ()
  "Start the signal-cli JSON-RPC process."
  (interactive)
  (unless (executable-find sgn-cli-program)
    (user-error "The signal-cli executable '%s' was not found. Please install it or check `sgn-cli-program'" sgn-cli-program))

  (unless sgn-account
    (user-error "Variable `sgn-account' is not set"))

  (when (get-process sgn--process-name)
    (delete-process sgn--process-name))

  ;; State cleanup
  (setq sgn--partial-line "")
  (clrhash sgn--request-buffer-map)
  (clrhash sgn--pending-callbacks)

  (let ((proc (make-process
               :name sgn--process-name
               :buffer sgn--buffer-stderr
               :command (list sgn-cli-program "-a" sgn-account "jsonRpc")
               :filter #'sgn--process-filter
               :sentinel #'sgn--process-sentinel
               :coding 'utf-8-unix)))
    (set-process-query-on-exit-flag proc nil)
    (sgn--log "Sgn service started.")
    (run-at-time sgn--contact-refresh-delay nil #'sgn-refresh-contacts)
    (message "Sgn service started.")))

(defun sgn-stop ()
  "Stop the Sgn service."
  (interactive)
  (when (get-process sgn--process-name)
    (delete-process sgn--process-name)
    (message "Sgn service stopped.")))

(defun sgn--send-rpc (method params &optional target-buffer callback)
  "Send a JSON-RPC payload with METHOD and PARAMS.
If TARGET-BUFFER is non-nil, map the request ID to that buffer for
error handling.  If CALLBACK is non-nil, call it with the result
when a successful response arrives."
  (unless (get-process sgn--process-name)
    (error "Sgn service not running. M-x sgn-start"))
  (let* ((id (cl-incf sgn--rpc-id-counter))
         (req `((jsonrpc . "2.0")
                (method . ,method)
                (params . ,params)
                (id . ,id)))
         (json-str (json-encode req)))
    (when target-buffer
      (puthash id (buffer-name target-buffer) sgn--request-buffer-map))
    (when callback
      (puthash id callback sgn--pending-callbacks))
    (sgn--log "SEND: %s" json-str)
    (process-send-string sgn--process-name (concat json-str "\n"))
    id))

;;; Parsing & Dispatch

(defun sgn--process-filter (_proc string)
  "Accumulate output from process and parse complete JSON objects from STRING."
  (setq sgn--partial-line (concat sgn--partial-line string))
  (when (> (length sgn--partial-line) sgn--max-partial-line-length)
    (setq sgn--partial-line "")
    (sgn--log "WARNING: Buffer overflow protection triggered. Dropped data."))

  (let ((lines (split-string sgn--partial-line "\n")))
    (if (string-suffix-p "\n" sgn--partial-line)
        (setq sgn--partial-line "")
      (setq sgn--partial-line (car (last lines)))
      (setq lines (butlast lines)))

    (dolist (line lines)
      (setq line (string-trim line))
      (when (and (not (string-empty-p line)) (string-prefix-p "{" line))
        (sgn--log "RECV: %s" line)
        (condition-case err
            (let ((json (json-read-from-string line)))
              (sgn--dispatch json))
          (error (sgn--log "JSON Error: %s" err)))))))

(defun sgn--process-sentinel (_proc event)
  "Log process EVENT for debugging."
  (sgn--log "Process Event: %s" event)
  (when (string-prefix-p "exited" event)
    (message "Sgn process exited.")))

(defun sgn--dispatch (json)
  "Dispatch JSON object to appropriate handler."
  (let ((method (alist-get 'method json))
        (error-obj (alist-get 'error json))
        (id (alist-get 'id json))
        (result (alist-get 'result json))
        (params (alist-get 'params json)))
    (cond
     ((equal method "receive") (sgn--handle-receive params))
     (error-obj (sgn--handle-error id error-obj))
     ((and id result) (sgn--handle-result id result)))))

(defun sgn--handle-error (id error-obj)
  "Handle RPC errors for request ID using ERROR-OBJ."
  (let* ((buf-name (gethash id sgn--request-buffer-map))
         (msg (alist-get 'message error-obj)))
    (sgn--log "RPC Error [%s]: %s" id msg)
    (remhash id sgn--request-buffer-map)
    (remhash id sgn--pending-callbacks)
    (when (and buf-name (get-buffer buf-name))
      (with-current-buffer buf-name
        (sgn--insert-system-msg (format "ERROR: %s" msg) 'sgn-error-face)))))

(defun sgn--handle-result (id result)
  "Handle a successful RPC response for request ID with RESULT.
Invokes and removes the pending callback, if any."
  (let ((callback (gethash id sgn--pending-callbacks)))
    (remhash id sgn--request-buffer-map)
    (when callback
      (remhash id sgn--pending-callbacks)
      (funcall callback result))))

(defun sgn--populate-contacts (result)
  "Populate the contact map and active chats from a listContacts RESULT."
  (let ((contacts (sgn--ensure-list result)))
    (dolist (contact contacts)
      (let ((number (alist-get 'number contact))
            (name (alist-get 'name contact)))
        (when (and number name (not (string-empty-p name)))
          (puthash number name sgn--contact-map)
          (puthash number t sgn--active-chats)))))
  (sgn--dashboard-refresh))

(defun sgn--populate-groups (result)
  "Populate active chats from a listGroups RESULT."
  (let ((groups (sgn--ensure-list result)))
    (dolist (group groups)
      (let ((group-id (alist-get 'id group))
            (name (alist-get 'name group)))
        (when group-id
          (puthash group-id t sgn--active-chats)
          (when (and name (not (string-empty-p name)))
            (puthash group-id name sgn--contact-map))))))
  (sgn--dashboard-refresh))

(defun sgn-refresh-contacts ()
  "Fetch contacts and groups from signal-cli and populate the dashboard."
  (interactive)
  (sgn--send-rpc "listContacts" nil nil #'sgn--populate-contacts)
  (sgn--send-rpc "listGroups" nil nil #'sgn--populate-groups))

(defun sgn--handle-receive (params)
  "Handle new messages, attachments, stickers, and sync events from PARAMS."
  (let* ((envelope (alist-get 'envelope params))
         (source (or (alist-get 'sourceNumber envelope) (alist-get 'source envelope)))
         (source-name (alist-get 'sourceName envelope))
         (data (alist-get 'dataMessage envelope))
         (sync (alist-get 'syncMessage envelope))
         (typing (alist-get 'typingMessage envelope))
         (group-info (alist-get 'groupInfo data)))

    (when (and source source-name)
      (puthash source source-name sgn--contact-map))

    (let ((chat-id (if group-info (alist-get 'groupId group-info) source)))
      (when chat-id
        (puthash chat-id t sgn--active-chats)
        (sgn--dashboard-refresh)

        ;; 1. Incoming Data (Text OR Attachments)
        (when data
          (let ((msg-text (alist-get 'message data))
                (attachments (alist-get 'attachments data))
                (sticker (alist-get 'sticker data))
                (sender (or source-name source)))

            (when (or msg-text attachments sticker)
              (sgn--insert-msg chat-id sender msg-text attachments sticker nil)

              ;; Notify
              (let ((notify-body (cond (msg-text msg-text)
                                       (sticker "[Sticker]")
                                       (attachments "[Attachment]")
                                       (t "New Message"))))
                (notifications-notify :title (format "Sgn: %s" sender)
                                      :body notify-body))

              (when sgn-auto-open-buffer
                (display-buffer (sgn--get-buffer chat-id))))))

        ;; 2. Sync (My sent messages)
        (when sync
          (let* ((sent (alist-get 'sentMessage sync))
                 (sync-group (alist-get 'groupInfo sent))
                 (sync-id (or (and sync-group (alist-get 'groupId sync-group))
                              (alist-get 'destinationNumber sent)))
                 (msg-text (alist-get 'message sent))
                 (attachments (alist-get 'attachments sent))
                 (sticker (alist-get 'sticker sent)))
            (when (and sync-id (or msg-text attachments sticker))
              (puthash sync-id t sgn--active-chats)
              (sgn--insert-msg sync-id "Me" msg-text attachments sticker t))))

        ;; 3. Typing
        (when (and typing (string= "STARTED" (alist-get 'action typing)))
          (with-current-buffer (sgn--get-buffer chat-id)
            (setq mode-line-process (format " [%s...]" (or source-name source)))))))))

;;; Media & Stickers

(defun sgn--find-sticker (pack-id sticker-id)
  "Find the local sticker file for PACK-ID and STICKER-ID using manifest.json."
  (let* ((base-dir (expand-file-name "stickers/" sgn-data-directory))
         (pack-dir (expand-file-name pack-id base-dir))
         (manifest-file (expand-file-name "manifest.json" pack-dir)))
    (if (file-exists-p manifest-file)
        ;; METHOD 1: Use manifest.json (Accurate)
        (condition-case nil
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (manifest (json-read-file manifest-file))
                   (stickers (alist-get 'stickers manifest))
                   (sticker-info (seq-find (lambda (s) (= (alist-get 'id s) sticker-id)) stickers))
                   (file-name (alist-get 'file sticker-info)))
              (when file-name
                (expand-file-name file-name pack-dir)))
          (error nil))
      ;; METHOD 2: Fallback (Guessing)
      (let* ((path-no-ext (expand-file-name (number-to-string sticker-id) pack-dir))
             (path-webp (concat path-no-ext ".webp")))
        (cond
         ((file-exists-p path-webp) path-webp)
         ((file-exists-p path-no-ext) path-no-ext)
         (t nil))))))

(defun sgn--convert-apng-to-gif (file)
  "Convert APNG FILE to a temporary GIF using ImageMagick `convert'.
Returns the path to the temporary GIF."
  (let ((tmp-gif (make-temp-file "sgn-sticker-" nil ".gif")))
    (if (executable-find "convert")
        (with-temp-buffer
          ;; "apng:" forces APNG decoding; "-coalesce" merges frames
          ;; into standalone images (required for correct GIF animation)
          (if (eq 0 (call-process "convert" nil nil nil
                                  (concat "apng:" file)
                                  "-coalesce"
                                  tmp-gif))
              tmp-gif
            (sgn--log "Failed to convert APNG to GIF. `convert' exit code non-zero.")
            ;; Cleanup failed file if it was created
            (when (file-exists-p tmp-gif) (delete-file tmp-gif))
            nil))
      (sgn--log "ImageMagick `convert' not found. Cannot animate APNG.")
      (when (file-exists-p tmp-gif) (delete-file tmp-gif))
      nil)))

(defun sgn--insert-media (attachments sticker)
  "Insert media for ATTACHMENTS and STICKER into the current buffer."
  (when sticker
    (sgn--insert-sticker sticker))
  (when attachments
    (let ((att-list (sgn--ensure-list attachments)))
      (dolist (att att-list)
        (sgn--insert-attachment att)))))

(defun sgn--insert-sticker (sticker)
  "Insert STICKER media into the current buffer."
  (let* ((pack-id (alist-get 'packId sticker))
         (sticker-id (alist-get 'stickerId sticker))
         (emoji (or (alist-get 'emoji sticker) sgn--sticker-fallback-emoji))
         (file (when (and pack-id sticker-id)
                 (sgn--find-sticker pack-id sticker-id))))
    (insert "\n")
    (cond
     ((and file (file-exists-p file))
      (sgn--insert-sticker-image file emoji))
     (t
      (insert (propertize (format "[Sticker %s]" emoji)
                          'face 'font-lock-constant-face))))))

(defun sgn--insert-sticker-image (file emoji)
  "Insert sticker image from FILE, with EMOJI as fallback label.
Handles APNG-to-GIF conversion when ImageMagick is available."
  (let* ((type (image-type-from-file-header file))
         (final-file file)
         (converted nil))
    (when (and (eq type 'png) (executable-find "convert"))
      (let ((gif (sgn--convert-apng-to-gif file)))
        (when gif
          (setq final-file gif)
          (setq converted t))))
    (let ((image (sgn--insert-inline-image final-file sgn--sticker-max-width)))
      (when converted
        (run-at-time sgn--temp-file-cleanup-delay nil
                     (lambda (f) (when (file-exists-p f) (delete-file f)))
                     final-file))
      (unless image
        (insert (propertize (format "[Sticker %s (Render Failed)]" emoji)
                            'face 'font-lock-warning-face))))))

(defun sgn--insert-attachment (att)
  "Insert a single attachment ATT into the current buffer."
  (let* ((stored (alist-get 'storedFilename att))
         (type (alist-get 'contentType att))
         (name (or (alist-get 'filename att) "attachment"))
         (att-id (alist-get 'id att))
         (path (or (and stored (file-exists-p stored) stored)
                   (let ((std (expand-file-name
                               (format "attachments/%s" att-id)
                               sgn-data-directory)))
                     (and (file-exists-p std) std)))))
    (insert "\n")
    (cond
     ((and path type (string-prefix-p "image/" type))
      (sgn--insert-inline-image path))
     (path
      (insert-button (format "[File: %s]" name)
                     'action (lambda (_) (browse-url-of-file path))
                     'face 'link
                     'help-echo (format "Type: %s\nPath: %s" type path)))
     (t
      (insert-button (format "[File: %s (Not Downloaded)]" name)
                     'action (lambda (_)
                               (message "File not found: attachments/%s" att-id))
                     'face 'font-lock-comment-face)))))

(defun sgn--insert-inline-image (path &optional max-width)
  "Insert an inline image from PATH with optional MAX-WIDTH.
MAX-WIDTH defaults to `sgn--image-max-width'.  Animate
multi-frame images when `sgn-enable-animation' is non-nil.
Return the image object, or nil if creation failed."
  (let ((image (create-image path nil nil
                             :max-width (or max-width sgn--image-max-width))))
    (when image
      (insert-image image)
      (when (and sgn-enable-animation (fboundp 'image-animate)
                 (image-multi-frame-p image))
        (image-animate image nil t)))
    image))

;;; Buffer & UI Management

(defvar-local sgn--chat-id nil
  "The Signal recipient ID associated with the current buffer.")

(defvar-local sgn--input-marker nil
  "Marker indicating the start of the editable input area.")

(defun sgn--guard-cursor ()
  "Ensure cursor stays in the editable prompt area."
  (let ((limit (if sgn--input-marker (marker-position sgn--input-marker) (point-min))))
    (when (< (point) limit)
      (goto-char limit))))

(define-derived-mode sgn-chat-mode fundamental-mode "Sgn"
  "Major mode for Signal chats."
  (setq-local paragraph-start (regexp-quote sgn-prompt))
  (visual-line-mode 1)

  ;; Setup Input Marker
  (setq sgn--input-marker (make-marker))

  (add-hook 'post-command-hook #'sgn--guard-cursor nil t)
  (local-set-key (kbd "RET") #'sgn--send-input)
  (local-set-key (kbd "C-c C-c") #'sgn--send-input)
  (local-set-key (kbd "C-c C-a") #'sgn-attach-file))

(defun sgn--get-buffer (id)
  "Get or create a chat buffer for ID."
  (let* ((buf-name (format "*Sgn: %s*" id))
         (buffer (get-buffer buf-name)))
    (unless buffer
      (setq buffer (get-buffer-create buf-name))
      (with-current-buffer buffer
        (sgn-chat-mode)
        (setq sgn--chat-id id)
        (sgn--draw-prompt)))
    buffer))

(defun sgn--draw-prompt ()
  "Draw the input prompt and update the input marker."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (insert (propertize sgn-prompt
                        'read-only t
                        'face 'minibuffer-prompt
                        'rear-nonsticky '(read-only face)
                        'front-sticky '(read-only face)))
    ;; Update the marker to the end of the prompt
    (set-marker sgn--input-marker (point))))

(defun sgn--insert-msg (id name text attachments sticker is-me)
  "Insert text and media into the buffer for chat ID.
NAME is the sender, TEXT is the content, ATTACHMENTS and STICKER contain
media data, and IS-ME is non-nil if the message is from the user."
  (with-current-buffer (sgn--get-buffer id)
    (let ((inhibit-read-only t)
          (name-face (if is-me 'sgn-my-msg-face 'sgn-other-msg-face)))
      (save-excursion
        ;; Move to just before the prompt
        (goto-char (marker-position sgn--input-marker))
        (forward-line 0) ;; Ensure we are at start of prompt line (usually empty above)

        ;; If the previous line isn't a newline, insert one
        (unless (bolp) (insert "\n"))

        ;; Delete the prompt from the view momentarily (optional, but cleaner)
        (delete-region (point) (point-max))

        ;; Insert Message
        (insert (propertize (format-time-string "[%H:%M] ") 'face 'sgn-timestamp-face))
        (insert (propertize (concat "<" name "> ") 'face name-face))
        (when text (insert text))
        (when (or attachments sticker)
          (when text (insert "\n"))
          (sgn--insert-media attachments sticker))
        (insert "\n")

        ;; Redraw Prompt at the new bottom
        (sgn--draw-prompt)))

    (let ((win (get-buffer-window (current-buffer))))
      (when win (set-window-point win (point-max))))))

(defun sgn--insert-system-msg (text face)
  "Insert a system message with TEXT using FACE."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (if sgn--input-marker (marker-position sgn--input-marker) (point-max)))
      (forward-line 0)
      (delete-region (point) (point-max))
      (insert (propertize (concat "*** " text "\n") 'face face))
      (sgn--draw-prompt))))

;;; Interactive Commands

(defun sgn--is-group-id (id)
  "Return non-nil if ID looks like a Signal group ID.
Uses a heuristic: phone numbers start with \"+\" and UUIDs
contain \"-\"; anything else is assumed to be a base64 group ID."
  (not (or (string-prefix-p "+" id)
           (string-match-p "-" id))))

(defun sgn--send-input ()
  "Send the input from the prompt to the current chat."
  (interactive)
  (let* ((start (marker-position sgn--input-marker))
         (end (point-max))
         (text (string-trim (buffer-substring-no-properties start end))))
    (unless (string-empty-p text)
      (let ((inhibit-read-only t))
        (delete-region start end))

      (sgn--send-rpc "send"
                     (sgn--build-send-params sgn--chat-id `((message . ,text)))
                     (current-buffer))

      (sgn--insert-msg sgn--chat-id "Me" text nil nil t))))

;;;###autoload
(defun sgn-attach-file (file-path)
  "Send FILE-PATH as an attachment to the current chat."
  (interactive "fAttachment: ")
  (unless sgn--chat-id
    (user-error "Not in a Signal chat buffer"))
  (let ((full-path (expand-file-name file-path)))
    (sgn--send-rpc "send"
                   (sgn--build-send-params sgn--chat-id
                                           `((attachments . [,full-path])))
                   (current-buffer))
    (sgn--insert-msg sgn--chat-id "Me"
                     (format "[Sending: %s]" (file-name-nondirectory full-path))
                     nil nil t)))

(defun sgn--build-send-params (chat-id base-params)
  "Add recipient or group addressing to BASE-PARAMS for CHAT-ID."
  (if (sgn--is-group-id chat-id)
      (cons `(groupId . ,chat-id) base-params)
    (cons `(recipient . [,chat-id]) base-params)))

;;;###autoload
(defun sgn-chat (recipient)
  "Open a chat buffer for RECIPIENT (phone number or group ID)."
  (interactive "sSignal Recipient (+Phone): ")
  (let ((buffer (sgn--get-buffer recipient)))
    (switch-to-buffer buffer)
    (message "Chat opened.")))

;;; Dashboard

(defvar sgn-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'sgn--dashboard-open-entry)
    (define-key map (kbd "n") #'forward-button)
    (define-key map (kbd "p") #'backward-button)
    (define-key map (kbd "g") #'sgn--dashboard-refresh)
    map)
  "Keymap for `sgn-dashboard-mode'.")

(define-derived-mode sgn-dashboard-mode special-mode "Sgn List"
  "Major mode for the list of active Signal chats.")

(defun sgn--dashboard-draw ()
  "Redraw the dashboard content."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Active Chats:\n")
    (insert "-------------\n")
    (maphash (lambda (id _)
               (let ((name (gethash id sgn--contact-map id)))
                 (insert-button (format "%s (%s)" name id)
                                'action #'sgn--dashboard-open-entry
                                'sgn-id id
                                'follow-link t)
                 (insert "\n")))
             sgn--active-chats)))

(defun sgn--dashboard-refresh ()
  "Refresh the dashboard buffer."
  (interactive)
  (let ((buf (get-buffer "*Sgn List*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((line (line-number-at-pos)))
          (sgn--dashboard-draw)
          (goto-char (point-min))
          (forward-line (1- line)))))))

;;;###autoload
(defun sgn-dashboard ()
  "Open the Sgn Dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*Sgn List*")))
    (with-current-buffer buf
      (sgn-dashboard-mode)
      (sgn--dashboard-draw))
    (switch-to-buffer buf)))

(defun sgn--dashboard-open-entry (&optional btn)
  "Open chat for the button at point or BTN."
  (interactive)
  (let* ((button (or btn (button-at (point))))
         (id (and button (button-get button 'sgn-id))))
    (if id
        (sgn-chat id)
      (user-error "No chat entry at point"))))

(provide 'sgn)
;;; sgn.el ends here
