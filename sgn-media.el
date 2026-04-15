;;; sgn-media.el --- Media display and handling for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

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

;; Media handling for sgn: inline image rendering, sticker support
;; (with APNG to GIF conversion), attachment display, voice note
;; playback, and link preview rendering.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'image)
(require 'button)

(declare-function sgn--log "sgn")

(defvar sgn-data-directory)

;;;; Customization

(defcustom sgn-image-max-width 300
  "Maximum pixel width for inline images."
  :type 'integer
  :group 'sgn)

(defcustom sgn-sticker-max-width 150
  "Maximum pixel width for sticker images."
  :type 'integer
  :group 'sgn)

(defcustom sgn-enable-animation t
  "If non-nil, animate stickers and GIFs."
  :type 'boolean
  :group 'sgn)

;;;; Constants

(defconst sgn-media--temp-file-cleanup-delay "5 sec"
  "Delay before deleting temporary converted sticker files.")

(defconst sgn-media--sticker-fallback-emoji "🧩"
  "Emoji shown when a sticker image cannot be rendered.")

(defconst sgn-media--link-preview-width 35
  "Character width for link preview boxes.")

;;;; Private helpers

(defun sgn-media--ensure-list (value)
  "Coerce VALUE to a list if it is a vector.
`json-read' returns JSON arrays as vectors; this normalizes them
for `dolist' iteration."
  (if (vectorp value) (append value nil) value))

(defun sgn-media--convert-apng-to-gif (file)
  "Convert APNG FILE to a temporary GIF using ImageMagick `convert'.
Return the path to the temporary GIF file, or nil on failure."
  (let ((tmp-gif (make-temp-file "sgn-sticker-" nil ".gif")))
    (if (executable-find "convert")
        (with-temp-buffer
          (if (eq 0 (call-process "convert" nil nil nil
                                  (concat "apng:" file)
                                  "-coalesce"
                                  tmp-gif))
              tmp-gif
            (sgn--log "Failed to convert APNG to GIF: %s" file)
            (when (file-exists-p tmp-gif) (delete-file tmp-gif))
            nil))
      (sgn--log "ImageMagick `convert' not found; cannot convert APNG sticker.")
      (when (file-exists-p tmp-gif) (delete-file tmp-gif))
      nil)))

(defun sgn-media--insert-sticker (sticker)
  "Insert STICKER media into the current buffer.
STICKER is an alist with keys `packId', `stickerId', and optionally `emoji'."
  (let* ((pack-id (alist-get 'packId sticker))
         (sticker-id (alist-get 'stickerId sticker))
         (emoji (or (alist-get 'emoji sticker)
                    sgn-media--sticker-fallback-emoji))
         (file (when (and pack-id sticker-id)
                 (sgn-media-find-sticker pack-id sticker-id))))
    (insert "\n")
    (cond
     ((and file (file-exists-p file))
      (sgn-media--insert-sticker-image file emoji))
     (t
      (insert (propertize (format "[Sticker %s]" emoji)
                          'face 'font-lock-constant-face))))))

(defun sgn-media--insert-sticker-image (file emoji)
  "Insert the sticker image at FILE into the current buffer.
EMOJI is used as fallback text if rendering fails.  If the image is
an APNG, attempt conversion to GIF for animation support."
  (let* ((type (image-type-from-file-header file))
         (final-file file)
         (converted nil))
    ;; Attempt APNG-to-GIF conversion for animated stickers.
    (when (and (eq type 'png) (executable-find "convert"))
      (let ((gif (sgn-media--convert-apng-to-gif file)))
        (when gif
          (setq final-file gif)
          (setq converted t))))
    (let ((image (sgn-media-insert-inline-image final-file sgn-sticker-max-width)))
      ;; Clean up the temporary GIF after a delay.
      (when converted
        (run-at-time sgn-media--temp-file-cleanup-delay nil
                     (lambda (f) (when (file-exists-p f) (delete-file f)))
                     final-file))
      (unless image
        (insert (propertize (format "[Sticker %s (Render Failed)]" emoji)
                            'face 'font-lock-warning-face))))))

(defun sgn-media--insert-attachment (att)
  "Insert a single attachment ATT into the current buffer.
ATT is an alist with keys `storedFilename', `contentType', `filename', and `id'.
Images are rendered inline; other files are shown as clickable buttons."
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
     ;; Inline image rendering.
     ((and path type (string-prefix-p "image/" type))
      (or (sgn-media-insert-inline-image path)
          (insert (propertize (format "[Image: %s]" name)
                              'face 'font-lock-warning-face))))
     ;; Clickable file button for downloaded non-image attachments.
     (path
      (insert-button (format "[File: %s]" name)
                     'action (lambda (_) (browse-url-of-file path))
                     'face 'link
                     'help-echo (format "Type: %s\nPath: %s" type path)))
     ;; Placeholder for attachments not yet downloaded.
     (t
      (insert-button (format "[File: %s (Not Downloaded)]" name)
                     'action (lambda (_)
                               (message "File not found: attachments/%s" att-id))
                     'face 'font-lock-comment-face)))))

(defun sgn-media--format-duration (seconds)
  "Format SECONDS as a human-readable duration string (M:SS)."
  (let ((mins (/ seconds 60))
        (secs (% seconds 60)))
    (format "%d:%02d" mins secs)))

(defun sgn-media--detect-audio-player ()
  "Return a cons (PROGRAM . ARGS-PREFIX) for playing audio, or nil.
On macOS, use \"afplay\".  On Linux, try \"paplay\" then \"aplay\"."
  (cond
   ((executable-find "afplay") (cons "afplay" nil))
   ((executable-find "paplay") (cons "paplay" nil))
   ((executable-find "aplay")  (cons "aplay" nil))
   (t nil)))

(defun sgn-media--truncate-or-pad (str width)
  "Truncate or right-pad STR to exactly WIDTH characters."
  (if (> (length str) width)
      (concat (substring str 0 (- width 1)) "…")
    (concat str (make-string (- width (length str)) ?\s))))

(defun sgn-media--extract-domain (url)
  "Extract the display domain from URL.
Strips the scheme and any trailing path, returning e.g. \"example.com\"."
  (if (string-match "\\`https?://\\([^/]+\\)" url)
      (match-string 1 url)
    url))

;;;; Public API

(defun sgn-media-insert (attachments sticker)
  "Insert media for ATTACHMENTS and STICKER into the current buffer.
ATTACHMENTS is a list or vector of attachment alists (from JSON).
STICKER is a sticker alist or nil."
  (when sticker
    (sgn-media--insert-sticker sticker))
  (when attachments
    (let ((att-list (sgn-media--ensure-list attachments)))
      (dolist (att att-list)
        (sgn-media--insert-attachment att)))))

(defun sgn-media-insert-inline-image (path &optional max-width)
  "Insert an inline image from PATH into the current buffer.
MAX-WIDTH defaults to `sgn-image-max-width'.  If `sgn-enable-animation'
is non-nil and the image has multiple frames, animate it.
Return the image object on success, or nil if rendering fails."
  (let ((image (create-image path nil nil
                             :max-width (or max-width sgn-image-max-width))))
    (when image
      (insert-image image)
      (when (and sgn-enable-animation
                 (fboundp 'image-animate)
                 (image-multi-frame-p image))
        (image-animate image nil t)))
    image))

(defun sgn-media-find-sticker (pack-id sticker-id)
  "Find the local sticker file for PACK-ID and STICKER-ID.
Look up the sticker in the pack's manifest.json first; fall back
to searching by numeric ID with common extensions.
Return the file path, or nil if not found."
  (let* ((base-dir (expand-file-name "stickers/" sgn-data-directory))
         (pack-dir (expand-file-name pack-id base-dir))
         (manifest-file (expand-file-name "manifest.json" pack-dir)))
    (if (file-exists-p manifest-file)
        (condition-case nil
            (let* ((json-object-type 'alist)
                   (json-array-type 'list)
                   (manifest (json-read-file manifest-file))
                   (stickers (alist-get 'stickers manifest))
                   (sticker-info (seq-find
                                  (lambda (s)
                                    (= (alist-get 'id s) sticker-id))
                                  stickers))
                   (file-name (alist-get 'file sticker-info)))
              (when file-name
                (expand-file-name file-name pack-dir)))
          (error nil))
      ;; No manifest: try common file paths by numeric ID.
      (let* ((path-no-ext (expand-file-name (number-to-string sticker-id)
                                            pack-dir))
             (path-webp (concat path-no-ext ".webp")))
        (cond
         ((file-exists-p path-webp) path-webp)
         ((file-exists-p path-no-ext) path-no-ext)
         (t nil))))))

(defun sgn-media-insert-voice-note (file-path duration)
  "Insert a voice note display element for FILE-PATH with DURATION in seconds.
Renders as a clickable button: 🎤 M:SS [▶ Play].
Pressing RET on the element plays the audio via `sgn-media-play-audio'."
  (let ((label (format "🎤 %s [▶ Play]"
                       (sgn-media--format-duration duration))))
    (insert "\n")
    (insert-button label
                   'action (lambda (_) (sgn-media-play-audio file-path))
                   'face 'link
                   'help-echo (format "Voice note: %s (%s)"
                                      (file-name-nondirectory file-path)
                                      (sgn-media--format-duration duration)))))

(defun sgn-media-play-audio (file-path)
  "Play audio at FILE-PATH using an external player.
Uses `play-sound-file' if available as a built-in, otherwise launches
an external process (afplay on macOS, paplay or aplay on Linux)."
  (cond
   ;; Prefer the built-in when available (Emacs compiled with sound support).
   ((and (fboundp 'play-sound-file) (file-exists-p file-path))
    (condition-case err
        (play-sound-file file-path)
      (error
       ;; play-sound-file may signal if Emacs lacks sound support at
       ;; runtime; fall through to external player.
       (sgn--log "play-sound-file failed: %s; trying external player." err)
       (sgn-media--play-audio-external file-path))))
   ((file-exists-p file-path)
    (sgn-media--play-audio-external file-path))
   (t
    (message "Voice note file not found: %s" file-path))))

(defun sgn-media--play-audio-external (file-path)
  "Play audio at FILE-PATH using an external command-line player."
  (let ((player (sgn-media--detect-audio-player)))
    (if player
        (start-process "sgn-audio" nil (car player) file-path)
      (message "No audio player found (tried afplay, paplay, aplay)."))))

(defun sgn-media-insert-link-preview (title description url
                                            &optional thumbnail-path)
  "Insert a link preview box with TITLE, DESCRIPTION, URL, and THUMBNAIL-PATH.
Renders a box-drawing framed preview.  Pressing RET on the preview opens URL.
THUMBNAIL-PATH, if non-nil and pointing to an existing file, is rendered as
a small inline image above the text content."
  (let* ((inner-width (- sgn-media--link-preview-width 4))
         (top    (concat "  ┌" (make-string (- sgn-media--link-preview-width 2) ?─) "┐"))
         (bottom (concat "  └" (make-string (- sgn-media--link-preview-width 2) ?─) "┘"))
         (title-line (sgn-media--truncate-or-pad
                      (concat "📎 " (or title "Link")) inner-width))
         (desc-line (sgn-media--truncate-or-pad
                     (or description "") inner-width))
         (domain-line (sgn-media--truncate-or-pad
                       (sgn-media--extract-domain url) inner-width))
         (box (concat top "\n"
                      "  │ " title-line " │\n"
                      "  │ " desc-line  " │\n"
                      "  │ " domain-line " │\n"
                      bottom)))
    (insert "\n")
    ;; Optional thumbnail above the box.
    (when (and thumbnail-path (file-exists-p thumbnail-path))
      (sgn-media-insert-inline-image thumbnail-path sgn-sticker-max-width)
      (insert "\n"))
    (insert-button box
                   'action (lambda (_) (browse-url url))
                   'face nil
                   'help-echo (format "Open: %s" url)
                   'follow-link t)))

(provide 'sgn-media)
;;; sgn-media.el ends here
