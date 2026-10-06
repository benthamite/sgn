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

;; Media for sgn: finding attachment and sticker files, rendering them
;; in chat buffers, opening them, and playing voice notes.
;;
;; The database stores what signal-cli reports about each attachment
;; (its id, type and name), not a path: the path is worked out when
;; the message is drawn, from `sgn-data-directory'.  Stickers are
;; resolved the same way, so a sticker whose pack signal-cli downloads
;; later shows up once it is there.  Files sent from Emacs are
;; referred to by their own path.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'image)

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
  "If non-nil, animate animated images."
  :type 'boolean
  :group 'sgn)

(defcustom sgn-audio-players
  '(("mpv" "--no-video" "--really-quiet")
    ("ffplay" "-nodisp" "-autoexit" "-loglevel" "quiet")
    ("afplay"))
  "Programs tried, in order, to play voice notes.
Each entry is a program followed by arguments to put before the
file name.  Signal voice notes are AAC audio, which the player
must be able to decode."
  :type '(repeat (cons string (repeat string)))
  :group 'sgn)

;;;; Locating files

(defun sgn-media--attachment-path (id)
  "Return the file signal-cli stored attachment ID in, or nil.
ID comes from the network, so it must name a regular file
directly inside the attachments directory."
  (when (and (stringp id)
             (not (string-match-p "/" id))
             (not (member id '("" "." ".."))))
    (let ((path (expand-file-name
                 id (expand-file-name "attachments" sgn-data-directory))))
      (and (file-regular-p path) path))))

(defun sgn-media-find-sticker (pack-id sticker-id)
  "Return the file of sticker STICKER-ID in pack PACK-ID, or nil."
  (when (and (stringp pack-id) (string-match-p "\\`[0-9a-fA-F]+\\'" pack-id)
             (integerp sticker-id))
    (let* ((pack-dir (expand-file-name pack-id (expand-file-name
                                                "stickers" sgn-data-directory)))
           (manifest (expand-file-name "manifest.json" pack-dir))
           (file (or (sgn-media--manifest-file manifest sticker-id)
                     (number-to-string sticker-id)))
           (path (expand-file-name file pack-dir)))
      (and (file-regular-p path) path))))

(defun sgn-media--manifest-file (manifest sticker-id)
  "Return the file name MANIFEST gives for STICKER-ID, or nil."
  (when (file-readable-p manifest)
    (ignore-errors
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (stickers (alist-get 'stickers (json-read-file manifest)))
             (entry (seq-find (lambda (s) (eql (alist-get 'id s) sticker-id))
                              stickers))
             (file (alist-get 'file entry)))
        (and (stringp file) (not (string-match-p "/" file)) file)))))

(defun sgn-media-path (media)
  "Return the file holding MEDIA, a media plist, or nil if absent."
  (cond
   ((plist-get media :local-path)
    (let ((path (plist-get media :local-path)))
      (and (file-regular-p path) path)))
   ((plist-get media :sticker-pack-id)
    (sgn-media-find-sticker (plist-get media :sticker-pack-id)
                            (plist-get media :sticker-id)))
   (t (sgn-media--attachment-path (plist-get media :attachment-id)))))

(defun sgn-media-delete-files (media-list)
  "Delete the attachment files signal-cli stored for MEDIA-LIST.
Files sent from Emacs and stickers are left alone."
  (dolist (media media-list)
    (when-let* (((not (plist-get media :local-path)))
                ((not (plist-get media :sticker-pack-id)))
                (path (sgn-media--attachment-path
                       (plist-get media :attachment-id))))
      (condition-case err
          (delete-file path)
        (file-error (sgn--log "Could not delete %s: %S" path err))))))

;;;; Rendering

(defun sgn-media-render (media)
  "Insert a line showing MEDIA, a media plist, at point.
A failure to show it inserts a placeholder rather than an error."
  (condition-case err
      (sgn-media--render media (sgn-media-path media))
    (error
     (sgn--log "Could not render media %S: %S" media err)
     (insert (propertize "  [Media could not be shown]\n"
                         'face 'font-lock-warning-face)))))

(defun sgn-media--render (media path)
  "Insert a line showing MEDIA, whose file is PATH or nil."
  (let ((type (or (plist-get media :content-type) ""))
        (sticker (plist-get media :sticker-pack-id))
        (start (point)))
    (insert "  ")
    (cond
     ((null path)
      (insert (propertize (if sticker "[Sticker]" "[Media not downloaded]")
                          'face 'shadow)))
     ((eql (plist-get media :is-voice) 1)
      (insert (propertize "🎤 Voice note (RET to play)" 'face 'link
                          'sgn-voice-note path)))
     ((or sticker (string-prefix-p "image/" type))
      (unless (sgn-media--insert-image
               path (if sticker sgn-sticker-max-width sgn-image-max-width))
        (insert (propertize (format "[Image: %s]" (sgn-media--name media path))
                            'face 'link))))
     (t
      (insert (propertize (format "[File: %s]" (sgn-media--name media path))
                          'face 'link))))
    (when path
      (put-text-property start (point) 'sgn-media-path path)
      (put-text-property start (point) 'help-echo
                         (format "%s\nRET: open" path)))
    (insert "\n")))

(defun sgn-media--name (media path)
  "Return the name to show for MEDIA, whose file is PATH."
  (or (plist-get media :file-name) (file-name-nondirectory path)))

(defun sgn-media--insert-image (path max-width)
  "Insert the image in PATH no wider than MAX-WIDTH; return non-nil on success."
  (when-let* (((display-images-p))
              (image (create-image path nil nil :max-width max-width)))
    (insert-image image "[image]")
    (when (and sgn-enable-animation (image-multi-frame-p image))
      (image-animate image nil t))
    t))

;;;; Opening and playing

(defvar sgn-media--player nil
  "The process playing a voice note, or nil.")

(defun sgn-media-open (path)
  "Open the media file PATH with the system's default application."
  (browse-url-of-file path))

(defun sgn-media-play-audio (path)
  "Play the audio file PATH, or stop the voice note playing.
Pressing it again while it plays stops it."
  (if (process-live-p sgn-media--player)
      (progn (delete-process sgn-media--player)
             (setq sgn-media--player nil)
             (message "Stopped."))
    (let ((player (cl-find-if (lambda (p) (executable-find (car p)))
                              sgn-audio-players)))
      (unless player
        (user-error "No audio player found; see `sgn-audio-players'"))
      (setq sgn-media--player
            (make-process :name "sgn-audio"
                          :command (append player (list path))
                          :noquery t
                          :sentinel #'sgn-media--player-sentinel))
      (message "Playing voice note (RET again to stop)..."))))

(defun sgn-media--player-sentinel (proc _event)
  "Report a failure of the audio player PROC."
  (unless (process-live-p proc)
    (when (and (eq (process-status proc) 'exit)
               (not (zerop (process-exit-status proc))))
      (message "sgn: %s could not play the voice note (exit %d)"
               (car (process-command proc)) (process-exit-status proc)))))

(provide 'sgn-media)
;;; sgn-media.el ends here
