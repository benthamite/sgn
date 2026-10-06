;;; sgn-format.el --- Text formatting for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Signal text formatting: rendering incoming style ranges and
;; mentions, and parsing lightweight markup typed in Emacs into style
;; ranges.
;;
;; Signal measures style and mention ranges in UTF-16 code units, so
;; every character outside the Basic Multilingual Plane, such as most
;; emoji, counts as two.  Ranges are kept in those units everywhere
;; outside this file and converted to Emacs character positions only
;; here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(declare-function sgn-contacts-get-name "sgn-contacts")
(declare-function sgn-store-self-p "sgn-store")

;;;; Faces

(defface sgn-spoiler-face
  '((((background light)) :foreground "#333333" :background "#333333")
    (((background dark)) :foreground "#cccccc" :background "#cccccc"))
  "Face for spoiler text (concealed — foreground matches background)."
  :group 'sgn)

(defface sgn-spoiler-revealed-face
  '((t :inherit highlight))
  "Face for revealed spoiler text."
  :group 'sgn)

(defface sgn-strikethrough-face
  '((t :strike-through t))
  "Face for strikethrough text."
  :group 'sgn)

(defface sgn-monospace-face
  '((t :inherit fixed-pitch))
  "Face for monospace text."
  :group 'sgn)

(defface sgn-mention-face
  '((t :inherit link :underline nil))
  "Face for mentions of other people."
  :group 'sgn)

(defface sgn-mention-self-face
  '((t :inherit match))
  "Face for mentions of you."
  :group 'sgn)

;;;; UTF-16 offsets

(defun sgn-format--utf16-width (char)
  "Return the number of UTF-16 code units CHAR takes."
  (if (> char #xFFFF) 2 1))

(defun sgn-format-utf16-to-char (string offset)
  "Return the character position in STRING at UTF-16 OFFSET.
An OFFSET beyond the end of STRING gives its length."
  (let ((units 0) (pos 0) (len (length string)))
    (while (and (< pos len) (< units offset))
      (cl-incf units (sgn-format--utf16-width (aref string pos)))
      (cl-incf pos))
    pos))

(defun sgn-format-char-to-utf16 (string pos)
  "Return the UTF-16 offset of character position POS in STRING."
  (cl-loop for i below (min pos (length string))
           sum (sgn-format--utf16-width (aref string i))))

;;;; Rendering

(defconst sgn-format--style-faces
  '(("BOLD" . bold)
    ("ITALIC" . italic)
    ("STRIKETHROUGH" . sgn-strikethrough-face)
    ("MONOSPACE" . sgn-monospace-face)
    ("SPOILER" . sgn-spoiler-face))
  "Mapping from Signal style names to faces.")

(defun sgn-format-read-ranges (json)
  "Return the ranges stored in JSON as (NAME START LENGTH) lists.
JSON holds a list of such lists; older databases hold objects with
`style' or `uuid', `start' and `length' keys instead."
  (when (and json (not (string-empty-p json)))
    (mapcar (lambda (range)
              (if (and (consp range) (consp (car range)))
                  (list (or (alist-get 'style range) (alist-get 'uuid range))
                        (alist-get 'start range) (alist-get 'length range))
                range))
            (json-parse-string json :object-type 'alist :array-type 'list
                               :null-object nil :false-object nil))))

(defun sgn-format-ranges-to-json (ranges)
  "Return RANGES, a list of (NAME START LENGTH), as JSON, or nil if empty."
  (when ranges
    (json-encode (vconcat (mapcar #'vconcat ranges)))))

(defun sgn-format-render (body styles mentions &optional revealed)
  "Return BODY with STYLES and MENTIONS applied.
STYLES is a list of (STYLE START LENGTH) and MENTIONS of (PERSON
START LENGTH), in UTF-16 units.  Mentions are replaced by @NAME.
Spoilers are concealed unless REVEALED is non-nil."
  (let ((text (copy-sequence body)))
    (pcase-dolist (`(,style ,start ,length) styles)
      (sgn-format--apply-style text style start length revealed))
    (dolist (mention (sort (copy-sequence mentions)
                           (lambda (a b) (> (nth 1 a) (nth 1 b)))))
      (setq text (sgn-format--apply-mention text body mention)))
    text))

(defun sgn-format--apply-style (text style start length revealed)
  "Apply STYLE to the range START, LENGTH of TEXT, in UTF-16 units.
A spoiler is concealed unless REVEALED is non-nil."
  (let* ((beg (sgn-format-utf16-to-char text start))
         (end (sgn-format-utf16-to-char text (+ start length)))
         (face (if (and revealed (equal style "SPOILER"))
                   'sgn-spoiler-revealed-face
                 (cdr (assoc style sgn-format--style-faces)))))
    (when (and face (< beg end))
      (add-face-text-property beg end face nil text)
      (when (equal style "SPOILER")
        (put-text-property beg end 'sgn-spoiler t text)))))

(defun sgn-format--apply-mention (text body mention)
  "Return TEXT with MENTION replaced by the mentioned person's name.
MENTION is (PERSON START LENGTH) in UTF-16 units of BODY, which
has the same characters as TEXT."
  (pcase-let* ((`(,person ,start ,length) mention)
               (beg (sgn-format-utf16-to-char body start))
               (end (sgn-format-utf16-to-char body (+ start length))))
    (if (and person (< beg end) (<= end (length text)))
        (concat (substring text 0 beg)
                (propertize (concat "@" (sgn-contacts-get-name person))
                            'face (if (sgn-store-self-p person)
                                      'sgn-mention-self-face
                                    'sgn-mention-face))
                (substring text end))
      text)))

;;;; Parsing markup

(defconst sgn-format--markup
  '(("`" . "MONOSPACE")
    ("||" . "SPOILER")
    ("*" . "BOLD")
    ("_" . "ITALIC")
    ("~" . "STRIKETHROUGH"))
  "Markup delimiters and their styles, in the order they are parsed.
Text inside monospace spans is not parsed for other markup.")

(defun sgn-format-parse-markup (text)
  "Parse lightweight markup in TEXT.
Return a plist (:text PLAIN :styles STYLES), where PLAIN is TEXT
without the markup and STYLES a list of (STYLE START LENGTH) in
UTF-16 units of PLAIN.

`*bold*', `_italic_', `~strikethrough~', `` `monospace` '' and
`||spoiler||' are recognized.  An opening delimiter must start a
word and be followed by a non-blank; a closing one must follow a
non-blank and end a word, so `snake_case' and `2*3*4' are left
alone.  URLs are never parsed.  A delimiter preceded by a
backslash is literal, and the backslash is dropped."
  (let ((protected (sgn-format--url-mask text))
        (pairs nil))
    (dolist (rule sgn-format--markup)
      (setq pairs (nconc pairs (sgn-format--find-pairs
                                text (car rule) (cdr rule) protected))))
    (sgn-format--strip-markup text pairs)))

(defun sgn-format--url-mask (text)
  "Return a bool-vector marking the characters of URLs in TEXT."
  (let ((mask (make-bool-vector (length text) nil))
        (start 0))
    (while (string-match "\\(?:https?://\\|www\\.\\)[^[:space:]]+" text start)
      (cl-loop for i from (match-beginning 0) below (match-end 0)
               do (aset mask i t))
      (setq start (match-end 0)))
    mask))

(defun sgn-format--word-char-p (char)
  "Return non-nil if CHAR is a letter or digit."
  (and char (string-match-p "\\`[[:alnum:]]\\'" (string char))))

(defun sgn-format--blank-p (char)
  "Return non-nil if CHAR is nil or whitespace."
  (or (null char) (memq char '(?\s ?\t ?\n ?\r))))

(defun sgn-format--char (text pos)
  "Return the character of TEXT at POS, or nil outside it."
  (and (>= pos 0) (< pos (length text)) (aref text pos)))

(defun sgn-format--delimiter-at-p (text pos delim protected)
  "Return non-nil if DELIM occurs in TEXT at POS outside PROTECTED.
A delimiter that is part of a longer run of its character, as in
`**', does not count."
  (let ((len (length delim))
        (char (aref delim 0)))
    (and (<= (+ pos len) (length text))
         (string= (substring text pos (+ pos len)) delim)
         (cl-loop for i from pos below (+ pos len) never (aref protected i))
         (not (eq (sgn-format--char text (1- pos)) char))
         (not (eq (sgn-format--char text (+ pos len)) char)))))

(defun sgn-format--before (text pos)
  "Return the character before POS in TEXT, skipping an escaping backslash."
  (let ((prev (sgn-format--char text (1- pos))))
    (if (eq prev ?\\) (sgn-format--char text (- pos 2)) prev)))

(defun sgn-format--opener-p (text pos delim protected)
  "Return non-nil if DELIM at POS in TEXT can open a styled span."
  (and (sgn-format--delimiter-at-p text pos delim protected)
       (not (sgn-format--word-char-p (sgn-format--before text pos)))
       (not (sgn-format--blank-p
             (sgn-format--char text (+ pos (length delim)))))))

(defun sgn-format--closer-p (text pos delim protected)
  "Return non-nil if DELIM at POS in TEXT can close a styled span."
  (and (sgn-format--delimiter-at-p text pos delim protected)
       (not (sgn-format--blank-p (sgn-format--before text pos)))
       (not (sgn-format--word-char-p
             (sgn-format--char text (+ pos (length delim)))))))

(defun sgn-format--find-pairs (text delim style protected)
  "Return the matched DELIM pairs in TEXT, styled STYLE.
Characters marked in PROTECTED are skipped; the delimiters of each
pair found, and for monospace its content, are marked in turn.
Each pair is a plist with :open, :close, :len, :style and
:escaped, which is non-nil if either delimiter is backslashed."
  (let ((len (length delim))
        (pos 0)
        (pairs nil))
    (while (< pos (length text))
      (if-let* (((sgn-format--opener-p text pos delim protected))
                (close (cl-loop for j from (+ pos len 1) below (length text)
                                when (sgn-format--closer-p text j delim protected)
                                return j)))
          (progn
            (push (list :open pos :close close :len len :style style
                        :escaped (or (eq (sgn-format--char text (1- pos)) ?\\)
                                     (eq (sgn-format--char text (1- close)) ?\\)))
                  pairs)
            (cl-loop for i from pos below (+ close len)
                     when (or (equal style "MONOSPACE")
                              (< i (+ pos len)) (>= i close))
                     do (aset protected i t))
            (setq pos (+ close len)))
        (cl-incf pos)))
    (nreverse pairs)))

(defun sgn-format--strip-markup (text pairs)
  "Remove the markup of PAIRS from TEXT and compute the styles.
An escaped pair loses its backslashes but keeps its delimiters."
  (let ((removals nil)
        (spans nil))
    (dolist (pair pairs)
      (let ((open (plist-get pair :open))
            (close (plist-get pair :close))
            (len (plist-get pair :len)))
        (if (plist-get pair :escaped)
            (dolist (pos (list open close))
              (when (eq (sgn-format--char text (1- pos)) ?\\)
                (push (cons (1- pos) pos) removals)))
          (push (cons open (+ open len)) removals)
          (push (cons close (+ close len)) removals)
          (push (list (plist-get pair :style) (+ open len) close) spans))))
    (setq removals (sort removals (lambda (a b) (< (car a) (car b)))))
    (let ((plain (sgn-format--remove-intervals text removals)))
      (list :text plain
            :styles
            (sort (mapcar
                   (pcase-lambda (`(,style ,beg ,end))
                     (let ((b (sgn-format--shift beg removals))
                           (e (sgn-format--shift end removals)))
                       (list style (sgn-format-char-to-utf16 plain b)
                             (- (sgn-format-char-to-utf16 plain e)
                                (sgn-format-char-to-utf16 plain b)))))
                   spans)
                  (lambda (a b) (< (nth 1 a) (nth 1 b))))))))

(defun sgn-format--remove-intervals (text intervals)
  "Return TEXT without INTERVALS, sorted (START . END) pairs."
  (let ((parts nil) (prev 0))
    (pcase-dolist (`(,start . ,end) intervals)
      (push (substring text prev start) parts)
      (setq prev end))
    (push (substring text prev) parts)
    (apply #'concat (nreverse parts))))

(defun sgn-format--shift (pos removals)
  "Return POS after the characters of REMOVALS before it are removed."
  (- pos (cl-loop for (start . end) in removals
                  when (<= end pos) sum (- end start))))

;;;; Composing markup from styles

(defun sgn-format-to-markup (body styles)
  "Return BODY with STYLES written back as markup, for editing.
STYLES is a list of (STYLE START LENGTH) in UTF-16 units.
Delimiter characters that would otherwise be read as markup are
escaped.  Overlapping styles are written innermost last."
  (let ((inserts nil))
    (pcase-dolist (`(,style ,start ,length) styles)
      (when-let* ((delim (car (rassoc style sgn-format--markup))))
        (push (cons (sgn-format-utf16-to-char body start) delim) inserts)
        (push (cons (sgn-format-utf16-to-char body (+ start length)) delim)
              inserts)))
    (let ((escaped (sgn-format--escape-literal-markup body))
          (result nil) (prev 0))
      (pcase-dolist (`(,pos . ,delim)
                     (sort inserts (lambda (a b) (< (car a) (car b)))))
        (push (sgn-format--escaped-substring body escaped prev pos) result)
        (push delim result)
        (setq prev pos))
      (push (sgn-format--escaped-substring body escaped prev (length body)) result)
      (apply #'concat (nreverse result)))))

(defun sgn-format--escape-literal-markup (body)
  "Return the positions in BODY of delimiters that would parse as markup."
  (let ((protected (sgn-format--url-mask body))
        (positions nil))
    (dolist (rule sgn-format--markup)
      (dolist (pair (sgn-format--find-pairs body (car rule) (cdr rule)
                                            (copy-sequence protected)))
        (push (plist-get pair :open) positions)
        (push (plist-get pair :close) positions)))
    positions))

(defun sgn-format--escaped-substring (body escaped start end)
  "Return BODY from START to END, with a backslash before ESCAPED positions."
  (let ((parts nil))
    (cl-loop for i from start below end
             do (when (memq i escaped) (push "\\" parts))
             (push (string (aref body i)) parts))
    (apply #'concat (nreverse parts))))

;;;; Previews

(defun sgn-format-preview (msg)
  "Return MSG's text on one line, with mentions and spoilers concealed.
Return nil if MSG has no text."
  (when-let* ((body (plist-get msg :body)))
    (unless (string-empty-p body)
      (let ((text (sgn-format-render
                   body
                   (sgn-format-read-ranges (plist-get msg :styles-json))
                   (sgn-format-read-ranges (plist-get msg :mentions-json)))))
        (dotimes (i (length text))
          (when (get-text-property i 'sgn-spoiler text)
            (aset text i ?▒)))
        (replace-regexp-in-string "\n" " " (substring-no-properties text))))))

(provide 'sgn-format)
;;; sgn-format.el ends here
