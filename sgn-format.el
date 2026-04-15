;;; sgn-format.el --- Text formatting for sgn  -*- lexical-binding: t; -*-

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

;; Text formatting for sgn, the Emacs Signal client.  Handles two
;; concerns:
;;
;; 1. Rendering incoming styles — Signal sends text style ranges as
;;    JSON objects with `start', `length', and `style' keys.  This
;;    module applies the corresponding Emacs faces to rendered text.
;;
;; 2. Composing outgoing markup — users type lightweight markup
;;    (*bold*, _italic_, ~strikethrough~, `monospace`, ||spoiler||)
;;    in the input area, and this module parses it into plain text
;;    plus Signal style ranges for sending.

;;; Code:

(require 'cl-lib)

;;;; Faces

(defface sgn-spoiler-face
  '((((background light)) :foreground "#333333" :background "#333333")
    (((background dark)) :foreground "#cccccc" :background "#cccccc"))
  "Face for spoiler text (concealed — foreground matches background)."
  :group 'sgn)

(defface sgn-spoiler-revealed-face
  '((t :inherit default :background "#e0e0e0"))
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

;;;; Constants

(defconst sgn-format--style-face-alist
  '(("BOLD" . bold)
    ("ITALIC" . italic)
    ("STRIKETHROUGH" . sgn-strikethrough-face)
    ("MONOSPACE" . sgn-monospace-face)
    ("SPOILER" . sgn-spoiler-face))
  "Mapping from Signal style names to Emacs faces.")

(defconst sgn-format--markup-rules
  '(("||" . "SPOILER")
    ("*"  . "BOLD")
    ("_"  . "ITALIC")
    ("~"  . "STRIKETHROUGH")
    ("`"  . "MONOSPACE"))
  "Alist mapping markup delimiters to Signal style names.
The two-character delimiter || must come first so the parser
checks it before the single-character delimiters.")

;;;; Rendering incoming styles

(defun sgn-format-apply-styles (text styles-json)
  "Apply Signal text styles to TEXT string.
STYLES-JSON is a JSON string encoding an array of style objects,
each with `start', `length', and `style' keys.
Return a propertized copy of TEXT with appropriate faces applied.
If STYLES-JSON is nil or empty, return TEXT unchanged.
Styles may overlap; all applicable faces are combined."
  (if (or (null styles-json)
          (string-empty-p styles-json))
      text
    (let* ((json-array-type 'list)
           (json-object-type 'alist)
           (styles (condition-case nil
                       (json-read-from-string styles-json)
                     (error nil))))
      (if (null styles)
          text
        (let ((result (copy-sequence text)))
          (dolist (style-obj styles)
            (sgn-format--apply-one-style result style-obj))
          result)))))

(defun sgn-format--apply-one-style (text style-obj)
  "Apply a single STYLE-OBJ to propertized TEXT in place.
STYLE-OBJ is an alist with `start', `length', and `style' keys.
Adds the corresponding face and, for spoilers, also sets the
`sgn-spoiler' text property."
  (let* ((start (alist-get 'start style-obj))
         (length (alist-get 'length style-obj))
         (style-name (alist-get 'style style-obj))
         (face (cdr (assoc style-name sgn-format--style-face-alist)))
         (end (min (+ start length) (length text))))
    (when (and face (< start (length text)))
      (add-face-text-property start end face nil text)
      (when (equal style-name "SPOILER")
        (put-text-property start end 'sgn-spoiler t text)))))

(defun sgn-format-reveal-spoiler-at-point ()
  "Reveal the spoiler text at point by changing its face.
Interactive command — bound in chat mode.  Finds the contiguous
region of the `sgn-spoiler' text property at point and replaces
`sgn-spoiler-face' with `sgn-spoiler-revealed-face'."
  (interactive)
  (unless (get-text-property (point) 'sgn-spoiler)
    (user-error "No spoiler at point"))
  (let* ((beg (sgn-format--property-region-start (point) 'sgn-spoiler))
         (end (sgn-format--property-region-end (point) 'sgn-spoiler))
         (inhibit-read-only t))
    (sgn-format--swap-face beg end 'sgn-spoiler-face 'sgn-spoiler-revealed-face)))

(defun sgn-format--property-region-start (pos prop)
  "Return the start of the contiguous region with PROP around POS."
  (if (and (> pos (point-min))
           (get-text-property (1- pos) prop))
      (previous-single-property-change pos prop)
    pos))

(defun sgn-format--property-region-end (pos prop)
  "Return the end of the contiguous region with PROP around POS."
  (if (get-text-property pos prop)
      (or (next-single-property-change pos prop) (point-max))
    pos))

(defun sgn-format--swap-face (beg end old-face new-face)
  "In the region BEG to END, replace OLD-FACE with NEW-FACE.
Walks the region character by character, replacing OLD-FACE in
the `face' property with NEW-FACE while preserving other faces."
  (let ((pos beg))
    (while (< pos end)
      (let* ((current (get-text-property pos 'face))
             (replaced (sgn-format--replace-face-in-spec current old-face new-face)))
        (put-text-property pos (1+ pos) 'face replaced))
      (setq pos (1+ pos)))))

(defun sgn-format--replace-face-in-spec (spec old-face new-face)
  "Replace OLD-FACE with NEW-FACE in face SPEC.
SPEC may be a single face, a list of faces, or nil."
  (cond
   ((null spec) new-face)
   ((eq spec old-face) new-face)
   ((listp spec)
    (mapcar (lambda (f) (if (eq f old-face) new-face f)) spec))
   (t spec)))

;;;; Composing outgoing markup

(defun sgn-format-parse-markup (text)
  "Parse lightweight markup in TEXT.
Return a plist (:text PLAIN-TEXT :styles STYLES-ALIST-LIST).
PLAIN-TEXT has markup delimiters removed.
STYLES-ALIST-LIST is a list of alists, each with keys `start',
`length', `style'.

Markup rules:
- *bold* → BOLD (delimiter: single *)
- _italic_ → ITALIC (delimiter: single _)
- ~strikethrough~ → STRIKETHROUGH (delimiter: single ~)
- `monospace` → MONOSPACE (delimiter: single `)
- ||spoiler|| → SPOILER (delimiter: double ||)

Delimiters must not be preceded/followed by whitespace on the
inner side.  Nested markup is supported (styles may overlap).
Unmatched delimiters are left as literal characters."
  (let ((ranges nil)
        (all-ranges nil))
    ;; Collect raw ranges for each delimiter type independently.
    ;; Each range is recorded as positions in the ORIGINAL text.
    (dolist (rule sgn-format--markup-rules)
      (let ((delim (car rule))
            (style (cdr rule)))
        (setq ranges (sgn-format--find-pairs text delim style))
        (setq all-ranges (nconc all-ranges ranges))))
    ;; Sort ranges by their opening delimiter position (earliest first),
    ;; breaking ties by putting longer delimiters first.
    (setq all-ranges
          (sort all-ranges
                (lambda (a b)
                  (let ((a-open (plist-get a :open-start))
                        (b-open (plist-get b :open-start)))
                    (or (< a-open b-open)
                        (and (= a-open b-open)
                             (> (plist-get a :delim-len)
                                (plist-get b :delim-len))))))))
    ;; Build the output: remove delimiters, compute final positions.
    (sgn-format--build-output text all-ranges)))

(defun sgn-format--find-pairs (text delim style)
  "Find all matched pairs of DELIM in TEXT for STYLE.
Return a list of plists, each with :open-start, :open-end,
:close-start, :close-end, :delim-len, and :style.
Scans left-to-right, greedily matching opening and closing
delimiters.  Skips escaped delimiters (preceded by backslash).
The inner side of a delimiter must not be whitespace."
  (let ((dlen (length delim))
        (tlen (length text))
        (pos 0)
        (result nil)
        (open-pos nil))
    (while (< pos tlen)
      (cond
       ;; Skip escaped delimiters.
       ((and (eq (aref text pos) ?\\)
             (< (+ pos 1) tlen)
             (<= (+ pos 1 dlen) tlen)
             (string= (substring text (+ pos 1) (+ pos 1 dlen)) delim))
        (setq pos (+ pos 1 dlen)))
       ;; Check for delimiter match at this position.
       ((and (<= (+ pos dlen) tlen)
             (string= (substring text pos (+ pos dlen)) delim))
        (if open-pos
            ;; Closing delimiter: inner side is character before this pos.
            ;; The character just before the closing delimiter must not be whitespace.
            (let ((before-close (aref text (1- pos))))
              (if (memq before-close '(?\s ?\t ?\n))
                  ;; Not a valid close; treat as new opening.
                  (setq open-pos pos
                        pos (+ pos dlen))
                ;; Valid close.
                (push (list :open-start open-pos
                            :open-end (+ open-pos dlen)
                            :close-start pos
                            :close-end (+ pos dlen)
                            :delim-len dlen
                            :style style)
                      result)
                (setq open-pos nil
                      pos (+ pos dlen))))
          ;; Opening delimiter: inner side is character after the delimiter.
          ;; The character just after the opening delimiter must not be whitespace.
          (if (and (< (+ pos dlen) tlen)
                   (not (memq (aref text (+ pos dlen)) '(?\s ?\t ?\n))))
              (setq open-pos pos
                    pos (+ pos dlen))
            ;; Not valid opening; skip past it.
            (setq pos (+ pos dlen)))))
       (t (setq pos (1+ pos)))))
    (nreverse result)))

(defun sgn-format--build-output (text ranges)
  "Remove delimiters from TEXT based on RANGES and compute final style positions.
RANGES is a sorted list of plists describing matched delimiter pairs.
Return a plist (:text PLAIN-TEXT :styles STYLES-ALIST-LIST)."
  (if (null ranges)
      (let ((cleaned (sgn-format--unescape-delimiters text)))
        (list :text cleaned :styles nil))
    ;; Collect all delimiter intervals to remove, sorted by position.
    (let* ((removals (sgn-format--collect-removals ranges))
           (cleaned (sgn-format--remove-intervals text removals))
           (styles (sgn-format--compute-styles ranges removals)))
      ;; Unescape any remaining backslash-escaped delimiters.
      (let* ((unescape-result (sgn-format--unescape-with-offsets cleaned))
             (final-text (car unescape-result))
             (escape-offsets (cdr unescape-result))
             (final-styles (sgn-format--adjust-styles-for-escapes
                            styles escape-offsets)))
        (list :text final-text :styles final-styles)))))

(defun sgn-format--collect-removals (ranges)
  "Collect all delimiter intervals to remove from RANGES.
Return a sorted list of (start . end) pairs."
  (let ((removals nil))
    (dolist (r ranges)
      (push (cons (plist-get r :open-start) (plist-get r :open-end)) removals)
      (push (cons (plist-get r :close-start) (plist-get r :close-end)) removals))
    (sort removals (lambda (a b) (< (car a) (car b))))))

(defun sgn-format--remove-intervals (text intervals)
  "Remove INTERVALS (list of (start . end) pairs) from TEXT.
INTERVALS must be sorted by start position and non-overlapping."
  (let ((parts nil)
        (prev-end 0))
    (dolist (iv intervals)
      (when (> (car iv) prev-end)
        (push (substring text prev-end (car iv)) parts))
      (setq prev-end (cdr iv)))
    (when (< prev-end (length text))
      (push (substring text prev-end) parts))
    (apply #'concat (nreverse parts))))

(defun sgn-format--compute-styles (ranges removals)
  "Compute final style alists from RANGES after delimiter REMOVALS.
Each removal shifts subsequent positions.  Return a list of alists
with `start', `length', and `style' keys."
  (let ((styles nil))
    (dolist (r ranges)
      (let* ((content-start-orig (plist-get r :open-end))
             (content-end-orig (plist-get r :close-start))
             (adj-start (sgn-format--adjusted-pos content-start-orig removals))
             (adj-end (sgn-format--adjusted-pos content-end-orig removals))
             (len (- adj-end adj-start)))
        (when (> len 0)
          (push `((start . ,adj-start)
                  (length . ,len)
                  (style . ,(plist-get r :style)))
                styles))))
    (nreverse styles)))

(defun sgn-format--adjusted-pos (pos removals)
  "Adjust POS by subtracting characters removed before it in REMOVALS.
REMOVALS is a sorted list of (start . end) pairs."
  (let ((offset 0))
    (dolist (iv removals)
      (cond
       ((<= (cdr iv) pos)
        (setq offset (+ offset (- (cdr iv) (car iv)))))
       ((< (car iv) pos)
        ;; pos is inside a removal — shouldn't happen for content boundaries,
        ;; but handle gracefully.
        (setq offset (+ offset (- pos (car iv)))))))
    (- pos offset)))

(defun sgn-format--unescape-delimiters (text)
  "Remove backslash escapes from delimiter characters in TEXT.
E.g., \\* becomes *, \\| becomes |."
  (let ((result (make-string (length text) 0))
        (ri 0)
        (ti 0)
        (tlen (length text)))
    (while (< ti tlen)
      (if (and (eq (aref text ti) ?\\)
               (< (1+ ti) tlen)
               (sgn-format--delimiter-char-p (aref text (1+ ti))))
          (progn
            (aset result ri (aref text (1+ ti)))
            (setq ri (1+ ri))
            (setq ti (+ ti 2)))
        (aset result ri (aref text ti))
        (setq ri (1+ ri))
        (setq ti (1+ ti))))
    (substring result 0 ri)))

(defun sgn-format--unescape-with-offsets (text)
  "Remove backslash escapes from TEXT and track position adjustments.
Return (CLEANED-TEXT . OFFSETS) where OFFSETS is a list of
positions (in the intermediate text) where a backslash was removed."
  (let ((result (make-string (length text) 0))
        (ri 0)
        (ti 0)
        (tlen (length text))
        (offsets nil))
    (while (< ti tlen)
      (if (and (eq (aref text ti) ?\\)
               (< (1+ ti) tlen)
               (sgn-format--delimiter-char-p (aref text (1+ ti))))
          (progn
            (push ti offsets)
            (aset result ri (aref text (1+ ti)))
            (setq ri (1+ ri))
            (setq ti (+ ti 2)))
        (aset result ri (aref text ti))
        (setq ri (1+ ri))
        (setq ti (1+ ti))))
    (cons (substring result 0 ri) (nreverse offsets))))

(defun sgn-format--adjust-styles-for-escapes (styles escape-offsets)
  "Adjust STYLES start/length values for backslash removals at ESCAPE-OFFSETS.
Each offset in ESCAPE-OFFSETS is a position in the intermediate text
where a single backslash character was removed."
  (if (null escape-offsets)
      styles
    (mapcar
     (lambda (style)
       (let* ((start (alist-get 'start style))
              (len (alist-get 'length style))
              (end (+ start len))
              (start-adj (sgn-format--count-before start escape-offsets))
              (end-adj (sgn-format--count-before end escape-offsets))
              (new-start (- start start-adj))
              (new-end (- end end-adj)))
         `((start . ,new-start)
           (length . ,(- new-end new-start))
           (style . ,(alist-get 'style style)))))
     styles)))

(defun sgn-format--count-before (pos offsets)
  "Count how many values in OFFSETS are strictly less than POS.
OFFSETS must be sorted in ascending order."
  (let ((count 0))
    (dolist (o offsets)
      (if (< o pos)
          (setq count (1+ count))
        (cl-return count)))
    count))

(defun sgn-format--delimiter-char-p (char)
  "Return non-nil if CHAR is a character used in markup delimiters."
  (memq char '(?* ?_ ?~ ?` ?|)))

;;;; JSON serialization

(defun sgn-format-styles-to-json (styles)
  "Convert STYLES (list of alists with start/length/style) to a JSON string.
Return a JSON array string, or nil if STYLES is empty."
  (if (null styles)
      nil
    (json-encode (apply #'vector styles))))

(provide 'sgn-format)
;;; sgn-format.el ends here
