;;; sgn-link.el --- Link signal-cli as a Signal device  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Pablo Stafforini

;; Author: Pablo Stafforini <pablo@stafforini.com>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Link signal-cli to a Signal account as a secondary device, from
;; Emacs.  Signal removes linked devices that stay offline for a long
;; time; `sgn-link' is how to recover.  It shows the linking QR code,
;; restarts sgn once the phone has scanned it, and offers to fill the
;; history gap from Signal Desktop.

;;; Code:

(require 'cl-lib)

(declare-function sgn--log "sgn")
(declare-function sgn-start "sgn")
(declare-function sgn-stop "sgn")
(declare-function sgn-import-desktop-available-p "sgn-import")
(declare-function sgn-import-from-desktop "sgn-import")

(defvar sgn-account)
(defvar sgn-cli-program)

;;;; Configuration

(defcustom sgn-link-device-name "Emacs (sgn)"
  "Device name shown in the phone's list of linked devices."
  :type 'string
  :group 'sgn)

(defcustom sgn-link-qrencode-program "qrencode"
  "Path to the qrencode executable used to draw the linking QR code.
Without it, `sgn-link' shows the linking URI as text only."
  :type 'file
  :group 'sgn)

(defconst sgn-link--buffer-name "*sgn link*"
  "Name of the buffer showing the linking QR code.")

(defconst sgn-link--process-name "sgn-link"
  "Name of the signal-cli linking process.")

(defvar sgn-link--output ""
  "Output received so far from the signal-cli linking process.")

;;;; Command

;;;###autoload
(defun sgn-link ()
  "Link signal-cli to your Signal account and restart sgn.
Show a QR code to scan from the phone under Settings > Linked
devices.  Once linking succeeds, start sgn and offer to import
the messages missed while unlinked from Signal Desktop."
  (interactive)
  (require 'sgn)
  (unless (executable-find sgn-cli-program)
    (user-error "The signal-cli executable `%s' was not found" sgn-cli-program))
  (when (get-process sgn-link--process-name)
    (delete-process sgn-link--process-name))
  ;; signal-cli locks the account while linking, so sgn must not
  ;; start it again until linking is over.
  (sgn-stop)
  (setq sgn-link--output "")
  (sgn-link--show "Waiting for signal-cli to produce a linking code...")
  (let ((proc (make-process
               :name sgn-link--process-name
               :command (list sgn-cli-program "link" "-n" sgn-link-device-name)
               :connection-type 'pipe
               :filter #'sgn-link--filter
               :sentinel #'sgn-link--sentinel
               :coding 'utf-8-unix)))
    (set-process-query-on-exit-flag proc nil)))

(defun sgn-link-in-progress-p ()
  "Return non-nil while `sgn-link' is linking."
  (process-live-p (get-process sgn-link--process-name)))

(defun sgn-link--kill-process ()
  "Stop linking when the linking buffer is killed."
  (when-let* ((proc (get-process sgn-link--process-name)))
    (set-process-sentinel proc #'ignore)
    (delete-process proc)))

(defun sgn-link--filter (_proc string)
  "Accumulate STRING and show the QR code once the linking URI arrives."
  (let ((had-uri (sgn-link--uri)))
    (setq sgn-link--output (concat sgn-link--output string))
    (when (and (not had-uri) (sgn-link--uri))
      (sgn-link--show-qr (sgn-link--uri)))))

(defun sgn-link--uri ()
  "Return the linking URI printed by signal-cli, or nil.
Only a complete line counts, since output can arrive in pieces."
  (when (string-match "\\(sgnl://[^[:space:]]+\\)\n" sgn-link--output)
    (match-string 1 sgn-link--output)))

(defun sgn-link--sentinel (proc event)
  "Handle the end of the linking PROC, described by EVENT."
  (sgn--log "sgn-link: %s" (string-trim event))
  (unless (process-live-p proc)
    (if (and (eq (process-status proc) 'exit)
             (zerop (process-exit-status proc)))
        ;; Leave the sentinel before asking questions.
        (run-at-time 0 nil #'sgn-link--finish)
      (sgn-link--show
       (format "Linking failed:\n\n%s\n\nRun M-x sgn-link to try again."
               (sgn-link--error-output))))))

(defun sgn-link--error-output ()
  "Return the linking output other than the URI."
  (let ((output (string-join
                 (cl-remove-if (lambda (line) (string-prefix-p "sgnl://" line))
                               (split-string sgn-link--output "\n" t
                                             "[[:space:]]+"))
                 "\n")))
    (if (string-empty-p output) "signal-cli exited with an error" output)))

(defun sgn-link--finish ()
  "Restart sgn after a successful link and offer to fill the history gap."
  (when-let* ((buf (get-buffer sgn-link--buffer-name)))
    (kill-buffer buf))
  (sgn-link--check-account)
  (sgn-start)
  (message "sgn: device linked")
  (when (and (sgn-import-desktop-available-p)
             (y-or-n-p "Import messages missed while unlinked from Signal Desktop? "))
    (sgn-import-from-desktop)))

(defun sgn-link--check-account ()
  "Warn if the linked number differs from `sgn-account'."
  (when (and (string-match "Associated with: \\(\\+[0-9]+\\)" sgn-link--output)
             (not (equal (match-string 1 sgn-link--output) sgn-account)))
    (display-warning
     'sgn (format "Linked %s, but `sgn-account' is %s"
                  (match-string 1 sgn-link--output) sgn-account))))

;;;; Display

(defun sgn-link--show (text)
  "Display the linking buffer with TEXT."
  (with-current-buffer (get-buffer-create sgn-link--buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text "\n"))
    (special-mode)
    (add-hook 'kill-buffer-hook #'sgn-link--kill-process nil t))
  (pop-to-buffer sgn-link--buffer-name))

(defun sgn-link--show-qr (uri)
  "Display URI as a QR code with scanning instructions."
  (sgn-link--show
   "On your phone, open Signal > Settings > Linked devices > Link new device,
and scan this code.  It expires after about two minutes.\n")
  (with-current-buffer sgn-link--buffer-name
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (sgn-link--insert-qr uri))))

(defun sgn-link--insert-qr (uri)
  "Insert URI as a QR code, or as text when qrencode is unavailable."
  (cond
   ((not (executable-find sgn-link-qrencode-program))
    (insert (format "Install qrencode to see the QR code here.  Linking URI:\n\n%s\n"
                    uri)))
   ((display-images-p)
    (insert-image (create-image (sgn-link--qrencode uri "PNG") 'png t)))
   (t
    (insert (decode-coding-string (sgn-link--qrencode uri "UTF8") 'utf-8)))))

(defun sgn-link--qrencode (uri type)
  "Return URI encoded by qrencode as TYPE, as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'binary)
          (status (call-process sgn-link-qrencode-program nil t nil
                                "-t" type "-s" "8" "-o" "-" uri)))
      (unless (eql status 0)
        (error "The qrencode program failed with status %s" status)))
    (buffer-string)))

(provide 'sgn-link)
;;; sgn-link.el ends here
