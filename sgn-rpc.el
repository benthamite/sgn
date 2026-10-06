;;; sgn-rpc.el --- JSON-RPC process management for sgn  -*- lexical-binding: t; -*-

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

;; This module manages the signal-cli JSON-RPC subprocess for sgn: its
;; lifecycle, the framing of requests and responses, and the
;; bookkeeping of requests waiting for a reply.
;;
;; signal-cli runs as `signal-cli -a ACCOUNT jsonRpc' and talks over a
;; pipe.  Each request is recorded as an `sgn-rpc--request' until its
;; reply arrives.  A reply calls the request's callback; an error reply
;; calls its error callback, or reports the error.  When the process
;; stops, every pending request's error callback is called with an
;; `abandoned' error, since its reply can no longer arrive.
;;
;; Requests are never retried: a send whose reply was lost may still
;; have been delivered, and sending it again could duplicate it.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'subr-x)

(declare-function sgn--log "sgn")

;; Variables defined in sgn.el, referenced here.
(defvar sgn-account)
(defvar sgn-cli-program)

;;; Constants

(defconst sgn-rpc--process-name "signal-rpc"
  "Internal name for the signal-cli JSON-RPC process.")

(defconst sgn-rpc--stderr-buffer-name " *sgn-stderr*"
  "Name of the hidden buffer that collects signal-cli's stderr.")

(defconst sgn-rpc--max-partial-line-length (* 64 1024 1024)
  "Maximum length of an incomplete line before it is discarded.
Complete lines are never discarded; this only bounds the memory
used by a line signal-cli never terminates.")

;;; Configuration

(defvar sgn-rpc-receive-handler nil
  "Function called with the params of each \"receive\" notification.")

(defvar sgn-rpc-failure-change-hook nil
  "Hook run after `sgn-rpc--failure' is set or cleared.")

;;; Internal state

(cl-defstruct (sgn-rpc--request (:constructor sgn-rpc--request-create)
                                (:copier nil))
  "A JSON-RPC request waiting for its reply."
  (id nil :documentation "The request's JSON-RPC id.")
  (method nil :documentation "The JSON-RPC method name.")
  (callback nil :documentation "Function called with the result.")
  (error-callback nil :documentation "Function called with the error object."))

(defvar sgn-rpc--id-counter 0
  "Counter for JSON-RPC request IDs.")

(defvar sgn-rpc--pending (make-hash-table :test 'eql)
  "Map from request ID to its `sgn-rpc--request'.")

(defvar sgn-rpc--partial-line ""
  "Output received after the last newline from signal-cli.")

(defvar sgn-rpc--failure nil
  "Description of the last abnormal exit of signal-cli, or nil.
`sgn-rpc-start' clears it.")

;;; Lifecycle

(defun sgn-rpc-start ()
  "Start the signal-cli JSON-RPC subprocess.
Any previous process is stopped first.  The process talks over a
pipe: a pty's line discipline would truncate long requests."
  (unless sgn-cli-program
    (user-error "Variable `sgn-cli-program' is not set"))
  (unless (executable-find sgn-cli-program)
    (user-error "The signal-cli executable `%s' was not found" sgn-cli-program))
  (unless sgn-account
    (user-error "Variable `sgn-account' is not set"))
  (sgn-rpc-stop)
  (setq sgn-rpc--partial-line "")
  (sgn-rpc--set-failure nil)
  (let ((proc (make-process
               :name sgn-rpc--process-name
               :command (list sgn-cli-program "-a" sgn-account "jsonRpc")
               :connection-type 'pipe
               :stderr (sgn-rpc--fresh-stderr-buffer)
               :filter #'sgn-rpc--process-filter
               :sentinel #'sgn-rpc--process-sentinel
               :coding 'utf-8-unix
               :noquery t)))
    (sgn--log "signal-cli started (pid %s)." (process-id proc))))

(defun sgn-rpc--fresh-stderr-buffer ()
  "Return the stderr buffer, emptied."
  (let ((buf (get-buffer-create sgn-rpc--stderr-buffer-name)))
    (with-current-buffer buf
      (erase-buffer))
    buf))

(defun sgn-rpc-stop ()
  "Stop the signal-cli JSON-RPC subprocess and abandon pending requests."
  (when-let* ((proc (get-process sgn-rpc--process-name)))
    ;; Detach the sentinel first: a stop is deliberate, not a failure.
    (set-process-sentinel proc #'ignore)
    (delete-process proc)
    (sgn--log "signal-cli stopped."))
  (sgn-rpc--abandon-pending))

(defun sgn-rpc-alive-p ()
  "Return non-nil if the signal-cli JSON-RPC process is running."
  (when-let* ((proc (get-process sgn-rpc--process-name)))
    (process-live-p proc)))

(defun sgn-rpc--abandon-pending ()
  "Forget all pending requests, calling their error callbacks.
The error object passed has a non-nil `abandoned' entry, because
the request may or may not have taken effect."
  (let ((requests (hash-table-values sgn-rpc--pending)))
    (clrhash sgn-rpc--pending)
    (dolist (request requests)
      (when-let* ((errback (sgn-rpc--request-error-callback request)))
        (sgn-rpc--call-safely
         errback '((message . "signal-cli stopped before replying")
                   (abandoned . t)))))))

;;; Sending

(defun sgn-rpc-send (method params &optional callback error-callback)
  "Send a JSON-RPC request for METHOD with PARAMS.
CALLBACK, if non-nil, is called with the result.  ERROR-CALLBACK,
if non-nil, is called with the error object when signal-cli
reports an error or stops before replying; without one, errors
are logged and shown in the echo area.  Signal a `user-error' if
signal-cli is not running.  Return the request ID."
  (unless (sgn-rpc-alive-p)
    (user-error "%s" (or (sgn-rpc-failure-advice)
                         "sgn is not running; run M-x sgn-start")))
  (let* ((id (cl-incf sgn-rpc--id-counter))
         (json-str (json-encode `((jsonrpc . "2.0")
                                  (method . ,method)
                                  (params . ,params)
                                  (id . ,id)))))
    (puthash id (sgn-rpc--request-create :id id :method method
                                         :callback callback
                                         :error-callback error-callback)
             sgn-rpc--pending)
    (sgn--log "SEND: %s" json-str)
    (process-send-string sgn-rpc--process-name (concat json-str "\n"))
    id))

;;; Process filter and parsing

(defun sgn-rpc--process-filter (_proc string)
  "Parse complete lines of STRING received from signal-cli.
The text after the last newline is kept for the next call."
  (let* ((data (concat sgn-rpc--partial-line string))
         (lines (split-string data "\n")))
    (setq sgn-rpc--partial-line (car (last lines)))
    (when (> (length sgn-rpc--partial-line) sgn-rpc--max-partial-line-length)
      (sgn--log "WARNING: discarding an unterminated line of %d characters"
                (length sgn-rpc--partial-line))
      (setq sgn-rpc--partial-line ""))
    (dolist (line (butlast lines))
      (sgn-rpc--handle-line (string-trim line)))))

(defun sgn-rpc--handle-line (line)
  "Parse LINE from signal-cli's stdout and dispatch it."
  (cond
   ((string-empty-p line))
   ((not (string-prefix-p "{" line))
    (sgn--log "OUTPUT: %s" line))
   (t
    (sgn--log "RECV: %s" line)
    (let ((json (condition-case err
                    (sgn-rpc-parse-json line)
                  (json-parse-error
                   (sgn--log "JSON parse error: %S" err)
                   nil))))
      (when json
        (sgn-rpc--dispatch json))))))

(defun sgn-rpc-parse-json (string)
  "Parse the JSON in STRING the way sgn reads signal-cli output.
Objects become alists, arrays lists, and null and false nil."
  (json-parse-string string :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun sgn-rpc--dispatch (json)
  "Route the parsed JSON message to its handler.
Errors raised by handlers are logged and shown, and do not stop
the handling of later messages."
  (let ((id (alist-get 'id json)))
    (cond
     ((equal (alist-get 'method json) "receive")
      (when sgn-rpc-receive-handler
        (sgn-rpc--call-safely sgn-rpc-receive-handler
                              (alist-get 'params json))))
     ((not id))
     ((assq 'error json)
      (sgn-rpc--handle-error id (alist-get 'error json)))
     (t
      (sgn-rpc--handle-result id (alist-get 'result json))))))

(defun sgn-rpc--call-safely (function &rest args)
  "Call FUNCTION with ARGS, logging and showing any error it signals."
  (condition-case err
      (apply function args)
    (error
     (sgn--log "ERROR in %S: %S" function err)
     (message "sgn: internal error: %s" (error-message-string err)))))

(defun sgn-rpc--handle-result (id result)
  "Pass RESULT to the callback of the pending request ID."
  (when-let* ((request (gethash id sgn-rpc--pending)))
    (remhash id sgn-rpc--pending)
    (when-let* ((callback (sgn-rpc--request-callback request)))
      (sgn-rpc--call-safely callback result))))

(defun sgn-rpc--handle-error (id error-obj)
  "Pass ERROR-OBJ to the error callback of the pending request ID.
Without an error callback, log the error and show it."
  (let ((request (gethash id sgn-rpc--pending))
        (msg (alist-get 'message error-obj)))
    (remhash id sgn-rpc--pending)
    (sgn--log "RPC error [id=%s method=%s code=%s]: %s"
              id (and request (sgn-rpc--request-method request))
              (alist-get 'code error-obj) msg)
    (cond
     ((null request))
     ((sgn-rpc--request-error-callback request)
      (sgn-rpc--call-safely (sgn-rpc--request-error-callback request)
                            error-obj))
     (t
      (message "sgn: %s failed: %s" (sgn-rpc--request-method request) msg)))))

;;; Failure reporting

(defun sgn-rpc--process-sentinel (proc event)
  "Record the abnormal end of PROC, described by EVENT.
Pending requests are abandoned whenever the process ends."
  (sgn--log "Process event: %s" (string-trim event))
  (unless (process-live-p proc)
    (unless (and (eq (process-status proc) 'exit)
                 (zerop (process-exit-status proc)))
      (sgn-rpc--set-failure
       (format "signal-cli %s%s" (string-trim event)
               (if-let* ((output (sgn-rpc--last-output)))
                   (format ": %s" output)
                 ""))))
    (sgn-rpc--abandon-pending)
    (message "sgn: %s" (or (sgn-rpc-failure-advice) "signal-cli exited"))))

(defun sgn-rpc--last-output ()
  "Return the last line signal-cli wrote to stderr, or nil.
signal-cli reports fatal errors, such as an unregistered account,
there before exiting."
  (when-let* ((buf (get-buffer sgn-rpc--stderr-buffer-name)))
    (with-current-buffer buf
      (let ((lines (split-string (buffer-string) "\n" t "[[:space:]]+")))
        (car (last (cl-remove-if
                    (lambda (line) (string-prefix-p "Process " line))
                    lines)))))))

(defun sgn-rpc--set-failure (failure)
  "Set `sgn-rpc--failure' to FAILURE and run the change hook if it changed."
  (unless (equal failure sgn-rpc--failure)
    (setq sgn-rpc--failure failure)
    (run-hooks 'sgn-rpc-failure-change-hook)))

(defun sgn-rpc-unlinked-p ()
  "Return non-nil if signal-cli failed because the device is not linked.
Signal removes linked devices that stay offline for a long time,
after which signal-cli reports the account as not registered."
  (and sgn-rpc--failure
       (string-match-p "is not registered" sgn-rpc--failure)))

(defun sgn-rpc-failure-advice ()
  "Return `sgn-rpc--failure' followed by the command that fixes it."
  (when sgn-rpc--failure
    (format "%s; run M-x %s" (string-remove-suffix "." sgn-rpc--failure)
            (if (sgn-rpc-unlinked-p) "sgn-link" "sgn-start"))))

;;; Addressing

(defun sgn-rpc-address (chat-id group-p)
  "Return the RPC parameters addressing CHAT-ID.
GROUP-P non-nil means CHAT-ID is a base64 group ID; otherwise it
is a person's ACI or phone number."
  (if group-p
      `((groupId . ,chat-id))
    `((recipient . ,(vector chat-id)))))

;;; Convenience functions for specific RPC methods

(defun sgn-rpc-send-message (address text &optional extras callback
                                     error-callback)
  "Send TEXT to ADDRESS, an alist from `sgn-rpc-address'.
EXTRAS is an alist of additional `send' parameters, such as
`quoteTimestamp' or `attachments'.  CALLBACK and ERROR-CALLBACK
are as in `sgn-rpc-send'.  The result's `timestamp' is the
message's Signal timestamp.  Return the request ID."
  (sgn-rpc-send "send"
                (append address (when text `((message . ,text))) extras)
                callback error-callback))

(defun sgn-rpc-send-reaction (address emoji target-author target-ts
                                      &optional remove callback error-callback)
  "Send reaction EMOJI to ADDRESS for the message TARGET-AUTHOR sent at TARGET-TS.
If REMOVE is non-nil, remove the reaction instead.  CALLBACK and
ERROR-CALLBACK are as in `sgn-rpc-send'."
  (sgn-rpc-send "sendReaction"
                (append address
                        `((emoji . ,emoji)
                          (targetAuthor . ,target-author)
                          (targetTimestamp . ,target-ts))
                        (when remove '((remove . t))))
                callback error-callback))

(defun sgn-rpc-send-typing (address &optional stop)
  "Send a typing indicator to ADDRESS; with STOP, a stop indicator.
Failures are only logged."
  (sgn-rpc-send "sendTyping" (append address (when stop '((stop . t))))
                nil #'ignore))

(defun sgn-rpc-send-receipt (recipient timestamps &optional type)
  "Send a TYPE receipt for TIMESTAMPS to RECIPIENT.
TYPE defaults to \"read\".  Failures are only logged."
  (sgn-rpc-send "sendReceipt"
                `((recipient . ,recipient)
                  (targetTimestamp . ,(vconcat timestamps))
                  (type . ,(or type "read")))
                nil #'ignore))

(defun sgn-rpc-remote-delete (address target-ts &optional callback error-callback)
  "Delete for everyone, in ADDRESS, the message sent at TARGET-TS.
CALLBACK and ERROR-CALLBACK are as in `sgn-rpc-send'."
  (sgn-rpc-send "remoteDelete" (append address `((targetTimestamp . ,target-ts)))
                callback error-callback))

(defun sgn-rpc-send-pin (address target-author target-ts unpin
                                 &optional callback error-callback)
  "Pin, in ADDRESS, the message TARGET-AUTHOR sent at TARGET-TS.
With UNPIN non-nil, unpin it instead.  CALLBACK and ERROR-CALLBACK
are as in `sgn-rpc-send'."
  (sgn-rpc-send (if unpin "sendUnpinMessage" "sendPinMessage")
                (append address `((targetAuthor . ,target-author)
                                  (targetTimestamp . ,target-ts)))
                callback error-callback))

(defun sgn-rpc-list-contacts (callback)
  "Request the contact list from signal-cli, passing it to CALLBACK."
  (sgn-rpc-send "listContacts" nil callback #'ignore))

(defun sgn-rpc-list-groups (callback)
  "Request the group list from signal-cli, passing it to CALLBACK."
  (sgn-rpc-send "listGroups" nil callback #'ignore))

(provide 'sgn-rpc)
;;; sgn-rpc.el ends here
