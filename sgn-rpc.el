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

;; This module manages the signal-cli JSON-RPC subprocess for sgn.  It
;; handles process lifecycle (start, stop, health check), sending
;; JSON-RPC requests with callback support, parsing incoming JSON
;; responses, dispatching received messages and errors, and providing
;; convenience functions for all supported signal-cli RPC methods.
;;
;; The process is started with `--receive-mode=manual' so that no
;; messages are delivered until `sgn-rpc-subscribe-receive' is called.
;; This prevents message loss during initialization (contact loading,
;; group syncing, etc.).
;;
;; Error classification distinguishes transient errors (I/O, rate
;; limiting) from permanent ones (invalid params, untrusted identity),
;; and automatically retries idempotent operations on transient failure.

;;; Code:

(require 'json)
(require 'cl-lib)

(declare-function sgn--log "sgn")

;; Variables defined in sgn.el, referenced here.
(defvar sgn-account)
(defvar sgn-cli-program)
(defvar sgn-data-directory)

;;; Constants

(defconst sgn-rpc--process-name "signal-rpc"
  "Internal name for the signal-cli JSON-RPC process.")

(defconst sgn-rpc--buffer-stderr " *sgn-stderr*"
  "Name of the hidden buffer used for process stderr.")

(defconst sgn-rpc--max-partial-line-length 100000
  "Maximum bytes to buffer before discarding an incomplete JSON line.
Prevents unbounded memory growth if signal-cli emits a very long
line without a terminating newline.")

(defconst sgn-rpc--retry-delay 2
  "Seconds to wait before retrying a transient RPC error.")

(defconst sgn-rpc--idempotent-methods '("send" "sendReaction" "sendReceipt" "sendTyping")
  "RPC methods that are safe to retry on transient errors.
These methods produce the same result whether executed once or
twice, so automatic retry on I/O or rate-limit errors is safe.")

(defconst sgn-rpc--retryable-error-codes '(-3 -5)
  "JSON-RPC error codes considered transient and retryable.
-3 is an I/O error and -5 is a rate-limit error from signal-cli.")

(defconst sgn-rpc--permanent-error-codes '(-1 -4 -32602 -32601)
  "JSON-RPC error codes considered permanent and non-retryable.
-1 is a user error, -4 is an untrusted identity key, -32602 is
invalid params, and -32601 is method not found.")

;;; Handler variables

(defvar sgn-rpc-receive-handler nil
  "Function called with (params) when a \"receive\" method arrives.
Set by `sgn.el' during initialization to orchestrate message handling.")

(defvar sgn-rpc-error-handler nil
  "Function called with (id error-obj) for RPC errors.
If nil, errors are logged but not reported to a buffer.")

;;; Internal state

(defvar sgn-rpc--id-counter 0
  "Monotonically increasing counter for JSON-RPC request IDs.")

(defvar sgn-rpc--pending-callbacks (make-hash-table :test 'equal)
  "Mapping of RPC request ID to callback function.
Each callback is called with the result value when a successful
response arrives for that ID.")

(defvar sgn-rpc--request-methods (make-hash-table :test 'equal)
  "Mapping of RPC request ID to method name.
Used by the error handler to determine whether a failed request
is safe to retry (i.e. the method is idempotent).")

(defvar sgn-rpc--request-params (make-hash-table :test 'equal)
  "Mapping of RPC request ID to params alist.
Stored alongside the method name so that transient errors can be
retried with the original parameters.")

(defvar sgn-rpc--retried-ids (make-hash-table :test 'equal)
  "Set of RPC request IDs that have already been retried once.
Prevents infinite retry loops: each request is retried at most once.")

(defvar sgn-rpc--partial-line ""
  "Buffer string for incomplete JSON lines received from the process.
The process filter accumulates data here until a complete
newline-terminated JSON object is available for parsing.")

;;; Lifecycle

(defun sgn-rpc-start ()
  "Start the signal-cli JSON-RPC subprocess.
Cleans up any existing process state before starting.  In jsonRpc
mode, signal-cli receives messages automatically."
  (unless sgn-cli-program
    (user-error "Variable `sgn-cli-program' is not set"))
  (unless (executable-find sgn-cli-program)
    (user-error "The signal-cli executable '%s' was not found.\
 Please install it or check `sgn-cli-program'" sgn-cli-program))
  (unless sgn-account
    (user-error "Variable `sgn-account' is not set"))
  ;; Clean up old process if still lingering.
  (when (get-process sgn-rpc--process-name)
    (delete-process sgn-rpc--process-name))
  ;; Reset all state.
  (setq sgn-rpc--partial-line "")
  (clrhash sgn-rpc--pending-callbacks)
  (clrhash sgn-rpc--request-methods)
  (clrhash sgn-rpc--request-params)
  (clrhash sgn-rpc--retried-ids)
  ;; Start the process.
  (let ((proc (make-process
               :name sgn-rpc--process-name
               :buffer sgn-rpc--buffer-stderr
               :command (list sgn-cli-program
                             "-a" sgn-account
                             "jsonRpc")
               :filter #'sgn-rpc--process-filter
               :sentinel #'sgn-rpc--process-sentinel
               :coding 'utf-8-unix)))
    (set-process-query-on-exit-flag proc nil)
    (sgn--log "sgn RPC process started.")
    (message "sgn service started.")))

(defun sgn-rpc-stop ()
  "Stop the signal-cli JSON-RPC subprocess."
  (when (get-process sgn-rpc--process-name)
    (delete-process sgn-rpc--process-name)
    (sgn--log "sgn RPC process stopped.")
    (message "sgn service stopped.")))

(defun sgn-rpc-alive-p ()
  "Return non-nil if the signal-cli JSON-RPC process is running."
  (and (get-process sgn-rpc--process-name)
       (process-live-p (get-process sgn-rpc--process-name))))

;;; Generic send

(defun sgn-rpc-send (method params &optional callback)
  "Send a JSON-RPC request with METHOD and PARAMS.
Return the integer request ID.  If CALLBACK is non-nil, it is
called with the result value when a successful response arrives.
The method name is stored alongside the callback so the error
handler can determine retry eligibility."
  (unless (sgn-rpc-alive-p)
    (error "sgn service not running.  M-x sgn-start"))
  (let* ((id (cl-incf sgn-rpc--id-counter))
         (req `((jsonrpc . "2.0")
                (method . ,method)
                (params . ,params)
                (id . ,id)))
         (json-str (json-encode req)))
    (puthash id method sgn-rpc--request-methods)
    (puthash id params sgn-rpc--request-params)
    (when callback
      (puthash id callback sgn-rpc--pending-callbacks))
    (sgn--log "SEND: %s" json-str)
    (process-send-string sgn-rpc--process-name (concat json-str "\n"))
    id))

;;; Process filter and parsing

(defun sgn-rpc--process-filter (_proc string)
  "Accumulate process output and parse complete JSON objects from STRING.
Incomplete lines are buffered in `sgn-rpc--partial-line' until a
newline delimiter arrives.  An overflow guard discards data if the
buffer exceeds `sgn-rpc--max-partial-line-length'."
  (setq sgn-rpc--partial-line (concat sgn-rpc--partial-line string))
  ;; Overflow protection: discard if buffer grows too large.
  (when (> (length sgn-rpc--partial-line) sgn-rpc--max-partial-line-length)
    (setq sgn-rpc--partial-line "")
    (sgn--log "WARNING: Buffer overflow protection triggered.  Dropped data."))
  (let ((lines (split-string sgn-rpc--partial-line "\n")))
    ;; If the accumulated data ends with a newline, all lines are complete.
    ;; Otherwise, the last element is an incomplete line to carry over.
    (if (string-suffix-p "\n" sgn-rpc--partial-line)
        (setq sgn-rpc--partial-line "")
      (setq sgn-rpc--partial-line (car (last lines)))
      (setq lines (butlast lines)))
    (dolist (line lines)
      (setq line (string-trim line))
      (when (and (not (string-empty-p line))
                 (string-prefix-p "{" line))
        (sgn--log "RECV: %s" line)
        (condition-case err
            (let ((json (json-read-from-string line)))
              (sgn-rpc--dispatch json))
          (error (sgn--log "JSON parse error: %s" err)))))))

(defun sgn-rpc--process-sentinel (_proc event)
  "Log process EVENT for debugging."
  (sgn--log "Process event: %s" (string-trim event))
  (when (string-prefix-p "exited" event)
    (message "sgn process exited.")))

;;; Dispatch

(defun sgn-rpc--dispatch (json)
  "Route a parsed JSON response to the appropriate handler.
Incoming notifications (method = \"receive\") are forwarded to
`sgn-rpc-receive-handler'.  Error responses are handled with
retry logic for transient failures.  Successful results invoke
their pending callback."
  (let ((method (alist-get 'method json))
        (error-obj (alist-get 'error json))
        (id (alist-get 'id json))
        (result (alist-get 'result json))
        (params (alist-get 'params json)))
    (cond
     ;; Incoming message notification from signal-cli.
     ((equal method "receive")
      (if sgn-rpc-receive-handler
          (funcall sgn-rpc-receive-handler params)
        (sgn--log "Received message but no receive handler is set.")))
     ;; RPC error response.
     (error-obj
      (sgn-rpc--handle-error id error-obj))
     ;; Successful RPC result.
     ((and id result)
      (sgn-rpc--handle-result id result))
     ;; Result may be null/empty for void methods; still clean up.
     (id
      (sgn-rpc--handle-result id result)))))

;;; Error handling and retry

(defun sgn-rpc--retryable-error-p (code)
  "Return non-nil if error CODE is transient and retryable."
  (memq code sgn-rpc--retryable-error-codes))

(defun sgn-rpc--handle-error (id error-obj)
  "Handle a JSON-RPC error response for request ID with ERROR-OBJ.
For transient errors on idempotent methods that have not already
been retried, schedule a single retry after `sgn-rpc--retry-delay'
seconds.  For permanent or non-retryable errors, report via
`sgn-rpc-error-handler' if set, otherwise just log."
  (let* ((code (alist-get 'code error-obj))
         (msg (alist-get 'message error-obj))
         (method (gethash id sgn-rpc--request-methods))
         (params (gethash id sgn-rpc--request-params))
         (callback (gethash id sgn-rpc--pending-callbacks))
         (already-retried (gethash id sgn-rpc--retried-ids)))
    (sgn--log "RPC error [id=%s method=%s code=%s]: %s" id method code msg)
    ;; Attempt retry for transient errors on idempotent methods.
    (if (and (sgn-rpc--retryable-error-p code)
             method
             (member method sgn-rpc--idempotent-methods)
             (not already-retried))
        (progn
          ;; Mark the original ID as retried so we don't loop.
          (puthash id t sgn-rpc--retried-ids)
          ;; Clean up the old request's bookkeeping.
          (remhash id sgn-rpc--pending-callbacks)
          (remhash id sgn-rpc--request-methods)
          (remhash id sgn-rpc--request-params)
          (sgn--log "Scheduling retry for %s (id=%s) in %ds"
                    method id sgn-rpc--retry-delay)
          (run-at-time sgn-rpc--retry-delay nil
                       #'sgn-rpc-send method params callback))
      ;; Not retryable: clean up and report.
      (remhash id sgn-rpc--pending-callbacks)
      (remhash id sgn-rpc--request-methods)
      (remhash id sgn-rpc--request-params)
      (remhash id sgn-rpc--retried-ids)
      (let ((classification
             (cond
              ((sgn-rpc--retryable-error-p code) "transient (retry exhausted)")
              ((memq code sgn-rpc--permanent-error-codes) "permanent")
              (t "unknown"))))
        (sgn--log "RPC error classified as %s [code=%s]: %s" classification code msg))
      (when sgn-rpc-error-handler
        (funcall sgn-rpc-error-handler id error-obj)))))

(defun sgn-rpc--handle-result (id result)
  "Handle a successful JSON-RPC response for request ID with RESULT.
Invokes and removes the pending callback, if any, then cleans up
all bookkeeping for this request."
  (let ((callback (gethash id sgn-rpc--pending-callbacks)))
    (remhash id sgn-rpc--pending-callbacks)
    (remhash id sgn-rpc--request-methods)
    (remhash id sgn-rpc--request-params)
    (remhash id sgn-rpc--retried-ids)
    (when callback
      (funcall callback result))))

;;; Addressing helper

(defun sgn-rpc--build-address (chat-id)
  "Return an alist addressing CHAT-ID for signal-cli RPC methods.
Phone numbers (starting with \"+\") and UUIDs (containing \"-\")
are individual recipients and produce (recipient . [CHAT-ID]).
Everything else is treated as a base64 group ID and produces
\(groupId . CHAT-ID)."
  (if (or (string-prefix-p "+" chat-id)
          (string-match-p "-" chat-id))
      `((recipient . ,(vector chat-id)))
    `((groupId . ,chat-id))))

;;; Subscribe receive

(defun sgn-rpc-subscribe-receive (&optional _callback)
  "No-op for compatibility.
In jsonRpc mode signal-cli receives messages automatically."
  nil)

;;; Convenience functions for specific RPC methods

(defun sgn-rpc-send-message (chat-id text &optional extras)
  "Send TEXT to CHAT-ID.
EXTRAS is an optional alist of additional parameters (e.g.
quoteTimestamp, quoteAuthor, editTimestamp, attachments).
Return the request ID."
  (let ((params (append (sgn-rpc--build-address chat-id)
                        `((message . ,text))
                        extras)))
    (sgn-rpc-send "send" params)))

(defun sgn-rpc-send-reaction (chat-id emoji target-author target-ts &optional remove)
  "Send reaction EMOJI to message identified by TARGET-AUTHOR and TARGET-TS.
CHAT-ID is the conversation.  If REMOVE is non-nil, remove the
reaction instead of adding it.  Return the request ID."
  (let ((params (append (sgn-rpc--build-address chat-id)
                        `((emoji . ,emoji)
                          (targetAuthor . ,target-author)
                          (targetTimestamp . ,target-ts))
                        (when remove '((remove . t))))))
    (sgn-rpc-send "sendReaction" params)))

(defun sgn-rpc-send-typing (chat-id &optional stop)
  "Send typing indicator to CHAT-ID.
If STOP is non-nil, send a stop-typing notification instead.
Return the request ID."
  (let ((params (append (sgn-rpc--build-address chat-id)
                        (when stop '((stop . t))))))
    (sgn-rpc-send "sendTyping" params)))

(defun sgn-rpc-send-receipt (recipient timestamps &optional type)
  "Send receipt for TIMESTAMPS to RECIPIENT.
TIMESTAMPS is a list of Signal message timestamps (integers).
TYPE defaults to \"read\"; other valid values are \"viewed\" and
\"delivered\".  Return the request ID."
  (let ((params `((recipient . ,recipient)
                  (targetTimestamp . ,(vconcat timestamps))
                  (type . ,(or type "read")))))
    (sgn-rpc-send "sendReceipt" params)))

(defun sgn-rpc-remote-delete (chat-id target-ts)
  "Send a remote delete for the message at TARGET-TS in CHAT-ID.
Return the request ID."
  (let ((params (append (sgn-rpc--build-address chat-id)
                        `((targetTimestamp . ,target-ts)))))
    (sgn-rpc-send "remoteDelete" params)))

(defun sgn-rpc-send-edit (chat-id text edit-ts &optional extras)
  "Send edited TEXT for the message at EDIT-TS in CHAT-ID.
EXTRAS is an optional alist of additional parameters.
Return the request ID."
  (let ((params (append (sgn-rpc--build-address chat-id)
                        `((message . ,text)
                          (editTimestamp . ,edit-ts))
                        extras)))
    (sgn-rpc-send "send" params)))

(defun sgn-rpc-list-contacts (&optional callback)
  "Request the contact list from signal-cli.
If CALLBACK is non-nil, it is called with the result.
Return the request ID."
  (sgn-rpc-send "listContacts" nil callback))

(defun sgn-rpc-list-groups (&optional callback)
  "Request the group list from signal-cli.
If CALLBACK is non-nil, it is called with the result.
Return the request ID."
  (sgn-rpc-send "listGroups" nil callback))

(defun sgn-rpc-send-poll-create (chat-id question options)
  "Create a poll in CHAT-ID with QUESTION and OPTIONS.
OPTIONS is a list of option strings.  Return the request ID."
  (let ((params (append (sgn-rpc--build-address chat-id)
                        `((pollQuestion . ,question)
                          (pollOptions . ,(vconcat options))))))
    (sgn-rpc-send "sendPollCreate" params)))

(defun sgn-rpc-send-poll-vote (chat-id poll-author poll-ts option-indexes vote-count)
  "Vote in a poll in CHAT-ID.
POLL-AUTHOR and POLL-TS identify the poll message.
OPTION-INDEXES is a list of integers (the selected option indexes).
VOTE-COUNT is the monotonically increasing vote count for this
client.  Return the request ID."
  (let ((params (append (sgn-rpc--build-address chat-id)
                        `((pollAuthor . ,poll-author)
                          (pollTimestamp . ,poll-ts)
                          (pollOption . ,(vconcat option-indexes))
                          (voteCount . ,vote-count)))))
    (sgn-rpc-send "sendPollVote" params)))

(defun sgn-rpc-send-pin (chat-id target-author target-ts &optional duration)
  "Pin the message identified by TARGET-AUTHOR and TARGET-TS in CHAT-ID.
If DURATION is non-nil, it specifies the pin duration in seconds.
Return the request ID."
  (let ((params (append (sgn-rpc--build-address chat-id)
                        `((targetAuthor . ,target-author)
                          (targetTimestamp . ,target-ts))
                        (when duration
                          `((pinDuration . ,duration))))))
    (sgn-rpc-send "sendPinMessage" params)))

(defun sgn-rpc-send-unpin (chat-id target-author target-ts)
  "Unpin the message identified by TARGET-AUTHOR and TARGET-TS in CHAT-ID.
Return the request ID."
  (let ((params (append (sgn-rpc--build-address chat-id)
                        `((targetAuthor . ,target-author)
                          (targetTimestamp . ,target-ts)))))
    (sgn-rpc-send "sendUnpinMessage" params)))

(provide 'sgn-rpc)
;;; sgn-rpc.el ends here
