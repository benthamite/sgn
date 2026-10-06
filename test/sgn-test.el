;;; sgn-test.el --- Tests for sgn  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for sgn.  Envelopes are built by `sgn-test-envelope' and
;; friends, which mirror the JSON signal-cli 0.14 emits (the records
;; in org.asamk.signal.json: JsonMessageEnvelope, JsonDataMessage,
;; JsonSyncDataMessage, ...), including the fields sgn ignores and
;; their null and false values.  They are encoded to JSON and parsed
;; back the way `sgn-rpc' parses signal-cli's output, so the tests see
;; exactly the shapes sgn sees at runtime.  No real message data is
;; used.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sgn)

;;;; Identities

(defconst sgn-test-self-number "+15550000000")
(defconst sgn-test-self-uuid "00000000-0000-4000-8000-000000000000")
(defconst sgn-test-alice-number "+15551111111")
(defconst sgn-test-alice-uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(defconst sgn-test-bob-number "+15552222222")
(defconst sgn-test-bob-uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(defconst sgn-test-group "+AbCdEfGhIjKlMnOpQrStUvWxYz0123456789abcdefg=")

;;;; Environment

(defmacro sgn-test-with-session (&rest body)
  "Run BODY with a fresh database and sgn state, and no signal-cli.
Sent requests are collected in `sgn-test-sent' (newest first) as
parsed JSON; `sgn-test-messages' collects echo-area messages."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "sgn-test-" t))
          (sgn-db-directory dir)
          (sgn-data-directory (expand-file-name "signal-cli" dir))
          (sgn-db--connection nil)
          (sgn-db--transaction-depth 0)
          (sgn-account sgn-test-self-number)
          (sgn-store--self-uuid nil)
          (sgn-contacts--cache (make-hash-table :test 'equal))
          (sgn-contacts--numbers (make-hash-table :test 'equal))
          (sgn-chat--buffers (make-hash-table :test 'equal))
          (sgn-rpc--pending (make-hash-table :test 'eql))
          (sgn-rpc--id-counter 0)
          (sgn-rpc--partial-line "")
          (sgn-rpc--failure nil)
          (sgn-rpc-receive-handler #'sgn--handle-receive)
          (sgn-notify--queued nil)
          (sgn-notify--timer nil)
          (sgn-desktop-notifications nil)
          (sgn-send-typing nil)
          (sgn-test-sent nil)
          (sgn-test-messages nil)
          (sgn-test-notifications nil))
     (ignore sgn-test-sent sgn-test-messages sgn-test-notifications)
     (cl-letf (((symbol-function 'sgn--log) #'ignore)
               ((symbol-function 'sgn-rpc-alive-p) (lambda () t))
               ((symbol-function 'process-send-string)
                (lambda (_proc string)
                  (push (sgn-rpc-parse-json string) sgn-test-sent)))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (when fmt (push (apply #'format fmt args) sgn-test-messages))))
               ((symbol-function 'sgn-notify--desktop-notify)
                (lambda (title body _chat-id)
                  (push (cons title body) sgn-test-notifications)))
               ((symbol-function 'run-at-time) #'sgn-test--defer))
       (let ((sgn-test--deferred nil))
         (unwind-protect
             (progn
               (sgn-db-init)
               ,@body)
           (dolist (buf (sgn-chat-buffers))
             (let ((kill-buffer-hook nil))
               (kill-buffer buf)))
           (sgn-db-close)
           (delete-directory dir t))))))

(defvar sgn-test-sent nil)
(defvar sgn-test-messages nil)
(defvar sgn-test-notifications nil)
(defvar sgn-test--deferred nil
  "Functions deferred with `run-at-time' in tests, as (TIME FUNCTION ARGS).")

(defun sgn-test--defer (time _repeat function &rest args)
  "Record FUNCTION with ARGS to run at TIME; return a dummy timer."
  (push (list time function args) sgn-test--deferred)
  (timer-create))

(defun sgn-test-run-deferred (&optional all)
  "Run deferred functions due immediately, or with ALL, every one."
  (let ((due (cl-remove-if-not (lambda (d) (or all (memq (car d) '(0 nil))))
                               sgn-test--deferred)))
    (setq sgn-test--deferred (cl-set-difference sgn-test--deferred due))
    (dolist (d (reverse due))
      (apply (nth 1 d) (nth 2 d)))
    (when (and due (cl-some (lambda (d) (memq (car d) '(0 nil)))
                            sgn-test--deferred))
      (sgn-test-run-deferred))))

(defun sgn-test-receive (envelope)
  "Feed ENVELOPE to sgn as signal-cli would, then run deferred redraws."
  (sgn-rpc--handle-line
   (json-encode `((jsonrpc . "2.0") (method . "receive")
                  (params . ((envelope . ,envelope)
                             (account . ,sgn-test-self-number))))))
  (sgn-test-run-deferred))

(defun sgn-test-reply (result &optional id)
  "Answer request ID (default: the latest) with RESULT."
  (sgn-rpc--handle-line
   (json-encode `((jsonrpc . "2.0") (result . ,result)
                  (id . ,(or id (alist-get 'id (car sgn-test-sent)))))))
  (sgn-test-run-deferred))

(defun sgn-test-error (message &optional id code)
  "Answer request ID (default: the latest) with an error MESSAGE and CODE."
  (sgn-rpc--handle-line
   (json-encode `((jsonrpc . "2.0")
                  (error . ((code . ,(or code -1)) (message . ,message)))
                  (id . ,(or id (alist-get 'id (car sgn-test-sent)))))))
  (sgn-test-run-deferred))

(defun sgn-test-send-result (timestamp &rest types)
  "Return a signal-cli send result with TIMESTAMP and recipient TYPES."
  `((timestamp . ,timestamp)
    (results . ,(vconcat
                 (mapcar (lambda (type)
                           `((recipientAddress . ((uuid . ,sgn-test-alice-uuid)
                                                  (number . ,sgn-test-alice-number)
                                                  (username . nil)))
                             (type . ,type)))
                         (or types '("SUCCESS")))))))

(defun sgn-test-last-request ()
  "Return the params of the latest request sent."
  (alist-get 'params (car sgn-test-sent)))

;;;; Envelope builders (mirroring signal-cli 0.14's JSON)

(cl-defun sgn-test-data-message (&key (timestamp 1000) message group
                                      attachments sticker quote reaction
                                      remote-delete mentions text-styles
                                      (expires 0) expiration-update
                                      pin unpin poll)
  "Return a JsonDataMessage with the given fields."
  (append
   `((timestamp . ,timestamp)
     (message . ,message)
     (expiresInSeconds . ,expires)
     (isExpirationUpdate . ,(if expiration-update t :json-false))
     (viewOnce . :json-false))
   (when reaction `((reaction . ,reaction)))
   (when quote `((quote . ,quote)))
   (when mentions `((mentions . ,(vconcat mentions))))
   (when attachments `((attachments . ,(vconcat attachments))))
   (when sticker `((sticker . ,sticker)))
   (when remote-delete `((remoteDelete . ((timestamp . ,remote-delete)))))
   (when poll `((pollCreate . ((question . ,poll) (allowMultiple . :json-false)
                               (options . ["a" "b"])))))
   (when text-styles `((textStyles . ,(vconcat text-styles))))
   (when group `((groupInfo . ((groupId . ,group) (groupName . nil)
                               (revision . 3) (type . "DELIVER")))))
   (when pin `((pinMessage . ,pin)))
   (when unpin `((unpinMessage . ,unpin)))))

(cl-defun sgn-test-envelope (&key (number sgn-test-alice-number)
                                  (uuid sgn-test-alice-uuid)
                                  (name "Alice")
                                  (timestamp 1000)
                                  data edit sync receipt typing)
  "Return a JsonMessageEnvelope from NUMBER/UUID with one payload."
  (append
   `((source . ,(or number uuid))
     (sourceNumber . ,number)
     (sourceUuid . ,uuid)
     (sourceName . ,name)
     (sourceDevice . 1)
     (timestamp . ,timestamp)
     (serverReceivedTimestamp . ,(1+ timestamp))
     (serverDeliveredTimestamp . ,(+ 2 timestamp)))
   (when data `((dataMessage . ,data)))
   (when edit `((editMessage . ,edit)))
   (when sync `((syncMessage . ,sync)))
   (when receipt `((receiptMessage . ,receipt)))
   (when typing `((typingMessage . ,typing)))))

(defun sgn-test-incoming (text &optional timestamp &rest keys)
  "Return an envelope of Alice sending TEXT at TIMESTAMP.
KEYS are passed to `sgn-test-data-message'."
  (let ((ts (or timestamp 1000)))
    (sgn-test-envelope :timestamp ts
                       :data (apply #'sgn-test-data-message
                                    :timestamp ts :message text keys))))

(defun sgn-test-sync-sent (data &optional destination-uuid)
  "Return an envelope syncing DATA sent from this account's phone.
DESTINATION-UUID defaults to Alice's."
  (sgn-test-envelope
   :number sgn-test-self-number :uuid sgn-test-self-uuid :name "Me"
   :timestamp (alist-get 'timestamp data)
   :sync `((sentMessage
            . ,(append `((destination . ,sgn-test-alice-number)
                         (destinationNumber . ,(and destination-uuid nil))
                         (destinationUuid . ,(or destination-uuid
                                                 sgn-test-alice-uuid)))
                       data)))))

(defun sgn-test-attachment (id &optional type voice)
  "Return a JsonAttachment with ID, content TYPE and VOICE flag."
  `((contentType . ,(or type "image/jpeg")) (filename . nil) (id . ,id)
    (size . 1234) (width . 10) (height . 10) (caption . nil)
    (uploadTimestamp . 1) (isVoiceNote . ,(if voice t :json-false))))

(defun sgn-test-chat-messages (chat-id)
  "Return the messages of CHAT-ID."
  (sgn-db-get-messages chat-id 1000))

(defun sgn-test-body (chat-id)
  "Return the bodies of CHAT-ID's messages, oldest first."
  (mapcar (lambda (m) (plist-get m :body)) (sgn-test-chat-messages chat-id)))

(defun sgn-test-history (chat-id)
  "Return the history text of CHAT-ID's buffer, after pending redraws."
  (sgn-test-run-deferred)
  (with-current-buffer (sgn-chat-buffer chat-id)
    (buffer-substring-no-properties (point-min) sgn-chat--prompt-start)))

;;;; RPC

(ert-deftest sgn-test-rpc-delivers-long-requests ()
  "A request longer than a pty's line limit reaches signal-cli intact."
  (let* ((script (make-temp-file "sgn-test-cli-" nil ".sh"
                                 (concat "#!/bin/sh\n"
                                         "IFS= read -r line\n"
                                         "printf '{\"jsonrpc\":\"2.0\",\"id\":1,"
                                         "\"result\":{\"length\":%d}}\\n' "
                                         "\"${#line}\"\n")))
         (sgn-cli-program script)
         (sgn-account "+15550000000")
         (sgn-rpc--pending (make-hash-table :test 'eql))
         (sgn-rpc--id-counter 0)
         (sgn-rpc--failure nil)
         (result nil))
    (unwind-protect
        (cl-letf (((symbol-function 'sgn--log) #'ignore)
                  ((symbol-function 'message) #'ignore))
          (set-file-modes script #o700)
          (sgn-rpc-start)
          (sgn-rpc-send "send" `((message . ,(make-string 4000 ?a)))
                        (lambda (r) (setq result r)))
          (with-timeout (5)
            (while (not result)
              (accept-process-output (get-process sgn-rpc--process-name) 0.1)))
          (should (> (alist-get 'length result) 4000)))
      (sgn-rpc-stop)
      (delete-file script))))

(ert-deftest sgn-test-rpc-result-and-error-callbacks ()
  "Replies reach the request's callback; errors its error callback."
  (sgn-test-with-session
    (let (result error-obj)
      (sgn-rpc-send "a" nil (lambda (r) (setq result r)))
      (sgn-rpc-send "b" nil nil (lambda (e) (setq error-obj e)))
      (sgn-test-reply '((ok . t)) 1)
      (sgn-test-error "nope" 2)
      (should (equal result '((ok . t))))
      (should (equal (alist-get 'message error-obj) "nope"))
      (should (zerop (hash-table-count sgn-rpc--pending))))))

(ert-deftest sgn-test-rpc-error-without-callback-is-shown ()
  "An error for a request without an error callback is shown."
  (sgn-test-with-session
    (sgn-rpc-send "sendReaction" nil)
    (sgn-test-error "Untrusted identity")
    (should (cl-some (lambda (m) (string-match-p "Untrusted identity" m))
                     sgn-test-messages))))

(ert-deftest sgn-test-rpc-errors-are-never-retried ()
  "A transient error is reported once, and the request is not resent."
  (sgn-test-with-session
    (let ((errors 0))
      (sgn-rpc-send "send" nil nil (lambda (_) (cl-incf errors)))
      (sgn-test-error "I/O" nil -3)
      (sgn-test-run-deferred t)
      (should (= errors 1))
      (should (= (length sgn-test-sent) 1)))))

(ert-deftest sgn-test-rpc-stop-abandons-pending ()
  "Stopping signal-cli tells pending requests they were abandoned."
  (sgn-test-with-session
    (let (error-obj)
      (sgn-rpc-send "send" nil nil (lambda (e) (setq error-obj e)))
      (sgn-rpc-stop)
      (should (alist-get 'abandoned error-obj)))))

(ert-deftest sgn-test-rpc-filter-joins-chunks-and-keeps-complete-lines ()
  "Lines split across chunks are joined; a long line never drops others."
  (sgn-test-with-session
    (let (results)
      (sgn-rpc-send "a" nil (lambda (r) (push r results)))
      (sgn-rpc-send "b" nil (lambda (r) (push r results)))
      (sgn-rpc--process-filter
       nil (format "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":\"%s\"}\n{\"jsonrpc\""
                   (make-string 200000 ?x)))
      (sgn-rpc--process-filter nil ":\"2.0\",\"id\":2,\"result\":\"b\"}\n")
      (should (equal (car results) "b"))
      (should (= (length (cadr results)) 200000)))))

(ert-deftest sgn-test-rpc-handler-error-does-not-stop-later-lines ()
  "An error in one handler is reported and later lines are still handled."
  (sgn-test-with-session
    (let (second)
      (sgn-rpc-send "a" nil (lambda (_) (error "Boom")))
      (sgn-rpc-send "b" nil (lambda (r) (setq second r)))
      (sgn-rpc--process-filter
       nil (concat "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":1}\n"
                   "{\"jsonrpc\":\"2.0\",\"id\":2,\"result\":2}\n"))
      (should (eql second 2))
      (should (cl-some (lambda (m) (string-match-p "Boom" m)) sgn-test-messages)))))

(ert-deftest sgn-test-rpc-abnormal-exit-reports-stderr ()
  "An abnormal exit is recorded with the last line signal-cli printed."
  (sgn-test-with-session
    (with-current-buffer (get-buffer-create sgn-rpc--stderr-buffer-name)
      (erase-buffer)
      (insert "INFO starting\nUser +15550000000 is not registered.\n"))
    (let ((proc (make-process :name "sgn-test-dead" :command '("true"))))
      (while (process-live-p proc) (accept-process-output proc 0.05))
      (cl-letf (((symbol-function 'process-exit-status) (lambda (_) 1)))
        (sgn-rpc--process-sentinel proc "exited abnormally with code 1\n")))
    (should (string-match-p "not registered" sgn-rpc--failure))
    (should (sgn-rpc-unlinked-p))
    (should (string-suffix-p "run M-x sgn-link" (sgn-rpc-failure-advice)))))

;;;; Events

(ert-deftest sgn-test-event-data-message ()
  "A data message becomes a message event with every field normalized."
  (let* ((envelope (sgn-test-incoming
                    "😀 hi @"  1000
                    :text-styles '(((style . "BOLD") (start . 3) (length . 2)))
                    :mentions `(((name . "+1") (number . ,sgn-test-bob-number)
                                 (uuid . ,sgn-test-bob-uuid) (start . 6)
                                 (length . 1)))
                    :quote `((id . 900) (author . ,sgn-test-self-number)
                             (authorNumber . ,sgn-test-self-number)
                             (authorUuid . ,sgn-test-self-uuid)
                             (text . "earlier") (attachments . []))
                    :attachments (list (sgn-test-attachment "v.aac" "audio/aac" t)
                                       (sgn-test-attachment "l.txt"
                                                            "text/x-signal-plain"))
                    :expires 3600))
         (events (sgn-event-from-envelope
                  (sgn-rpc-parse-json
                   (json-encode `((envelope . ,envelope))))
                  sgn-test-self-uuid))
         (message (cl-find 'message events :key (lambda (e) (plist-get e :kind)))))
    (should (equal (plist-get message :chat-id) sgn-test-alice-uuid))
    (should (equal (plist-get message :sender) sgn-test-alice-uuid))
    (should-not (plist-get message :outgoing))
    (should (equal (plist-get message :styles) '(("BOLD" 3 2))))
    (should (equal (plist-get message :mentions) `((,sgn-test-bob-uuid 6 1))))
    (should (equal (plist-get message :quote)
                   `(:ts 900 :author ,sgn-test-self-uuid :body "earlier")))
    (should (= (length (plist-get message :attachments)) 1))
    (should (eql (plist-get (car (plist-get message :attachments)) :is-voice) 1))
    (should (eql (plist-get message :expires-in) 3600))
    (should (cl-find 'contact events :key (lambda (e) (plist-get e :kind))))))

(defun sgn-test-events (envelope)
  "Return the events of ENVELOPE, round-tripped through JSON."
  (sgn-event-from-envelope
   (sgn-rpc-parse-json (json-encode `((envelope . ,envelope))))
   sgn-test-self-uuid))

(defun sgn-test-event (kind envelope)
  "Return the KIND event of ENVELOPE."
  (cl-find kind (sgn-test-events envelope) :key (lambda (e) (plist-get e :kind))))

(ert-deftest sgn-test-event-group-ids-are-groups ()
  "A group ID starting with + is a group, not a phone number."
  (let ((event (sgn-test-event 'message (sgn-test-incoming "hi" 1 :group sgn-test-group))))
    (should (equal (plist-get event :chat-id) sgn-test-group))
    (should (equal (plist-get event :chat-type) "group"))))

(ert-deftest sgn-test-event-edit ()
  "An editMessage envelope becomes an edit of the original timestamp."
  (let ((event (sgn-test-event
                'edit (sgn-test-envelope
                       :edit `((targetSentTimestamp . 1000)
                               (dataMessage . ,(sgn-test-data-message
                                                :timestamp 2000
                                                :message "fixed")))))))
    (should (equal (plist-get event :target-ts) 1000))
    (should (equal (plist-get event :timestamp) 2000))
    (should (equal (plist-get event :body) "fixed"))))

(ert-deftest sgn-test-event-sync-sent-and-edit ()
  "Messages and edits sent from another device are outgoing, from self."
  (let ((sent (sgn-test-event 'message (sgn-test-sync-sent
                                        (sgn-test-data-message :timestamp 5
                                                               :message "yo"))))
        (edit (sgn-test-event
               'edit
               (sgn-test-envelope
                :number sgn-test-self-number :uuid sgn-test-self-uuid
                :sync `((sentMessage
                         . ((destinationNumber . ,sgn-test-alice-number)
                            (destinationUuid . ,sgn-test-alice-uuid)
                            (timestamp . 7) (message . nil)
                            (editMessage
                             . ((targetSentTimestamp . 5)
                                (dataMessage . ,(sgn-test-data-message
                                                 :timestamp 7
                                                 :message "yo!")))))))))))
    (should (plist-get sent :outgoing))
    (should (equal (plist-get sent :sender) sgn-test-self-uuid))
    (should (equal (plist-get sent :chat-id) sgn-test-alice-uuid))
    (should (equal (plist-get edit :sender) sgn-test-self-uuid))
    (should (equal (plist-get edit :chat-id) sgn-test-alice-uuid))
    (should (equal (plist-get edit :target-ts) 5))))

(ert-deftest sgn-test-event-receipts-reads-and-typing ()
  "Receipts, read syncs and typing messages use signal-cli's field names."
  (let ((receipt (sgn-test-event
                  'receipt (sgn-test-envelope
                            :receipt '((when . 9) (isDelivery . :json-false)
                                       (isRead . t) (isViewed . :json-false)
                                       (timestamps . [5 6])))))
        (read (sgn-test-event
               'read (sgn-test-envelope
                      :number sgn-test-self-number :uuid sgn-test-self-uuid
                      :sync `((readMessages
                               . [((sender . ,sgn-test-alice-number)
                                   (senderNumber . ,sgn-test-alice-number)
                                   (senderUuid . ,sgn-test-alice-uuid)
                                   (timestamp . 1000))])))))
        (typing (sgn-test-event
                 'typing (sgn-test-envelope
                          :typing `((action . "STARTED") (timestamp . 1)
                                    (groupId . ,sgn-test-group))))))
    (should (equal (plist-get receipt :type) "read"))
    (should (equal (plist-get receipt :timestamps) '(5 6)))
    (should (equal (plist-get read :entries) `((,sgn-test-alice-uuid 1000))))
    (should (equal (plist-get typing :chat-id) sgn-test-group))
    (should (plist-get typing :started))))

(ert-deftest sgn-test-event-reactions-deletes-pins-timers ()
  "Reactions, deletes, pins, unpins and timer updates are recognized."
  (let ((reaction (sgn-test-event
                   'reaction
                   (sgn-test-incoming
                    nil 2000
                    :reaction `((emoji . "👍") (targetAuthor . ,sgn-test-self-number)
                                (targetAuthorNumber . ,sgn-test-self-number)
                                (targetAuthorUuid . ,sgn-test-self-uuid)
                                (targetSentTimestamp . 1500)
                                (isRemove . :json-false)))))
        (delete (sgn-test-event 'delete (sgn-test-incoming nil 2000 :remote-delete 1000)))
        (pin (sgn-test-event
              'pin (sgn-test-incoming
                    nil 2000 :pin `((targetAuthorNumber . nil)
                                    (targetAuthorUuid . ,sgn-test-bob-uuid)
                                    (targetSentTimestamp . 1000)
                                    (pinDurationSeconds . 0)))))
        (unpin (sgn-test-event
                'pin (sgn-test-incoming
                      nil 2000 :unpin `((targetAuthor . ,sgn-test-bob-uuid)
                                        (targetAuthorNumber . nil)
                                        (targetAuthorUuid . ,sgn-test-bob-uuid)
                                        (targetSentTimestamp . 1000)))))
        (timer (sgn-test-event 'timer (sgn-test-incoming nil 2000 :expires 60
                                                         :expiration-update t))))
    (should (equal (plist-get reaction :target-author) sgn-test-self-uuid))
    (should (equal (plist-get reaction :target-ts) 1500))
    (should-not (plist-get reaction :remove))
    (should (equal (plist-get delete :target-ts) 1000))
    (should (equal (plist-get pin :target-author) sgn-test-bob-uuid))
    (should-not (plist-get pin :unpin))
    (should (plist-get unpin :unpin))
    (should (eql (plist-get timer :seconds) 60))))

(ert-deftest sgn-test-event-polls-show-as-messages ()
  "An incoming poll is shown as a message with its question."
  (should (equal (plist-get (sgn-test-event 'message (sgn-test-incoming
                                                      nil 1 :poll "Lunch?"))
                            :body)
                 "📊 Lunch?")))

;;;; Store

(ert-deftest sgn-test-store-duplicate-delivery-is-stored-once ()
  "A redelivered envelope adds nothing and announces nothing."
  (sgn-test-with-session
    (let ((new 0))
      (let ((sgn-store-new-message-functions (list (lambda (&rest _) (cl-incf new)))))
        (sgn-test-receive (sgn-test-incoming "first" 1000))
        (sgn-test-receive (sgn-test-incoming "second" 2000
                                             :attachments (list (sgn-test-attachment "b.jpg"))))
        (sgn-test-receive (sgn-test-incoming "second" 2000
                                             :attachments (list (sgn-test-attachment "b.jpg")))))
      (should (= new 2))
      (let ((messages (sgn-test-chat-messages sgn-test-alice-uuid)))
        (should (equal (mapcar (lambda (m) (plist-get m :body)) messages)
                       '("first" "second")))
        (should-not (sgn-db-get-media (plist-get (car messages) :rowid)))
        (should (= (length (sgn-db-get-media (plist-get (cadr messages) :rowid)))
                   1))))))

(ert-deftest sgn-test-store-edits ()
  "Edits apply, older edits are ignored, and early edits wait."
  (sgn-test-with-session
    (let ((edit (lambda (ts body)
                  (sgn-test-envelope
                   :edit `((targetSentTimestamp . 1000)
                           (dataMessage . ,(sgn-test-data-message
                                            :timestamp ts :message body)))))))
      (sgn-test-receive (funcall edit 3000 "early edit"))
      (should-not (sgn-test-chat-messages sgn-test-alice-uuid))
      (sgn-test-receive (sgn-test-incoming "original" 1000))
      (should (equal (sgn-test-body sgn-test-alice-uuid) '("early edit")))
      (sgn-test-receive (funcall edit 2000 "older edit"))
      (should (equal (sgn-test-body sgn-test-alice-uuid) '("early edit")))
      (sgn-test-receive (funcall edit 4000 "newest"))
      (should (equal (sgn-test-body sgn-test-alice-uuid) '("newest"))))))

(ert-deftest sgn-test-store-edit-to-plain-clears-styles ()
  "An edit without styles removes the original's styles."
  (sgn-test-with-session
    (sgn-test-receive (sgn-test-incoming "bold" 1000 :text-styles
                                         '(((style . "BOLD") (start . 0) (length . 4)))))
    (sgn-test-receive (sgn-test-envelope
                       :edit `((targetSentTimestamp . 1000)
                               (dataMessage . ,(sgn-test-data-message
                                                :timestamp 2000 :message "plain")))))
    (should-not (plist-get (car (sgn-test-chat-messages sgn-test-alice-uuid))
                           :styles-json))))

(ert-deftest sgn-test-store-remote-deletes ()
  "Deletes remove content and attachment files, only from the sender,
and a delete that arrives first applies when the message does."
  (sgn-test-with-session
    (let* ((dir (expand-file-name "attachments" sgn-data-directory))
           (file (expand-file-name "pic.jpg" dir)))
      (make-directory dir t)
      (with-temp-file file (insert "x"))
      (sgn-test-receive (sgn-test-incoming "photo" 1000
                                           :attachments (list (sgn-test-attachment "pic.jpg"))))
      ;; Bob cannot delete Alice's message.
      (sgn-test-receive (sgn-test-envelope
                         :number sgn-test-bob-number :uuid sgn-test-bob-uuid
                         :data (sgn-test-data-message :timestamp 1500
                                                      :remote-delete 1000)))
      (should (equal (sgn-test-body sgn-test-alice-uuid) '("photo")))
      (sgn-test-receive (sgn-test-incoming nil 2000 :remote-delete 1000))
      (let ((msg (car (sgn-test-chat-messages sgn-test-alice-uuid))))
        (should (eql (plist-get msg :deleted) 1))
        (should-not (plist-get msg :body))
        (should-not (sgn-db-get-media (plist-get msg :rowid))))
      (should-not (file-exists-p file))
      (sgn-test-receive (sgn-test-incoming nil 4000 :remote-delete 3000))
      (sgn-test-receive (sgn-test-incoming "regret" 3000))
      (should (eql (plist-get (cadr (sgn-test-chat-messages sgn-test-alice-uuid))
                              :deleted)
                   1)))))

(ert-deftest sgn-test-store-read-sync ()
  "Messages read on another device are read here, even if they arrive later."
  (sgn-test-with-session
    (let ((read (lambda (ts)
                  (sgn-test-envelope
                   :number sgn-test-self-number :uuid sgn-test-self-uuid
                   :sync `((readMessages
                            . [((sender . ,sgn-test-alice-number)
                                (senderNumber . ,sgn-test-alice-number)
                                (senderUuid . ,sgn-test-alice-uuid)
                                (timestamp . ,ts))]))))))
      (sgn-test-receive (sgn-test-incoming "a" 1000))
      (should (= (plist-get (sgn-db-get-chat sgn-test-alice-uuid) :unread) 1))
      (sgn-test-receive (funcall read 1000))
      (sgn-test-receive (funcall read 2000))
      (sgn-test-receive (sgn-test-incoming "b" 2000))
      (should (= (plist-get (sgn-db-get-chat sgn-test-alice-uuid) :unread) 0)))))

(ert-deftest sgn-test-store-reactions ()
  "Reactions can precede their message, and older updates lose."
  (sgn-test-with-session
    (let ((react (lambda (ts emoji remove)
                   (sgn-test-incoming
                    nil ts :reaction `((emoji . ,emoji)
                                       (targetAuthorNumber . ,sgn-test-alice-number)
                                       (targetAuthorUuid . ,sgn-test-alice-uuid)
                                       (targetSentTimestamp . 1000)
                                       (isRemove . ,(if remove t :json-false)))))))
      (sgn-test-receive (funcall react 3000 "👍" nil))
      (sgn-test-receive (sgn-test-incoming "hi" 1000))
      (should (equal (sgn-db-get-reactions sgn-test-alice-uuid sgn-test-alice-uuid 1000)
                     `((,sgn-test-alice-uuid "👍"))))
      (sgn-test-receive (funcall react 2000 "👎" nil))
      (should (equal (cadar (sgn-db-get-reactions sgn-test-alice-uuid
                                                  sgn-test-alice-uuid 1000))
                     "👍"))
      (sgn-test-receive (funcall react 4000 "👍" t))
      (should-not (sgn-db-get-reactions sgn-test-alice-uuid sgn-test-alice-uuid 1000)))))

(ert-deftest sgn-test-store-receipts ()
  "Read receipts for our messages are recorded and shown."
  (sgn-test-with-session
    (sgn-store-learn-self sgn-test-self-uuid)
    (sgn-test-receive (sgn-test-sync-sent (sgn-test-data-message :timestamp 5
                                                                 :message "yo")))
    (sgn-test-receive (sgn-test-envelope
                       :receipt '((when . 9) (isDelivery . :json-false)
                                  (isRead . t) (isViewed . :json-false)
                                  (timestamps . [5]))))
    (should (equal (sgn-db-receipt-types 5) '("read")))
    (sgn-chat-get-buffer sgn-test-alice-uuid)
    (should (string-match-p "yo ✓✓" (sgn-test-history sgn-test-alice-uuid)))))

(ert-deftest sgn-test-store-learns-self-and-merges ()
  "Learning this account's ACI rewrites messages stored under its number."
  (sgn-test-with-session
    (sgn-test-receive (sgn-test-sync-sent (sgn-test-data-message :timestamp 5
                                                                 :message "mine")))
    (should (equal sgn-store--self-uuid sgn-test-self-uuid))
    (should (equal (plist-get (car (sgn-test-chat-messages sgn-test-alice-uuid))
                              :sender)
                   sgn-test-self-uuid))))

(ert-deftest sgn-test-store-timers-and-expiry ()
  "Timers update the chat; incoming messages expire once read."
  (sgn-test-with-session
    (sgn-test-receive (sgn-test-incoming nil 900 :expires 60 :expiration-update t))
    (should (eql (plist-get (sgn-db-get-chat sgn-test-alice-uuid) :expiration) 60))
    (sgn-test-receive (sgn-test-incoming "secret" 1000 :expires 60))
    (let ((msg (car (sgn-test-chat-messages sgn-test-alice-uuid))))
      (should-not (plist-get msg :expires-at))
      (sgn-mark-chat-read sgn-test-alice-uuid)
      (setq msg (sgn-db-get-message-by-rowid (plist-get msg :rowid)))
      (should (plist-get msg :expires-at))
      (sgn-db-update-message (plist-get msg :rowid) :expires-at 1)
      (should-not (sgn-test-chat-messages sgn-test-alice-uuid))
      (sgn-store-purge-expired)
      (should (zerop (sgn-db-count-messages sgn-test-alice-uuid))))))

;;;; Database

(ert-deftest sgn-test-db-fts-survives-content-less-rows ()
  "Deleting and editing around attachment-only messages keeps FTS working."
  (sgn-test-with-session
    (dotimes (i 4)
      (sgn-test-receive (sgn-test-incoming (format "text %d" i) (+ 100 i))))
    (dotimes (i 4)
      (sgn-test-receive (sgn-test-incoming nil (+ 200 i)
                                           :attachments (list (sgn-test-attachment
                                                               (format "%d.jpg" i)))))
      (sgn-test-receive (sgn-test-incoming nil (+ 300 i) :remote-delete (+ 200 i))))
    (dolist (msg (sgn-test-chat-messages sgn-test-alice-uuid))
      (sgn-db-update-message (plist-get msg :rowid) :send-status "sent")
      (when (plist-get msg :body)
        (sgn-db-update-message (plist-get msg :rowid) :body "edited")))
    (sgn-db-execute "INSERT INTO messages_fts(messages_fts) VALUES ('integrity-check')")
    (should (= (length (sgn-db-search "edited")) 4))))

(ert-deftest sgn-test-db-search-takes-words-literally ()
  "Punctuation and FTS5 operators in a query are searched for literally."
  (sgn-test-with-session
    (sgn-test-receive (sgn-test-incoming "don't send e-mail, C++ AND more" 1000))
    (dolist (query '("don't" "e-mail" "C++" "AND" "\"unterminated" "x:y"))
      (should (listp (sgn-db-search query))))
    (should (sgn-db-search "don't"))
    (should (sgn-db-search "e-mail"))))

(ert-deftest sgn-test-db-merge-identity ()
  "Merging a number into an ACI folds chats and duplicate messages."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-alice-number "individual")
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (dolist (spec `((,sgn-test-alice-number ,sgn-test-alice-number 1 "old")
                    (,sgn-test-alice-number ,sgn-test-alice-number 2 "both")
                    (,sgn-test-alice-uuid ,sgn-test-alice-uuid 2 "both")))
      (sgn-db-insert-message (list :chat-id (nth 0 spec) :sender (nth 1 spec)
                                   :timestamp (nth 2 spec) :body (nth 3 spec))))
    (sgn-db-set-reaction sgn-test-alice-number sgn-test-alice-number 1
                         sgn-test-bob-number "👍" 1)
    (should (sgn-db-merge-identity sgn-test-alice-number sgn-test-alice-uuid))
    (should-not (sgn-db-chat-type sgn-test-alice-number))
    (should (equal (sgn-test-body sgn-test-alice-uuid) '("old" "both")))
    (should (equal (sgn-db-get-reactions sgn-test-alice-uuid sgn-test-alice-uuid 1)
                   `((,sgn-test-bob-number "👍"))))
    (should-not (sgn-db-merge-identity sgn-test-alice-number sgn-test-alice-uuid))))

(ert-deftest sgn-test-db-migrates-version-2 ()
  "A version 2 database is rebuilt with its data and unread state."
  (let* ((dir (make-temp-file "sgn-test-" t))
         (db (sqlite-open (expand-file-name "sgn.db" dir))))
    (unwind-protect
        (progn
          (dolist (sql
                   '("CREATE TABLE chats (id TEXT PRIMARY KEY, name TEXT,
                       type TEXT NOT NULL, last_msg_ts INTEGER,
                       unread INTEGER DEFAULT 0, muted INTEGER DEFAULT 0,
                       pinned INTEGER DEFAULT 0, draft TEXT,
                       expiration INTEGER DEFAULT 0)"
                     "CREATE TABLE messages (rowid INTEGER PRIMARY KEY AUTOINCREMENT,
                       chat_id TEXT NOT NULL REFERENCES chats(id), sender TEXT NOT NULL,
                       timestamp INTEGER NOT NULL, body TEXT, type TEXT NOT NULL,
                       quote_ts INTEGER, quote_author TEXT, quote_body TEXT,
                       edited_at INTEGER, deleted INTEGER DEFAULT 0,
                       expires_in INTEGER DEFAULT 0, expire_started_at INTEGER,
                       expires_at INTEGER, styles_json TEXT, raw_json TEXT,
                       send_status TEXT, UNIQUE(chat_id, sender, timestamp))"
                     "CREATE VIRTUAL TABLE messages_fts USING fts5(body,
                       content='messages', content_rowid='rowid')"
                     "CREATE TABLE reactions (message_rowid INTEGER, chat_id TEXT NOT NULL,
                       target_author TEXT NOT NULL, target_timestamp INTEGER NOT NULL,
                       sender TEXT NOT NULL, emoji TEXT NOT NULL, removed INTEGER DEFAULT 0,
                       UNIQUE(chat_id, target_author, target_timestamp, sender))"
                     "CREATE TABLE media (id INTEGER PRIMARY KEY AUTOINCREMENT,
                       message_rowid INTEGER NOT NULL, chat_id TEXT NOT NULL,
                       content_type TEXT NOT NULL, file_path TEXT, file_name TEXT,
                       is_voice INTEGER DEFAULT 0, is_sticker INTEGER DEFAULT 0,
                       width INTEGER, height INTEGER)"
                     "CREATE TABLE receipts (message_rowid INTEGER, chat_id TEXT NOT NULL,
                       target_author TEXT NOT NULL, target_timestamp INTEGER NOT NULL,
                       recipient TEXT NOT NULL, type TEXT NOT NULL,
                       received_at INTEGER NOT NULL)"
                     "CREATE TABLE polls (message_rowid INTEGER UNIQUE)"
                     "CREATE TABLE pins (message_rowid INTEGER, chat_id TEXT NOT NULL,
                       target_author TEXT NOT NULL, target_timestamp INTEGER NOT NULL,
                       pinned_by TEXT NOT NULL, pinned_at INTEGER NOT NULL,
                       pin_expires_at INTEGER,
                       PRIMARY KEY(chat_id, target_author, target_timestamp))"
                     "INSERT INTO chats (id, name, type, unread) VALUES
                       ('+1', 'Al', 'individual', 1), ('g', '', 'group', 0)"
                     "INSERT INTO messages (chat_id, sender, timestamp, body, type) VALUES
                       ('+1', '+1', 1, 'one', 'data'), ('+1', '+1', 2, 'two', 'data'),
                       ('+1', '+0', 3, 'mine', 'sync')"
                     "INSERT INTO media (message_rowid, chat_id, content_type, file_path)
                       VALUES (1, '+1', 'image/png', '/x/signal-cli/attachments/a.png'),
                              (2, '+1', 'text/x-signal-plain',
                               '/x/signal-cli/attachments/l.txt')"
                     "INSERT INTO receipts (chat_id, target_author, target_timestamp,
                       recipient, type, received_at)
                       VALUES ('+1', '+0', 3, '+1', 'read', 9)"
                     "INSERT INTO reactions (chat_id, target_author, target_timestamp,
                       sender, emoji, removed) VALUES ('+1', '+1', 1, '+0', '👍', 0)"
                     "PRAGMA user_version = 2"))
            (sqlite-execute db sql))
          (sqlite-close db)
          (setq db nil)
          (let ((sgn-db-directory dir)
                (sgn-db--connection nil))
            (cl-letf (((symbol-function 'sgn--log) #'ignore))
              (sgn-db-init)
              (unwind-protect
                  (progn
                    (should (= (sgn-db--schema-version) 3))
                    (should (equal (mapcar (lambda (m) (list (plist-get m :body)
                                                             (plist-get m :outgoing)
                                                             (and (plist-get m :read-at) t)))
                                           (sgn-db-get-messages "+1"))
                                   '(("one" 0 t) ("two" 0 nil) ("mine" 1 t))))
                    (should (equal (plist-get (car (sgn-db-get-media 1)) :attachment-id)
                                   "a.png"))
                    (should (sgn-db-get-reactions "+1" "+1" 1))
                    (should-not (sgn-db-get-media 2))
                    (should (equal (sgn-db-receipt-types 3) '("read")))
                    (should (directory-files (expand-file-name "backups" dir)
                                             nil "\\`sgn-v2-.*\\.db\\'"))
                    (should-not (plist-get (sgn-db-get-chat "g") :name))
                    (should (sgn-db-search "two")))
                (sgn-db-close)))))
      (when db (sqlite-close db))
      (delete-directory dir t))))

(ert-deftest sgn-test-db-rejects-newer-schema ()
  "A database from a newer sgn is not touched."
  (let* ((dir (make-temp-file "sgn-test-" t))
         (db (sqlite-open (expand-file-name "sgn.db" dir))))
    (sqlite-execute db "PRAGMA user_version = 99")
    (sqlite-close db)
    (unwind-protect
        (let ((sgn-db-directory dir)
              (sgn-db--connection nil))
          (cl-letf (((symbol-function 'sgn--log) #'ignore))
            (should-error (sgn-db-init))
            (should-not sgn-db--connection)))
      (delete-directory dir t))))

;;;; Formatting

(ert-deftest sgn-test-format-parse-markup ()
  "Markup is parsed only at word boundaries, and styles count UTF-16 units."
  (dolist (case '(("hi *there*" "hi there" (("BOLD" 3 5)))
                  ("😀 *bold*" "😀 bold" (("BOLD" 3 4)))
                  ("snake_case_name" "snake_case_name" nil)
                  ("https://example.com/a_b_c" "https://example.com/a_b_c" nil)
                  ("2*3*4 = 24" "2*3*4 = 24" nil)
                  ("**bold**" "**bold**" nil)
                  ("C:\\_dir" "C:\\_dir" nil)
                  ("\\*not bold\\*" "*not bold*" nil)
                  ("`a *b*` _c_" "a *b* c" (("MONOSPACE" 0 5) ("ITALIC" 6 1)))
                  ("||spoiler|| ~gone~" "spoiler gone"
                   (("SPOILER" 0 7) ("STRIKETHROUGH" 8 4)))))
    (let ((parsed (sgn-format-parse-markup (car case))))
      (should (equal (plist-get parsed :text) (nth 1 case)))
      (should (equal (plist-get parsed :styles) (nth 2 case))))))

(ert-deftest sgn-test-format-markup-round-trip ()
  "Writing styles back as markup and parsing it gives the original."
  (dolist (case '(("use _init_ and this" (("BOLD" 15 4)))
                  ("😀 bold" (("ITALIC" 3 4)))))
    (let ((parsed (sgn-format-parse-markup (sgn-format-to-markup (car case)
                                                                 (cadr case)))))
      (should (equal (plist-get parsed :text) (car case)))
      (should (equal (plist-get parsed :styles) (cadr case))))))

(ert-deftest sgn-test-format-render ()
  "Styles land on the right characters after emoji; mentions show names."
  (sgn-test-with-session
    (sgn-contacts-set-name sgn-test-bob-uuid "Bob")
    (let ((text (sgn-format-render "😀 bold ￼!" '(("BOLD" 3 4))
                                   `((,sgn-test-bob-uuid 8 1)))))
      (should (equal (substring-no-properties text) "😀 bold @Bob!"))
      (should (memq 'bold (ensure-list (get-text-property 2 'face text))))
      (should-not (get-text-property 6 'face text))
      (should (eq (get-text-property 7 'face text) 'sgn-mention-face)))))

;;;; Chat buffers

(ert-deftest sgn-test-chat-buffers-are-found-by-id ()
  "Two chats with the same name get separate buffers and their own messages."
  (sgn-test-with-session
    (sgn-contacts-set-name sgn-test-alice-uuid "Sam")
    (sgn-contacts-set-name sgn-test-bob-uuid "Sam")
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (sgn-db-ensure-chat sgn-test-bob-uuid "individual")
    (let ((a (sgn-chat-get-buffer sgn-test-alice-uuid))
          (b (sgn-chat-get-buffer sgn-test-bob-uuid)))
      (should-not (eq a b))
      (sgn-test-receive (sgn-test-envelope
                         :number sgn-test-bob-number :uuid sgn-test-bob-uuid
                         :data (sgn-test-data-message :timestamp 5 :message "from bob")))
      (should (string-match-p "from bob" (sgn-test-history sgn-test-bob-uuid)))
      (should-not (string-match-p "from bob" (sgn-test-history sgn-test-alice-uuid))))))

(ert-deftest sgn-test-chat-follows-renames ()
  "A renamed contact's buffer is renamed and keeps receiving messages."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (let ((buf (sgn-chat-get-buffer sgn-test-alice-uuid)))
      (sgn-contacts--record-contacts
       `(((number . ,sgn-test-alice-number) (uuid . ,sgn-test-alice-uuid)
          (name . "Alice Smith"))))
      (sgn-test-receive (sgn-test-incoming "hello" 1000))
      (should (equal (buffer-name buf) "*sgn: Alice Smith*"))
      (should (string-match-p "hello" (sgn-test-history sgn-test-alice-uuid))))))

(ert-deftest sgn-test-chat-history-in-timestamp-order ()
  "A message delivered late is shown before newer ones."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (sgn-chat-get-buffer sgn-test-alice-uuid)
    (sgn-test-receive (sgn-test-incoming "newer" 2000))
    (sgn-test-receive (sgn-test-incoming "older" 1000))
    (should (string-match-p "older[^z-a]*newer" (sgn-test-history sgn-test-alice-uuid)))))

(ert-deftest sgn-test-chat-redraw-keeps-input-and-undo ()
  "Redrawing the history keeps the input, point within it, and undo."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (with-current-buffer (sgn-chat-get-buffer sgn-test-alice-uuid)
      (buffer-enable-undo)
      (goto-char (point-max))
      (insert "draft")
      (undo-boundary)
      (insert " more")
      (undo-boundary)
      (backward-char 3)
      (sgn-test-receive (sgn-test-incoming "incoming" 1000))
      (should (equal (sgn-chat--input-text) "draft more"))
      (should (equal (buffer-substring (point) (point-max)) "ore"))
      (let ((last-command nil))
        (undo))
      (should (equal (sgn-chat--input-text) "draft")))))

(ert-deftest sgn-test-chat-history-is-read-only ()
  "Neither headers nor message lines can be edited."
  (sgn-test-with-session
    (sgn-test-receive (sgn-test-incoming "hi" 1000))
    (with-current-buffer (sgn-chat-get-buffer sgn-test-alice-uuid)
      (goto-char (point-min))
      (should-error (insert "x") :type 'text-read-only)
      (forward-line 1)
      (should-error (insert "x") :type 'text-read-only))))

(ert-deftest sgn-test-chat-empty-prompt ()
  "With an empty prompt, incoming messages do not become input."
  (sgn-test-with-session
    (let ((sgn-prompt ""))
      (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
      (with-current-buffer (sgn-chat-get-buffer sgn-test-alice-uuid)
        (sgn-test-receive (sgn-test-incoming "hi" 1000))
        (should (equal (sgn-chat--input-text) ""))))))

(defun sgn-test-type-and-send (chat-id text)
  "Type TEXT into CHAT-ID's buffer and send it."
  (with-current-buffer (sgn-chat-get-buffer chat-id)
    (goto-char (point-max))
    (insert text)
    (sgn-chat-send-input)))

(ert-deftest sgn-test-send-status-lifecycle ()
  "A sent message shows its status and takes Signal's timestamp."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (sgn-test-type-and-send sgn-test-alice-uuid "hello")
    (should (string-match-p "hello (sending…)" (sgn-test-history sgn-test-alice-uuid)))
    (sgn-test-reply (sgn-test-send-result 1790000000000))
    (let ((msg (car (sgn-test-chat-messages sgn-test-alice-uuid))))
      (should (equal (plist-get msg :send-status) "sent"))
      (should (equal (plist-get msg :timestamp) 1790000000000)))
    (should-not (string-match-p "sending" (sgn-test-history sgn-test-alice-uuid)))))

(ert-deftest sgn-test-send-failures ()
  "Rejected sends are not sent; lost and timed-out ones are unconfirmed."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (sgn-test-type-and-send sgn-test-alice-uuid "one")
    (sgn-test-error "Untrusted identity")
    (sgn-test-type-and-send sgn-test-alice-uuid "two")
    (sgn-test-type-and-send sgn-test-alice-uuid "three")
    (sgn-test-run-deferred t)
    (sgn-rpc-stop)
    (sgn-test-run-deferred)
    (let ((history (sgn-test-history sgn-test-alice-uuid)))
      (should (string-match-p "one (not sent)" history))
      (should (string-match-p "two (not confirmed" history))
      (should (string-match-p "three (not confirmed" history)))))

(ert-deftest sgn-test-send-late-confirmation ()
  "A confirmation after the timeout still marks the message sent."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (sgn-test-type-and-send sgn-test-alice-uuid "slow")
    (sgn-test-run-deferred t)
    (should (equal (plist-get (car (sgn-test-chat-messages sgn-test-alice-uuid))
                              :send-status)
                   "unconfirmed"))
    (sgn-test-reply (sgn-test-send-result 1790000000000))
    (should (equal (plist-get (car (sgn-test-chat-messages sgn-test-alice-uuid))
                              :send-status)
                   "sent"))))

(ert-deftest sgn-test-send-wire-format ()
  "Styles go out as start:length:STYLE strings in UTF-16 units; groups by groupId."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-group "group")
    (sgn-test-type-and-send sgn-test-group "😀 *bold*")
    (let ((params (sgn-test-last-request)))
      (should (equal (alist-get 'groupId params) sgn-test-group))
      (should-not (alist-get 'recipient params))
      (should (equal (alist-get 'message params) "😀 bold"))
      (should (equal (alist-get 'textStyle params) '("3:4:BOLD"))))))

(ert-deftest sgn-test-send-keeps-input-when-not-running ()
  "If sgn cannot start, the input is kept and nothing is stored."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (setq sgn-rpc--failure "signal-cli exited abnormally")
    (cl-letf (((symbol-function 'sgn-rpc-alive-p) #'ignore))
      (should-error (sgn-test-type-and-send sgn-test-alice-uuid "keep me")
                    :type 'user-error))
    (with-current-buffer (sgn-chat-buffer sgn-test-alice-uuid)
      (should (equal (sgn-chat--input-text) "keep me")))
    (should-not (sgn-test-chat-messages sgn-test-alice-uuid))))

(ert-deftest sgn-test-reply-quotes-the-message ()
  "A reply sends the quoted message's Signal timestamp and author."
  (sgn-test-with-session
    (sgn-test-receive (sgn-test-incoming "question" 1000))
    (with-current-buffer (sgn-chat-get-buffer sgn-test-alice-uuid)
      (goto-char (point-min))
      (forward-line 1)
      (sgn-reply)
      (goto-char (point-max))
      (insert "answer")
      (sgn-chat-send-input))
    (let ((params (sgn-test-last-request)))
      (should (equal (alist-get 'quoteTimestamp params) 1000))
      (should (equal (alist-get 'quoteAuthor params) sgn-test-alice-uuid)))))

(ert-deftest sgn-test-edit-updates-locally-and-restores-on-failure ()
  "Our edit shows at once, is sent with editTimestamp, and is undone if rejected."
  (sgn-test-with-session
    (sgn-store-learn-self sgn-test-self-uuid)
    (sgn-test-receive (sgn-test-sync-sent (sgn-test-data-message
                                           :timestamp 5 :message "tpyo")))
    (with-current-buffer (sgn-chat-get-buffer sgn-test-alice-uuid)
      (goto-char (point-min))
      (forward-line 1)
      (sgn-edit)
      (should (equal (sgn-chat--input-text) "tpyo"))
      (sgn-chat--set-input "typo")
      (sgn-chat-send-input))
    (should (equal (alist-get 'editTimestamp (sgn-test-last-request)) 5))
    (should (equal (sgn-test-body sgn-test-alice-uuid) '("typo")))
    (sgn-test-error "nope")
    (should (equal (sgn-test-body sgn-test-alice-uuid) '("tpyo")))))

(ert-deftest sgn-test-actions-refuse-unconfirmed-targets ()
  "Reacting to a message without a confirmed timestamp is refused."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (sgn-test-type-and-send sgn-test-alice-uuid "pending")
    (with-current-buffer (sgn-chat-buffer sgn-test-alice-uuid)
      (sgn-test-run-deferred)
      (goto-char (point-min))
      (forward-line 1)
      (should-error (sgn-react) :type 'user-error))))

(ert-deftest sgn-test-reaction-rolls-back-on-error ()
  "A rejected reaction is removed again."
  (sgn-test-with-session
    (sgn-test-receive (sgn-test-incoming "nice" 1000))
    (with-current-buffer (sgn-chat-get-buffer sgn-test-alice-uuid)
      (goto-char (point-min))
      (forward-line 1)
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "👍 thumbs up")))
        (sgn-react)))
    (should (sgn-db-get-reactions sgn-test-alice-uuid sgn-test-alice-uuid 1000))
    (should (equal (alist-get 'targetAuthor (sgn-test-last-request))
                   sgn-test-alice-uuid))
    (sgn-test-error "nope")
    (should-not (sgn-db-get-reactions sgn-test-alice-uuid sgn-test-alice-uuid 1000))))

(ert-deftest sgn-test-mark-read-sends-receipts ()
  "Reading a chat marks its messages read and sends receipts to their senders."
  (sgn-test-with-session
    (sgn-test-receive (sgn-test-incoming "a" 1000))
    (sgn-test-receive (sgn-test-incoming "b" 2000))
    (setq sgn-test-sent nil)
    (sgn-mark-chat-read sgn-test-alice-uuid)
    (should (zerop (plist-get (sgn-db-get-chat sgn-test-alice-uuid) :unread)))
    (let ((params (sgn-test-last-request)))
      (should (equal (alist-get 'recipient params) sgn-test-alice-uuid))
      (should (equal (sort (alist-get 'targetTimestamp params) #'<) '(1000 2000))))))

(defun sgn-test-typing-sent ()
  "Return the typing requests sent, oldest first, as `start' or `stop'."
  (mapcar (lambda (r) (if (alist-get 'stop (alist-get 'params r)) 'stop 'start))
          (reverse (cl-remove-if-not
                    (lambda (r) (equal (alist-get 'method r) "sendTyping"))
                    sgn-test-sent))))

(ert-deftest sgn-test-typing-indicators-sent ()
  "Typing sends indicators; restoring a draft and sending do not start one."
  (sgn-test-with-session
    (let ((sgn-send-typing t))
      (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
      (sgn-db-save-draft sgn-test-alice-uuid "saved")
      (with-current-buffer (sgn-chat-get-buffer sgn-test-alice-uuid)
        (should (equal (sgn-chat--input-text) "saved"))
        (should-not (sgn-test-typing-sent))
        (goto-char (point-max))
        (insert "!")
        (should (equal (sgn-test-typing-sent) '(start)))
        (sgn-chat-send-input)
        (should (equal (sgn-test-typing-sent) '(start stop)))
        (should-not (sgn-db-get-draft sgn-test-alice-uuid))))))

(ert-deftest sgn-test-typing-display-per-sender ()
  "One member stopping does not hide another who is still typing."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-group "group")
    (sgn-contacts-set-name sgn-test-alice-uuid "Alice")
    (sgn-contacts-set-name sgn-test-bob-uuid "Bob")
    (with-current-buffer (sgn-chat-get-buffer sgn-test-group)
      (sgn-chat-on-typing sgn-test-group sgn-test-alice-uuid t)
      (sgn-chat-on-typing sgn-test-group sgn-test-bob-uuid t)
      (sgn-chat-on-typing sgn-test-group sgn-test-alice-uuid nil)
      (should (string-match-p "Bob is typing" header-line-format))
      (should-not (string-match-p "Alice" header-line-format)))))

(ert-deftest sgn-test-delete-unsent-message-locally ()
  "A message that never went out can be deleted from sgn."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (sgn-test-type-and-send sgn-test-alice-uuid "stuck")
    (sgn-test-run-deferred t)
    (with-current-buffer (sgn-chat-buffer sgn-test-alice-uuid)
      (sgn-test-run-deferred)
      (goto-char (point-min))
      (forward-line 1)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (sgn-delete)))
    (should-not (sgn-test-chat-messages sgn-test-alice-uuid))
    (should-not (cl-find "remoteDelete" sgn-test-sent
                         :key (lambda (r) (alist-get 'method r)) :test #'equal))))

(ert-deftest sgn-test-chat-rename-to-prefix ()
  "A rename to a prefix of the old name still renames the buffer."
  (sgn-test-with-session
    (sgn-contacts-set-name sgn-test-alice-uuid "Bobby")
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (let ((buf (sgn-chat-get-buffer sgn-test-alice-uuid)))
      (sgn-contacts-set-name sgn-test-alice-uuid "Bob")
      (with-current-buffer buf (sgn-chat--redraw))
      (should (equal (buffer-name buf) "*sgn: Bob*")))))

(ert-deftest sgn-test-abandoned-edit-keeps-message-usable ()
  "An edit lost when signal-cli stops leaves the message targetable."
  (sgn-test-with-session
    (sgn-store-learn-self sgn-test-self-uuid)
    (sgn-test-receive (sgn-test-sync-sent (sgn-test-data-message
                                           :timestamp 5 :message "one")))
    (let ((msg (car (sgn-test-chat-messages sgn-test-alice-uuid))))
      (sgn-send-edit msg "two")
      (sgn-rpc-stop)
      (setq msg (car (sgn-test-chat-messages sgn-test-alice-uuid)))
      (should (equal (plist-get msg :body) "two"))
      (should (sgn-actions--targetable msg)))))

(ert-deftest sgn-test-empty-prompt-keeps-typed-input ()
  "With an empty prompt, text typed before a redraw stays input."
  (sgn-test-with-session
    (let ((sgn-prompt ""))
      (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
      (with-current-buffer (sgn-chat-get-buffer sgn-test-alice-uuid)
        (goto-char (point-max))
        (insert "my draft")
        (sgn-test-receive (sgn-test-incoming "hi" 1000))
        (should (equal (sgn-chat--input-text) "my draft"))
        (should (string-match-p "hi" (sgn-test-history sgn-test-alice-uuid)))))))

(ert-deftest sgn-test-edit-sets-aside-the-input ()
  "Editing keeps what was being typed, for after the edit or its cancellation."
  (sgn-test-with-session
    (sgn-store-learn-self sgn-test-self-uuid)
    (sgn-test-receive (sgn-test-sync-sent (sgn-test-data-message
                                           :timestamp 5 :message "old")))
    (with-current-buffer (sgn-chat-get-buffer sgn-test-alice-uuid)
      (goto-char (point-max))
      (insert "half-written")
      (goto-char (point-min))
      (forward-line 1)
      (sgn-edit)
      (should (equal (sgn-chat--input-text) "old"))
      (sgn-chat-cancel-action)
      (should (equal (sgn-chat--input-text) "half-written"))
      (insert "!")
      (should (equal (sgn-chat--input-text) "half-written!"))
      (goto-char (point-min))
      (forward-line 1)
      (sgn-edit)
      (sgn-chat--set-input "new")
      (sgn-chat-send-input)
      (should (equal (sgn-chat--input-text) "half-written!")))))

(ert-deftest sgn-test-cancel-reply-leaves-point-in-input ()
  "After cancelling a reply, typing goes into the input."
  (sgn-test-with-session
    (sgn-test-receive (sgn-test-incoming "q" 1000))
    (with-current-buffer (sgn-chat-get-buffer sgn-test-alice-uuid)
      (goto-char (point-min))
      (forward-line 1)
      (sgn-reply)
      (sgn-chat-cancel-action)
      (insert "typed")
      (should (equal (sgn-chat--input-text) "typed")))))

(ert-deftest sgn-test-number-buffer-follows-merge ()
  "A chat opened by phone number keeps its buffer once merged into the ACI."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-alice-number "individual")
    (let ((buf (sgn-chat-get-buffer sgn-test-alice-number)))
      (sgn-test-type-and-send sgn-test-alice-number "hi there")
      (sgn-test-reply (sgn-test-send-result 1500))
      (sgn-test-receive (sgn-test-incoming "hello back" 2000))
      (should (eq (sgn-chat-buffer sgn-test-alice-uuid) buf))
      (should (equal (buffer-local-value 'sgn-chat-id buf) sgn-test-alice-uuid))
      (let ((history (sgn-test-history sgn-test-alice-uuid)))
        (should (string-match-p "hi there" history))
        (should (string-match-p "hello back" history))))))

(ert-deftest sgn-test-stop-with-pending-send-is-quiet ()
  "Stopping sgn while a send is pending causes no error afterwards."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-alice-uuid "individual")
    (sgn-chat-on-typing sgn-test-alice-uuid sgn-test-alice-uuid t)
    (sgn-test-type-and-send sgn-test-alice-uuid "in flight")
    (cl-letf (((symbol-function 'sgn-global-mode) #'ignore))
      (sgn-stop))
    (sgn-test-run-deferred t)
    (should-not sgn-db--connection)
    (sgn-db-init)))

(ert-deftest sgn-test-interrupted-edit-leaves-message-sent ()
  "An edit interrupted by a restart is unconfirmed; the message stays sent."
  (sgn-test-with-session
    (sgn-store-learn-self sgn-test-self-uuid)
    (sgn-test-receive (sgn-test-sync-sent (sgn-test-data-message
                                           :timestamp 5 :message "one")))
    (sgn-send-edit (car (sgn-test-chat-messages sgn-test-alice-uuid)) "two")
    (sgn-db-settle-interrupted-sends)
    (let ((msg (car (sgn-test-chat-messages sgn-test-alice-uuid))))
      (should (equal (plist-get msg :edit-status) "unconfirmed"))
      (should-not (plist-get msg :send-status))
      (should (sgn-actions--targetable msg)))
    (sgn-chat-get-buffer sgn-test-alice-uuid)
    (should (string-match-p "two (edited) (edit not confirmed)"
                            (sgn-test-history sgn-test-alice-uuid)))))

(ert-deftest sgn-test-undecryptable-message-is-shown ()
  "A message signal-cli could not decrypt is shown, not dropped."
  (sgn-test-with-session
    (sgn-rpc--handle-line
     (json-encode `((jsonrpc . "2.0") (method . "receive")
                    (params . ((exception . ((message . "Untrusted identity")
                                             (type . "UntrustedIdentityException")))
                               (envelope . ,(sgn-test-envelope :timestamp 7000))
                               (account . ,sgn-test-self-number))))))
    (should (string-match-p "could not be read: Untrusted identity"
                            (car (sgn-test-body sgn-test-alice-uuid))))))

(ert-deftest sgn-test-group-members-are-merged ()
  "A group member's number is merged into their ACI from the group list."
  (sgn-test-with-session
    (sgn-db-ensure-chat sgn-test-group "group")
    (sgn-db-insert-message (list :chat-id sgn-test-group :sender sgn-test-bob-number
                                 :timestamp 1 :body "hi"))
    (sgn-contacts--record-groups
     `(((id . ,sgn-test-group) (name . "G")
        (members . (((number . ,sgn-test-bob-number) (uuid . ,sgn-test-bob-uuid)
                     (isAdmin . nil))
                    ((number . ,sgn-test-self-number) (uuid . ,sgn-test-self-uuid)
                     (isAdmin . nil)))))))
    (should (equal (plist-get (car (sgn-test-chat-messages sgn-test-group)) :sender)
                   sgn-test-bob-uuid))
    (should (equal sgn-store--self-uuid sgn-test-self-uuid))))

;;;; Dashboard, notifications, search

(ert-deftest sgn-test-dashboard-lists-chats ()
  "The dashboard shows unnamed groups and previews with mentions resolved."
  (sgn-test-with-session
    (sgn-test-receive (sgn-test-incoming "hi" 1000 :group sgn-test-group))
    (with-current-buffer (get-buffer-create sgn-dashboard--buffer-name)
      (unwind-protect
          (progn
            (sgn-dashboard-mode)
            (sgn-dashboard--populate)
            (should (string-match-p "Unnamed group" (buffer-string)))
            (should (string-match-p "Alice: hi" (buffer-string))))
        (kill-buffer)))))

(ert-deftest sgn-test-notifications-coalesce ()
  "A backlog makes one notification per chat; read messages make none."
  (sgn-test-with-session
    (let ((sgn-desktop-notifications t))
      (cl-letf (((symbol-function 'sgn-chat-visible-p) #'ignore))
        (sgn-test-receive (sgn-test-incoming "one" 1000))
        (sgn-test-receive (sgn-test-incoming "two" 2000))
        (sgn-test-run-deferred t)
        (should (equal sgn-test-notifications '(("Alice" . "2 new messages"))))
        (setq sgn-test-notifications nil)
        (sgn-test-receive (sgn-test-incoming "three" 3000))
        (sgn-mark-chat-read sgn-test-alice-uuid)
        (sgn-test-run-deferred t)
        (should-not sgn-test-notifications)))))

(ert-deftest sgn-test-notification-conceals-spoilers ()
  "Spoiler text does not appear in notifications."
  (sgn-test-with-session
    (let ((sgn-desktop-notifications t))
      (cl-letf (((symbol-function 'sgn-chat-visible-p) #'ignore))
        (sgn-test-receive (sgn-test-incoming "the end" 1000 :text-styles
                                             '(((style . "SPOILER") (start . 4)
                                                (length . 3)))))
        (sgn-test-run-deferred t)
        (should (equal (cdar sgn-test-notifications) "the ▒▒▒"))))))

(ert-deftest sgn-test-search-navigation ()
  "n and p move between results; refresh keeps the chat scope."
  (sgn-test-with-session
    (dotimes (i 3)
      (sgn-test-receive (sgn-test-incoming (format "word %d" i) (+ 1000 i))))
    (sgn-test-receive (sgn-test-envelope
                       :number sgn-test-bob-number :uuid sgn-test-bob-uuid
                       :data (sgn-test-data-message :timestamp 5000 :message "word")))
    (cl-letf (((symbol-function 'switch-to-buffer) #'ignore))
      (sgn-search--run "word" sgn-test-alice-uuid)
      (with-current-buffer "*sgn Search*"
        (let ((first (point)))
          (sgn-search-next-result)
          (sgn-search-next-result)
          (sgn-search-prev-result)
          (sgn-search-prev-result)
          (should (= (point) first)))
        (sgn-search-refresh)
        (should-not (string-match-p "Bob" (buffer-string)))))))

;;;; Media

(ert-deftest sgn-test-media-paths-are-confined ()
  "Attachment IDs cannot name files outside the attachments directory."
  (let ((sgn-data-directory (make-temp-file "sgn-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "attachments" sgn-data-directory))
          (with-temp-file (expand-file-name "attachments/ok.jpg" sgn-data-directory))
          (should (sgn-media-path '(:attachment-id "ok.jpg")))
          (should-not (sgn-media-path '(:attachment-id "..")))
          (should-not (sgn-media-path '(:attachment-id "../x")))
          (should-not (sgn-media-path '(:attachment-id "."))))
      (delete-directory sgn-data-directory t))))

(ert-deftest sgn-test-media-render-survives-bad-files ()
  "A file that is not an image renders as a placeholder, not an error."
  (let ((sgn-data-directory (make-temp-file "sgn-test-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "attachments" sgn-data-directory))
          (with-temp-file (expand-file-name "attachments/bad.jpg" sgn-data-directory)
            (insert "not an image"))
          (with-temp-buffer
            (cl-letf (((symbol-function 'sgn--log) #'ignore))
              (sgn-media-render '(:attachment-id "bad.jpg" :content-type "image/jpeg")))
            (should (string-match-p "\\[" (buffer-string)))))
      (delete-directory sgn-data-directory t))))

;;;; Import

(ert-deftest sgn-test-import-from-export ()
  "Desktop messages, quotes and reactions import once; expired ones never."
  (sgn-test-with-session
    (sgn-store-learn-self sgn-test-self-uuid)
    (let* ((file (make-temp-file "sgn-export-" nil ".db"))
           (db (sqlite-open file)))
      (unwind-protect
          (progn
            (dolist (sql
                     `("CREATE TABLE conversations (id, type, name, profileFullName,
                         e164, serviceId, groupId)"
                       "CREATE TABLE messages (sent_at, type, sourceServiceId, body,
                         conversationId, expireTimer, expirationStartTimestamp, json)"
                       ,(format "INSERT INTO conversations VALUES
                          ('c1', 'private', 'Alice', NULL, '%s', '%s', NULL),
                          ('c0', 'private', NULL, NULL, '%s', '%s', NULL)"
                                sgn-test-alice-number sgn-test-alice-uuid
                                sgn-test-self-number sgn-test-self-uuid)))
              (sqlite-execute db sql))
            (sqlite-execute
             db "INSERT INTO messages VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
             (list 1000 "incoming" sgn-test-alice-uuid "hello ￼" "c1" 0 nil
                   (json-encode `((reactions . [((emoji . "❤️") (fromId . "c0")
                                                 (targetTimestamp . 1000)
                                                 (timestamp . 1100))])
                                  (bodyRanges . [((start . 6) (length . 1)
                                                  (mentionAci . ,sgn-test-self-uuid))])))))
            (sqlite-execute
             db "INSERT INTO messages VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
             (list 2000 "outgoing" nil "reply" "c1" 0 nil
                   (json-encode `((quote . ((id . 1000)
                                            (authorAci . ,sgn-test-alice-uuid)
                                            (text . "hello")))))))
            (should (equal (sgn-import--import db) '(2 . 1)))
            (should (equal (sgn-import--import db) '(0 . 0)))
            (let ((messages (sgn-test-chat-messages sgn-test-alice-uuid)))
              (should (equal (mapcar (lambda (m) (plist-get m :body)) messages)
                             '("hello ￼" "reply")))
              (should (equal (sgn-format-read-ranges
                              (plist-get (car messages) :mentions-json))
                             `((,sgn-test-self-uuid 6 1))))
              (should (equal (plist-get (cadr messages) :sender) sgn-test-self-uuid))
              (should (equal (plist-get (cadr messages) :quote-author)
                             sgn-test-alice-uuid)))
            (should (equal (sgn-db-get-reactions sgn-test-alice-uuid
                                                 sgn-test-alice-uuid 1000)
                           `((,sgn-test-self-uuid "❤️")))))
        (sqlite-close db)
        (delete-file file)))))

(provide 'sgn-test)
;;; sgn-test.el ends here
