;;; sgn-test.el --- Tests for sgn  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT test suite for sgn, covering: pure utilities, process
;; filter/dispatch, callback machinery, contact population, buffer
;; management, SQLite persistence, formatting, and actions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sgn)

;;;; Test helpers

(defmacro sgn-test-with-clean-state (&rest body)
  "Run BODY with all sgn mutable state reset to fresh defaults."
  (declare (indent 0) (debug body))
  `(let ((sgn-rpc--partial-line "")
         (sgn-rpc--id-counter 0)
         (sgn-rpc--pending-callbacks (make-hash-table :test 'equal))
         (sgn-rpc--request-methods (make-hash-table :test 'equal))
         (sgn-rpc--request-params (make-hash-table :test 'equal))
         (sgn-rpc--retried-ids (make-hash-table :test 'equal))
         (sgn-contacts--cache (make-hash-table :test 'equal))
         (sgn-account "+15550000000"))
     ,@body))

(defmacro sgn-test-with-db (&rest body)
  "Run BODY with a fresh temporary SQLite database."
  (declare (indent 0) (debug body))
  `(let* ((sgn-db-directory (make-temp-file "sgn-test-" t))
          (sgn-db--connection nil)
          (sgn-account "+15550000000"))
     (unwind-protect
         (progn
           (sgn-db-init)
           ,@body)
       (sgn-db-close)
       (delete-directory sgn-db-directory t))))

(defmacro sgn-test-with-chat-buffer (id &rest body)
  "Run BODY in a fresh chat buffer for ID, cleaning up afterward."
  (declare (indent 1) (debug body))
  (let ((buf (gensym "buf"))
        (dbdir (gensym "dbdir")))
    `(let* ((,dbdir (make-temp-file "sgn-test-" t))
            (sgn-db-directory ,dbdir)
            (sgn-db--connection nil)
            (sgn-rpc--partial-line "")
            (sgn-rpc--id-counter 0)
            (sgn-rpc--pending-callbacks (make-hash-table :test 'equal))
            (sgn-rpc--request-methods (make-hash-table :test 'equal))
            (sgn-rpc--request-params (make-hash-table :test 'equal))
            (sgn-rpc--retried-ids (make-hash-table :test 'equal))
            (sgn-contacts--cache (make-hash-table :test 'equal))
            (sgn-account "+15550000000")
            (,buf nil))
       (unwind-protect
           (progn
             (sgn-db-init)
             (setq ,buf (get-buffer-create (format "*sgn: %s*" ,id)))
             (with-current-buffer ,buf
               (sgn-chat-mode)
               (setq sgn-chat-id ,id)
               (sgn-chat--draw-prompt)
               ,@body))
         (when (buffer-live-p ,buf) (kill-buffer ,buf))
         (sgn-db-close)
         (delete-directory ,dbdir t)))))

;;;; Tier 1 — Pure utility functions

(ert-deftest sgn-test-ensure-list-vector ()
  "Vectors are coerced to lists."
  (should (equal (sgn--ensure-list [1 2 3]) '(1 2 3))))

(ert-deftest sgn-test-ensure-list-already-list ()
  "Lists pass through unchanged."
  (should (equal (sgn--ensure-list '(1 2 3)) '(1 2 3))))

(ert-deftest sgn-test-ensure-list-nil ()
  "nil passes through as nil."
  (should (null (sgn--ensure-list nil))))

(ert-deftest sgn-test-ensure-list-empty-vector ()
  "Empty vector becomes nil (empty list)."
  (should (null (sgn--ensure-list []))))

(ert-deftest sgn-test-is-group-id-phone ()
  "Phone numbers are not group IDs."
  (should-not (sgn--is-group-id "+15550000000")))

(ert-deftest sgn-test-is-group-id-uuid ()
  "UUIDs are not group IDs."
  (should-not (sgn--is-group-id "a1b2c3d4-e5f6-7890-abcd-ef1234567890")))

(ert-deftest sgn-test-is-group-id-base64 ()
  "Base64 strings are group IDs."
  (should (sgn--is-group-id "dGVzdGdyb3VwaWQ9")))

;;;; Tier 2 — RPC process filter and dispatch

(ert-deftest sgn-test-process-filter-complete-line ()
  "A complete JSON line is dispatched."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn-rpc--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn-rpc--process-filter
         nil "{\"method\":\"receive\",\"params\":{}}\n")
        (should (= (length dispatched) 1))
        (should (equal (alist-get 'method (car dispatched))
                       "receive"))))))

(ert-deftest sgn-test-process-filter-partial-then-complete ()
  "Partial lines are buffered until newline arrives."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn-rpc--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn-rpc--process-filter nil "{\"id\":1,\"result\":")
        (should (null dispatched))
        (sgn-rpc--process-filter nil "\"ok\"}\n")
        (should (= (length dispatched) 1))
        (should (equal (alist-get 'result (car dispatched))
                       "ok"))))))

(ert-deftest sgn-test-process-filter-two-lines ()
  "Two complete lines dispatch twice."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn-rpc--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn-rpc--process-filter
         nil
         (concat "{\"id\":1,\"result\":\"a\"}\n"
                 "{\"id\":2,\"result\":\"b\"}\n"))
        (should (= (length dispatched) 2))))))

(ert-deftest sgn-test-process-filter-skips-non-json ()
  "Non-JSON lines are silently skipped."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn-rpc--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn-rpc--process-filter nil "some log output\n")
        (should (null dispatched))))))

(ert-deftest sgn-test-process-filter-overflow ()
  "Buffer is cleared when it exceeds max length."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn-rpc--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn-rpc--process-filter
         nil (make-string (1+ sgn-rpc--max-partial-line-length) ?x))
        (should (string-empty-p sgn-rpc--partial-line))
        (sgn-rpc--process-filter nil "{\"id\":99,\"result\":\"ok\"}\n")
        (should (= (length dispatched) 1))))))

(ert-deftest sgn-test-process-filter-malformed-json ()
  "Malformed JSON does not crash."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn-rpc--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn-rpc--process-filter nil "{not valid json}\n")
        (should (null dispatched))))))

;;;; Tier 3 — Dispatch routing

(ert-deftest sgn-test-dispatch-receive ()
  "Method \"receive\" calls the receive handler."
  (sgn-test-with-clean-state
    (let* ((received nil)
           (sgn-rpc-receive-handler
            (lambda (params) (setq received params))))
      (sgn-rpc--dispatch '((method . "receive")
                           (params . ((envelope . t)))))
      (should (equal received '((envelope . t)))))))

(ert-deftest sgn-test-dispatch-error ()
  "Error objects route to handle-error."
  (sgn-test-with-clean-state
    (let ((error-args nil))
      (cl-letf (((symbol-function 'sgn-rpc--handle-error)
                 (lambda (id err) (setq error-args (list id err)))))
        (sgn-rpc--dispatch '((id . 5)
                             (error . ((message . "boom")))))
        (should (equal (car error-args) 5))
        (should (equal (alist-get 'message (cadr error-args)) "boom"))))))

(ert-deftest sgn-test-dispatch-result ()
  "ID + result invokes the callback."
  (sgn-test-with-clean-state
    (let ((result-args nil))
      (cl-letf (((symbol-function 'sgn-rpc--handle-result)
                 (lambda (id result) (setq result-args (list id result)))))
        (sgn-rpc--dispatch '((id . 3) (result . "ok")))
        (should (equal result-args '(3 "ok")))))))

;;;; Tier 4 — Callback and error machinery

(ert-deftest sgn-test-handle-result-invokes-callback ()
  "Stored callback is invoked with the result value."
  (sgn-test-with-clean-state
    (let ((received-result nil))
      (puthash 42 (lambda (r) (setq received-result r))
               sgn-rpc--pending-callbacks)
      (puthash 42 "send" sgn-rpc--request-methods)
      (sgn-rpc--handle-result 42 '((status . "ok")))
      (should (equal (alist-get 'status received-result) "ok"))
      (should-not (gethash 42 sgn-rpc--pending-callbacks))
      (should-not (gethash 42 sgn-rpc--request-methods)))))

(ert-deftest sgn-test-handle-result-no-callback ()
  "Without a callback, handle-result still cleans up."
  (sgn-test-with-clean-state
    (puthash 10 "send" sgn-rpc--request-methods)
    (sgn-rpc--handle-result 10 "ok")
    (should-not (gethash 10 sgn-rpc--request-methods))))

(ert-deftest sgn-test-handle-error-cleans-up ()
  "Error handling removes entries from all maps."
  (sgn-test-with-clean-state
    (puthash 7 (lambda (_) nil) sgn-rpc--pending-callbacks)
    (puthash 7 "send" sgn-rpc--request-methods)
    (puthash 7 '((message . "hi")) sgn-rpc--request-params)
    (cl-letf (((symbol-function 'sgn--log) #'ignore))
      (sgn-rpc--handle-error 7 '((code . -1) (message . "user error"))))
    (should-not (gethash 7 sgn-rpc--pending-callbacks))
    (should-not (gethash 7 sgn-rpc--request-methods))
    (should-not (gethash 7 sgn-rpc--request-params))))

(ert-deftest sgn-test-error-classification-retryable ()
  "I/O (-3) and rate-limit (-5) codes are retryable."
  (should (sgn-rpc--retryable-error-p -3))
  (should (sgn-rpc--retryable-error-p -5)))

(ert-deftest sgn-test-error-classification-permanent ()
  "User error (-1) and invalid params (-32602) are not retryable."
  (should-not (sgn-rpc--retryable-error-p -1))
  (should-not (sgn-rpc--retryable-error-p -32602)))

;;;; Tier 5 — Address builder

(ert-deftest sgn-test-address-phone ()
  "Phone number produces recipient key."
  (let ((addr (sgn-rpc--build-address "+15550000000")))
    (should (equal (alist-get 'recipient addr) ["+15550000000"]))
    (should-not (alist-get 'groupId addr))))

(ert-deftest sgn-test-address-group ()
  "Group ID produces groupId key."
  (let ((addr (sgn-rpc--build-address "dGVzdGdyb3Vw")))
    (should (equal (alist-get 'groupId addr) "dGVzdGdyb3Vw"))
    (should-not (alist-get 'recipient addr))))

(ert-deftest sgn-test-address-uuid ()
  "UUID produces recipient key (not group)."
  (let ((addr (sgn-rpc--build-address "a1b2c3d4-e5f6-7890-abcd-ef1234567890")))
    (should (alist-get 'recipient addr))
    (should-not (alist-get 'groupId addr))))

;;;; Tier 6 — SQLite persistence

(ert-deftest sgn-test-db-init-and-close ()
  "Database initializes and closes without error."
  (sgn-test-with-db
    (should sgn-db--connection)))

(ert-deftest sgn-test-db-chat-upsert-and-get ()
  "Insert and retrieve a chat."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (let ((chat (sgn-db-get-chat "+1555")))
      (should chat)
      (should (equal (plist-get chat :name) "Alice"))
      (should (equal (plist-get chat :type) "individual")))))

(ert-deftest sgn-test-db-chat-update ()
  "Updating a chat preserves existing fields."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (sgn-db-upsert-chat "+1555" :unread 3)
    (let ((chat (sgn-db-get-chat "+1555")))
      (should (equal (plist-get chat :name) "Alice"))
      (should (equal (plist-get chat :unread) 3)))))

(ert-deftest sgn-test-db-chat-list-sorted ()
  "Chats are returned sorted by pinned then last_msg_ts."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1001" :name "Old" :type "individual" :last-msg-ts 100)
    (sgn-db-upsert-chat "+1002" :name "New" :type "individual" :last-msg-ts 200)
    (sgn-db-upsert-chat "+1003" :name "Pinned" :type "individual"
                        :last-msg-ts 50 :pinned 1)
    (let ((chats (sgn-db-get-chats)))
      (should (= (length chats) 3))
      ;; Pinned first, then newest
      (should (equal (plist-get (nth 0 chats) :name) "Pinned"))
      (should (equal (plist-get (nth 1 chats) :name) "New"))
      (should (equal (plist-get (nth 2 chats) :name) "Old")))))

(ert-deftest sgn-test-db-message-insert-and-get ()
  "Insert and retrieve a message."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (let ((rowid (sgn-db-insert-message
                  (list :chat-id "+1555"
                        :sender "+1555"
                        :timestamp 1000
                        :body "Hello"
                        :type "data"))))
      (should rowid)
      (let ((msg (sgn-db-get-message-by-rowid rowid)))
        (should msg)
        (should (equal (plist-get msg :body) "Hello"))
        (should (equal (plist-get msg :sender) "+1555"))))))

(ert-deftest sgn-test-db-message-get-by-triple ()
  "Retrieve a message by chat_id + sender + timestamp."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (sgn-db-insert-message
     (list :chat-id "+1555" :sender "+1555" :timestamp 2000
           :body "Test" :type "data"))
    (let ((msg (sgn-db-get-message "+1555" "+1555" 2000)))
      (should msg)
      (should (equal (plist-get msg :body) "Test")))))

(ert-deftest sgn-test-db-message-pagination ()
  "Messages are returned in chronological order with limit."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (dotimes (i 10)
      (sgn-db-insert-message
       (list :chat-id "+1555" :sender "+1555"
             :timestamp (+ 1000 i)
             :body (format "msg-%d" i) :type "data")))
    (let ((msgs (sgn-db-get-messages "+1555" 3)))
      (should (= (length msgs) 3))
      ;; Most recent 3, in chronological order
      (should (equal (plist-get (nth 0 msgs) :body) "msg-7"))
      (should (equal (plist-get (nth 1 msgs) :body) "msg-8"))
      (should (equal (plist-get (nth 2 msgs) :body) "msg-9")))))

(ert-deftest sgn-test-db-message-update ()
  "Update message fields."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (let ((rowid (sgn-db-insert-message
                  (list :chat-id "+1555" :sender "+1555" :timestamp 1000
                        :body "Original" :type "data"))))
      (sgn-db-update-message rowid (list :body "Edited" :edited-at 2000))
      (let ((msg (sgn-db-get-message-by-rowid rowid)))
        (should (equal (plist-get msg :body) "Edited"))
        (should (equal (plist-get msg :edited-at) 2000))))))

(ert-deftest sgn-test-db-message-delete ()
  "Deleting marks message and clears body."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (let ((rowid (sgn-db-insert-message
                  (list :chat-id "+1555" :sender "+1555" :timestamp 1000
                        :body "Delete me" :type "data"))))
      (sgn-db-delete-message rowid)
      (let ((msg (sgn-db-get-message-by-rowid rowid)))
        (should (equal (plist-get msg :deleted) 1))
        (should (null (plist-get msg :body)))))))

(ert-deftest sgn-test-db-reactions ()
  "Insert, get, and remove reactions."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (let ((rowid (sgn-db-insert-message
                  (list :chat-id "+1555" :sender "+1555" :timestamp 1000
                        :body "React to me" :type "data"))))
      ;; Add reaction
      (sgn-db-upsert-reaction
       (list :message-rowid rowid :chat-id "+1555"
             :target-author "+1555" :target-timestamp 1000
             :sender "+15550000000" :emoji "👍"))
      (let ((reactions (sgn-db-get-reactions rowid)))
        (should (= (length reactions) 1))
        (should (equal (plist-get (car reactions) :emoji) "👍")))
      ;; Remove reaction
      (sgn-db-remove-reaction "+1555" "+1555" 1000 "+15550000000")
      (should (null (sgn-db-get-reactions rowid))))))

(ert-deftest sgn-test-db-media ()
  "Insert and retrieve media."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (let ((msg-rowid (sgn-db-insert-message
                      (list :chat-id "+1555" :sender "+1555" :timestamp 1000
                            :body nil :type "data"))))
      (sgn-db-insert-media
       (list :message-rowid msg-rowid :chat-id "+1555"
             :content-type "image/jpeg" :file-path "/tmp/test.jpg"))
      (let ((media (sgn-db-get-media msg-rowid)))
        (should (= (length media) 1))
        (should (equal (plist-get (car media) :content-type) "image/jpeg"))))))

(ert-deftest sgn-test-db-search ()
  "FTS5 search finds matching messages."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (sgn-db-insert-message
     (list :chat-id "+1555" :sender "+1555" :timestamp 1000
           :body "dinner at seven" :type "data"))
    (sgn-db-insert-message
     (list :chat-id "+1555" :sender "+1555" :timestamp 2000
           :body "lunch tomorrow" :type "data"))
    (let ((results (sgn-db-search "dinner")))
      (should (= (length results) 1))
      (should (string-match-p "dinner" (plist-get (car results) :snippet))))))

(ert-deftest sgn-test-db-search-excludes-deleted ()
  "FTS5 search excludes deleted messages."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (let ((rowid (sgn-db-insert-message
                  (list :chat-id "+1555" :sender "+1555" :timestamp 1000
                        :body "secret dinner" :type "data"))))
      (sgn-db-delete-message rowid)
      (should (null (sgn-db-search "dinner"))))))

(ert-deftest sgn-test-db-unread-tracking ()
  "Unread counts increment and reset."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (sgn-db-increment-unread "+1555")
    (sgn-db-increment-unread "+1555")
    (let ((chat (sgn-db-get-chat "+1555")))
      (should (equal (plist-get chat :unread) 2)))
    (sgn-db-set-unread "+1555" 0)
    (let ((chat (sgn-db-get-chat "+1555")))
      (should (equal (plist-get chat :unread) 0)))))

(ert-deftest sgn-test-db-drafts ()
  "Draft save and restore."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (sgn-db-save-draft "+1555" "Hello wor")
    (should (equal (sgn-db-get-draft "+1555") "Hello wor"))
    (sgn-db-save-draft "+1555" nil)
    (should (null (sgn-db-get-draft "+1555")))))

(ert-deftest sgn-test-db-polls ()
  "Insert and retrieve polls."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (let ((rowid (sgn-db-insert-message
                  (list :chat-id "+1555" :sender "+1555" :timestamp 1000
                        :body "📊 Poll" :type "data"))))
      (sgn-db-upsert-poll
       (list :message-rowid rowid :chat-id "+1555"
             :poll-author "+1555" :poll-timestamp 1000
             :question "What?" :options-json "[\"A\",\"B\"]"))
      (let ((poll (sgn-db-get-poll rowid)))
        (should poll)
        (should (equal (plist-get poll :question) "What?"))))))

(ert-deftest sgn-test-db-pins ()
  "Insert, get, and remove pins."
  (sgn-test-with-db
    (sgn-db-upsert-chat "+1555" :name "Alice" :type "individual")
    (let ((rowid (sgn-db-insert-message
                  (list :chat-id "+1555" :sender "+1555" :timestamp 1000
                        :body "Pin me" :type "data"))))
      (sgn-db-insert-pin
       (list :message-rowid rowid :chat-id "+1555"
             :target-author "+1555" :target-timestamp 1000
             :pinned-by "+15550000000" :pinned-at 2000))
      (let ((pins (sgn-db-get-pins "+1555")))
        (should (= (length pins) 1)))
      (sgn-db-remove-pin "+1555" "+1555" 1000)
      (should (null (sgn-db-get-pins "+1555"))))))

;;;; Tier 7 — Text formatting

(ert-deftest sgn-test-format-apply-styles-nil ()
  "Nil styles returns text unchanged."
  (should (equal (sgn-format-apply-styles "hello" nil) "hello")))

(ert-deftest sgn-test-format-apply-styles-bold ()
  "Bold style applies bold face."
  (let* ((styles "[{\"start\":0,\"length\":5,\"style\":\"BOLD\"}]")
         (result (sgn-format-apply-styles "hello" styles)))
    (should (equal (get-text-property 0 'face result) 'bold))))

(ert-deftest sgn-test-format-apply-styles-spoiler ()
  "Spoiler style applies face and sets sgn-spoiler property."
  (let* ((styles "[{\"start\":0,\"length\":6,\"style\":\"SPOILER\"}]")
         (result (sgn-format-apply-styles "secret" styles)))
    (should (get-text-property 0 'sgn-spoiler result))
    (should (equal (get-text-property 0 'face result) 'sgn-spoiler-face))))

(ert-deftest sgn-test-format-parse-markup-bold ()
  "Bold markup is parsed correctly."
  (let ((result (sgn-format-parse-markup "*hello*")))
    (should (equal (plist-get result :text) "hello"))
    (let ((styles (plist-get result :styles)))
      (should (= (length styles) 1))
      (should (equal (alist-get 'style (car styles)) "BOLD"))
      (should (equal (alist-get 'start (car styles)) 0))
      (should (equal (alist-get 'length (car styles)) 5)))))

(ert-deftest sgn-test-format-parse-markup-no-markup ()
  "Plain text passes through unchanged."
  (let ((result (sgn-format-parse-markup "hello world")))
    (should (equal (plist-get result :text) "hello world"))
    (should (null (plist-get result :styles)))))

(ert-deftest sgn-test-format-parse-markup-spoiler ()
  "Spoiler markup with double-pipe is parsed."
  (let ((result (sgn-format-parse-markup "||secret||")))
    (should (equal (plist-get result :text) "secret"))
    (let ((styles (plist-get result :styles)))
      (should (= (length styles) 1))
      (should (equal (alist-get 'style (car styles)) "SPOILER")))))

(ert-deftest sgn-test-format-styles-to-json ()
  "Styles list converts to JSON array."
  (let ((styles '(((start . 0) (length . 5) (style . "BOLD")))))
    (let ((json (sgn-format-styles-to-json styles)))
      (should (stringp json))
      (should (string-prefix-p "[" json)))))

(ert-deftest sgn-test-format-styles-to-json-nil ()
  "Empty styles returns nil."
  (should (null (sgn-format-styles-to-json nil))))

;;;; Tier 8 — Contacts

(ert-deftest sgn-test-contacts-get-name-cached ()
  "Cached names are returned."
  (sgn-test-with-clean-state
    (sgn-contacts-set-name "+1555" "Alice")
    (should (equal (sgn-contacts-get-name "+1555") "Alice"))))

(ert-deftest sgn-test-contacts-get-name-fallback ()
  "Unknown IDs fall back to the ID itself."
  (sgn-test-with-clean-state
    (should (equal (sgn-contacts-get-name "+9999") "+9999"))))

(ert-deftest sgn-test-contacts-display-sender-self ()
  "Own account shows as \"You\"."
  (sgn-test-with-clean-state
    (should (equal (sgn-contacts-display-sender "+15550000000") "You"))))

(ert-deftest sgn-test-contacts-display-sender-other ()
  "Other accounts show resolved names."
  (sgn-test-with-clean-state
    (sgn-contacts-set-name "+1666" "Bob")
    (should (equal (sgn-contacts-display-sender "+1666") "Bob"))))

;;;; Tier 9 — Chat buffer

(ert-deftest sgn-test-chat-buffer-mode ()
  "Chat buffer has correct mode and local vars."
  (sgn-test-with-chat-buffer "+1555"
    (should (eq major-mode 'sgn-chat-mode))
    (should (equal sgn-chat-id "+1555"))
    (should (markerp sgn-chat--input-marker))))

(ert-deftest sgn-test-chat-prompt ()
  "Prompt is inserted and marker is positioned."
  (sgn-test-with-chat-buffer "+1555"
    (should (string-match-p
             (regexp-quote sgn-prompt)
             (buffer-substring-no-properties (point-min) (point-max))))
    (should (> (marker-position sgn-chat--input-marker) (point-min)))))

(ert-deftest sgn-test-chat-get-input-text ()
  "Input text is correctly extracted."
  (sgn-test-with-chat-buffer "+1555"
    (goto-char (point-max))
    (insert "hello world")
    (should (equal (sgn-chat--get-input-text) "hello world"))))

(ert-deftest sgn-test-chat-timestamp-format-smart ()
  "Smart timestamp shows time for today's messages."
  (let ((sgn-timestamp-format 'smart)
        (now-ms (truncate (* (float-time) 1000))))
    (should (string-match-p "^[0-9][0-9]:[0-9][0-9]$"
                            (sgn-chat--format-timestamp now-ms)))))

(ert-deftest sgn-test-chat-format-duration ()
  "Duration formatting works correctly."
  (should (equal (sgn-chat--format-duration 30) "30s"))
  (should (equal (sgn-chat--format-duration 3600) "1h"))
  (should (equal (sgn-chat--format-duration 86400) "1d")))

;;;; Tier 10 — Integration: filter → result → callback

(ert-deftest sgn-test-filter-to-callback-integration ()
  "Complete flow: filter → dispatch → handle-result → callback."
  (sgn-test-with-clean-state
    (let ((callback-result nil))
      (puthash 1 (lambda (r) (setq callback-result r))
               sgn-rpc--pending-callbacks)
      (puthash 1 "test" sgn-rpc--request-methods)
      (cl-letf (((symbol-function 'sgn--log) #'ignore))
        (sgn-rpc--process-filter
         nil
         (concat "{\"jsonrpc\":\"2.0\","
                 "\"id\":1,"
                 "\"result\":{\"status\":\"ok\"}}\n")))
      (should (equal (alist-get 'status callback-result) "ok"))
      (should-not (gethash 1 sgn-rpc--pending-callbacks)))))

;;;; Tier 11 — Column to keyword conversion

(ert-deftest sgn-test-db-column-to-keyword ()
  "SQL column names convert to kebab-case keywords."
  (should (eq (sgn-db--column-to-keyword "chat_id") :chat-id))
  (should (eq (sgn-db--column-to-keyword "last_msg_ts") :last-msg-ts))
  (should (eq (sgn-db--column-to-keyword "rowid") :rowid)))

(ert-deftest sgn-test-db-row-to-plist ()
  "Row and columns convert to a proper plist."
  (let ((result (sgn-db--row-to-plist '("alice" 42)
                                       '("name" "age"))))
    (should (equal (plist-get result :name) "alice"))
    (should (equal (plist-get result :age) 42))))

(provide 'sgn-test)
;;; sgn-test.el ends here
