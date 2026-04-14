;;; sgn-test.el --- Tests for sgn.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT test suite for sgn.el (Signal client via signal-cli JSON-RPC).
;; Covers: pure utilities, process filter/dispatch, callback machinery,
;; contact population, and buffer management.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sgn)

;;;; Test helpers

(defmacro sgn-test-with-clean-state (&rest body)
  "Run BODY with all sgn mutable state reset to fresh defaults."
  (declare (indent 0) (debug body))
  `(let ((sgn--partial-line "")
         (sgn--contact-map (make-hash-table :test 'equal))
         (sgn--active-chats (make-hash-table :test 'equal))
         (sgn--pending-callbacks (make-hash-table :test 'equal))
         (sgn--request-buffer-map (make-hash-table :test 'equal))
         (sgn--rpc-id-counter 0))
     ,@body))

(defmacro sgn-test-with-chat-buffer (id &rest body)
  "Run BODY in a fresh chat buffer for ID, cleaning up afterward."
  (declare (indent 1) (debug body))
  (let ((buf (gensym "buf")))
    `(sgn-test-with-clean-state
       (let ((,buf (sgn--get-buffer ,id)))
         (unwind-protect
             (with-current-buffer ,buf
               ,@body)
           (kill-buffer ,buf))))))

;;;; Tier 1 — Pure utility functions

;;; sgn--ensure-list

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

(ert-deftest sgn-test-ensure-list-nested-vector ()
  "Nested vectors are only coerced at the top level."
  (let ((result (sgn--ensure-list [1 [2 3]])))
    (should (listp result))
    (should (vectorp (cadr result)))))

;;; sgn--is-group-id

(ert-deftest sgn-test-is-group-id-phone-number ()
  "Phone numbers (starting with +) are not group IDs."
  (should-not (sgn--is-group-id "+15550000000")))

(ert-deftest sgn-test-is-group-id-uuid ()
  "UUIDs (containing -) are not group IDs."
  (should-not (sgn--is-group-id "a1b2c3d4-e5f6-7890-abcd-ef1234567890")))

(ert-deftest sgn-test-is-group-id-base64 ()
  "Base64 strings (no + prefix or -) are identified as group IDs."
  (should (sgn--is-group-id "dGVzdGdyb3VwaWQ9")))

(ert-deftest sgn-test-is-group-id-base64-with-equals ()
  "Base64 with padding is still a group ID."
  (should (sgn--is-group-id "YWJjZGVmZw==")))

;;; sgn--build-send-params

(ert-deftest sgn-test-build-send-params-phone ()
  "Phone number gets `recipient' key with vector value."
  (let ((result (sgn--build-send-params "+15550000000"
                                        '((message . "hi")))))
    (should (equal (alist-get 'recipient result) ["+15550000000"]))
    (should-not (alist-get 'groupId result))
    (should (equal (alist-get 'message result) "hi"))))

(ert-deftest sgn-test-build-send-params-group ()
  "Group ID gets `groupId' key with string value."
  (let ((result (sgn--build-send-params "dGVzdGdyb3Vw"
                                        '((message . "hi")))))
    (should (equal (alist-get 'groupId result) "dGVzdGdyb3Vw"))
    (should-not (alist-get 'recipient result))
    (should (equal (alist-get 'message result) "hi"))))

(ert-deftest sgn-test-build-send-params-preserves-base ()
  "BASE-PARAMS are preserved in output."
  (let* ((base '((message . "hello") (attachments . ["file.jpg"])))
         (result (sgn--build-send-params "+1555" base)))
    (should (equal (alist-get 'message result) "hello"))
    (should (equal (alist-get 'attachments result) ["file.jpg"]))))

;;;; Tier 2 — Process filter and dispatch

;;; sgn--process-filter

(ert-deftest sgn-test-process-filter-complete-line ()
  "A complete JSON line (terminated by newline) is dispatched."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn--process-filter
         nil "{\"method\":\"receive\",\"params\":{}}\n")
        (should (= (length dispatched) 1))
        (should (equal (alist-get 'method (car dispatched))
                       "receive"))))))

(ert-deftest sgn-test-process-filter-partial-then-complete ()
  "Partial lines are buffered until the newline arrives."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn--process-filter nil "{\"id\":1,\"result\":")
        (should (null dispatched))
        (sgn--process-filter nil "\"ok\"}\n")
        (should (= (length dispatched) 1))
        (should (equal (alist-get 'result (car dispatched))
                       "ok"))))))

(ert-deftest sgn-test-process-filter-two-lines-in-one-chunk ()
  "Two complete lines in one chunk dispatch twice."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn--process-filter
         nil
         (concat "{\"id\":1,\"result\":\"a\"}\n"
                 "{\"id\":2,\"result\":\"b\"}\n"))
        (should (= (length dispatched) 2))))))

(ert-deftest sgn-test-process-filter-skips-non-json ()
  "Non-JSON lines (not starting with {) are silently skipped."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn--process-filter nil "some log output\n")
        (should (null dispatched))))))

(ert-deftest sgn-test-process-filter-skips-empty-lines ()
  "Empty lines between JSON objects are skipped."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn--process-filter
         nil "\n\n{\"id\":1,\"result\":\"x\"}\n\n")
        (should (= (length dispatched) 1))))))

(ert-deftest sgn-test-process-filter-overflow-protection ()
  "Buffer is cleared when it exceeds the max length."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn--process-filter
         nil (make-string (1+ sgn--max-partial-line-length) ?x))
        (should (string-empty-p sgn--partial-line))
        (sgn--process-filter
         nil "{\"id\":99,\"result\":\"ok\"}\n")
        (should (= (length dispatched) 1))))))

(ert-deftest sgn-test-process-filter-malformed-json ()
  "Malformed JSON does not crash and produces no dispatch."
  (sgn-test-with-clean-state
    (let ((dispatched nil))
      (cl-letf (((symbol-function 'sgn--dispatch)
                 (lambda (json) (push json dispatched)))
                ((symbol-function 'sgn--log) #'ignore))
        (sgn--process-filter nil "{not valid json}\n")
        (should (null dispatched))))))

;;; sgn--dispatch

(ert-deftest sgn-test-dispatch-receive ()
  "Method \"receive\" routes to `sgn--handle-receive' with PARAMS."
  (sgn-test-with-clean-state
    (let ((received nil))
      (cl-letf (((symbol-function 'sgn--handle-receive)
                 (lambda (params) (setq received params))))
        (sgn--dispatch '((method . "receive")
                         (params . ((envelope . t)))))
        (should (equal received '((envelope . t))))))))

(ert-deftest sgn-test-dispatch-error ()
  "Error objects route to `sgn--handle-error'."
  (sgn-test-with-clean-state
    (let ((error-args nil))
      (cl-letf (((symbol-function 'sgn--handle-error)
                 (lambda (id err)
                   (setq error-args (list id err)))))
        (sgn--dispatch '((id . 5)
                         (error . ((message . "boom")))))
        (should (equal (car error-args) 5))
        (should (equal (alist-get 'message (cadr error-args))
                       "boom"))))))

(ert-deftest sgn-test-dispatch-result ()
  "ID + result routes to `sgn--handle-result'."
  (sgn-test-with-clean-state
    (let ((result-args nil))
      (cl-letf (((symbol-function 'sgn--handle-result)
                 (lambda (id result)
                   (setq result-args (list id result)))))
        (sgn--dispatch '((id . 3) (result . "ok")))
        (should (equal result-args '(3 "ok")))))))

(ert-deftest sgn-test-dispatch-unknown-method-no-crash ()
  "Unknown method without error or result is silently ignored."
  (sgn-test-with-clean-state
    (let ((called nil))
      (cl-letf (((symbol-function 'sgn--handle-receive)
                 (lambda (_) (setq called t))))
        (sgn--dispatch '((method . "unknownMethod")
                         (params . nil)))
        (should-not called)))))

;;;; Tier 3 — Callback and error machinery

;;; sgn--handle-result

(ert-deftest sgn-test-handle-result-invokes-callback ()
  "Stored callback is invoked with the result value."
  (sgn-test-with-clean-state
    (let ((received-result nil))
      (puthash 42 (lambda (r) (setq received-result r))
               sgn--pending-callbacks)
      (puthash 42 "*test*" sgn--request-buffer-map)
      (sgn--handle-result 42 '((contacts . [])))
      (should (equal received-result '((contacts . []))))
      (should-not (gethash 42 sgn--pending-callbacks))
      (should-not (gethash 42 sgn--request-buffer-map)))))

(ert-deftest sgn-test-handle-result-no-callback ()
  "Without a callback, handle-result still cleans up the map."
  (sgn-test-with-clean-state
    (puthash 10 "*some-buf*" sgn--request-buffer-map)
    (sgn--handle-result 10 "ok")
    (should-not (gethash 10 sgn--request-buffer-map))))

;;; sgn--handle-error

(ert-deftest sgn-test-handle-error-cleans-up-maps ()
  "Error handling removes entries from both maps."
  (sgn-test-with-clean-state
    (puthash 7 (lambda (_) nil) sgn--pending-callbacks)
    (puthash 7 "*nonexistent*" sgn--request-buffer-map)
    (cl-letf (((symbol-function 'sgn--log) #'ignore))
      (sgn--handle-error 7 '((message . "test error"))))
    (should-not (gethash 7 sgn--pending-callbacks))
    (should-not (gethash 7 sgn--request-buffer-map))))

(ert-deftest sgn-test-handle-error-inserts-into-buffer ()
  "Error message is inserted into the mapped chat buffer."
  (sgn-test-with-clean-state
    (let ((buf (sgn--get-buffer "+1test-error")))
      (unwind-protect
          (progn
            (puthash 8 (buffer-name buf) sgn--request-buffer-map)
            (cl-letf (((symbol-function 'sgn--log) #'ignore))
              (sgn--handle-error
               8 '((message . "rpc failed"))))
            (with-current-buffer buf
              (should (string-match-p
                       "ERROR: rpc failed"
                       (buffer-substring-no-properties
                        (point-min) (point-max))))))
        (kill-buffer buf)))))

;;;; Tier 4 — Contact and group population

;;; sgn--populate-contacts

(ert-deftest sgn-test-populate-contacts-from-vector ()
  "Contacts vector populates contact-map and active-chats."
  (sgn-test-with-clean-state
    (cl-letf (((symbol-function 'sgn--dashboard-refresh) #'ignore))
      (sgn--populate-contacts
       [((number . "+1555") (name . "Alice"))
        ((number . "+1666") (name . "Bob"))])
      (should (equal (gethash "+1555" sgn--contact-map) "Alice"))
      (should (equal (gethash "+1666" sgn--contact-map) "Bob"))
      (should (eq (gethash "+1555" sgn--active-chats) t))
      (should (eq (gethash "+1666" sgn--active-chats) t)))))

(ert-deftest sgn-test-populate-contacts-skips-empty-name ()
  "Contacts with empty or missing name are skipped."
  (sgn-test-with-clean-state
    (cl-letf (((symbol-function 'sgn--dashboard-refresh) #'ignore))
      (sgn--populate-contacts
       [((number . "+1555") (name . ""))
        ((number . "+1666") (name . "Bob"))
        ((number . "+1777"))])
      (should-not (gethash "+1555" sgn--contact-map))
      (should (equal (gethash "+1666" sgn--contact-map) "Bob"))
      (should-not (gethash "+1777" sgn--contact-map)))))

(ert-deftest sgn-test-populate-contacts-skips-missing-number ()
  "Contacts without a number are skipped."
  (sgn-test-with-clean-state
    (cl-letf (((symbol-function 'sgn--dashboard-refresh) #'ignore))
      (sgn--populate-contacts [((name . "Ghost"))])
      (should (= (hash-table-count sgn--contact-map) 0)))))

;;; sgn--populate-groups

(ert-deftest sgn-test-populate-groups-from-vector ()
  "Groups populate both active-chats and contact-map."
  (sgn-test-with-clean-state
    (cl-letf (((symbol-function 'sgn--dashboard-refresh) #'ignore))
      (sgn--populate-groups
       [((id . "grp1==") (name . "Family"))
        ((id . "grp2==") (name . "Work"))])
      (should (eq (gethash "grp1==" sgn--active-chats) t))
      (should (equal (gethash "grp1==" sgn--contact-map) "Family"))
      (should (equal (gethash "grp2==" sgn--contact-map) "Work")))))

(ert-deftest sgn-test-populate-groups-no-name ()
  "Groups without a name are in active-chats but not contact-map."
  (sgn-test-with-clean-state
    (cl-letf (((symbol-function 'sgn--dashboard-refresh) #'ignore))
      (sgn--populate-groups [((id . "grp3=="))])
      (should (eq (gethash "grp3==" sgn--active-chats) t))
      (should-not (gethash "grp3==" sgn--contact-map)))))

(ert-deftest sgn-test-populate-groups-empty-name ()
  "Groups with empty name are in active-chats but not contact-map."
  (sgn-test-with-clean-state
    (cl-letf (((symbol-function 'sgn--dashboard-refresh) #'ignore))
      (sgn--populate-groups [((id . "grp4==") (name . ""))])
      (should (eq (gethash "grp4==" sgn--active-chats) t))
      (should-not (gethash "grp4==" sgn--contact-map)))))

;;;; Tier 5 — Buffer and UI management

;;; sgn--get-buffer

(ert-deftest sgn-test-get-buffer-creates-with-mode ()
  "Creates a new buffer with `sgn-chat-mode' and correct chat ID."
  (sgn-test-with-clean-state
    (let ((buf (sgn--get-buffer "+1test-mode")))
      (unwind-protect
          (with-current-buffer buf
            (should (eq major-mode 'sgn-chat-mode))
            (should (equal sgn--chat-id "+1test-mode"))
            (should (markerp sgn--input-marker)))
        (kill-buffer buf)))))

(ert-deftest sgn-test-get-buffer-reuses-existing ()
  "Returns the same buffer on second call for the same ID."
  (sgn-test-with-clean-state
    (let ((buf1 (sgn--get-buffer "+1reuse"))
          (buf2 (sgn--get-buffer "+1reuse")))
      (unwind-protect
          (should (eq buf1 buf2))
        (kill-buffer buf1)))))

;;; sgn--draw-prompt

(ert-deftest sgn-test-draw-prompt-inserts-prompt ()
  "Inserts the prompt string and advances the input marker."
  (sgn-test-with-chat-buffer "+1prompt-test"
    (let ((marker-pos (marker-position sgn--input-marker)))
      (should (string-match-p
               (regexp-quote sgn-prompt)
               (buffer-substring-no-properties
                (point-min) (point-max))))
      (should (> marker-pos (point-min))))))

;;; sgn--guard-cursor

(ert-deftest sgn-test-guard-cursor-moves-to-marker ()
  "Cursor before the input marker is moved to the marker."
  (sgn-test-with-chat-buffer "+1guard-test"
    (goto-char (point-min))
    (sgn--guard-cursor)
    (should (= (point) (marker-position sgn--input-marker)))))

(ert-deftest sgn-test-guard-cursor-stays-after-marker ()
  "Cursor at or after the input marker is not moved."
  (sgn-test-with-chat-buffer "+1guard-test2"
    (goto-char (point-max))
    (insert "some input")
    (let ((pos (point)))
      (sgn--guard-cursor)
      (should (= (point) pos)))))

;;; sgn--insert-msg

(ert-deftest sgn-test-insert-msg-text-only ()
  "Text message adds sender name, timestamp, and text."
  (sgn-test-with-chat-buffer "+1msg-test"
    (cl-letf (((symbol-function 'sgn--dashboard-refresh) #'ignore))
      (sgn--insert-msg "+1msg-test" "Alice" "Hello world"
                       nil nil nil)
      (let ((content (buffer-substring-no-properties
                      (point-min) (point-max))))
        (should (string-match-p "<Alice>" content))
        (should (string-match-p "Hello world" content))
        (should (string-match-p (regexp-quote sgn-prompt)
                                content))))))

(ert-deftest sgn-test-insert-msg-my-message ()
  "Own messages use the \"Me\" name."
  (sgn-test-with-chat-buffer "+1me-test"
    (cl-letf (((symbol-function 'sgn--dashboard-refresh) #'ignore))
      (sgn--insert-msg "+1me-test" "Me" "My message"
                       nil nil t)
      (let ((content (buffer-substring-no-properties
                      (point-min) (point-max))))
        (should (string-match-p "<Me>" content))
        (should (string-match-p "My message" content))))))

(ert-deftest sgn-test-insert-msg-preserves-prompt ()
  "After inserting a message, the prompt and marker remain valid."
  (sgn-test-with-chat-buffer "+1prompt-preserve"
    (cl-letf (((symbol-function 'sgn--dashboard-refresh) #'ignore))
      (sgn--insert-msg "+1prompt-preserve" "Bob" "Test"
                       nil nil nil)
      (should (markerp sgn--input-marker))
      (should (> (marker-position sgn--input-marker) (point-min)))
      (goto-char (point-max))
      (should (string-match-p
               (regexp-quote sgn-prompt)
               (buffer-substring-no-properties
                (line-beginning-position) (point-max)))))))

;;; Dashboard

(ert-deftest sgn-test-dashboard-draw ()
  "Dashboard lists active chats with names from contact-map."
  (sgn-test-with-clean-state
    (puthash "+1555" "Alice" sgn--contact-map)
    (puthash "+1555" t sgn--active-chats)
    (puthash "grp1==" "Family" sgn--contact-map)
    (puthash "grp1==" t sgn--active-chats)
    (let ((buf (get-buffer-create "*Sgn List*")))
      (unwind-protect
          (with-current-buffer buf
            (sgn-dashboard-mode)
            (sgn--dashboard-draw)
            (let ((content (buffer-substring-no-properties
                            (point-min) (point-max))))
              (should (string-match-p "Alice" content))
              (should (string-match-p "Family" content))
              (should (string-match-p "Active Chats:" content))))
        (kill-buffer buf)))))

(ert-deftest sgn-test-dashboard-draw-uses-id-as-fallback ()
  "When contact-map has no name, the ID itself is shown."
  (sgn-test-with-clean-state
    (puthash "+1999" t sgn--active-chats)
    (let ((buf (get-buffer-create "*Sgn List*")))
      (unwind-protect
          (with-current-buffer buf
            (sgn-dashboard-mode)
            (sgn--dashboard-draw)
            (let ((content (buffer-substring-no-properties
                            (point-min) (point-max))))
              (should (string-match-p "\\+1999" content))))
        (kill-buffer buf)))))

;;; sgn--insert-system-msg

(ert-deftest sgn-test-insert-system-msg ()
  "System messages appear with *** prefix and the prompt is preserved."
  (sgn-test-with-chat-buffer "+1sys-test"
    (sgn--insert-system-msg "Connection lost" 'sgn-error-face)
    (let ((content (buffer-substring-no-properties
                    (point-min) (point-max))))
      (should (string-match-p "\\*\\*\\* Connection lost" content))
      (should (string-match-p (regexp-quote sgn-prompt) content)))))

;;;; Integration — end-to-end filter → result → callback

(ert-deftest sgn-test-filter-to-callback-integration ()
  "Complete flow: filter → dispatch → handle-result → callback."
  (sgn-test-with-clean-state
    (let ((callback-result nil))
      (puthash 1 (lambda (r) (setq callback-result r))
               sgn--pending-callbacks)
      (cl-letf (((symbol-function 'sgn--log) #'ignore))
        (sgn--process-filter
         nil
         (concat "{\"jsonrpc\":\"2.0\","
                 "\"id\":1,"
                 "\"result\":{\"status\":\"ok\"}}\n")))
      (should (equal (alist-get 'status callback-result) "ok"))
      (should-not (gethash 1 sgn--pending-callbacks)))))

(provide 'sgn-test)
;;; sgn-test.el ends here
