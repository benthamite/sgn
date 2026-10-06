;;; sgn-event.el --- Normalize signal-cli envelopes for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; signal-cli delivers everything that arrives for the account as a
;; "receive" notification wrapping a `JsonMessageEnvelope'.  This
;; module is the only place that reads those envelopes: it turns each
;; one into a list of events, plists with a `:kind', which the rest of
;; sgn consumes.
;;
;; The field names follow signal-cli 0.14's `org.asamk.signal.json'
;; records (JsonMessageEnvelope, JsonDataMessage, JsonEditMessage,
;; JsonSyncMessage, JsonSyncDataMessage, JsonSyncReadMessage,
;; JsonReceiptMessage, JsonTypingMessage, JsonReaction, JsonQuote,
;; JsonAttachment, JsonSticker, JsonMention, JsonTextStyle,
;; JsonGroupInfo, JsonPinMessage, JsonUnpinMessage, JsonRemoteDelete).
;;
;; People are identified by ACI (the `...Uuid' fields), falling back
;; to the phone number when signal-cli does not report an ACI.  Style
;; and mention offsets are kept in UTF-16 code units, as Signal sends
;; them.
;;
;; Event kinds and their keys:
;;
;;   contact   :uuid :number :name
;;   group     :chat-id :name
;;   message   :chat-id :chat-type :sender :outgoing :timestamp :body
;;             :styles :mentions :attachments :sticker :quote
;;             :expires-in :raw
;;   edit      :chat-id :chat-type :sender :target-ts :timestamp :body
;;             :styles :mentions
;;   delete    :chat-id :chat-type :sender :target-ts :timestamp
;;   reaction  :chat-id :chat-type :sender :target-author :target-ts
;;             :emoji :remove :timestamp
;;   pin       :chat-id :chat-type :sender :target-author :target-ts
;;             :unpin :timestamp
;;   timer     :chat-id :chat-type :sender :seconds :timestamp
;;   read      :entries, a list of (SENDER TIMESTAMP)
;;   receipt   :sender :type :timestamps :when
;;   typing    :chat-id :sender :started
;;
;; A message signal-cli could not decrypt becomes a message event
;; whose body says so.

;;; Code:

(require 'cl-lib)
(require 'json)

(defun sgn-event--person (number uuid)
  "Return the identifier for a person with phone NUMBER and ACI UUID."
  (or uuid number))

(defun sgn-event--non-empty (string)
  "Return STRING unless it is nil or empty."
  (and (stringp string) (not (string-empty-p string)) string))

(defun sgn-event-from-envelope (params self)
  "Return the events in a signal-cli receive notification's PARAMS.
SELF is this account's identifier, used as the sender of messages
sent from its other devices; nil to use the envelope's source."
  (let* ((envelope (alist-get 'envelope params))
         (number (alist-get 'sourceNumber envelope))
         (uuid (alist-get 'sourceUuid envelope))
         (source (sgn-event--person number uuid))
         (data (alist-get 'dataMessage envelope))
         (edit (alist-get 'editMessage envelope))
         (sync (alist-get 'syncMessage envelope))
         (receipt (alist-get 'receiptMessage envelope))
         (typing (alist-get 'typingMessage envelope))
         (exception (alist-get 'exception params)))
    (append
     (when uuid
       (list (list :kind 'contact :uuid uuid :number number
                   :name (sgn-event--non-empty (alist-get 'sourceName envelope)))))
     (cond
      ((not source) nil)
      (data (sgn-event--data data source nil nil envelope))
      (edit (sgn-event--edit edit source nil nil))
      ;; Sync messages come from this account's other devices, so
      ;; their source is this account.
      (sync (sgn-event--sync sync (or uuid self source) envelope))
      (receipt (sgn-event--receipt receipt source))
      (typing (sgn-event--typing typing source))
      (exception (sgn-event--undecryptable exception source envelope))))))

(defun sgn-event--undecryptable (exception source envelope)
  "Return a message event saying a message from SOURCE was lost.
EXCEPTION is the JsonError signal-cli reported for ENVELOPE, for
instance when the sender's safety number changed."
  (list (list :kind 'message :chat-id source :chat-type "individual"
              :sender source :timestamp (alist-get 'timestamp envelope)
              :body (format "⚠ A message from this contact could not be read: %s"
                            (or (alist-get 'message exception)
                                (alist-get 'type exception)
                                "unknown error"))
              :expires-in 0 :raw envelope)))

(defun sgn-event--chat (data peer)
  "Return (CHAT-ID . CHAT-TYPE) for DATA exchanged with PEER.
DATA is a JsonDataMessage; PEER is the other party of a direct chat."
  (if-let* ((group (alist-get 'groupId (alist-get 'groupInfo data))))
      (cons group "group")
    (cons peer "individual")))

(defun sgn-event--data (data sender peer outgoing envelope)
  "Return the events in the JsonDataMessage DATA from SENDER.
PEER is the other party when SENDER is this account, else nil.
OUTGOING is non-nil for messages sent from this account.  ENVELOPE
is kept as the raw form of stored messages."
  (pcase-let* ((`(,chat-id . ,chat-type) (sgn-event--chat data (or peer sender)))
               (timestamp (alist-get 'timestamp data))
               (base (list :chat-id chat-id :chat-type chat-type
                           :sender sender :timestamp timestamp))
               (group-name (sgn-event--non-empty
                            (alist-get 'groupName (alist-get 'groupInfo data))))
               (reaction (alist-get 'reaction data))
               (delete (alist-get 'remoteDelete data))
               (pin (or (alist-get 'pinMessage data)
                        (alist-get 'unpinMessage data))))
    (append
     (when group-name
       (list (list :kind 'group :chat-id chat-id :name group-name)))
     (cond
      (reaction
       (list (append (list :kind 'reaction
                           :target-author (sgn-event--person
                                           (alist-get 'targetAuthorNumber reaction)
                                           (alist-get 'targetAuthorUuid reaction))
                           :target-ts (alist-get 'targetSentTimestamp reaction)
                           :emoji (alist-get 'emoji reaction)
                           :remove (alist-get 'isRemove reaction))
                     base)))
      (delete
       (list (append (list :kind 'delete
                           :target-ts (alist-get 'timestamp delete))
                     base)))
      (pin
       (list (append (list :kind 'pin
                           :target-author (sgn-event--person
                                           (alist-get 'targetAuthorNumber pin)
                                           (alist-get 'targetAuthorUuid pin))
                           :target-ts (alist-get 'targetSentTimestamp pin)
                           :unpin (and (alist-get 'unpinMessage data) t))
                     base)))
      ((alist-get 'isExpirationUpdate data)
       (list (append (list :kind 'timer
                           :seconds (or (alist-get 'expiresInSeconds data) 0))
                     base)))
      ((sgn-event--content-p data)
       (list (append (list :kind 'message :outgoing outgoing
                           :raw envelope)
                     (sgn-event--content data)
                     base)))))))

(defun sgn-event--content-p (data)
  "Return non-nil if the JsonDataMessage DATA carries content to show."
  (or (sgn-event--non-empty (alist-get 'message data))
      (alist-get 'attachments data)
      (alist-get 'sticker data)
      (alist-get 'pollCreate data)))

(defun sgn-event--content (data)
  "Return the content keys of a message event for the JsonDataMessage DATA."
  (let ((poll (alist-get 'pollCreate data))
        (quote-data (alist-get 'quote data)))
    (list :body (or (alist-get 'message data)
                    (and poll (format "📊 %s" (alist-get 'question poll))))
          :styles (sgn-event--styles (alist-get 'textStyles data))
          :mentions (sgn-event--mentions (alist-get 'mentions data))
          :attachments (delq nil (mapcar #'sgn-event--attachment
                                         (alist-get 'attachments data)))
          :sticker (when-let* ((sticker (alist-get 'sticker data)))
                     (list :pack-id (alist-get 'packId sticker)
                           :sticker-id (alist-get 'stickerId sticker)))
          :quote (when quote-data
                   (list :ts (alist-get 'id quote-data)
                         :author (sgn-event--person
                                  (alist-get 'authorNumber quote-data)
                                  (alist-get 'authorUuid quote-data))
                         :body (alist-get 'text quote-data)))
          :expires-in (or (alist-get 'expiresInSeconds data) 0))))

(defun sgn-event--styles (styles)
  "Return the JsonTextStyle list STYLES as (STYLE START LENGTH) lists."
  (mapcar (lambda (s) (list (alist-get 'style s) (alist-get 'start s)
                            (alist-get 'length s)))
          styles))

(defun sgn-event--mentions (mentions)
  "Return the JsonMention list MENTIONS as (PERSON START LENGTH) lists."
  (mapcar (lambda (m) (list (sgn-event--person (alist-get 'number m)
                                               (alist-get 'uuid m))
                            (alist-get 'start m) (alist-get 'length m)))
          mentions))

(defun sgn-event--attachment (attachment)
  "Return the media plist for the JsonAttachment ATTACHMENT, or nil.
The long-text attachment Signal uses for long messages is left
out, since signal-cli already puts its text in the message body."
  (let ((type (alist-get 'contentType attachment)))
    (unless (equal type "text/x-signal-plain")
      (list :content-type type
            :attachment-id (alist-get 'id attachment)
            :file-name (alist-get 'filename attachment)
            :is-voice (if (alist-get 'isVoiceNote attachment) 1 0)
            :width (alist-get 'width attachment)
            :height (alist-get 'height attachment)
            :size (alist-get 'size attachment)))))

(defun sgn-event--edit (edit sender peer outgoing)
  "Return the events in the JsonEditMessage EDIT from SENDER.
PEER and OUTGOING are as in `sgn-event--data'."
  (let ((data (alist-get 'dataMessage edit)))
    (pcase-let ((`(,chat-id . ,chat-type) (sgn-event--chat data (or peer sender))))
      (list (list :kind 'edit :chat-id chat-id :chat-type chat-type
                  :sender sender :outgoing outgoing
                  :target-ts (alist-get 'targetSentTimestamp edit)
                  :timestamp (alist-get 'timestamp data)
                  :body (alist-get 'message data)
                  :styles (sgn-event--styles (alist-get 'textStyles data))
                  :mentions (sgn-event--mentions (alist-get 'mentions data)))))))

(defun sgn-event--sync (sync self envelope)
  "Return the events in the JsonSyncMessage SYNC.
SELF identifies this account.  ENVELOPE is the raw envelope."
  (let ((sent (alist-get 'sentMessage sync))
        (read (alist-get 'readMessages sync)))
    (append
     (when sent
       (let ((peer (sgn-event--person (alist-get 'destinationNumber sent)
                                      (alist-get 'destinationUuid sent))))
         (if-let* ((edit (alist-get 'editMessage sent)))
             (sgn-event--edit edit self peer t)
           (sgn-event--data sent self peer t envelope))))
     (when read
       (list (list :kind 'read
                   :entries (mapcar (lambda (r)
                                      (list (sgn-event--person
                                             (alist-get 'senderNumber r)
                                             (alist-get 'senderUuid r))
                                            (alist-get 'timestamp r)))
                                    read)))))))

(defun sgn-event--receipt (receipt sender)
  "Return the event for the JsonReceiptMessage RECEIPT from SENDER."
  (list (list :kind 'receipt :sender sender
              :type (cond ((alist-get 'isRead receipt) "read")
                          ((alist-get 'isViewed receipt) "viewed")
                          (t "delivered"))
              :timestamps (alist-get 'timestamps receipt)
              :when (alist-get 'when receipt))))

(defun sgn-event--typing (typing sender)
  "Return the event for the JsonTypingMessage TYPING from SENDER."
  (list (list :kind 'typing :sender sender
              :chat-id (or (alist-get 'groupId typing) sender)
              :started (equal (alist-get 'action typing) "STARTED"))))

(provide 'sgn-event)
;;; sgn-event.el ends here
