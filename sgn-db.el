;;; sgn-db.el --- SQLite persistence for sgn  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Keenan Salandy

;; Author: Keenan Salandy <keenan@salandy.dev>
;; Maintainer: Pablo Stafforini <pablo@stafforini.com>
;; Package-Requires: ((emacs "29.1"))

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

;; SQLite persistence layer for sgn, the Emacs Signal client.  Provides
;; all database operations for chats, messages, reactions, media,
;; receipts, polls, and pins using Emacs 29+'s built-in sqlite.el.

;;; Code:

(require 'cl-lib)

(declare-function sgn--log "sgn")
(defvar sgn-account)

;;;; Customization

(defcustom sgn-db-directory (expand-file-name "sgn" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Directory for sgn SQLite database."
  :type 'directory
  :group 'sgn)

;;;; Internal state

(defvar sgn-db--connection nil
  "Active SQLite database connection, or nil.")

;;;; Internal helpers

(defun sgn-db--column-to-keyword (column)
  "Convert SQL COLUMN name string to a keyword symbol.
Replaces underscores with hyphens and prepends a colon."
  (intern (concat ":" (replace-regexp-in-string "_" "-" column))))

(defun sgn-db--row-to-plist (row columns)
  "Convert ROW (list of values) and COLUMNS (list of name strings) to a plist.
Column names are converted to kebab-case keywords."
  (cl-loop for col in columns
           for val in row
           nconc (list (sgn-db--column-to-keyword col) val)))

(defun sgn-db--ensure ()
  "Signal an error if the database connection is not open."
  (unless sgn-db--connection
    (error "sgn database not initialized; call `sgn-db-init' first")))

(defun sgn-db--db-path ()
  "Return the full path to the sgn database file."
  (expand-file-name "sgn.db" sgn-db-directory))

;;;; Schema

(defun sgn-db--check-fts5 (db)
  "Verify FTS5 support is available in DB.
Creates and drops a temporary FTS5 table.  Signals `user-error' on failure."
  (condition-case nil
      (progn
        (sqlite-execute db "CREATE VIRTUAL TABLE IF NOT EXISTS _fts5_check USING fts5(x)")
        (sqlite-execute db "DROP TABLE IF EXISTS _fts5_check"))
    (error
     (user-error "SQLite FTS5 extension is not available; sgn-db requires it"))))

(defun sgn-db--create-tables (db)
  "Create all tables in DB if they do not exist."
  (sgn-db--create-chats-table db)
  (sgn-db--create-messages-table db)
  (sgn-db--create-fts-table db)
  (sgn-db--create-fts-triggers db)
  (sgn-db--create-reactions-table db)
  (sgn-db--create-media-table db)
  (sgn-db--create-receipts-table db)
  (sgn-db--create-polls-table db)
  (sgn-db--create-pins-table db)
  (sgn-db--create-indexes db))

(defun sgn-db--create-chats-table (db)
  "Create the chats table in DB."
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS chats (
    id          TEXT PRIMARY KEY,
    name        TEXT,
    type        TEXT NOT NULL,
    last_msg_ts INTEGER,
    unread      INTEGER DEFAULT 0,
    muted       INTEGER DEFAULT 0,
    pinned      INTEGER DEFAULT 0,
    draft       TEXT,
    expiration  INTEGER DEFAULT 0
)"))

(defun sgn-db--create-messages-table (db)
  "Create the messages table in DB."
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS messages (
    rowid       INTEGER PRIMARY KEY AUTOINCREMENT,
    chat_id     TEXT NOT NULL REFERENCES chats(id),
    sender      TEXT NOT NULL,
    timestamp   INTEGER NOT NULL,
    body        TEXT,
    type        TEXT NOT NULL,
    quote_ts    INTEGER,
    quote_author TEXT,
    quote_body  TEXT,
    edited_at   INTEGER,
    deleted     INTEGER DEFAULT 0,
    expires_in  INTEGER DEFAULT 0,
    expire_started_at INTEGER,
    expires_at  INTEGER,
    styles_json TEXT,
    raw_json    TEXT,
    UNIQUE(chat_id, sender, timestamp)
)"))

(defun sgn-db--create-fts-table (db)
  "Create the FTS5 virtual table in DB."
  (sqlite-execute db "CREATE VIRTUAL TABLE IF NOT EXISTS messages_fts USING fts5(
    body,
    content='messages',
    content_rowid='rowid'
)"))

(defun sgn-db--create-fts-triggers (db)
  "Create FTS synchronization triggers in DB."
  (sqlite-execute db "CREATE TRIGGER IF NOT EXISTS messages_ai AFTER INSERT ON messages BEGIN
    INSERT INTO messages_fts(rowid, body)
    SELECT new.rowid, new.body
    WHERE new.body IS NOT NULL AND new.deleted = 0;
END")
  (sqlite-execute db "CREATE TRIGGER IF NOT EXISTS messages_ad AFTER DELETE ON messages BEGIN
    INSERT INTO messages_fts(messages_fts, rowid, body) VALUES('delete', old.rowid, old.body);
END")
  (sqlite-execute db "CREATE TRIGGER IF NOT EXISTS messages_au AFTER UPDATE ON messages BEGIN
    INSERT INTO messages_fts(messages_fts, rowid, body) VALUES('delete', old.rowid, old.body);
    INSERT INTO messages_fts(rowid, body)
    SELECT new.rowid, new.body
    WHERE new.body IS NOT NULL AND new.deleted = 0;
END"))

(defun sgn-db--create-reactions-table (db)
  "Create the reactions table in DB."
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS reactions (
    message_rowid INTEGER REFERENCES messages(rowid) ON DELETE CASCADE,
    chat_id     TEXT NOT NULL,
    target_author TEXT NOT NULL,
    target_timestamp INTEGER NOT NULL,
    sender      TEXT NOT NULL,
    emoji       TEXT NOT NULL,
    removed     INTEGER DEFAULT 0,
    UNIQUE(chat_id, target_author, target_timestamp, sender)
)"))

(defun sgn-db--create-media-table (db)
  "Create the media table in DB."
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS media (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    message_rowid INTEGER NOT NULL REFERENCES messages(rowid) ON DELETE CASCADE,
    chat_id     TEXT NOT NULL,
    content_type TEXT NOT NULL,
    file_path   TEXT,
    file_name   TEXT,
    is_voice    INTEGER DEFAULT 0,
    is_sticker  INTEGER DEFAULT 0,
    width       INTEGER,
    height      INTEGER
)"))

(defun sgn-db--create-receipts-table (db)
  "Create the receipts table in DB."
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS receipts (
    message_rowid INTEGER REFERENCES messages(rowid) ON DELETE CASCADE,
    chat_id     TEXT NOT NULL,
    target_author TEXT NOT NULL,
    target_timestamp INTEGER NOT NULL,
    recipient   TEXT NOT NULL,
    type        TEXT NOT NULL,
    received_at INTEGER NOT NULL,
    UNIQUE(chat_id, target_author, target_timestamp, recipient, type)
)"))

(defun sgn-db--create-polls-table (db)
  "Create the polls table in DB."
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS polls (
    message_rowid INTEGER UNIQUE REFERENCES messages(rowid) ON DELETE CASCADE,
    chat_id     TEXT NOT NULL,
    poll_author TEXT NOT NULL,
    poll_timestamp INTEGER NOT NULL,
    question    TEXT NOT NULL,
    options_json TEXT NOT NULL,
    votes_json  TEXT,
    self_vote_count INTEGER DEFAULT 0,
    closed      INTEGER DEFAULT 0,
    UNIQUE(chat_id, poll_author, poll_timestamp)
)"))

(defun sgn-db--create-pins-table (db)
  "Create the pins table in DB."
  (sqlite-execute db "CREATE TABLE IF NOT EXISTS pins (
    message_rowid INTEGER REFERENCES messages(rowid) ON DELETE CASCADE,
    chat_id     TEXT NOT NULL,
    target_author TEXT NOT NULL,
    target_timestamp INTEGER NOT NULL,
    pinned_by   TEXT NOT NULL,
    pinned_at   INTEGER NOT NULL,
    pin_expires_at INTEGER,
    PRIMARY KEY(chat_id, target_author, target_timestamp)
)"))

(defun sgn-db--create-indexes (db)
  "Create performance indexes in DB."
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_messages_chat_ts ON messages(chat_id, timestamp)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_messages_chat_sender_ts ON messages(chat_id, sender, timestamp)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_reactions_target ON reactions(chat_id, target_author, target_timestamp)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_media_message ON media(message_rowid)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_receipts_target ON receipts(chat_id, target_author, target_timestamp)"))

(defun sgn-db--set-schema-version (db version)
  "Set the schema VERSION in DB via PRAGMA user_version."
  (sqlite-execute db (format "PRAGMA user_version = %d" version)))

(defun sgn-db--get-schema-version (db)
  "Return the current schema version from DB."
  (caar (sqlite-select db "PRAGMA user_version")))

(defun sgn-db--maybe-create-schema (db)
  "Create schema in DB if it is at version 0 (fresh database)."
  (when (zerop (sgn-db--get-schema-version db))
    (sgn-db--create-tables db)
    (sgn-db--set-schema-version db 1)
    (sgn--log "sgn-db: schema created (version 1)")))

;;;; Database lifecycle

(defun sgn-db-init ()
  "Initialize the SQLite database.
Create directory and tables if needed.  Verify that `sqlite-available-p'
returns non-nil and that FTS5 is supported; signal an error otherwise."
  (unless (sqlite-available-p)
    (error "SQLite support is not available in this Emacs build"))
  (unless (file-directory-p sgn-db-directory)
    (make-directory sgn-db-directory t))
  (let ((db (sqlite-open (sgn-db--db-path))))
    (sgn-db--check-fts5 db)
    (sqlite-execute db "PRAGMA foreign_keys = ON")
    (sqlite-execute db "PRAGMA journal_mode = WAL")
    (sgn-db--maybe-create-schema db)
    (setq sgn-db--connection db)
    (sgn--log "sgn-db: initialized (%s)" (sgn-db--db-path))))

(defun sgn-db-close ()
  "Close the database connection."
  (when sgn-db--connection
    (sqlite-close sgn-db--connection)
    (setq sgn-db--connection nil)
    (sgn--log "sgn-db: connection closed")))

;;;; Chat CRUD

(defconst sgn-db--chat-columns
  '("id" "name" "type" "last_msg_ts" "unread" "muted" "pinned" "draft" "expiration")
  "Column names for the chats table.")

(defconst sgn-db--chat-attr-to-column
  '((:name . "name")
    (:type . "type")
    (:last-msg-ts . "last_msg_ts")
    (:unread . "unread")
    (:muted . "muted")
    (:pinned . "pinned")
    (:draft . "draft")
    (:expiration . "expiration"))
  "Alist mapping chat attribute keywords to SQL column names.")

(defun sgn-db-upsert-chat (id &rest attrs)
  "Create or update chat ID with ATTRS plist.
ATTRS keys: :name :type :last-msg-ts :unread :muted :pinned :draft
:expiration.  Only non-nil ATTRS are updated (existing values preserved)."
  (sgn-db--ensure)
  (let ((provided (sgn-db--extract-provided-attrs attrs sgn-db--chat-attr-to-column)))
    (if (sgn-db--chat-exists-p id)
        (when provided
          (sgn-db--execute-chat-update id provided))
      (sgn-db--execute-chat-insert id provided))))

(defun sgn-db--extract-provided-attrs (attrs attr-to-column)
  "Extract non-nil attributes from ATTRS plist using ATTR-TO-COLUMN mapping.
Return list of (column . value) pairs."
  (cl-loop for (key . col) in attr-to-column
           for val = (plist-get attrs key)
           when val collect (cons col val)))

(defun sgn-db--chat-exists-p (id)
  "Return non-nil if a chat with ID exists in the database."
  (caar (sqlite-select sgn-db--connection
                       "SELECT 1 FROM chats WHERE id = ?"
                       (list id))))

(defun sgn-db--execute-chat-insert (id provided)
  "Insert a new chat row for ID with PROVIDED (column . value) pairs.
Ensures the required `type' column has a default of \"direct\"."
  (let* ((has-type (cl-assoc "type" provided :test #'string=))
         (all (if has-type provided (cons '("type" . "direct") provided)))
         (columns (mapcar #'car all))
         (values (mapcar #'cdr all))
         (col-list (sgn-db--join-columns (cons "id" columns)))
         (placeholders (sgn-db--make-placeholders (1+ (length columns)))))
    (sqlite-execute sgn-db--connection
                    (format "INSERT INTO chats (%s) VALUES (%s)"
                            col-list placeholders)
                    (cons id values))))

(defun sgn-db--execute-chat-update (id provided)
  "Update existing chat ID with PROVIDED (column . value) pairs."
  (sgn-db--execute-update "chats" "id" id provided))

(defun sgn-db--join-columns (columns)
  "Join COLUMNS list into a comma-separated string."
  (mapconcat #'identity columns ", "))

(defun sgn-db--make-placeholders (n)
  "Return a string of N comma-separated question mark placeholders."
  (mapconcat (lambda (_) "?") (make-list n nil) ", "))

(defun sgn-db-get-chat (id)
  "Return chat plist for ID, or nil."
  (sgn-db--ensure)
  (let ((row (car (sqlite-select sgn-db--connection
                                 "SELECT id, name, type, last_msg_ts, unread, muted, pinned, draft, expiration FROM chats WHERE id = ?"
                                 (list id)))))
    (when row
      (sgn-db--row-to-plist row sgn-db--chat-columns))))

(defun sgn-db-get-chats (&optional include-empty)
  "Return list of chat plists sorted by pinned DESC, last_msg_ts DESC.
Unless INCLUDE-EMPTY is non-nil, exclude chats with no messages
\(last_msg_ts IS NULL)."
  (sgn-db--ensure)
  (let* ((sql (concat "SELECT id, name, type, last_msg_ts, unread, muted, pinned, draft, expiration FROM chats"
                      (unless include-empty " WHERE last_msg_ts IS NOT NULL")
                      " ORDER BY pinned DESC, last_msg_ts DESC"))
         (rows (sqlite-select sgn-db--connection sql)))
    (mapcar (lambda (row) (sgn-db--row-to-plist row sgn-db--chat-columns))
            rows)))

;;;; Message CRUD

(defconst sgn-db--message-columns
  '("rowid" "chat_id" "sender" "timestamp" "body" "type"
    "quote_ts" "quote_author" "quote_body" "edited_at" "deleted"
    "expires_in" "expire_started_at" "expires_at" "styles_json" "raw_json")
  "Column names for message query results.")

(defconst sgn-db--message-select
  "SELECT rowid, chat_id, sender, timestamp, body, type, quote_ts, quote_author, quote_body, edited_at, deleted, expires_in, expire_started_at, expires_at, styles_json, raw_json FROM messages"
  "Base SELECT clause for message queries.")

(defun sgn-db-insert-message (attrs)
  "Insert message from ATTRS plist.  Return the new rowid.
ATTRS keys: :chat-id :sender :timestamp :body :type :quote-ts
:quote-author :quote-body :edited-at :deleted :expires-in
:expire-started-at :expires-at :styles-json :raw-json.
Uses INSERT OR IGNORE for the UNIQUE constraint."
  (sgn-db--ensure)
  (sqlite-execute
   sgn-db--connection
   "INSERT OR IGNORE INTO messages (chat_id, sender, timestamp, body, type, quote_ts, quote_author, quote_body, edited_at, deleted, expires_in, expire_started_at, expires_at, styles_json, raw_json) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
   (sgn-db--message-values-from-attrs attrs))
  (caar (sqlite-select sgn-db--connection "SELECT last_insert_rowid()")))

(defun sgn-db--message-values-from-attrs (attrs)
  "Extract an ordered list of values from message ATTRS plist."
  (list (plist-get attrs :chat-id)
        (plist-get attrs :sender)
        (plist-get attrs :timestamp)
        (plist-get attrs :body)
        (plist-get attrs :type)
        (plist-get attrs :quote-ts)
        (plist-get attrs :quote-author)
        (plist-get attrs :quote-body)
        (plist-get attrs :edited-at)
        (or (plist-get attrs :deleted) 0)
        (or (plist-get attrs :expires-in) 0)
        (plist-get attrs :expire-started-at)
        (plist-get attrs :expires-at)
        (plist-get attrs :styles-json)
        (plist-get attrs :raw-json)))

(defun sgn-db-get-messages (chat-id &optional limit offset)
  "Return list of message plists for CHAT-ID, ordered by timestamp ASC.
LIMIT defaults to 50.  OFFSET defaults to 0.  Retrieves the most
recent messages via ORDER BY timestamp DESC with LIMIT/OFFSET, then
reverses to produce chronological order."
  (sgn-db--ensure)
  (let* ((lim (or limit 50))
         (off (or offset 0))
         (sql (concat sgn-db--message-select
                      " WHERE chat_id = ? ORDER BY timestamp DESC LIMIT ? OFFSET ?"))
         (rows (sqlite-select sgn-db--connection sql (list chat-id lim off))))
    (nreverse
     (mapcar (lambda (row) (sgn-db--row-to-plist row sgn-db--message-columns))
             rows))))

(defun sgn-db-get-message (chat-id sender timestamp)
  "Return message plist matching CHAT-ID, SENDER, TIMESTAMP, or nil."
  (sgn-db--ensure)
  (let ((row (car (sqlite-select
                   sgn-db--connection
                   (concat sgn-db--message-select
                           " WHERE chat_id = ? AND sender = ? AND timestamp = ?")
                   (list chat-id sender timestamp)))))
    (when row
      (sgn-db--row-to-plist row sgn-db--message-columns))))

(defun sgn-db-get-message-by-rowid (rowid)
  "Return message plist for ROWID, or nil."
  (sgn-db--ensure)
  (let ((row (car (sqlite-select
                   sgn-db--connection
                   (concat sgn-db--message-select " WHERE rowid = ?")
                   (list rowid)))))
    (when row
      (sgn-db--row-to-plist row sgn-db--message-columns))))

(defconst sgn-db--message-updatable-attrs
  '((:body . "body")
    (:edited-at . "edited_at")
    (:deleted . "deleted")
    (:expires-in . "expires_in")
    (:expire-started-at . "expire_started_at")
    (:expires-at . "expires_at")
    (:styles-json . "styles_json"))
  "Alist mapping updatable message attribute keywords to SQL column names.")

(defun sgn-db-update-message (rowid attrs)
  "Update message ROWID with ATTRS plist.
ATTRS keys: :body :edited-at :deleted :expires-in :expire-started-at
:expires-at :styles-json."
  (sgn-db--ensure)
  (let ((provided (sgn-db--extract-provided-attrs attrs sgn-db--message-updatable-attrs)))
    (when provided
      (sgn-db--execute-update "messages" "rowid" rowid provided))))

(defun sgn-db--execute-update (table id-column id-value provided)
  "Execute UPDATE on TABLE where ID-COLUMN = ID-VALUE with PROVIDED pairs.
PROVIDED is a list of (column . value) cons cells."
  (let* ((set-clause (mapconcat (lambda (p) (format "%s = ?" (car p)))
                                provided ", "))
         (values (mapcar #'cdr provided))
         (sql (format "UPDATE %s SET %s WHERE %s = ?" table set-clause id-column)))
    (sqlite-execute sgn-db--connection sql (append values (list id-value)))))

(defun sgn-db-delete-message (rowid)
  "Mark message ROWID as deleted.
Set deleted=1, clear body and styles_json."
  (sgn-db--ensure)
  (sqlite-execute sgn-db--connection
                  "UPDATE messages SET deleted = 1, body = NULL, styles_json = NULL WHERE rowid = ?"
                  (list rowid)))

;;;; Reactions

(defconst sgn-db--reaction-columns
  '("message_rowid" "chat_id" "target_author" "target_timestamp"
    "sender" "emoji" "removed")
  "Column names for the reactions table.")

(defun sgn-db-upsert-reaction (attrs)
  "Insert or update reaction.
ATTRS: :message-rowid :chat-id :target-author :target-timestamp
:sender :emoji :removed."
  (sgn-db--ensure)
  (sqlite-execute
   sgn-db--connection
   "INSERT INTO reactions (message_rowid, chat_id, target_author, target_timestamp, sender, emoji, removed) VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT(chat_id, target_author, target_timestamp, sender) DO UPDATE SET emoji = excluded.emoji, removed = excluded.removed, message_rowid = excluded.message_rowid"
   (list (plist-get attrs :message-rowid)
         (plist-get attrs :chat-id)
         (plist-get attrs :target-author)
         (plist-get attrs :target-timestamp)
         (plist-get attrs :sender)
         (plist-get attrs :emoji)
         (or (plist-get attrs :removed) 0))))

(defun sgn-db-remove-reaction (chat-id target-author target-ts sender)
  "Mark the reaction as removed.
CHAT-ID, TARGET-AUTHOR, TARGET-TS, and SENDER identify the reaction."
  (sgn-db--ensure)
  (sqlite-execute
   sgn-db--connection
   "UPDATE reactions SET removed = 1 WHERE chat_id = ? AND target_author = ? AND target_timestamp = ? AND sender = ?"
   (list chat-id target-author target-ts sender)))

(defun sgn-db-get-reactions (message-rowid)
  "Return list of non-removed reaction plists for MESSAGE-ROWID."
  (sgn-db--ensure)
  (let ((rows (sqlite-select
               sgn-db--connection
               "SELECT message_rowid, chat_id, target_author, target_timestamp, sender, emoji, removed FROM reactions WHERE message_rowid = ? AND removed = 0"
               (list message-rowid))))
    (mapcar (lambda (row) (sgn-db--row-to-plist row sgn-db--reaction-columns))
            rows)))

;;;; Media

(defconst sgn-db--media-columns
  '("id" "message_rowid" "chat_id" "content_type" "file_path"
    "file_name" "is_voice" "is_sticker" "width" "height")
  "Column names for the media table.")

(defun sgn-db-insert-media (attrs)
  "Insert media record.  Return the new id.
ATTRS: :message-rowid :chat-id :content-type :file-path :file-name
:is-voice :is-sticker :width :height."
  (sgn-db--ensure)
  (sqlite-execute
   sgn-db--connection
   "INSERT INTO media (message_rowid, chat_id, content_type, file_path, file_name, is_voice, is_sticker, width, height) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
   (list (plist-get attrs :message-rowid)
         (plist-get attrs :chat-id)
         (plist-get attrs :content-type)
         (plist-get attrs :file-path)
         (plist-get attrs :file-name)
         (or (plist-get attrs :is-voice) 0)
         (or (plist-get attrs :is-sticker) 0)
         (plist-get attrs :width)
         (plist-get attrs :height)))
  (caar (sqlite-select sgn-db--connection "SELECT last_insert_rowid()")))

(defun sgn-db-get-media (message-rowid)
  "Return list of media plists for MESSAGE-ROWID."
  (sgn-db--ensure)
  (let ((rows (sqlite-select
               sgn-db--connection
               "SELECT id, message_rowid, chat_id, content_type, file_path, file_name, is_voice, is_sticker, width, height FROM media WHERE message_rowid = ?"
               (list message-rowid))))
    (mapcar (lambda (row) (sgn-db--row-to-plist row sgn-db--media-columns))
            rows)))

;;;; Receipts

(defconst sgn-db--receipt-columns
  '("message_rowid" "chat_id" "target_author" "target_timestamp"
    "recipient" "type" "received_at")
  "Column names for the receipts table.")

(defun sgn-db-upsert-receipt (attrs)
  "Insert or update receipt.
ATTRS: :message-rowid :chat-id :target-author :target-timestamp
:recipient :type :received-at."
  (sgn-db--ensure)
  (sqlite-execute
   sgn-db--connection
   "INSERT INTO receipts (message_rowid, chat_id, target_author, target_timestamp, recipient, type, received_at) VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT(chat_id, target_author, target_timestamp, recipient, type) DO UPDATE SET received_at = excluded.received_at, message_rowid = excluded.message_rowid"
   (list (plist-get attrs :message-rowid)
         (plist-get attrs :chat-id)
         (plist-get attrs :target-author)
         (plist-get attrs :target-timestamp)
         (plist-get attrs :recipient)
         (plist-get attrs :type)
         (plist-get attrs :received-at))))

(defun sgn-db-get-receipts (message-rowid)
  "Return list of receipt plists for MESSAGE-ROWID."
  (sgn-db--ensure)
  (let ((rows (sqlite-select
               sgn-db--connection
               "SELECT message_rowid, chat_id, target_author, target_timestamp, recipient, type, received_at FROM receipts WHERE message_rowid = ?"
               (list message-rowid))))
    (mapcar (lambda (row) (sgn-db--row-to-plist row sgn-db--receipt-columns))
            rows)))

;;;; Search

(defconst sgn-db--search-columns
  '("rowid" "chat_id" "sender" "timestamp" "body" "type"
    "quote_ts" "quote_author" "quote_body" "edited_at" "deleted"
    "expires_in" "expire_started_at" "expires_at" "styles_json" "raw_json"
    "snippet")
  "Column names for search query results (message columns plus snippet).")

(defun sgn-db-search (query &optional chat-id limit)
  "Full-text search for QUERY.  Return message plists with FTS5 highlighting.
If CHAT-ID is non-nil, restrict to that chat.  LIMIT defaults to 50.
Exclude deleted messages.  Add :snippet key to each result with
highlighted matches delimited by brackets."
  (sgn-db--ensure)
  (let* ((lim (or limit 50))
         (base (concat "SELECT m.rowid, m.chat_id, m.sender, m.timestamp, m.body, m.type,"
                       " m.quote_ts, m.quote_author, m.quote_body, m.edited_at, m.deleted,"
                       " m.expires_in, m.expire_started_at, m.expires_at, m.styles_json, m.raw_json,"
                       " highlight(messages_fts, 0, '[', ']') AS snippet"
                       " FROM messages_fts"
                       " JOIN messages m ON m.rowid = messages_fts.rowid"
                       " WHERE messages_fts MATCH ? AND m.deleted = 0"))
         (sql (sgn-db--build-search-sql base chat-id lim))
         (params (sgn-db--build-search-params query chat-id lim))
         (rows (sqlite-select sgn-db--connection sql params)))
    (mapcar (lambda (row) (sgn-db--row-to-plist row sgn-db--search-columns))
            rows)))

(defun sgn-db--build-search-sql (base chat-id limit)
  "Construct the full search SQL from BASE, optionally filtering by CHAT-ID.
LIMIT caps the number of results."
  (concat base
          (when chat-id " AND m.chat_id = ?")
          " ORDER BY m.timestamp DESC"
          (format " LIMIT %d" limit)))

(defun sgn-db--build-search-params (query chat-id _limit)
  "Build the parameter list for a search with QUERY and optional CHAT-ID.
LIMIT is used in the SQL string directly, not as a parameter."
  (if chat-id (list query chat-id) (list query)))

;;;; Unread tracking

(defun sgn-db-set-unread (chat-id count)
  "Set unread COUNT for CHAT-ID."
  (sgn-db--ensure)
  (sqlite-execute sgn-db--connection
                  "UPDATE chats SET unread = ? WHERE id = ?"
                  (list count chat-id)))

(defun sgn-db-increment-unread (chat-id)
  "Increment unread count for CHAT-ID by 1."
  (sgn-db--ensure)
  (sqlite-execute sgn-db--connection
                  "UPDATE chats SET unread = unread + 1 WHERE id = ?"
                  (list chat-id)))

;;;; Draft persistence

(defun sgn-db-save-draft (chat-id text)
  "Save draft TEXT for CHAT-ID.
If TEXT is nil or empty, clear the draft."
  (sgn-db--ensure)
  (let ((value (if (and text (not (string-empty-p text))) text nil)))
    (sqlite-execute sgn-db--connection
                    "UPDATE chats SET draft = ? WHERE id = ?"
                    (list value chat-id))))

(defun sgn-db-get-draft (chat-id)
  "Return draft text for CHAT-ID, or nil."
  (sgn-db--ensure)
  (caar (sqlite-select sgn-db--connection
                       "SELECT draft FROM chats WHERE id = ?"
                       (list chat-id))))

;;;; Polls

(defconst sgn-db--poll-columns
  '("message_rowid" "chat_id" "poll_author" "poll_timestamp"
    "question" "options_json" "votes_json" "self_vote_count" "closed")
  "Column names for the polls table.")

(defun sgn-db-upsert-poll (attrs)
  "Insert or update poll.
ATTRS: :message-rowid :chat-id :poll-author :poll-timestamp
:question :options-json :votes-json :self-vote-count :closed."
  (sgn-db--ensure)
  (sqlite-execute
   sgn-db--connection
   "INSERT INTO polls (message_rowid, chat_id, poll_author, poll_timestamp, question, options_json, votes_json, self_vote_count, closed) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(chat_id, poll_author, poll_timestamp) DO UPDATE SET votes_json = excluded.votes_json, self_vote_count = excluded.self_vote_count, closed = excluded.closed, message_rowid = excluded.message_rowid"
   (list (plist-get attrs :message-rowid)
         (plist-get attrs :chat-id)
         (plist-get attrs :poll-author)
         (plist-get attrs :poll-timestamp)
         (plist-get attrs :question)
         (plist-get attrs :options-json)
         (plist-get attrs :votes-json)
         (or (plist-get attrs :self-vote-count) 0)
         (or (plist-get attrs :closed) 0))))

(defun sgn-db-get-poll (message-rowid)
  "Return poll plist for MESSAGE-ROWID, or nil."
  (sgn-db--ensure)
  (let ((row (car (sqlite-select
                   sgn-db--connection
                   "SELECT message_rowid, chat_id, poll_author, poll_timestamp, question, options_json, votes_json, self_vote_count, closed FROM polls WHERE message_rowid = ?"
                   (list message-rowid)))))
    (when row
      (sgn-db--row-to-plist row sgn-db--poll-columns))))

;;;; Pins

(defconst sgn-db--pin-columns
  '("message_rowid" "chat_id" "target_author" "target_timestamp"
    "pinned_by" "pinned_at" "pin_expires_at")
  "Column names for the pins table.")

(defun sgn-db-insert-pin (attrs)
  "Insert pin.
ATTRS: :message-rowid :chat-id :target-author :target-timestamp
:pinned-by :pinned-at :pin-expires-at."
  (sgn-db--ensure)
  (sqlite-execute
   sgn-db--connection
   "INSERT OR REPLACE INTO pins (message_rowid, chat_id, target_author, target_timestamp, pinned_by, pinned_at, pin_expires_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
   (list (plist-get attrs :message-rowid)
         (plist-get attrs :chat-id)
         (plist-get attrs :target-author)
         (plist-get attrs :target-timestamp)
         (plist-get attrs :pinned-by)
         (plist-get attrs :pinned-at)
         (plist-get attrs :pin-expires-at))))

(defun sgn-db-remove-pin (chat-id target-author target-ts)
  "Remove pin identified by CHAT-ID, TARGET-AUTHOR, TARGET-TS."
  (sgn-db--ensure)
  (sqlite-execute
   sgn-db--connection
   "DELETE FROM pins WHERE chat_id = ? AND target_author = ? AND target_timestamp = ?"
   (list chat-id target-author target-ts)))

(defun sgn-db-get-pins (chat-id)
  "Return list of pin plists for CHAT-ID."
  (sgn-db--ensure)
  (let ((rows (sqlite-select
               sgn-db--connection
               "SELECT message_rowid, chat_id, target_author, target_timestamp, pinned_by, pinned_at, pin_expires_at FROM pins WHERE chat_id = ?"
               (list chat-id))))
    (mapcar (lambda (row) (sgn-db--row-to-plist row sgn-db--pin-columns))
            rows)))

;;;; Expiration

(defun sgn-db-purge-expired ()
  "Delete messages where expires_at is non-NULL and past current time.
FTS index entries are removed automatically by triggers.  Return
count of purged messages."
  (sgn-db--ensure)
  (let ((now (truncate (float-time))))
    (sqlite-execute sgn-db--connection
                    "DELETE FROM messages WHERE expires_at IS NOT NULL AND expires_at <= ?"
                    (list now))))

(provide 'sgn-db)
;;; sgn-db.el ends here
