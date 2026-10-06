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

;; SQLite persistence layer for sgn.  It owns the schema and its
;; migrations, and provides the queries the rest of sgn uses.
;;
;; Identity model.  A person is identified by their ACI (the UUID
;; signal-cli reports as `sourceUuid'); a phone number is only used
;; until the ACI is known, and `sgn-db-merge-identity' later rewrites
;; it everywhere.  A chat is identified by the person's ACI or by the
;; base64 group ID, and its `type' column says which.  A message is
;; identified by (chat_id, sender, timestamp), where timestamp is the
;; one Signal assigned.
;;
;; Reactions, receipts and pins refer to their message by that same
;; key rather than by rowid, so they can arrive before the message.
;; Edits, deletions and read markers that arrive first are kept in the
;; `pending' table and applied when the message is stored.
;;
;; Reads go through `sgn-db-select', which signals SQLite errors:
;; plain `sqlite-select' silently returns no rows when a statement
;; fails while stepping.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function sgn--log "sgn")

;;;; Customization

(defcustom sgn-db-directory
  (expand-file-name "sgn" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Directory for sgn SQLite database."
  :type 'directory
  :group 'sgn)

;;;; Internal state

(defvar sgn-db--connection nil
  "Active SQLite database connection, or nil.")

(defvar sgn-db--transaction-depth 0
  "Nesting depth of `sgn-db-with-transaction'.")

(defvar sgn-db-merge-functions nil
  "Functions called with a phone number and the ACI it was merged into.")

(defvar sgn-db--merged (make-hash-table :test 'equal)
  "Map from phone numbers already merged to their ACI, this session.")

(defconst sgn-db--schema-version 3
  "Schema version this code reads and writes.")

;;;; Low-level helpers

(defun sgn-db--db-path ()
  "Return the full path to the sgn database file."
  (expand-file-name "sgn.db" sgn-db-directory))

(defun sgn-db--ensure ()
  "Signal an error unless the database is open."
  (unless sgn-db--connection
    (user-error "The sgn database is not open; run M-x sgn-start")))

(defun sgn-db-select (sql &optional params)
  "Return the rows SQL with PARAMS selects, signalling SQLite errors."
  (sgn-db--ensure)
  (let ((set (sqlite-select sgn-db--connection sql params 'set))
        (rows nil))
    (unwind-protect
        (while-let ((row (sqlite-next set)))
          (push row rows))
      (sqlite-finalize set))
    (nreverse rows)))

(defun sgn-db-select-value (sql &optional params)
  "Return the first column of the first row SQL with PARAMS selects."
  (caar (sgn-db-select sql params)))

(defun sgn-db-execute (sql &optional params)
  "Execute SQL with PARAMS; return the number of changed rows."
  (sgn-db--ensure)
  (sqlite-execute sgn-db--connection sql params))

(defmacro sgn-db-with-transaction (&rest body)
  "Run BODY in a database transaction; nested uses join the outer one.
The transaction is rolled back if BODY signals an error."
  (declare (indent 0) (debug t))
  `(if (> sgn-db--transaction-depth 0)
       (let ((sgn-db--transaction-depth (1+ sgn-db--transaction-depth)))
         ,@body)
     (sgn-db--ensure)
     (let ((sgn-db--transaction-depth 1)
           (done nil))
       (sqlite-execute sgn-db--connection "BEGIN")
       (unwind-protect
           (prog1 (progn ,@body)
             (sqlite-execute sgn-db--connection "COMMIT")
             (setq done t))
         (unless done
           (sqlite-execute sgn-db--connection "ROLLBACK"))))))

(defun sgn-db--keyword (column)
  "Return the keyword for SQL COLUMN, with underscores as hyphens."
  (intern (concat ":" (string-replace "_" "-" column))))

(defun sgn-db--column (keyword)
  "Return the SQL column name for KEYWORD."
  (string-replace "-" "_" (substring (symbol-name keyword) 1)))

(defun sgn-db--row-to-plist (row columns)
  "Return ROW as a plist keyed by COLUMNS."
  (cl-loop for col in columns
           for val in row
           nconc (list (sgn-db--keyword col) val)))

(defun sgn-db--update (table key-column key attrs)
  "Set ATTRS in the TABLE row whose KEY-COLUMN is KEY.
ATTRS is a plist of column keywords and values; every key present
is written, and a nil value writes NULL."
  (when attrs
    (let (columns values)
      (cl-loop for (k v) on attrs by #'cddr
               do (push (format "%s = ?" (sgn-db--column k)) columns)
               (push v values))
      (sgn-db-execute
       (format "UPDATE %s SET %s WHERE %s = ?" table
               (string-join (nreverse columns) ", ") key-column)
       (append (nreverse values) (list key))))))

(defun sgn-db-now ()
  "Return the current time in milliseconds since the epoch."
  (truncate (* (float-time) 1000)))

;;;; Schema

(defconst sgn-db--schema
  '("CREATE TABLE meta (
       key   TEXT PRIMARY KEY,
       value TEXT)"
    "CREATE TABLE recipients (
       uuid   TEXT PRIMARY KEY,
       number TEXT,
       name   TEXT)"
    "CREATE INDEX idx_recipients_number ON recipients(number)"
    "CREATE TABLE chats (
       id          TEXT PRIMARY KEY,
       type        TEXT NOT NULL CHECK (type IN ('individual', 'group')),
       name        TEXT,
       last_msg_ts INTEGER,
       muted       INTEGER NOT NULL DEFAULT 0,
       pinned      INTEGER NOT NULL DEFAULT 0,
       draft       TEXT,
       expiration  INTEGER NOT NULL DEFAULT 0)"
    "CREATE TABLE messages (
       rowid       INTEGER PRIMARY KEY AUTOINCREMENT,
       chat_id     TEXT NOT NULL REFERENCES chats(id) ON UPDATE CASCADE,
       sender      TEXT NOT NULL,
       timestamp   INTEGER NOT NULL,
       outgoing    INTEGER NOT NULL DEFAULT 0,
       body        TEXT,
       styles_json TEXT,
       mentions_json TEXT,
       quote_ts    INTEGER,
       quote_author TEXT,
       quote_body  TEXT,
       edited_at   INTEGER,
       deleted     INTEGER NOT NULL DEFAULT 0,
       expires_in  INTEGER NOT NULL DEFAULT 0,
       expire_started_at INTEGER,
       expires_at  INTEGER,
       send_status TEXT,
       edit_status TEXT,
       read_at     INTEGER,
       raw_json    TEXT,
       UNIQUE(chat_id, sender, timestamp))"
    "CREATE INDEX idx_messages_chat_ts ON messages(chat_id, timestamp)"
    "CREATE INDEX idx_messages_sender_ts ON messages(sender, timestamp)"
    "CREATE INDEX idx_messages_unread ON messages(chat_id, read_at)"
    "CREATE INDEX idx_messages_expires ON messages(expires_at)"
    "CREATE VIRTUAL TABLE messages_fts USING fts5(
       body, content='messages', content_rowid='rowid')"
    ;; The index holds every row's body, NULL included, so that the
    ;; triggers stay consistent with a `rebuild' of the index.
    "CREATE TRIGGER messages_ai AFTER INSERT ON messages BEGIN
       INSERT INTO messages_fts(rowid, body) VALUES (new.rowid, new.body);
     END"
    "CREATE TRIGGER messages_ad AFTER DELETE ON messages BEGIN
       INSERT INTO messages_fts(messages_fts, rowid, body)
         VALUES ('delete', old.rowid, old.body);
       DELETE FROM reactions WHERE chat_id = old.chat_id
         AND target_author = old.sender AND target_timestamp = old.timestamp;
       DELETE FROM pins WHERE chat_id = old.chat_id
         AND target_author = old.sender AND target_timestamp = old.timestamp;
     END"
    "CREATE TRIGGER messages_au AFTER UPDATE OF body ON messages BEGIN
       INSERT INTO messages_fts(messages_fts, rowid, body)
         VALUES ('delete', old.rowid, old.body);
       INSERT INTO messages_fts(rowid, body) VALUES (new.rowid, new.body);
     END"
    "CREATE TABLE media (
       id            INTEGER PRIMARY KEY AUTOINCREMENT,
       message_rowid INTEGER NOT NULL REFERENCES messages(rowid) ON DELETE CASCADE,
       content_type  TEXT,
       attachment_id TEXT,
       local_path    TEXT,
       file_name     TEXT,
       is_voice      INTEGER NOT NULL DEFAULT 0,
       sticker_pack_id TEXT,
       sticker_id    INTEGER,
       width         INTEGER,
       height        INTEGER,
       size          INTEGER)"
    "CREATE INDEX idx_media_message ON media(message_rowid)"
    "CREATE TABLE reactions (
       chat_id          TEXT NOT NULL,
       target_author    TEXT NOT NULL,
       target_timestamp INTEGER NOT NULL,
       sender           TEXT NOT NULL,
       emoji            TEXT,
       ts               INTEGER NOT NULL,
       UNIQUE(chat_id, target_author, target_timestamp, sender))"
    "CREATE TABLE receipts (
       target_timestamp INTEGER NOT NULL,
       recipient        TEXT NOT NULL,
       type             TEXT NOT NULL,
       received_at      INTEGER NOT NULL,
       UNIQUE(target_timestamp, recipient, type))"
    "CREATE TABLE pins (
       chat_id          TEXT NOT NULL,
       target_author    TEXT NOT NULL,
       target_timestamp INTEGER NOT NULL,
       pinned_by        TEXT NOT NULL,
       pinned_at        INTEGER NOT NULL,
       PRIMARY KEY(chat_id, target_author, target_timestamp))"
    "CREATE TABLE pending (
       id               INTEGER PRIMARY KEY AUTOINCREMENT,
       chat_id          TEXT NOT NULL,
       target_author    TEXT NOT NULL,
       target_timestamp INTEGER NOT NULL,
       kind             TEXT NOT NULL,
       payload          TEXT,
       ts               INTEGER NOT NULL,
       created_at       INTEGER NOT NULL)"
    "CREATE INDEX idx_pending_target
       ON pending(chat_id, target_author, target_timestamp)")
  "Statements creating the current schema in an empty database.")

(defun sgn-db--create-schema ()
  "Create the current schema in the open, empty database."
  (dolist (statement sgn-db--schema)
    (sgn-db-execute statement)))

;;;; Migrations

(defun sgn-db--schema-version ()
  "Return the schema version of the open database."
  (sgn-db-select-value "PRAGMA user_version"))

(defun sgn-db--set-schema-version (version)
  "Record VERSION as the schema version of the open database."
  (sgn-db-execute (format "PRAGMA user_version = %d" version)))

(defun sgn-db--migrate ()
  "Bring the open database to `sgn-db--schema-version'.
Each step runs in its own transaction.  Signal an error for a
database written by a newer sgn."
  (let ((version (sgn-db--schema-version)))
    (when (> version sgn-db--schema-version)
      (error "The sgn database has schema version %d; this sgn supports %d"
             version sgn-db--schema-version))
    (when (< 0 version sgn-db--schema-version)
      (sgn-db--backup-before-migration version))
    (when (zerop version)
      (sgn-db-with-transaction
        (sgn-db--create-schema)
        (sgn-db--set-schema-version sgn-db--schema-version))
      (setq version sgn-db--schema-version))
    (when (= version 1)
      (sgn-db-with-transaction
        (sgn-db-execute "ALTER TABLE messages ADD COLUMN send_status TEXT")
        (sgn-db--set-schema-version 2))
      (setq version 2))
    (when (= version 2)
      (sgn-db--migrate-2-to-3)
      (setq version 3))
    version))

(defun sgn-db--backup-before-migration (version)
  "Snapshot the database, at schema VERSION, before migrating it."
  (let ((dir (expand-file-name "backups" sgn-db-directory)))
    (make-directory dir t)
    (sgn-db-backup (expand-file-name
                    (format-time-string (format "sgn-v%d-%%Y%%m%%d-%%H%%M%%S.db"
                                                version))
                    dir))))

(defun sgn-db--migrate-2-to-3 ()
  "Rebuild a version 2 database in the version 3 schema.
The old tables are renamed, the new schema created, and the data
copied across.  Foreign keys are off while tables are rebuilt."
  (sgn--log "sgn-db: migrating schema 2 to 3")
  (sgn-db-execute "PRAGMA foreign_keys = OFF")
  (unwind-protect
      (sgn-db-with-transaction
        (dolist (table '("chats" "messages" "media" "reactions" "receipts"
                         "pins" "polls"))
          (sgn-db-execute (format "ALTER TABLE %s RENAME TO old_%s" table table)))
        (sgn-db-execute "DROP TABLE messages_fts")
        (dolist (trigger '("messages_ai" "messages_ad" "messages_au"))
          (sgn-db-execute (format "DROP TRIGGER IF EXISTS %s" trigger)))
        (dolist (index '("idx_messages_chat_ts" "idx_messages_chat_sender_ts"
                         "idx_reactions_target" "idx_media_message"
                         "idx_receipts_target"))
          (sgn-db-execute (format "DROP INDEX IF EXISTS %s" index)))
        (sgn-db--create-schema)
        (sgn-db--copy-version-2-data)
        (dolist (table '("chats" "messages" "media" "reactions" "receipts"
                         "pins" "polls"))
          (sgn-db-execute (format "DROP TABLE old_%s" table)))
        (sgn-db-execute
         "INSERT INTO messages_fts(messages_fts) VALUES ('rebuild')")
        (sgn-db--set-schema-version 3))
    (sgn-db-execute "PRAGMA foreign_keys = ON")))

(defun sgn-db--copy-version-2-data ()
  "Copy rows from the renamed version 2 tables into the new schema.
Version 2 kept a per-chat unread counter; the newest that many
incoming messages of each chat stay unread, the rest are marked
read."
  (sgn-db-execute
   "INSERT INTO chats (id, type, name, last_msg_ts, muted, pinned, draft,
                       expiration)
    SELECT id, CASE WHEN type = 'group' THEN 'group' ELSE 'individual' END,
           NULLIF(name, ''), last_msg_ts, COALESCE(muted, 0),
           COALESCE(pinned, 0), draft, COALESCE(expiration, 0)
    FROM old_chats")
  (sgn-db-execute
   "INSERT INTO messages (rowid, chat_id, sender, timestamp, outgoing, body,
                          styles_json, quote_ts, quote_author, quote_body,
                          edited_at, deleted, expires_in, send_status,
                          read_at, raw_json)
    SELECT m.rowid, m.chat_id, m.sender, m.timestamp,
           CASE WHEN m.type = 'sync' THEN 1 ELSE 0 END,
           m.body, m.styles_json, m.quote_ts, m.quote_author, m.quote_body,
           m.edited_at, COALESCE(m.deleted, 0), COALESCE(m.expires_in, 0),
           m.send_status,
           CASE WHEN m.type = 'sync' THEN m.timestamp
                WHEN (SELECT count(*) FROM old_messages n
                      WHERE n.chat_id = m.chat_id AND n.type != 'sync'
                        AND (n.timestamp > m.timestamp
                             OR (n.timestamp = m.timestamp
                                 AND n.rowid > m.rowid)))
                     < COALESCE((SELECT unread FROM old_chats
                                 WHERE id = m.chat_id), 0)
                THEN NULL
                ELSE m.timestamp END,
           m.raw_json
    FROM old_messages m
    WHERE m.chat_id IN (SELECT id FROM old_chats)")
  (sgn-db-execute
   "INSERT INTO media (message_rowid, content_type, attachment_id,
                       local_path, file_name, is_voice, width, height)
    SELECT message_rowid, content_type,
           CASE WHEN file_path LIKE '%/attachments/%'
                THEN replace(file_path, rtrim(file_path,
                       replace(file_path, '/', '')), '')
                END,
           CASE WHEN file_path NOT LIKE '%/attachments/%' THEN file_path END,
           file_name, COALESCE(is_voice, 0), width, height
    FROM old_media
    WHERE message_rowid IN (SELECT rowid FROM messages)
      AND content_type IS NOT 'text/x-signal-plain'")
  (sgn-db-execute
   "INSERT OR IGNORE INTO receipts (target_timestamp, recipient, type,
                                    received_at)
    SELECT target_timestamp, recipient, type, received_at FROM old_receipts")
  ;; Read messages with a disappearing timer start counting down.
  (sgn-db-execute
   "UPDATE messages SET expire_started_at = read_at,
                        expires_at = read_at + expires_in * 1000
    WHERE expires_in > 0 AND read_at IS NOT NULL")
  (sgn-db-execute
   "INSERT OR IGNORE INTO reactions (chat_id, target_author, target_timestamp,
                                     sender, emoji, ts)
    SELECT chat_id, target_author, target_timestamp, sender,
           CASE WHEN removed = 1 THEN NULL ELSE emoji END, 0
    FROM old_reactions")
  (sgn-db-execute
   "INSERT OR IGNORE INTO pins (chat_id, target_author, target_timestamp,
                                pinned_by, pinned_at)
    SELECT chat_id, target_author, target_timestamp, pinned_by, pinned_at
    FROM old_pins"))

;;;; Database lifecycle

(defun sgn-db-init ()
  "Open the database, creating or migrating its schema as needed.
Signal an error unless SQLite with FTS5 is available."
  (unless (sqlite-available-p)
    (error "SQLite support is not available in this Emacs build"))
  (sgn-db-close)
  (clrhash sgn-db--merged)
  (make-directory sgn-db-directory t)
  (let ((db (sqlite-open (sgn-db--db-path))))
    (condition-case err
        (progn
          (setq sgn-db--connection db)
          (sgn-db--check-fts5)
          (sgn-db-execute "PRAGMA journal_mode = WAL")
          (sgn-db-execute "PRAGMA foreign_keys = ON")
          (sgn-db--migrate))
      (error
       (sqlite-close db)
       (setq sgn-db--connection nil)
       (signal (car err) (cdr err))))
    (sgn--log "sgn-db: opened %s" (sgn-db--db-path))))

(defun sgn-db--check-fts5 ()
  "Signal an error unless SQLite's FTS5 extension is available."
  (condition-case nil
      (progn
        (sgn-db-execute "CREATE VIRTUAL TABLE temp.sgn_fts5_check USING fts5(x)")
        (sgn-db-execute "DROP TABLE temp.sgn_fts5_check"))
    (error
     (user-error "SQLite's FTS5 extension is not available; sgn requires it"))))

(defun sgn-db-close ()
  "Close the database connection, if open."
  (when sgn-db--connection
    (sqlite-close sgn-db--connection)
    (setq sgn-db--connection nil)))

(defun sgn-db-backup (file)
  "Write a consistent snapshot of the database to FILE."
  (sgn-db-execute "VACUUM INTO ?" (list (expand-file-name file))))

;;;; Meta

(defun sgn-db-get-meta (key)
  "Return the meta value stored under KEY, or nil."
  (sgn-db-select-value "SELECT value FROM meta WHERE key = ?" (list key)))

(defun sgn-db-set-meta (key value)
  "Store VALUE under meta KEY."
  (sgn-db-execute "INSERT INTO meta (key, value) VALUES (?, ?)
                   ON CONFLICT(key) DO UPDATE SET value = excluded.value"
                  (list key value)))

;;;; Recipients

(defun sgn-db-upsert-recipient (uuid number name)
  "Record that UUID has phone NUMBER and display NAME.
Nil NUMBER or NAME leave the stored value unchanged."
  (sgn-db-execute
   "INSERT INTO recipients (uuid, number, name) VALUES (?, ?, ?)
    ON CONFLICT(uuid) DO UPDATE SET
      number = COALESCE(excluded.number, number),
      name = COALESCE(excluded.name, name)"
   (list uuid number name)))

(defun sgn-db-get-recipients ()
  "Return all recipients as a list of (UUID NUMBER NAME)."
  (sgn-db-select "SELECT uuid, number, name FROM recipients"))

(defun sgn-db-uuid-for-number (number)
  "Return the ACI recorded for phone NUMBER, or nil."
  (sgn-db-select-value "SELECT uuid FROM recipients WHERE number = ?"
                       (list number)))

;;;; Chats

(defconst sgn-db--chat-columns
  '("id" "type" "name" "last_msg_ts" "muted" "pinned" "draft" "expiration")
  "Columns of the chats table, in the order chat queries return them.")

(defun sgn-db--chat-select (where)
  "Return a SELECT of chat columns and unread count, followed by WHERE."
  (format "SELECT %s, (SELECT count(*) FROM messages m
                       WHERE m.chat_id = chats.id AND m.outgoing = 0
                         AND m.read_at IS NULL AND m.deleted = 0)
           FROM chats %s"
          (string-join sgn-db--chat-columns ", ") where))

(defun sgn-db--chat-row-to-plist (row)
  "Return chat ROW as a plist, including its computed :unread count."
  (append (sgn-db--row-to-plist row sgn-db--chat-columns)
          (list :unread (car (last row)))))

(defun sgn-db-ensure-chat (id type)
  "Create chat ID of TYPE (\"individual\" or \"group\") unless it exists."
  (sgn-db-execute "INSERT OR IGNORE INTO chats (id, type) VALUES (?, ?)"
                  (list id type)))

(defun sgn-db-update-chat (id &rest attrs)
  "Set ATTRS (a plist of chat column keywords) in chat ID.
Every key present is written; a nil value writes NULL."
  (sgn-db--update "chats" "id" id attrs))

(defun sgn-db-touch-chat (id timestamp)
  "Record TIMESTAMP as chat ID's latest activity, if it is newer."
  (sgn-db-execute
   "UPDATE chats SET last_msg_ts = max(COALESCE(last_msg_ts, 0), ?)
    WHERE id = ?"
   (list timestamp id)))

(defun sgn-db-get-chat (id)
  "Return chat ID as a plist, or nil."
  (when-let* ((row (car (sgn-db-select (sgn-db--chat-select "WHERE id = ?")
                                       (list id)))))
    (sgn-db--chat-row-to-plist row)))

(defun sgn-db-get-chats (&optional include-empty)
  "Return chats as plists, pinned first, then most recent first.
Unless INCLUDE-EMPTY is non-nil, only chats with messages."
  (mapcar #'sgn-db--chat-row-to-plist
          (sgn-db-select
           (sgn-db--chat-select
            (concat (unless include-empty
                      "WHERE EXISTS (SELECT 1 FROM messages m
                                     WHERE m.chat_id = chats.id)")
                    " ORDER BY pinned DESC, last_msg_ts DESC")))))

(defun sgn-db-chat-type (id)
  "Return the type of chat ID, \"individual\" or \"group\", or nil."
  (sgn-db-select-value "SELECT type FROM chats WHERE id = ?" (list id)))

(defun sgn-db-total-unread ()
  "Return the number of unread messages in chats that are not muted."
  (sgn-db-select-value
   "SELECT count(*) FROM messages m JOIN chats c ON c.id = m.chat_id
    WHERE m.outgoing = 0 AND m.read_at IS NULL AND m.deleted = 0
      AND c.muted = 0"))

;;;; Messages

(defconst sgn-db--message-columns
  '("rowid" "chat_id" "sender" "timestamp" "outgoing" "body" "styles_json"
    "mentions_json" "quote_ts" "quote_author" "quote_body" "edited_at"
    "deleted" "expires_in" "expire_started_at" "expires_at" "send_status"
    "edit_status" "read_at")
  "Columns message queries return, in order.")

(defun sgn-db--message-select (where)
  "Return a SELECT of message columns, followed by WHERE."
  (format "SELECT %s FROM messages %s"
          (string-join sgn-db--message-columns ", ") where))

(defun sgn-db--messages (where params)
  "Return the messages matching WHERE with PARAMS, as plists."
  (mapcar (lambda (row) (sgn-db--row-to-plist row sgn-db--message-columns))
          (sgn-db-select (sgn-db--message-select where) params)))

(defun sgn-db-insert-message (attrs)
  "Insert the message described by ATTRS, a plist of message columns.
Return its rowid, or nil if a message with the same chat, sender
and timestamp is already stored."
  (let (columns values)
    (cl-loop for (k v) on attrs by #'cddr
             do (push (sgn-db--column k) columns)
             (push v values))
    (sgn-db-select-value
     (format "INSERT INTO messages (%s) VALUES (%s)
              ON CONFLICT(chat_id, sender, timestamp) DO NOTHING
              RETURNING rowid"
             (string-join (nreverse columns) ", ")
             (string-join (make-list (length values) "?") ", "))
     (nreverse values))))

(defun sgn-db-update-message (rowid &rest attrs)
  "Set ATTRS (a plist of message column keywords) in message ROWID.
Every key present is written; a nil value writes NULL."
  (sgn-db--update "messages" "rowid" rowid attrs))

(defun sgn-db-get-message (chat-id sender timestamp)
  "Return the message SENDER sent at TIMESTAMP in CHAT-ID, or nil."
  (car (sgn-db--messages "WHERE chat_id = ? AND sender = ? AND timestamp = ?"
                         (list chat-id sender timestamp))))

(defun sgn-db-get-message-by-rowid (rowid)
  "Return message ROWID as a plist, or nil."
  (car (sgn-db--messages "WHERE rowid = ?" (list rowid))))

(defun sgn-db-get-messages (chat-id &optional limit)
  "Return the latest LIMIT messages of CHAT-ID, oldest first.
LIMIT defaults to 50.  Messages past their expiry are left out."
  (nreverse
   (sgn-db--messages
    "WHERE chat_id = ? AND (expires_at IS NULL OR expires_at > ?)
     ORDER BY timestamp DESC, rowid DESC LIMIT ?"
    (list chat-id (sgn-db-now) (or limit 50)))))

(defun sgn-db-latest-messages ()
  "Return a hash table from chat ID to that chat's latest message."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (row (sgn-db-select
                  (format
                   "SELECT %s FROM (SELECT *, row_number() OVER (
                      PARTITION BY chat_id ORDER BY timestamp DESC, rowid DESC)
                      AS position
                    FROM messages WHERE expires_at IS NULL OR expires_at > ?)
                    WHERE position = 1"
                   (string-join sgn-db--message-columns ", "))
                  (list (sgn-db-now))))
      (let ((msg (sgn-db--row-to-plist row sgn-db--message-columns)))
        (puthash (plist-get msg :chat-id) msg table)))
    table))

(defun sgn-db-count-messages (chat-id)
  "Return the number of messages stored for CHAT-ID."
  (sgn-db-select-value "SELECT count(*) FROM messages WHERE chat_id = ?"
                       (list chat-id)))

(defun sgn-db-find-own-message (self-ids timestamp)
  "Return the message sent at TIMESTAMP by any sender in SELF-IDS, or nil."
  (car (sgn-db--messages
        (format "WHERE timestamp = ? AND sender IN (%s)"
                (string-join (make-list (length self-ids) "?") ", "))
        (cons timestamp self-ids))))

(defun sgn-db-mark-deleted (rowid)
  "Mark message ROWID deleted and drop its content and media.
Return its media, whose files the caller may delete."
  (let ((media (sgn-db-get-media rowid)))
    (sgn-db-execute
     "UPDATE messages SET deleted = 1, body = NULL, styles_json = NULL,
             mentions_json = NULL, quote_body = NULL, raw_json = NULL
      WHERE rowid = ?"
     (list rowid))
    (sgn-db-execute "DELETE FROM media WHERE message_rowid = ?" (list rowid))
    media))

(defun sgn-db-unread-messages (chat-id)
  "Return (SENDER TIMESTAMP ROWID) for each unread message in CHAT-ID."
  (sgn-db-select
   "SELECT sender, timestamp, rowid FROM messages
    WHERE chat_id = ? AND outgoing = 0 AND read_at IS NULL AND deleted = 0
    ORDER BY timestamp"
   (list chat-id)))

(defun sgn-db-mark-read (rowid time)
  "Mark message ROWID read at TIME, unless it already is.
A message with a disappearing timer starts counting down."
  (sgn-db-execute
   "UPDATE messages SET read_at = ?1,
      expire_started_at = CASE WHEN expires_in > 0
        THEN COALESCE(expire_started_at, ?1) END,
      expires_at = CASE WHEN expires_in > 0
        THEN COALESCE(expire_started_at, ?1) + expires_in * 1000 END
    WHERE rowid = ?2 AND read_at IS NULL"
   (list time rowid)))

(defun sgn-db-settle-interrupted-sends ()
  "Mark sends and edits that never got an outcome as unconfirmed.
Their replies can no longer arrive once sgn has been restarted."
  (sgn-db-execute "UPDATE messages SET send_status = 'unconfirmed'
                   WHERE send_status = 'sending'")
  (sgn-db-execute "UPDATE messages SET edit_status = 'unconfirmed'
                   WHERE edit_status = 'sending'"))

;;;; Expiry

(defun sgn-db-expired-messages (now)
  "Return (ROWID CHAT-ID) for messages that expired before NOW."
  (sgn-db-select "SELECT rowid, chat_id FROM messages
                  WHERE expires_at IS NOT NULL AND expires_at <= ?"
                 (list now)))

(defun sgn-db-delete-message (rowid)
  "Delete message ROWID and everything attached to it.
Return its media, whose files the caller may delete."
  (prog1 (sgn-db-get-media rowid)
    (sgn-db-execute "DELETE FROM messages WHERE rowid = ?" (list rowid))))

;;;; Media

(defconst sgn-db--media-columns
  '("id" "message_rowid" "content_type" "attachment_id" "local_path"
    "file_name" "is_voice" "sticker_pack_id" "sticker_id" "width" "height"
    "size")
  "Columns of the media table, in order.")

(defun sgn-db-insert-media (message-rowid attrs)
  "Store media described by ATTRS for MESSAGE-ROWID.
ATTRS is a plist of media column keywords."
  (let ((attrs (append (list :message-rowid message-rowid) attrs))
        columns values)
    (cl-loop for (k v) on attrs by #'cddr
             do (push (sgn-db--column k) columns)
             (push v values))
    (sgn-db-execute
     (format "INSERT INTO media (%s) VALUES (%s)"
             (string-join (nreverse columns) ", ")
             (string-join (make-list (length values) "?") ", "))
     (nreverse values))))

(defun sgn-db-get-media (message-rowid)
  "Return the media of MESSAGE-ROWID as plists."
  (mapcar (lambda (row) (sgn-db--row-to-plist row sgn-db--media-columns))
          (sgn-db-select
           (format "SELECT %s FROM media WHERE message_rowid = ? ORDER BY id"
                   (string-join sgn-db--media-columns ", "))
           (list message-rowid))))

;;;; Reactions

(defun sgn-db-set-reaction (chat-id target-author target-ts sender emoji ts)
  "Record SENDER's reaction EMOJI, made at TS, to a message.
The message is the one TARGET-AUTHOR sent at TARGET-TS in CHAT-ID.
A nil EMOJI records the reaction's removal.  An update older than
the stored one is ignored.  Return non-nil if the stored reaction
changed."
  (> (sgn-db-execute
      "INSERT INTO reactions (chat_id, target_author, target_timestamp, sender,
                              emoji, ts)
       VALUES (?, ?, ?, ?, ?, ?)
       ON CONFLICT(chat_id, target_author, target_timestamp, sender)
       DO UPDATE SET emoji = excluded.emoji, ts = excluded.ts
       WHERE excluded.ts > reactions.ts
          OR (excluded.ts = reactions.ts
              AND excluded.emoji IS NOT reactions.emoji)"
      (list chat-id target-author target-ts sender emoji ts))
     0))

(defun sgn-db-get-reactions (chat-id target-author target-ts)
  "Return (SENDER EMOJI) for the reactions to a message.
The message is the one TARGET-AUTHOR sent at TARGET-TS in CHAT-ID."
  (sgn-db-select
   "SELECT sender, emoji FROM reactions
    WHERE chat_id = ? AND target_author = ? AND target_timestamp = ?
      AND emoji IS NOT NULL
    ORDER BY ts"
   (list chat-id target-author target-ts)))

;;;; Receipts

(defun sgn-db-add-receipt (target-ts recipient type time)
  "Record that RECIPIENT sent a TYPE receipt at TIME for TARGET-TS.
TARGET-TS is the timestamp of one of our own messages."
  (sgn-db-execute
   "INSERT OR IGNORE INTO receipts (target_timestamp, recipient, type,
                                    received_at)
    VALUES (?, ?, ?, ?)"
   (list target-ts recipient type time)))

(defun sgn-db-receipt-types (target-ts)
  "Return the receipt types recorded for our message sent at TARGET-TS."
  (mapcar #'car (sgn-db-select
                 "SELECT DISTINCT type FROM receipts WHERE target_timestamp = ?"
                 (list target-ts))))

;;;; Pins

(defun sgn-db-set-pin (chat-id target-author target-ts pinned-by time)
  "Record that PINNED-BY pinned a message at TIME.
The message is the one TARGET-AUTHOR sent at TARGET-TS in CHAT-ID.
With a nil PINNED-BY, record that it was unpinned."
  (if pinned-by
      (sgn-db-execute
       "INSERT OR REPLACE INTO pins (chat_id, target_author, target_timestamp,
                                     pinned_by, pinned_at)
        VALUES (?, ?, ?, ?, ?)"
       (list chat-id target-author target-ts pinned-by time))
    (sgn-db-execute
     "DELETE FROM pins WHERE chat_id = ? AND target_author = ?
                         AND target_timestamp = ?"
     (list chat-id target-author target-ts))))

(defun sgn-db-pinned-p (chat-id target-author target-ts)
  "Return non-nil if a message in CHAT-ID is pinned.
The message is the one TARGET-AUTHOR sent at TARGET-TS."
  (sgn-db-select-value
   "SELECT 1 FROM pins WHERE chat_id = ? AND target_author = ?
                         AND target_timestamp = ?"
   (list chat-id target-author target-ts)))

;;;; Pending mutations

(defun sgn-db-add-pending (chat-id target-author target-ts kind payload ts)
  "Keep a KIND mutation, made at TS, for a message not yet stored.
The message is the one TARGET-AUTHOR sent at TARGET-TS in CHAT-ID.
PAYLOAD is a string, or nil."
  (sgn-db-execute
   "INSERT INTO pending (chat_id, target_author, target_timestamp, kind,
                         payload, ts, created_at)
    VALUES (?, ?, ?, ?, ?, ?, ?)"
   (list chat-id target-author target-ts kind payload ts (sgn-db-now))))

(defun sgn-db-take-pending (chat-id target-author target-ts)
  "Remove and return the pending mutations of a message, oldest first.
The message is the one TARGET-AUTHOR sent at TARGET-TS in CHAT-ID.
Mutations recorded for chat \"*\" apply to that message in any
chat.  Each is a list (KIND PAYLOAD TS)."
  (let* ((where "WHERE chat_id IN (?, '*') AND target_author = ?
                   AND target_timestamp = ?")
         (params (list chat-id target-author target-ts))
         (rows (sgn-db-select
                (concat "SELECT kind, payload, ts FROM pending " where
                        " ORDER BY ts, id")
                params)))
    (when rows
      (sgn-db-execute (concat "DELETE FROM pending " where) params))
    rows))

(defun sgn-db-purge-pending (before)
  "Forget pending mutations recorded before BEFORE."
  (sgn-db-execute "DELETE FROM pending WHERE created_at < ?" (list before)))

;;;; Drafts

(defun sgn-db-save-draft (chat-id text)
  "Save draft TEXT for CHAT-ID; nil or blank TEXT clears it."
  (sgn-db-update-chat chat-id :draft (unless (string-blank-p (or text ""))
                                       text)))

(defun sgn-db-get-draft (chat-id)
  "Return the draft saved for CHAT-ID, or nil."
  (sgn-db-select-value "SELECT draft FROM chats WHERE id = ?" (list chat-id)))

;;;; Search

(defun sgn-db--fts-query (query)
  "Return an FTS5 expression matching every word of QUERY.
Each word is quoted, so punctuation and FTS5 operators in QUERY
are searched for literally."
  (mapconcat (lambda (word)
               (concat "\"" (string-replace "\"" "\"\"" word) "\""))
             (split-string query nil t)
             " "))

(defun sgn-db-search (query &optional chat-id limit)
  "Return messages matching every word of QUERY, newest first.
If CHAT-ID is non-nil, search only that chat.  LIMIT defaults to
50.  Each message plist has a :snippet with matches in brackets."
  (let ((expression (sgn-db--fts-query query)))
    (unless (string-empty-p expression)
      (mapcar
       (lambda (row)
         (append (sgn-db--row-to-plist row sgn-db--message-columns)
                 (list :snippet (car (last row)))))
       (sgn-db-select
        (format "SELECT %s, highlight(messages_fts, 0, '[', ']')
                 FROM messages_fts JOIN messages m
                   ON m.rowid = messages_fts.rowid
                 WHERE messages_fts MATCH ? AND m.deleted = 0 %s
                 ORDER BY m.timestamp DESC LIMIT ?"
                (mapconcat (lambda (c) (concat "m." c))
                           sgn-db--message-columns ", ")
                (if chat-id "AND m.chat_id = ?" ""))
        (append (list expression) (when chat-id (list chat-id))
                (list (or limit 50))))))))

;;;; Identity merging

(defun sgn-db-identity-needs-merge-p (number)
  "Return non-nil if any stored data still refers to phone NUMBER."
  (eql 1 (sgn-db-select-value
          "SELECT EXISTS (SELECT 1 FROM chats WHERE id = ?1)
               OR EXISTS (SELECT 1 FROM messages WHERE sender = ?1)
               OR EXISTS (SELECT 1 FROM messages WHERE quote_author = ?1)
               OR EXISTS (SELECT 1 FROM reactions
                          WHERE sender = ?1 OR target_author = ?1)
               OR EXISTS (SELECT 1 FROM receipts WHERE recipient = ?1)
               OR EXISTS (SELECT 1 FROM pins
                          WHERE target_author = ?1 OR pinned_by = ?1)
               OR EXISTS (SELECT 1 FROM pending
                          WHERE target_author = ?1 OR chat_id = ?1)"
          (list number))))

(defun sgn-db-merge-identity (number uuid)
  "Rewrite every reference to phone NUMBER as the ACI UUID.
A chat stored under NUMBER is merged into the one under UUID.
Messages stored under both identities are merged into one, the
copy under UUID surviving.  Return non-nil if anything changed."
  (sgn-db-upsert-recipient uuid number nil)
  (when (and (not (equal (gethash number sgn-db--merged) uuid))
             (prog1 (sgn-db-identity-needs-merge-p number)
               (puthash number uuid sgn-db--merged)))
    (sgn-db-with-transaction
      (sgn-db--merge-senders number uuid)
      (sgn-db--merge-chat number uuid)
      (sgn-db-execute "UPDATE messages SET quote_author = ? WHERE quote_author = ?"
                      (list uuid number))
      (sgn-db--merge-column "reactions" "target_author" number uuid)
      (sgn-db--merge-column "reactions" "sender" number uuid)
      (sgn-db--merge-column "pins" "target_author" number uuid)
      (sgn-db--merge-column "pins" "pinned_by" number uuid)
      (sgn-db--merge-column "receipts" "recipient" number uuid)
      (sgn-db-execute "UPDATE pending SET target_author = ? WHERE target_author = ?"
                      (list uuid number))
      (sgn-db-execute "UPDATE pending SET chat_id = ? WHERE chat_id = ?"
                      (list uuid number)))
    (run-hook-with-args 'sgn-db-merge-functions number uuid)
    t))

(defun sgn-db--merge-senders (number uuid)
  "Rewrite messages sent by NUMBER as sent by UUID, merging duplicates."
  (sgn-db-execute "UPDATE OR IGNORE messages SET sender = ? WHERE sender = ?"
                  (list uuid number))
  (pcase-dolist (`(,rowid ,chat-id ,ts)
                 (sgn-db-select "SELECT rowid, chat_id, timestamp FROM messages
                                 WHERE sender = ?"
                                (list number)))
    (sgn-db--merge-duplicate
     rowid (sgn-db-select-value
            "SELECT rowid FROM messages
             WHERE chat_id = ? AND sender = ? AND timestamp = ?"
            (list chat-id uuid ts)))))

(defun sgn-db--merge-chat (number uuid)
  "Merge the chat stored under NUMBER into the chat under UUID."
  (cond
   ((not (sgn-db-chat-type number)))
   ((not (sgn-db-chat-type uuid))
    (sgn-db-execute "UPDATE chats SET id = ? WHERE id = ?" (list uuid number)))
   (t
    (sgn-db-execute
     "UPDATE chats SET
        last_msg_ts = max(COALESCE(last_msg_ts, 0),
          COALESCE((SELECT last_msg_ts FROM chats WHERE id = ?2), 0)),
        muted = max(muted, (SELECT muted FROM chats WHERE id = ?2)),
        pinned = max(pinned, (SELECT pinned FROM chats WHERE id = ?2)),
        draft = COALESCE(draft, (SELECT draft FROM chats WHERE id = ?2)),
        name = COALESCE(name, (SELECT name FROM chats WHERE id = ?2))
      WHERE id = ?1"
     (list uuid number))
    (sgn-db-execute "UPDATE OR IGNORE messages SET chat_id = ? WHERE chat_id = ?"
                    (list uuid number))
    (pcase-dolist (`(,rowid ,sender ,ts)
                   (sgn-db-select "SELECT rowid, sender, timestamp FROM messages
                                   WHERE chat_id = ?"
                                  (list number)))
      (sgn-db--merge-duplicate
       rowid (sgn-db-select-value
              "SELECT rowid FROM messages
               WHERE chat_id = ? AND sender = ? AND timestamp = ?"
              (list uuid sender ts))))
    (sgn-db-execute "DELETE FROM chats WHERE id = ?" (list number))))
  (sgn-db--merge-column "reactions" "chat_id" number uuid)
  (sgn-db--merge-column "pins" "chat_id" number uuid))

(defun sgn-db--merge-duplicate (duplicate survivor)
  "Fold message DUPLICATE into SURVIVOR, then delete DUPLICATE.
The survivor keeps its own media, or takes the duplicate's if it
has none, and counts as read if either copy was read."
  (sgn-db-execute
   "UPDATE media SET message_rowid = ?1 WHERE message_rowid = ?2
      AND NOT EXISTS (SELECT 1 FROM media WHERE message_rowid = ?1)"
   (list survivor duplicate))
  (sgn-db-execute
   "UPDATE messages SET read_at = COALESCE(read_at,
      (SELECT read_at FROM messages WHERE rowid = ?2))
    WHERE rowid = ?1"
   (list survivor duplicate))
  ;; The delete trigger drops reactions and pins keyed to the deleted
  ;; message; rekey the duplicate first so that the ones still to be
  ;; rewritten to the surviving identity are kept.
  (sgn-db-execute "UPDATE messages SET sender = sender || ' (merged)'
                   WHERE rowid = ?" (list duplicate))
  (sgn-db-execute "DELETE FROM messages WHERE rowid = ?" (list duplicate)))

(defun sgn-db--merge-column (table column number uuid)
  "Rewrite NUMBER as UUID in COLUMN of TABLE.
Rows that would duplicate an existing one are dropped."
  (sgn-db-execute (format "UPDATE OR IGNORE %s SET %s = ? WHERE %s = ?"
                          table column column)
                  (list uuid number))
  (sgn-db-execute (format "DELETE FROM %s WHERE %s = ?" table column)
                  (list number)))

(provide 'sgn-db)
;;; sgn-db.el ends here
