# sgn — Signal Client for Emacs

## Vision

A full-featured Signal messenger client for Emacs that can replace Signal Desktop for daily use. UX modeled after telega (Telegram client for Emacs). Built on signal-cli's JSON-RPC interface with local SQLite persistence for message history and search.

## Requirements

- **Emacs**: 29.1+ with SQLite support compiled in. Startup must verify
  `(sqlite-available-p)` and that the bundled SQLite supports FTS5; fail with
  a clear error if either check fails.
- **External**: `signal-cli` v0.14+ with a registered Signal account (`0.14.2`
  verified). Startup must check the installed version before enabling RPC
  methods that require newer signal-cli releases.
- **Optional**: ImageMagick (`convert`) for animated sticker support; `sox` (or
  another configured recorder) for outgoing voice notes; `terminal-notifier`
  on macOS if AppleScript notifications are not sufficient.
- **Dependencies**: No external Emacs packages — only built-in libraries

## Architecture

### File Structure

Split the monolithic `sgn.el` into focused modules:

```
sgn.el              — Entry point, custom group, autoloads, top-level commands
sgn-rpc.el          — JSON-RPC process management, send/receive, callbacks
sgn-db.el           — SQLite persistence layer (schema, queries, FTS5 search)
sgn-chat.el         — Chat buffer mode, message rendering, input handling
sgn-media.el        — Image/sticker/attachment/voice-note display and sending
sgn-contacts.el     — Contact and group management, completing-read, cache
sgn-actions.el      — Message actions: react, reply, edit, delete, forward, pin
sgn-format.el       — Text formatting: parse Signal styles, compose markup
sgn-notify.el       — Notifications: modeline/tab-bar indicator, desktop alerts
sgn-search.el       — Full-text search across conversations
sgn-dashboard.el    — Chat list buffer (root buffer, like telega's root)
```

### Data Flow

```
signal-cli daemon (JSON-RPC subprocess)
        ↕
    sgn-rpc.el          — Parse/send JSON-RPC messages
        ↓
    sgn-db.el           — Persist to SQLite
        ↓
    sgn-chat.el         — Render in chat buffer
    sgn-notify.el       — Trigger notification
    sgn-dashboard.el    — Update chat list
```

### SQLite Schema

Database location: `~/.local/share/sgn/sgn.db` (configurable via `sgn-db-directory`).

```sql
-- Conversations (1:1 and groups)
CREATE TABLE chats (
    id          TEXT PRIMARY KEY,   -- phone number or base64 group ID
    name        TEXT,               -- display name
    type        TEXT NOT NULL,      -- 'individual' or 'group'
    last_msg_ts INTEGER,            -- timestamp of most recent message
    unread      INTEGER DEFAULT 0,  -- unread message count
    muted       INTEGER DEFAULT 0,  -- 1 if muted
    pinned      INTEGER DEFAULT 0,  -- 1 if pinned to top of chat list
    draft       TEXT,               -- saved draft text
    expiration  INTEGER DEFAULT 0   -- disappearing message timer (seconds)
);

-- Messages
CREATE TABLE messages (
    rowid       INTEGER PRIMARY KEY AUTOINCREMENT,
    chat_id     TEXT NOT NULL REFERENCES chats(id),
    sender      TEXT NOT NULL,      -- canonical Signal author ID; never 'self'
    timestamp   INTEGER NOT NULL,   -- Signal timestamp (ms), serves as message ID
    body        TEXT,               -- message text (NULL for media-only)
    type        TEXT NOT NULL,      -- 'data', 'sync', 'system'
    quote_ts    INTEGER,            -- timestamp of quoted message (NULL if not a reply)
    quote_author TEXT,              -- author of quoted message
    quote_body  TEXT,               -- quoted body preview as received/sent
    edited_at   INTEGER,            -- timestamp of edit (NULL if not edited)
    deleted     INTEGER DEFAULT 0,  -- 1 if remotely deleted
    expires_in  INTEGER DEFAULT 0,  -- disappearing timer in seconds
    expire_started_at INTEGER,      -- local ms timestamp when expiration begins
    expires_at  INTEGER,            -- local ms timestamp when message should be purged
    styles_json TEXT,               -- JSON array of text style ranges
    raw_json    TEXT,               -- original JSON envelope/message fragment for migrations
    UNIQUE(chat_id, sender, timestamp)
);

-- Full-text search index
CREATE VIRTUAL TABLE messages_fts USING fts5(
    body,
    content='messages',
    content_rowid='rowid'
);

-- Triggers to keep FTS in sync
CREATE TRIGGER messages_ai AFTER INSERT ON messages BEGIN
    INSERT INTO messages_fts(rowid, body)
    SELECT new.rowid, new.body
    WHERE new.body IS NOT NULL AND new.deleted = 0;
END;
CREATE TRIGGER messages_ad AFTER DELETE ON messages BEGIN
    INSERT INTO messages_fts(messages_fts, rowid, body) VALUES('delete', old.rowid, old.body);
END;
CREATE TRIGGER messages_au AFTER UPDATE ON messages BEGIN
    INSERT INTO messages_fts(messages_fts, rowid, body) VALUES('delete', old.rowid, old.body);
    INSERT INTO messages_fts(rowid, body)
    SELECT new.rowid, new.body
    WHERE new.body IS NOT NULL AND new.deleted = 0;
END;

-- Reactions
CREATE TABLE reactions (
    message_rowid INTEGER REFERENCES messages(rowid) ON DELETE CASCADE,
    chat_id     TEXT NOT NULL,
    target_author TEXT NOT NULL,     -- Signal author ID of target message
    target_timestamp INTEGER NOT NULL,
    sender      TEXT NOT NULL,       -- canonical Signal ID of reactor
    emoji       TEXT NOT NULL,
    removed     INTEGER DEFAULT 0,
    UNIQUE(chat_id, target_author, target_timestamp, sender)
);

-- Attachments and media
CREATE TABLE media (
    id          INTEGER PRIMARY KEY AUTOINCREMENT,
    message_rowid INTEGER NOT NULL REFERENCES messages(rowid) ON DELETE CASCADE,
    chat_id     TEXT NOT NULL,
    content_type TEXT NOT NULL,      -- MIME type
    file_path   TEXT,                -- local path to file
    file_name   TEXT,                -- original filename
    is_voice    INTEGER DEFAULT 0,   -- 1 if voice note
    is_sticker  INTEGER DEFAULT 0,
    width       INTEGER,
    height      INTEGER
);

-- Read/delivery receipts
CREATE TABLE receipts (
    message_rowid INTEGER REFERENCES messages(rowid) ON DELETE CASCADE,
    chat_id     TEXT NOT NULL,
    target_author TEXT NOT NULL,
    target_timestamp INTEGER NOT NULL,
    recipient   TEXT NOT NULL,
    type        TEXT NOT NULL,       -- 'sent', 'delivered', 'read'
    received_at INTEGER NOT NULL,
    UNIQUE(chat_id, target_author, target_timestamp, recipient, type)
);

-- Polls
CREATE TABLE polls (
    message_rowid INTEGER UNIQUE REFERENCES messages(rowid) ON DELETE CASCADE,
    chat_id     TEXT NOT NULL,
    poll_author TEXT NOT NULL,
    poll_timestamp INTEGER NOT NULL,
    question    TEXT NOT NULL,
    options_json TEXT NOT NULL,       -- JSON array of option strings
    votes_json  TEXT,                 -- JSON object: option_index -> [voter_ids]
    self_vote_count INTEGER DEFAULT 0, -- incremented each time this client votes
    closed      INTEGER DEFAULT 0,
    UNIQUE(chat_id, poll_author, poll_timestamp)
);

-- Pinned messages
CREATE TABLE pins (
    message_rowid INTEGER REFERENCES messages(rowid) ON DELETE CASCADE,
    chat_id     TEXT NOT NULL,
    target_author TEXT NOT NULL,
    target_timestamp INTEGER NOT NULL,
    pinned_by   TEXT NOT NULL,
    pinned_at   INTEGER NOT NULL,
    pin_expires_at INTEGER,           -- NULL for indefinite pins
    PRIMARY KEY(chat_id, target_author, target_timestamp)
);
```

**Message identity rule**: local joins use `messages.rowid`; Signal RPC actions
use the protocol identity fields required by signal-cli (`targetAuthor` or
`pollAuthor` plus `targetTimestamp`/`pollTimestamp`). The database must store
the user's own canonical Signal number/ACI as `sender`; rendering maps that ID
to "You". Do not store the literal string `"self"` as a message author.

## Features — Phase 1: Core Messaging Rewrite

### 1.1 SQLite Persistence Layer (`sgn-db.el`)

- Initialize database on first run; run migrations on schema changes.
- All incoming messages (data, sync, system) persisted immediately on receipt,
  with canonical Signal author IDs and enough raw JSON to support future
  migrations.
- Provide query functions: `sgn-db-get-messages` (by chat, with limit/offset for pagination), `sgn-db-get-chats` (sorted by last_msg_ts), `sgn-db-insert-message`, `sgn-db-update-message`, etc.
- Track unread counts per chat.
- Store and restore drafts per chat.

### 1.2 Chat Buffer Rewrite (`sgn-chat.el`)

**Buffer model**: One buffer per stable chat ID. Buffer-local `sgn-chat-id` is
authoritative; display names are cosmetic and may collide or change. Buffer
names use `*sgn: <display-name>*` when unique, and append a short stable suffix
when needed (for example `*sgn: Alice <+1555>*` or a group-ID prefix).

**Message rendering** (telega-style, header + body with grouping):

```
── Alice · 12:03 ──────────────────────
  Hey, are you free tonight?
  I was thinking we could grab dinner

── You · 12:04 ────────────────────────
  Yes! What time?

── Alice · 12:05 ──────────────────────
  7pm at the usual place
  👍 You
```

- Consecutive messages from the same sender within 5 minutes are grouped under a single header.
- Timestamps use relative format when recent ("12:03"), absolute when older ("Apr 14, 12:03"), full date when older than a week.
- Own messages use `sgn-my-msg-face`, others use `sgn-other-msg-face`.
- Header line uses `sgn-header-face` (new face, inherits `bold`).
- Edited messages show "(edited)" indicator after the text.
- Deleted messages show "[Message deleted]" in `sgn-deleted-face`.
- Each message is a text property region with properties: `sgn-message-rowid`, `sgn-message-ts`, `sgn-message-sender`, `sgn-message-chat-id`, and `sgn-message-target-author` — enabling point-based commands without display-name lookups.

**Quoted messages** (replies):

```
── Alice · 12:10 ──────────────────────
  ┃ You: Yes! What time?
  How about 7pm?
```

- Quote rendered as a left-bordered block (using `│` or `┃` character) with the original author and truncated text.

**Reactions** displayed inline below the message:

```
  7pm at the usual place
  👍 You, Alice  ❤️ Bob
```

**Input area**:

- Multi-line input supported (S-RET or C-j for newline, RET to send).
- Prompt string configurable (default: `"> "`).
- Input area protected — cursor cannot move into read-only history.
- Draft auto-saved to SQLite when switching away from buffer, restored when returning.
- Show composing indicator while typing (send typing notifications to recipient).

**History loading**:

- On buffer open, load the most recent N messages from SQLite (default: 50, configurable via `sgn-history-page-size`).
- Phase 2 adds scroll-to-top pagination for older messages (like telega's "load more" at the top).
- Loading indicator shown while fetching any page.

**Delivery status data**:

```
  Yes! What time?                    ✓✓
```

- Receipts are stored in Phase 1.
- Phase 2 renders indicators on own messages: `✓` = sent, `✓✓` = delivered, colored `✓✓` = read.
- Rendering updates in real time as receipts arrive once the Phase 2 UI is implemented.

### 1.3 Reactions (`sgn-actions.el`)

- **Send reaction**: With point on a message, press `r`. Prompts via completing-read over emoji (by name, e.g., "thumbs up", "heart", "laughing").
- Sends `sendReaction` with recipient/group addressing, `emoji`, `targetAuthor`, and `targetTimestamp`.
- **Remove reaction**: Press `r` again on a message you already reacted to — sends `sendReaction` with `remove = t`.
- **Receive reactions**: Update the message display in real-time when a reaction notification arrives.
- **Storage**: Reactions stored in `reactions` table by `(chat_id, target_author, target_timestamp, sender)` and rendered below their target message.

### 1.4 Quote/Reply (`sgn-actions.el`)

- With point on a message, press `q`. The input area shows the quoted message context above the prompt:

```
  ┃ Replying to Alice: Yes! What time?
  > _
```

- Press C-g to cancel the reply.
- Sends with `quoteTimestamp`, `quoteAuthor`, `quoteMessage`, and quote style/mention/attachment params when present.
- Incoming replies rendered with the quote block as shown above.

### 1.5 Message Editing (`sgn-actions.el`)

- With point on your own message, press `e`. The message text is placed in the input area for editing.
- On send, uses `editTimestamp` param to signal-cli.
- Incoming edits update the message in SQLite and re-render the chat buffer.
- Edited messages show "(edited)" indicator.

### 1.6 Message Deletion (`sgn-actions.el`)

- With point on your own message, press `d`. Confirms with y-or-n-p, then sends `remoteDelete`.
- Outgoing deletes call `remoteDelete` with recipient/group addressing and `targetTimestamp`.
- Incoming remote deletes mark the message as deleted in SQLite, clear `body` and `styles_json`, remove it from FTS, and re-render as "[Message deleted]".

### 1.7 Contact & Group Completing-Read (`sgn-contacts.el`)

- `sgn-chat` (interactive): Prompts with completing-read over all contacts and groups.
  - Candidates include display name, phone number, and group name.
  - Each candidate carries a hidden stable chat ID; selection never resolves by display name alone.
  - Duplicate names are disambiguated in annotations with phone number or group-ID prefix.
  - Sorted by recency (most recent conversation first).
  - Annotation shows last message preview and timestamp (via `completion-extra-properties` or annotation function).
- Contact cache refreshed from signal-cli on `sgn-start` and periodically.
- Groups and contacts stored in the `chats` table.

### 1.8 Read Receipts (`sgn-rpc.el`, `sgn-db.el`)

- **Send**: When `sgn-send-read-receipts` is non-nil, send focus-gated receipts with `sendReceipt` only after a chat buffer is visible and focused. Do not use signal-cli's process-wide `--send-read-receipts` flag for this behavior.
- `sendReceipt` params: sender `recipient`, `targetTimestamp` list, and `type = "read"`.
- **Receive**: Store in `receipts` table; Phase 2 uses those rows for delivery status indicators on own messages.
- Clear unread count when chat buffer is focused.

### 1.9 Typing Indicators

- **Send**: Call `sendTyping` when user begins typing in input area; call `sendTyping` with `stop = t` when idle for 5 seconds or input is cleared.
- **Receive**: Display "Alice is typing..." in the chat buffer's header-line or at the bottom of the message area (above the prompt), with auto-clear after timeout.

### 1.10 RPC Layer Improvements (`sgn-rpc.el`)

- Factor out the JSON-RPC process management from `sgn.el`.
- Add `--receive-mode=manual` support: call `subscribeReceive` after setup is complete to avoid missing messages during initialization.
- Expose signal-cli's `--send-read-receipts` only as the advanced option `sgn-cli-auto-read-receipts` (default `nil`). When enabled, document that signal-cli sends read receipts for every incoming data message regardless of Emacs focus.
- Improve error handling: treat signal-cli JSON-RPC code `-3` (I/O) and `-5` (rate limit) as retryable/transient when the operation is idempotent; treat `-1` (user error), `-4` (untrusted key), `-32602`, and `-32601` as permanent/reportable unless the user takes corrective action.
- Support for all new RPC methods: `sendReaction`, `remoteDelete`, `sendReceipt`, `sendTyping`, `sendPinMessage`, `sendUnpinMessage`, `sendPollCreate`, `sendPollVote`, `sendPollTerminate`.
- Use camelCase JSON-RPC parameter names in code. JsonRpcNamespace accepts dash-separated aliases, but `sgn` should consistently emit names such as `groupId`, `targetAuthor`, `targetTimestamp`, `pollAuthor`, `pollTimestamp`, `voteCount`, `pinDuration`, `editTimestamp`, and `quoteTimestamp`.

## Features — Phase 2: History, Search & Dashboard

### 2.1 Full-Text Search (`sgn-search.el`)

- `sgn-search` (interactive): Prompts for a search query, searches across all conversations using FTS5.
- Deleted messages (`deleted = 1`) and expired/purged messages are excluded.
- Results displayed in a `*sgn Search*` buffer:

```
Search: dinner

── Alice · Apr 14, 12:05 ──────────────
  7pm at the usual place for [dinner]
  → *sgn: Alice*

── Family Group · Apr 12, 18:30 ────────
  Mom: Don't forget [dinner] Sunday
  → *sgn: Family Group*
```

- Each result is clickable/RET-able — jumps to that message in the chat buffer.
- Support filtering by chat, date range, sender.
- `sgn-search-in-chat`: Search within the current chat only.

### 2.2 Dashboard / Chat List (`sgn-dashboard.el`)

Telega-style root buffer showing all conversations:

```
*sgn*

  📌 Family Group           Mom: Don't forget...    12:01  (3)
  ● Alice Baker             7pm at the usual pl...  12:05  (1)
    Bob Wilson              Check out this link...  11:45
    Carol Chen              Thanks for the file     Apr 13
    Note to Self            Remember to buy...      Apr 12
```

- Sorted by last message timestamp (pinned chats always on top).
- Unread count shown in parentheses with `sgn-unread-face`.
- Bold/highlighted for chats with unread messages.
- Last message preview (truncated).
- Press RET to open chat, `d` to mark read, `M` to mute/unmute, `P` to pin/unpin.
- Press `g` to refresh, `s` to search, `c` to compose new message.
- Auto-updates when new messages arrive.

### 2.3 Unread Tracking

- Unread count per chat stored in `chats` table.
- Incremented on each incoming message when the chat buffer is not focused.
- Reset to 0 when the chat buffer gains focus (and read receipt is sent).
- Global unread count displayed in modeline/tab-bar.

### 2.4 Notification System (`sgn-notify.el`)

**Modeline/tab-bar indicator** (user-configurable):

- `sgn-notification-style`: `'modeline` or `'tab-bar` (default: `'modeline`).
- Modeline: Shows `[sgn:3]` in the global mode-line when there are unread messages.
- Tab-bar: Shows an unread indicator segment in the tab bar (integrates with user's existing tab-bar setup).
- Provide a lightweight minor mode (`sgn-global-mode`) that activates the indicator.

**Desktop notifications**:

- `sgn-desktop-notifications`: `t` or `nil` (default: `t`).
- Uses built-in `notifications-notify` on Linux, `do-applescript` or `terminal-notifier` on macOS.
- Shows sender name and message preview.
- Clicking the notification switches to the chat buffer.
- Suppress notifications for muted chats.

## Features — Phase 3: Polish & Advanced

### 3.1 Text Formatting (`sgn-format.el`)

**Rendering incoming styles**:

- Signal sends text styles as ranges: `{start, length, style}` where style is BOLD, ITALIC, STRIKETHROUGH, MONOSPACE, SPOILER.
- Render with appropriate faces: `bold`, `italic`, `sgn-strikethrough-face`, `sgn-monospace-face`, `sgn-spoiler-face`.
- Spoilers: Render with a concealing background color; clicking/RET reveals the text.

**Composing with markup**:

- Support lightweight markup in the input area:
  - `*bold*` → BOLD
  - `_italic_` → ITALIC
  - `~strikethrough~` → STRIKETHROUGH
  - `` `monospace` `` → MONOSPACE
  - `||spoiler||` → SPOILER
- Parse markup on send, convert to Signal style ranges, send as plain text + styles.

### 3.2 Media Improvements (`sgn-media.el`)

**Images**:

- Incoming images rendered as inline thumbnails (configurable max width, default 300px).
- Press RET on image to view full-size in a dedicated buffer or external viewer.
- Send images via `sgn-attach-file` or drag-and-drop (if Emacs supports it).

**Voice notes**:

- Incoming voice notes shown as a playable element: `🎤 0:15 [▶ Play]`.
- Press RET to play using `play-sound` or external player.
- Send voice notes via `sgn-send-voice-note` (records via `sox` or similar).

**Link previews**:

- Incoming link previews rendered with title, description, and thumbnail:
  ```
    ┌─────────────────────────────────┐
    │ 📎 Article Title                │
    │ Brief description of the link   │
    │ example.com                     │
    └─────────────────────────────────┘
  ```

**Stickers**: Keep existing APNG→GIF conversion; improve lookup reliability.

### 3.3 Mentions (`sgn-actions.el`)

- In group chats, type `@` to trigger completing-read over group members.
- Mentions rendered with a highlighted face (`sgn-mention-face`).
- Mentions of self additionally highlighted (`sgn-mention-self-face`).
- Sent using signal-cli's `mention` parameter format: `start:length:recipientNumber`.

### 3.4 Polls (`sgn-actions.el`)

- `sgn-create-poll` (interactive): Prompts for question and options, sends via `sendPollCreate`.
- Voting sends `sendPollVote` with recipient/group addressing, `pollAuthor`, `pollTimestamp`, selected `option` indexes, and monotonically increasing `voteCount` for this client.
- Incoming polls rendered as:
  ```
  📊 What should we have for dinner?
    1. Pizza        ▓▓▓▓░░░░ 3 votes
    2. Sushi        ▓▓▓▓▓▓░░ 4 votes
    3. Tacos        ▓░░░░░░░ 1 vote
    [Vote: 1/2/3]
  ```
- Press the vote key to vote (sends via `sendPollVote`).
- Poll creator can close the poll.

### 3.5 Pin Messages

- With point on a message in a group chat, press `P` to pin/unpin.
- Pinned messages indicated in the chat buffer with a 📌 marker.
- Stored in `pins` table.

### 3.6 Group Management (`sgn-contacts.el`)

- `sgn-create-group`: Create a new group (name + members via completing-read).
- `sgn-group-info`: Show group details (members, admins, settings) in a dedicated buffer.
- `sgn-group-add-member`, `sgn-group-remove-member`: Manage membership.
- `sgn-group-settings`: Change name, description, avatar, permissions, and disappearing message timer (`updateGroup` with `expiration`).
- `sgn-leave-group`: Leave a group (with confirmation).

### 3.7 Contact Management

- `sgn-block-contact`, `sgn-unblock-contact`: Block/unblock.
- `sgn-update-contact-name`: Set a local display name for a contact.
- Store contact metadata in `chats` table.

### 3.8 Note to Self

- `sgn-note-to-self`: Open the "Note to Self" chat (uses `--note-to-self` flag).
- Works like any other chat buffer.

### 3.9 Disappearing Messages

- Per-chat timer shown in the header line: `⏱ 24h` or `⏱ Off`.
- `sgn-set-disappearing`: Set the timer for the current chat (`updateGroup` or `updateContact` with `expiration`, depending on chat type).
- Messages with expiration show a subtle indicator.
- Client-side deletion after expiry uses `expire_started_at` and `expires_at`; cleanup must survive Emacs restarts and remove expired rows from both `messages` and FTS.

## Keybindings

### Chat buffer (read-only message area)

| Key   | Action              | Command                  |
|-------|---------------------|--------------------------|
| `r`   | React to message    | `sgn-react`              |
| `q`   | Quote/reply         | `sgn-reply`              |
| `e`   | Edit own message    | `sgn-edit`               |
| `d`   | Delete own message  | `sgn-delete`             |
| `f`   | Forward message     | `sgn-forward`            |
| `P`   | Pin/unpin message   | `sgn-toggle-pin`         |
| `c`   | Copy message text   | `sgn-copy-text`          |
| `@`   | Mention (in groups) | `sgn-mention`            |
| `RET` | Open media/link     | `sgn-open-at-point`      |
| `g`   | Refresh/scroll up   | `sgn-load-more-history`  |

### Chat buffer (input area)

| Key       | Action             | Command              |
|-----------|--------------------|----------------------|
| `RET`     | Send message       | `sgn-send-input`     |
| `S-RET`   | Newline            | `newline`            |
| `C-j`     | Newline            | `newline`            |
| `C-c C-a` | Attach file        | `sgn-attach-file`    |
| `C-c C-v` | Send voice note    | `sgn-send-voice-note`|
| `C-g`     | Cancel reply/edit  | `sgn-cancel-action`  |

### Dashboard buffer

| Key   | Action              | Command                  |
|-------|---------------------|--------------------------|
| `RET` | Open chat           | `sgn-dashboard-open`     |
| `c`   | Compose new message | `sgn-chat`               |
| `s`   | Search              | `sgn-search`             |
| `g`   | Refresh             | `sgn-dashboard-refresh`  |
| `d`   | Mark chat read      | `sgn-dashboard-mark-read`|
| `M`   | Mute/unmute         | `sgn-dashboard-toggle-mute`|
| `P`   | Pin/unpin chat      | `sgn-dashboard-toggle-pin`|
| `n`   | Next chat           | `next-line`              |
| `p`   | Previous chat       | `previous-line`          |

## Customization Options

### Core

| Variable                    | Default                          | Description                              |
|-----------------------------|----------------------------------|------------------------------------------|
| `sgn-account`               | `nil`                            | Registered Signal phone number (required)|
| `sgn-cli-program`           | `"signal-cli"`                   | Path to signal-cli executable            |
| `sgn-data-directory`        | `"~/.local/share/signal-cli"`    | signal-cli data directory                |
| `sgn-db-directory`          | `"~/.local/share/sgn"`           | SQLite database directory                |

### Display

| Variable                    | Default  | Description                                |
|-----------------------------|----------|--------------------------------------------|
| `sgn-history-page-size`     | `50`     | Messages loaded per page                   |
| `sgn-image-max-width`       | `300`    | Max width for inline images (pixels)       |
| `sgn-sticker-max-width`     | `150`    | Max width for stickers (pixels)            |
| `sgn-enable-animation`      | `t`      | Animate stickers and GIFs                  |
| `sgn-timestamp-format`      | `'smart` | `'smart`, `'absolute`, or `'relative`      |
| `sgn-message-grouping-interval` | `300` | Seconds within which to group consecutive messages from the same sender |
| `sgn-prompt`                | `"> "`   | Input prompt string                        |

### Behavior

| Variable                    | Default      | Description                              |
|-----------------------------|--------------|------------------------------------------|
| `sgn-send-read-receipts`    | `t`          | Send focus-gated read receipts with `sendReceipt` |
| `sgn-cli-auto-read-receipts`| `nil`        | Pass signal-cli `--send-read-receipts`; sends read receipts for every incoming data message, regardless of focus |
| `sgn-send-typing`           | `t`          | Send typing indicators                   |
| `sgn-auto-open-buffer`      | `nil`        | Auto-switch to chat on incoming message  |
| `sgn-notification-style`    | `'modeline`  | `'modeline` or `'tab-bar`               |
| `sgn-desktop-notifications` | `t`          | Show desktop notifications               |

### Faces

| Face                     | Inherits                      | Purpose                    |
|--------------------------|-------------------------------|----------------------------|
| `sgn-my-msg-face`        | `font-lock-function-name-face`| Your messages              |
| `sgn-other-msg-face`     | `font-lock-variable-name-face`| Others' messages           |
| `sgn-header-face`        | `bold`                        | Message group headers      |
| `sgn-timestamp-face`     | `shadow`                      | Timestamps                 |
| `sgn-unread-face`        | `warning`                     | Unread count indicators    |
| `sgn-mention-face`       | `highlight`                   | @mentions                  |
| `sgn-mention-self-face`  | `match`                       | @mentions of self          |
| `sgn-spoiler-face`       | (custom: bg=fg for concealing)| Spoiler text               |
| `sgn-strikethrough-face` | (custom: :strike-through t)   | Strikethrough text         |
| `sgn-monospace-face`     | `fixed-pitch`                 | Monospace text             |
| `sgn-deleted-face`       | `shadow`                      | Deleted message placeholder|
| `sgn-error-face`         | `error`                       | Error messages             |
| `sgn-receipt-face`       | `shadow`                      | Delivery status checkmarks |
| `sgn-quote-face`         | `font-lock-comment-face`      | Quoted message text        |

## Implementation Phases

### Phase 1: Core Messaging Rewrite

**Goal**: Replace the current single-file implementation with a modular, persistent messaging system.

1. **sgn-db.el**: SQLite schema, migrations, CRUD operations, FTS5 setup.
2. **sgn-rpc.el**: Extract and improve JSON-RPC layer. Add support for new methods (reactions, receipts, editing, deletion, typing). Add `--receive-mode=manual` support.
3. **sgn-contacts.el**: Contact/group cache backed by SQLite. Completing-read with annotations.
4. **sgn-chat.el**: New chat buffer mode with telega-style rendering, message grouping, text properties for point-based commands. Multi-line input. Draft persistence.
5. **sgn-actions.el**: Reactions (completing-read emoji), quote/reply, edit, delete. All using text property-based message targeting.
6. **sgn-media.el**: Extract and improve media handling. Better image rendering, attachment display.
7. **Receipt storage**: Store delivery/read receipt events and send focus-gated read receipts with `sendReceipt`.
8. **sgn.el**: Slim entry point with autoloads, custom group, top-level commands, and `Package-Requires` updated to Emacs 29.1+.
9. **Tests**: Update test suite for new module structure. Add tests for SQLite layer, new rendering, actions, and receipt privacy behavior.

### Phase 2: History, Search & Dashboard

1. **History loading**: Older-page pagination from SQLite when scrolling to the top of a chat buffer.
2. **sgn-search.el**: FTS5-powered search across conversations. Search results buffer with jump-to-message.
3. **sgn-dashboard.el**: Telega-style chat list with unread badges, last message preview, pinned chats.
4. **Unread tracking**: Per-chat unread counts, global indicator, mark-read on focus.
5. **Delivery status**: Checkmark indicators on own messages (sent/delivered/read).
6. **sgn-notify.el**: Modeline/tab-bar indicator (configurable). Desktop notifications.

### Phase 3: Polish & Advanced Features

1. **sgn-format.el**: Text formatting — render incoming Signal styles, compose with lightweight markup.
2. **Mentions**: @-completion in group chats, highlighted rendering.
3. **Polls**: Create, vote, display polls.
4. **Pin messages**: Pin/unpin in groups with visual indicator.
5. **Group management**: Create, edit, leave groups. Member management.
6. **Voice notes**: Play incoming, record and send.
7. **Link previews**: Render incoming link previews.
8. **Disappearing messages**: Timer management, client-side expiry.
9. **Note to Self**: Dedicated command.
10. **Contact management**: Block, unblock, rename.

## Non-Goals (Out of Scope)

- Voice/video calling (signal-cli support is experimental and not practical in Emacs).
- Stories (not supported by signal-cli).
- Username/PNI features (still evolving in Signal protocol).
- Registration/linking (use signal-cli directly for account setup).
- Backup import/export (use signal-cli's native backup tools).
