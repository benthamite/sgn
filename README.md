# `sgn`: Signal client for Emacs

A full-featured Signal messenger client for Emacs, built on `signal-cli`'s JSON-RPC interface with local SQLite persistence for message history and full-text search.

## Overview

`sgn` is a fork of [`signel`](https://github.com/keenban/signel) by Keenan Salandy.

`sgn` lets you send and receive Signal messages without leaving Emacs. It communicates with a `signal-cli` daemon over JSON-RPC, persists all conversations in a local SQLite database with FTS5 indexing, and renders chat buffers in a telega-style layout with message grouping, inline images, and text properties for point-based commands.

The package covers the core Signal messaging workflow: sending and receiving text, images, stickers, and voice notes; reacting to, quoting, editing, and deleting messages; creating and voting in polls; pinning messages; managing groups and contacts; and searching across your entire message history. A dashboard buffer provides an overview of all conversations with unread badges, last-message previews, and pinned chats.

`sgn` also supports importing your existing message history from Signal Desktop via its SQLCipher database, so you don't lose context when switching to an Emacs-based workflow.

## Installation

**Requirements**: Emacs 29.1+ compiled with SQLite support, and [`signal-cli`](https://github.com/AsamK/signal-cli) v0.14+ with a registered Signal account.

### package-vc (Emacs 30+)

```emacs-lisp
(use-package sgn
  :vc (:url "https://github.com/benthamite/sgn"))
```

### Elpaca

```emacs-lisp
(use-package sgn
  :ensure (:host github :repo "benthamite/sgn"))
```

### straight.el

```emacs-lisp
(use-package sgn
  :straight (:host github :repo "benthamite/sgn"))
```

No external Emacs package dependencies are required --- `sgn` uses only built-in libraries.

## Quick start

```emacs-lisp
(use-package sgn
  :vc (:url "https://github.com/benthamite/sgn")
  :config
  (setq sgn-account "+15550000000")) ;; your Signal phone number
```

Then:

1. `M-x sgn-start` --- initializes the database, starts `signal-cli`, and opens the dashboard.
2. `M-x sgn-chat` --- pick a contact or group to open a chat buffer.
3. Type your message and press `RET` to send.
4. Move point to any message and press `r` to react, `q` to quote-reply, `e` to edit, or `d` to delete.

## Documentation

For a comprehensive description of all user options, commands, and functions, see the [manual](https://stafforini.com/notes/sgn/).

## License

`sgn` is licensed under the [GNU General Public License v3](LICENSE).
