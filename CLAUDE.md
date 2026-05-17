# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`folgezett.el` is a single-file Emacs Lisp package that adds Luhmann-style folgezettel IDs to org-roam notes. All logic lives in `folgezett.el`.

## Development

Byte-compile to check for errors and warnings (requires the user's load path for org-roam and its dependencies):

```bash
emacs --batch \
  --eval "(add-to-list 'load-path \"~/.config/emacs/.local/straight/repos/llama/\")" \
  --eval "(add-to-list 'load-path \"~/.config/emacs/.local/straight/repos/cond-let/\")" \
  --eval "(add-to-list 'load-path \"~/.config/emacs/.local/straight/repos/org-roam/\")" \
  --eval "(add-to-list 'load-path \"~/.config/emacs/.local/straight/repos/magit/lisp/\")" \
  --eval "(add-to-list 'load-path \"~/.config/emacs/.local/straight/repos/dash.el/\")" \
  --eval "(add-to-list 'load-path \"~/.config/emacs/.local/straight/repos/emacsql/\")" \
  -f batch-byte-compile folgezett.el
```

Load interactively for manual testing:

```elisp
(load-file "/path/to/folgezett.el/folgezett.el")
(folgezett-setup)
```

## Architecture

The package is structured in layers from bottom to top:

1. **ID arithmetic** (`folgezett--ends-in-digit-p`, `folgezett--strip-last-segment`, `folgezett--increment-id`, `folgezett--first-child-id`, `folgezett--direct-child-p`, `folgezett--id-depth`) — pure string functions, no org-roam dependency.

2. **Database layer** (`folgezett--all-nodes`, `folgezett--all-ids`, `folgezett--node-by-fz-id`) — calls `org-roam-node-list` and reads the `FOLGEZETTEL_ID` property from each node's property alist. This is the only place org-roam's data model is queried.

3. **Next-ID computation** (`folgezett--next-child-id`, `folgezett--next-root-id`) — combines the two layers above: fetches all existing IDs, filters to siblings, finds the max, increments.

4. **UI & commands** (`folgezett--select-place`, `folgezett-assign-id`, `folgezett-reparent`, `folgezett-reparent-subtree`, `folgezett-goto-parent`, `folgezett-list-children`, `folgezett-show-tree`) — interactive entry points. All prompt and navigation logic lives here.

5. **Capture integration** (`folgezett--capture-hook` on `org-roam-capture-new-node-hook`) — hooks fire during capture setup, before the template is filled in, so the placement prompt appears immediately when a new node is opened. `folgezett--capture-finalize-hook` runs on `org-capture-after-finalize-hook` to rename captured files after the final title/template content is available.

6. **Filename integration** (`folgezett--rename-file-with-id`, `folgezett--rename-file-without-id`) — when `folgezett-include-id-in-filename` is non-nil, prepends the assigned ID to note filenames and strips it when IDs are removed.

7. **DB integration** (`folgezett--db-insert-parent-links` as `:after` advice on `org-roam-db-update-file`) — when `folgezett-db-link-parent` is non-nil, injects the `FOLGEZETTEL_PARENT_ID` property as an `id` link into org-roam's `links` table during indexing, so parent-child relationships appear in backlinks and org-roam-ui without adding text to note files.

8. **Export integration** (`folgezett--export-inject-parent` on `org-export-before-processing-functions`) — when `folgezett-export-parent-label` is non-nil, injects an in-memory parent link during export without modifying note files.

## ID Scheme

IDs alternate between number and letter segments:

```
1.1 → 1.1a → 1.1a1 → 1.1a1a
              → 1.1a2
    → 1.1b
2.1 → 2.1a …
```

- Number-terminal parent → child appends one letter (`1.1` → `1.1a`)
- Letter-terminal parent → child appends digits (`1.1a` → `1.1a1`)
- `folgezett--direct-child-p` enforces this: it only matches exactly one additional segment, preventing grandchildren from appearing as children.

## Key Customization Variables

| Variable | Default | Purpose |
|---|---|---|
| `folgezett-id-property` | `"FOLGEZETTEL_ID"` | Org property for the note's ID |
| `folgezett-parent-property` | `"FOLGEZETTEL_PARENT_ID"` | Org property for parent's roam ID |
| `folgezett-capture-keys` | `nil` | Restrict hook to specific capture keys |
| `folgezett-include-id-in-filename` | `nil` | Prepend ID to filename on assignment |
| `folgezett-show-id-in-completions` | `t` | Show IDs in org-roam completing-read |
| `folgezett-db-link-parent` | `nil` | Inject parent links into org-roam's DB |
| `folgezett-export-parent-label` | `"Previous"` | Export-only parent backlink label; nil disables |
