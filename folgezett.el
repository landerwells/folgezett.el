;;; folgezett.el --- Folgezettel IDs for org-roam -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Lander Wells

;; Author: Lander Wells <landerwells@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org-roam "2.0.0"))
;; Keywords: outlines tools org-roam zettelkasten
;; Homepage: https://github.com/landerwells/folgezett.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; folgezett.el implements the Luhmann folgezettel system for org-roam.
;;
;; When a new org-roam note is captured, the user is prompted to choose a
;; parent note.  A folgezettel ID is generated from the parent and stored in
;; the FOLGEZETTEL_ID property.  The parent relationship is recorded in
;; FOLGEZETTEL_PARENT_ID.
;;
;; ID structure (alternating number/letter segments):
;;
;;   Root notes:         1.1, 2.1, 3.1, ...
;;   Children of 1.1:    1.1a, 1.1b, 1.1c, ...
;;   Children of 1.1a:   1.1a1, 1.1a2, 1.1a3, ...
;;   Children of 1.1a1:  1.1a1a, 1.1a1b, ...
;;
;; Quick start:
;;
;;   (with-eval-after-load 'org-roam
;;     (require 'folgezett)
;;     (folgezett-setup))
;;
;; Commands:
;;
;;   folgezett-assign-id          Manually assign or reassign an ID
;;   folgezett-reparent           Re-parent current note (ID only)
;;   folgezett-reparent-subtree   Re-parent + recursively update descendants
;;   folgezett-goto-parent        Jump to the parent note
;;   folgezett-list-children      Pick and jump to a direct child
;;   folgezett-show-tree          Display the full folgezettel tree
;;
;; Evil users: RET in the tree buffer is bound for Emacs state only.
;; To use it in normal/motion state, add this to your configuration:
;;
;;   (with-eval-after-load 'folgezett
;;     (with-eval-after-load 'evil
;;       (evil-define-key* '(normal motion) folgezett-tree-mode-map
;;         (kbd "RET") #'folgezett-tree-visit-node)))

;;; Code:

(require 'org-roam)
(require 'org-roam-node)
(require 'cl-lib)

;;;; ── Customization ────────────────────────────────────────────────────────

(defgroup folgezett nil
  "Folgezettel ID system for org-roam."
  :group 'org-roam
  :prefix "folgezett-")

(defcustom folgezett-id-property "FOLGEZETTEL_ID"
  "Org property name that stores a note's folgezettel ID."
  :type 'string
  :group 'folgezett)

(defcustom folgezett-parent-property "FOLGEZETTEL_PARENT_ID"
  "Org property name that stores the parent note's org-roam node ID."
  :type 'string
  :group 'folgezett)

(defcustom folgezett-include-id-in-filename nil
  "When non-nil, prepend the folgezettel ID to the note's filename.
After renaming, run `org-roam-db-sync' to update the database."
  :type 'boolean
  :group 'folgezett)

(defcustom folgezett-capture-keys nil
  "Capture template keys that trigger folgezettel assignment.
When nil (the default), every new node capture triggers assignment."
  :type '(repeat string)
  :group 'folgezett)

(defcustom folgezett-show-id-in-completions t
  "When non-nil, show folgezettel IDs in org-roam node completions.
Takes effect when `folgezett-setup' or
`folgezett-enable-completion-display' is called."
  :type 'boolean
  :group 'folgezett)

(defcustom folgezett-export-parent-label "Previous"
  "Label prefix for the parent link injected during export.
When non-nil, `org-export-before-processing-functions' inserts a
line of the form \"LABEL: [[id:…][Title]]\" at the top of the
note body (after any property drawer and #+keyword lines, before
the first content), so ox-hugo and other backends render a parent
backlink without the link being stored on disk.  Set to nil to
disable."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'folgezett)

(defcustom folgezett-db-link-parent nil
  "When non-nil, inject parent links into org-roam's database.
This advises `org-roam-db-update-file' so that the FOLGEZETTEL_PARENT_ID
property is registered as an `id' link in org-roam's links table.
The result is that parent-child relationships appear in backlinks,
the org-roam buffer, and org-roam-ui — without adding any text to
your note files.  Requires `org-roam-db-sync' after enabling."
  :type 'boolean
  :group 'folgezett)

;;;; ── ID Helpers ───────────────────────────────────────────────────────────

(defun folgezett--ends-in-digit-p (id)
  "Return non-nil if ID ends with a digit."
  (and (stringp id)
       (not (string-empty-p id))
       (string-match-p "[0-9]$" id)))

(defun folgezett--strip-last-segment (id)
  "Remove the final segment of ID and return the result.
Returns nil when ID is already root-level (no parent)."
  (cond
   ;; Ends in a letter: strip that one character
   ((string-match "^\\(.*\\)[a-z]$" id)
    (let ((prefix (match-string 1 id)))
      (and (not (string-empty-p prefix)) prefix)))
   ;; Ends in digits preceded by a letter: strip digit run
   ((string-match "^\\(.*[a-z]\\)[0-9]+$" id)
    (match-string 1 id))
   ;; Root ID (e.g. "1.1") — no parent
   (t nil)))

(defun folgezett--increment-id (id)
  "Increment the terminal segment of ID to produce the next sibling.
Letter terminal: a→b … y→z.  Digit terminal: 1→2 … 9→10."
  (cond
   ((string-match "^\\(.*\\)\\([a-z]\\)$" id)
    (let ((letter (string-to-char (match-string 2 id))))
      (when (= letter ?z)
        (user-error "Folgezettel: exhausted letters at ID %s" id))
      (concat (match-string 1 id) (char-to-string (1+ letter)))))
   ((string-match "^\\(.*\\)\\([0-9]+\\)$" id)
    (concat (match-string 1 id)
            (number-to-string (1+ (string-to-number (match-string 2 id))))))
   (t (user-error "Cannot increment folgezettel ID: %s" id))))

(defun folgezett--first-child-id (parent-id)
  "Return the ID of the first child of PARENT-ID.
Number-terminal parents get a letter suffix; letter-terminal get a digit."
  (if (folgezett--ends-in-digit-p parent-id)
      (concat parent-id "a")
    (concat parent-id "1")))

(defun folgezett--direct-child-p (parent-id candidate-id)
  "Return non-nil if CANDIDATE-ID is a direct child of PARENT-ID."
  (and (string-prefix-p parent-id candidate-id)
       (let ((suffix (substring candidate-id (length parent-id))))
         (if (folgezett--ends-in-digit-p parent-id)
             (string-match-p "^[a-z]$" suffix)
           (string-match-p "^[0-9]+$" suffix)))))

(defun folgezett--id-depth (id)
  "Return the 0-based depth of ID in the folgezettel tree (root = 0)."
  (let ((segments 0)
        (pos 0)
        (len (length id)))
    (while (< pos len)
      (cond
       ((and (string-match "[0-9]+" id pos)
             (= (match-beginning 0) pos))
        (setq pos (match-end 0))
        (cl-incf segments))
       ((and (string-match "[a-z]+" id pos)
             (= (match-beginning 0) pos))
        (setq pos (match-end 0))
        (cl-incf segments))
       (t (cl-incf pos))))
    (max 0 (- segments 2))))

;;;; ── Database Access ──────────────────────────────────────────────────────

(defun folgezett--all-nodes ()
  "Return an alist of (fz-id . org-roam-node) for nodes that have a folgezettel ID."
  (let (result)
    (dolist (node (org-roam-node-list))
      (when-let ((fz-id (cdr (assoc folgezett-id-property
                                    (org-roam-node-properties node)))))
        (push (cons fz-id node) result)))
    result))

(defun folgezett--all-ids ()
  "Return a list of all existing folgezettel ID strings."
  (mapcar #'car (folgezett--all-nodes)))

(defun folgezett--node-by-fz-id (fz-id)
  "Return the org-roam node whose folgezettel ID equals FZ-ID, or nil."
  (cdr (assoc fz-id (folgezett--all-nodes))))

;;;; ── Next-ID Computation ──────────────────────────────────────────────────

(defun folgezett--max-id (ids)
  "Return the greatest sibling ID from the list IDS."
  (cl-reduce
   (lambda (best id)
     (if (and (folgezett--ends-in-digit-p best)
              (folgezett--ends-in-digit-p id))
         ;; Both digit-terminal: compare numerically on the trailing number
         (progn
           (string-match "[0-9]+$" best)
           (let ((bn (string-to-number (match-string 0 best))))
             (string-match "[0-9]+$" id)
             (if (> (string-to-number (match-string 0 id)) bn) id best)))
       ;; Otherwise alphabetic comparison suffices
       (if (string> id best) id best)))
   ids))

(defun folgezett--next-child-id (parent-id &optional exclude-id)
  "Return the next available child ID for PARENT-ID.
EXCLUDE-ID, if provided, is omitted from the sibling list (used when
re-parenting so the note's own current ID does not inflate the counter)."
  (let* ((all      (folgezett--all-ids))
         (children (cl-remove-if-not
                    (lambda (id)
                      (and (folgezett--direct-child-p parent-id id)
                           (not (equal id exclude-id))))
                    all)))
    (if children
        (folgezett--increment-id (folgezett--max-id children))
      (folgezett--first-child-id parent-id))))

(defun folgezett--next-root-id ()
  "Return the next available root-level folgezettel ID (e.g. \"1.1\", \"2.1\")."
  (let* ((all   (folgezett--all-ids))
         (roots (cl-remove-if-not
                 (lambda (id) (string-match-p "^[0-9]+\\.[0-9]+$" id))
                 all)))
    (if roots
        (let ((max-n (apply #'max
                            (mapcar (lambda (id)
                                      (string-to-number
                                       (car (split-string id "\\."))))
                                    roots))))
          (format "%d.1" (1+ max-n)))
      "1.1")))

;;;; ── Parent Selection UI ──────────────────────────────────────────────────

(defun folgezett--select-parent ()
  "Prompt the user to select a parent note via `completing-read'.
Returns the selected org-roam node, or nil when the user picks root."
  (let* ((nodes-alist (folgezett--all-nodes))
         (root-label  "[None — root-level note]")
         (candidates
          (cons (cons root-label nil)
                (mapcar (lambda (entry)
                          (cons (format "%-8s  %s"
                                        (car entry)
                                        (org-roam-node-title (cdr entry)))
                                (cdr entry)))
                        (sort nodes-alist
                              (lambda (a b) (string< (car a) (car b)))))))
         (choice (completing-read "Folgezettel parent: "
                                  (mapcar #'car candidates)
                                  nil t nil nil root-label)))
    (cdr (assoc choice candidates))))

;;;; ── Core: Assign ID ──────────────────────────────────────────────────────

(defun folgezett--prompt-for-id (&optional exclude-id)
  "Prompt for a parent note and return (NEW-FZ-ID . PARENT-NODE-ID).
PARENT-NODE-ID is nil when the user selects a root-level note.
EXCLUDE-ID, if provided, is excluded from sibling counting (see
`folgezett--next-child-id')."
  (let* ((parent-node  (folgezett--select-parent))
         (parent-fz-id (when parent-node
                         (cdr (assoc folgezett-id-property
                                     (org-roam-node-properties parent-node)))))
         (new-fz-id    (if parent-fz-id
                           (folgezett--next-child-id parent-fz-id exclude-id)
                         (folgezett--next-root-id)))
         (parent-id    (when parent-node (org-roam-node-id parent-node))))
    (cons new-fz-id parent-id)))

(defun folgezett--set-properties (fz-id parent-node-id)
  "Set FOLGEZETTEL_ID to FZ-ID and FOLGEZETTEL_PARENT_ID to PARENT-NODE-ID.
When PARENT-NODE-ID is nil the parent property is removed.
Uses `save-excursion' so point is not moved."
  (save-excursion
    (org-set-property folgezett-id-property fz-id)
    (if parent-node-id
        (org-set-property folgezett-parent-property parent-node-id)
      (org-delete-property folgezett-parent-property))))

;;;###autoload
(defun folgezett-assign-id ()
  "Assign (or reassign) a folgezettel ID to the org-roam node at point.
Prompts for a parent note, derives the next appropriate ID, and sets
the FOLGEZETTEL_ID (and optionally FOLGEZETTEL_PARENT_ID) property."
  (interactive)
  (unless (org-roam-node-at-point)
    (user-error "No org-roam node at point"))
  (let* ((existing-fz-id (cdr (assoc folgezett-id-property
                                      (org-roam-node-properties
                                       (org-roam-node-at-point)))))
         (id-pair   (folgezett--prompt-for-id existing-fz-id))
         (new-fz-id (car id-pair))
         (parent-id (cdr id-pair)))
    (folgezett--set-properties new-fz-id parent-id)
    (when folgezett-include-id-in-filename
      (folgezett--rename-file-with-id new-fz-id))
    (save-buffer)
    (org-roam-db-update-file)
    (message "folgezett: assigned ID %s" new-fz-id)
    new-fz-id))

;;;; ── File Renaming ────────────────────────────────────────────────────────

(defun folgezett--rename-file-with-id (fz-id)
  "Prepend FZ-ID to the filename of the current buffer.
The caller should run `org-roam-db-sync' afterward."
  (when-let* ((file (buffer-file-name))
              (dir  (file-name-directory file))
              (base (file-name-nondirectory file)))
    (unless (string-prefix-p (concat fz-id "--") base)
      (let ((new-file (expand-file-name (concat fz-id "--" base) dir)))
        (rename-file file new-file 1)
        (set-visited-file-name new-file t t)
        (message "folgezett: renamed → %s (run org-roam-db-sync)"
                 (file-name-nondirectory new-file))))))

;;;; ── Re-parenting ─────────────────────────────────────────────────────────

;;;###autoload
(defun folgezett-reparent ()
  "Choose a new parent and reassign the folgezettel ID of the current note.
Only the current note is updated; descendants keep their existing IDs.
See `folgezett-reparent-subtree' to update descendants as well."
  (interactive)
  (folgezett-assign-id))

(defun folgezett--descendants (fz-id)
  "Return a sorted list of all folgezettel IDs descended from FZ-ID."
  (sort (cl-remove-if-not
         (lambda (id)
           (and (string-prefix-p fz-id id) (not (string= fz-id id))))
         (folgezett--all-ids))
        #'string<))

(defun folgezett--remap-descendants (old-prefix new-prefix)
  "Replace OLD-PREFIX with NEW-PREFIX on every descendant ID."
  (dolist (old-id (folgezett--descendants old-prefix))
    (when-let ((node (folgezett--node-by-fz-id old-id)))
      (let ((new-id (concat new-prefix (substring old-id (length old-prefix)))))
        (with-current-buffer (find-file-noselect (org-roam-node-file node))
          (save-excursion
            (org-set-property folgezett-id-property new-id))
          (save-buffer))
        (message "folgezett: %s → %s" old-id new-id)))))

;;;###autoload
(defun folgezett-reparent-subtree ()
  "Re-parent the current note and recursively update all descendant IDs.
The structural suffix of each descendant is preserved; only the
leading prefix (the old ID of the re-parented note) is replaced."
  (interactive)
  (let* ((node (or (org-roam-node-at-point)
                   (user-error "No org-roam node at point")))
         (old-fz-id (cdr (assoc folgezett-id-property
                                 (org-roam-node-properties node)))))
    (unless old-fz-id
      (user-error "No folgezettel ID — run `folgezett-assign-id' first"))
    (let ((new-fz-id (folgezett-assign-id)))
      (when (and new-fz-id (not (string= old-fz-id new-fz-id)))
        (folgezett--remap-descendants old-fz-id new-fz-id)
        (message "folgezett: subtree %s → %s complete" old-fz-id new-fz-id)))))

;;;; ── Navigation ───────────────────────────────────────────────────────────

;;;###autoload
(defun folgezett-goto-parent ()
  "Visit the parent of the org-roam node at point."
  (interactive)
  (let* ((node (or (org-roam-node-at-point)
                   (user-error "No org-roam node at point")))
         (parent-node-id (cdr (assoc folgezett-parent-property
                                      (org-roam-node-properties node)))))
    (if parent-node-id
        (org-roam-node-visit (org-roam-node-from-id parent-node-id))
      (message "folgezett: this note is root-level (no parent)."))))

;;;###autoload
(defun folgezett-list-children ()
  "Select and visit a direct child of the org-roam node at point."
  (interactive)
  (let* ((node (or (org-roam-node-at-point)
                   (user-error "No org-roam node at point")))
         (fz-id (cdr (assoc folgezett-id-property
                             (org-roam-node-properties node)))))
    (unless fz-id
      (user-error "Current note has no folgezettel ID"))
    (let* ((all      (folgezett--all-nodes))
           (children (cl-remove-if-not
                      (lambda (e) (folgezett--direct-child-p fz-id (car e)))
                      all)))
      (if (null children)
          (message "folgezett: %s has no children." fz-id)
        (let* ((cands  (sort children (lambda (a b) (string< (car a) (car b)))))
               (labels (mapcar (lambda (e)
                                 (cons (format "%-8s  %s"
                                               (car e)
                                               (org-roam-node-title (cdr e)))
                                       (cdr e)))
                               cands))
               (choice (completing-read
                        (format "Children of %s: " fz-id)
                        (mapcar #'car labels) nil t))
               (target (cdr (assoc choice labels))))
          (when target (org-roam-node-visit target)))))))

;;;; ── Tree View ────────────────────────────────────────────────────────────

(defvar folgezett-tree-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'folgezett-tree-visit-node)
    map)
  "Keymap for `folgezett-tree-mode'.")

(define-derived-mode folgezett-tree-mode special-mode "Fz-Tree"
  "Major mode for the *Folgezettel Tree* buffer.
\\<folgezett-tree-mode-map>
\\[folgezett-tree-visit-node] Visit the note on the current line."
  :group 'folgezett)

(defun folgezett-tree-visit-node ()
  "Visit the org-roam node on the current line and close the tree buffer."
  (interactive)
  (if-let ((node (get-text-property (line-beginning-position) 'folgezett-node)))
      (let ((buf (current-buffer)))
        (quit-window)
        (kill-buffer buf)
        (org-roam-node-visit node))
    (user-error "No folgezettel node on this line")))

;;;###autoload
(defun folgezett-show-tree ()
  "Display the full folgezettel hierarchy in a dedicated buffer."
  (interactive)
  (let ((sorted (sort (folgezett--all-nodes)
                      (lambda (a b) (string< (car a) (car b))))))
    (with-current-buffer (get-buffer-create "*Folgezettel Tree*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Folgezettel Tree\n")
        (insert (make-string 60 ?─) "\n\n")
        (if (null sorted)
            (insert "  (no folgezettel notes found)\n")
          (dolist (entry sorted)
            (let* ((fz-id (car entry))
                   (node  (cdr entry))
                   (depth (folgezett--id-depth fz-id))
                   (pad   (make-string (* depth 2) ?\s))
                   (line  (format "%s%-8s  %s\n"
                                  pad fz-id
                                  (org-roam-node-title node))))
              (insert (propertize line 'folgezett-node node)))))
        (insert "\n"))
      (folgezett-tree-mode)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

;;;; ── Completion Display ───────────────────────────────────────────────────

(cl-defmethod org-roam-node-folgezettel-id ((node org-roam-node))
  "Return the folgezettel ID of NODE, or an empty string."
  (or (cdr (assoc folgezett-id-property (org-roam-node-properties node)))
      ""))

;;;###autoload
(defun folgezett-enable-completion-display ()
  "Add folgezettel IDs to `org-roam-node-display-template'."
  (setq org-roam-node-display-template
        (concat "${folgezettel-id:8} ${title:*} "
                (propertize "${tags:20}" 'face 'org-tag))))

;;;; ── DB Integration (parent backlinks) ────────────────────────────────────

(defun folgezett--db-insert-parent-links (&optional _file-path &rest _)
  "Insert link rows for FOLGEZETTEL_PARENT_ID properties.
Designed as :after advice on `org-roam-db-update-file'.  For every
node in the current buffer that has a parent property, a synthetic
link record is added to org-roam's links table so that backlinks and
the graph reflect the folgezettel parent-child relationship."
  (org-with-wide-buffer
    (goto-char (point-min))
    (while (re-search-forward
            (concat ":" folgezett-parent-property ":[ \t]+\\(.+\\)") nil t)
      (let ((parent-id (string-trim (match-string-no-properties 1)))
            (source    (org-roam-id-at-point)))
        (when (and source (not (string-empty-p parent-id)))
          (org-roam-db-query
           [:insert :into links :values $v1]
           (vector (point) source parent-id "id"
                   (list :outline (ignore-errors
                                    (org-get-outline-path 'with-self 'use-cache))))))))))

;;;; ── Export Integration ───────────────────────────────────────────────────

(defun folgezett--export-inject-parent (_backend)
  "Inject a \"Previous: …\" line into the export buffer before parsing.
Meant for `org-export-before-processing-functions': Org runs this
on a temporary copy of the buffer, so the inserted line does not
modify the file on disk.  The line is placed after any top-level
property drawer and #+keyword: lines, before the first body
content.  Honours `folgezett-export-parent-label' — when nil, does
nothing."
  (when folgezett-export-parent-label
    (org-with-wide-buffer
     (goto-char (point-min))
     (when (re-search-forward
            (concat "^[ \t]*:" folgezett-parent-property ":[ \t]+\\(.+\\)$")
            nil t)
       (let* ((parent-id (string-trim (match-string-no-properties 1)))
              (node (and (not (string-empty-p parent-id))
                         (ignore-errors (org-roam-node-from-id parent-id))))
              (title (and node (org-roam-node-title node))))
         (when (and node title)
           (goto-char (point-min))
           (when (looking-at-p "[ \t]*:PROPERTIES:")
             (re-search-forward "^[ \t]*:END:[ \t]*\n" nil t))
           (while (looking-at "^\\(?:[ \t]*#\\+[^\n]*\\|[ \t]*\\)\n")
             (goto-char (match-end 0)))
           (insert (format "%s: [[id:%s][%s]]\n\n"
                           folgezett-export-parent-label
                           parent-id title))))))))

;;;; ── Capture Hook ─────────────────────────────────────────────────────────

(defun folgezett--capture-hook ()
  "Hook for `org-roam-capture-new-node-hook'.
Prompts for a parent and writes folgezettel properties to the new node.
Skips the node if it already has a folgezettel ID, and respects
`folgezett-capture-keys' when it is non-nil."
  (when (or (null folgezett-capture-keys)
            (member (plist-get org-capture-plist :key)
                    folgezett-capture-keys))
    (let* ((node     (org-roam-node-at-point))
           (existing (when node
                       (cdr (assoc folgezett-id-property
                                   (org-roam-node-properties node))))))
      (unless existing
        (let* ((id-pair   (folgezett--prompt-for-id))
               (new-fz-id (car id-pair))
               (parent-id (cdr id-pair)))
          (folgezett--set-properties new-fz-id parent-id)
          (message "folgezett: assigned ID %s" new-fz-id))))))

;;;; ── Mode & Setup ─────────────────────────────────────────────────────────

;;;###autoload
(define-minor-mode folgezett-mode
  "Global minor mode: assign folgezettel IDs when capturing new org-roam nodes."
  :global t
  :group 'folgezett
  :lighter " Fz"
  (if folgezett-mode
      (progn
        (add-hook 'org-roam-capture-new-node-hook
                  #'folgezett--capture-hook)
        (add-hook 'org-export-before-processing-functions
                  #'folgezett--export-inject-parent)
        (when folgezett-db-link-parent
          (advice-add 'org-roam-db-update-file :after
                      #'folgezett--db-insert-parent-links)))
    (remove-hook 'org-roam-capture-new-node-hook
                 #'folgezett--capture-hook)
    (remove-hook 'org-export-before-processing-functions
                 #'folgezett--export-inject-parent)
    (advice-remove 'org-roam-db-update-file
                   #'folgezett--db-insert-parent-links)))

;;;###autoload
(defun folgezett-setup ()
  "Enable folgezett and integrate it with org-roam.

Call this after org-roam is loaded:

  (with-eval-after-load \\='org-roam
    (require \\='folgezett)
    (folgezett-setup))"
  (folgezett-mode 1)
  (when folgezett-show-id-in-completions
    (folgezett-enable-completion-display)))

(provide 'folgezett)

;;; folgezett.el ends here
