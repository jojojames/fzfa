;;; fzfa.el --- Async fuzzy completion via `fzf-native' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0.1
;; Package-Requires: ((emacs "29.1") (fzf-native "2.2"))
;; Keywords: matching, completion, fzf, fuzzy, fussy
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides async shell command completion using fzf-native for scoring.
;; The native layer handles process I/O on a background thread, ANSI
;; stripping, and parallel fzf scoring.  The Elisp layer provides
;; while-no-input responsiveness, a candidate cap, and a live stats overlay.
;;
;; Quick start:
;;   (fzfa-setup)   ; register completion style + category override
;;   (fzfa-find-files)

(require 'cl-lib)
(require 'fzf-native)

;;; Code:

(defgroup fzfa nil
  "Async fuzzy completion via fzf-native."
  :group 'completion
  :link '(url-link :tag "GitHub" "https://github.com/jojojames/fzfa"))

(defvar embark-keymap-alist)
(defvar embark-default-action-overrides)
(defvar embark-general-map)
(defvar fzf-native-case-mode)
(defvar fzf-native-fuzzy)
(defvar fzf-native-async-highlight)
(defvar fzf-native-batch-highlight)
(defvar fzf-native-max-line-length)
(defvar fzf-native-async-cache-size)
(defvar marginalia-annotate-file)
(declare-function icomplete-exhibit "icomplete")
(defvar icomplete-overlay)
(defvar ivy-text)
(defvar ivy--index)
(defvar ivy--all-candidates)
(defvar ivy-count-format)
(defvar ivy-completing-read-dynamic-collection)
(declare-function ivy--set-candidates "ivy")
(declare-function ivy--exhibit "ivy")
(declare-function ivy--insert-prompt "ivy")
(declare-function ivy-dispatching-call "ivy")
;; FILEONLY: `cl-defstruct' slot accessor — check-declare can't see
;; auto-generated bodies, only literal `defun's.  ARGLIST is `t'
;; (unspecified), not `nil' (which would mean "takes 0 args" and
;; trip byte-compile on the 1-arg call site).
(declare-function ivy-state-dynamic-collection "ivy" t t)
(defvar ivy-last)
(defvar ivy--actions-list)
(defvar ivy-pre-prompt-function)
(declare-function projectile-project-root "projectile")
(declare-function project-root "project")
(declare-function vertico--exhibit "vertico")
(defvar vertico--index)
(defvar vertico--input)
(defvar marginalia-annotators)
(declare-function fzf-native-score "ext:fzf-native-module"
                  (str query &optional slab))
(declare-function fzf-native-score-all "ext:fzf-native-module"
                  (collection query &optional slab))
(declare-function fzf-native-async-start "ext:fzf-native-module"
                  (command &optional dir))
(declare-function fzf-native-async-stop "ext:fzf-native-module" (handle))
(declare-function fzf-native-async-generation "ext:fzf-native-module" (handle))
(declare-function fzf-native-async-candidates "ext:fzf-native-module"
                  (handle filter &optional limit))
(declare-function fzf-native-async-stats "ext:fzf-native-module" (handle))
(declare-function fzf-native-async-result-fresh-p "ext:fzf-native-module"
                  (handle query))
(declare-function fzf-native-highlight-all "ext:fzf-native-module"
                  (collection query))
(declare-function fzf-native-highlight-one "ext:fzf-native-module" (cand query))
(declare-function fzfa-helm--completing-read "fzfa-helm")

;;; Debug logging

(defvar fzfa-debug nil
  "When non-nil at load time, compile debug logging into fzfa.

Set before loading or re-evaluating fzfa.el; toggling at runtime
has no effect (the check is macro-expanded at load time, like #ifdef).")

(defmacro fzfa--log (fmt &rest args)
  "Emit a debug message FMT with ARGS if `fzfa-debug' is non-nil at load.

Expands to nothing when disabled — zero runtime cost.  Logs to *Messages*
only; variable `inhibit-message' is bound around the `message' call so
the echo-area write is suppressed, otherwise it would stomp the
active minibuffer/mini-window display."
  (when (bound-and-true-p fzfa-debug)
    `(let ((inhibit-message t)) (message ,fmt ,@args))))

;;; Customization

(defcustom fzfa-max-candidates 10000
  "Max candidates returned to Elisp from `fzfa-completing-read'.

The full filtered/total counts are still tracked and shown in the prompt.
Set to nil or 0 to disable the cap (may be slow for very large result sets)."
  :type '(choice (const  :tag "No cap" nil)
                 (integer :tag "Max candidates"))
  :group 'fzfa)

(defcustom fzfa-refresh-delay 0.05
  "Seconds between polls for new C-side candidate generations.

The background reader thread increments a generation counter as lines
arrive; this timer checks that counter and schedules a display refresh
when new data is available.  Lower values feel more responsive but burn
more CPU on the polling loop."
  :type 'float
  :group 'fzfa)

(defcustom fzfa-input-debounce 0.1
  "Seconds of idle time to wait before retrying after interrupted scoring.

When the user types fast, `while-no-input' aborts the scoring call.  This
idle timer fires once typing pauses and re-triggers the display so results
self-heal."
  :type 'float
  :group 'fzfa)

(defcustom fzfa-input-throttle 0.2
  "Minimum seconds between display refreshes driven by new incoming data.

Even when new candidate generations arrive continuously (e.g. a fast `find'
streaming thousands of files), the completion UI is only re-exhibited once
per this interval.  The debounce retry path is unaffected — after the user
pauses typing the display always self-heals regardless of this value."
  :type 'float
  :group 'fzfa)

(defcustom fzfa-preview-delay nil
  "Seconds of idle time before live preview fires after a candidate change.

Used by commands that preview the highlighted candidate as the selection
moves (e.g. `fzfa-theme' loading the theme under point).  Implemented with
`run-with-idle-timer', so fast typing or arrow-key bursts naturally suppress
intermediate previews — the timer only fires once input settles.
A value of 0 previews immediately on every selection change, which can make
typing feel sluggish for expensive preview actions.  Set to nil (the
default) to disable automatic previews — users opt in to previewing
on demand via `fzfa-preview-key'."
  :type '(choice (const  :tag "Manual only" nil)
                 (number :tag "Idle seconds"))
  :group 'fzfa)

(defcustom fzfa-highlight 200
  "Controls C-side match highlighting of completion candidates.

nil or a negative integer — no highlighting.
t                        — highlight every returned candidate.
a positive integer N     — highlight only the top N candidates.
The C layer applies `completions-common-part' face to each contiguous
run of matched bytes via fzf_get_positions."
  :type '(choice (const   :tag "Disabled" nil)
                 (const   :tag "All candidates" t)
                 (integer :tag "Top N candidates"))
  :group 'fzfa)

(defcustom fzfa-batch-highlight 25
  "Controls C-side match highlighting for the synchronous scoring path.

nil — no highlighting.
a positive integer N — highlight only the top N candidates.

Sync counterpart to `fzfa-highlight'.  Bridged onto
`fzf-native-batch-highlight' by `fzfa--bridge-defcustoms' for every
`fzf-native-score-all' / `fzf-native-score' call fzfa makes."
  :type '(choice (const   :tag "Disabled" nil)
                 (integer :tag "Top N candidates"))
  :group 'fzfa)

(defcustom fzfa-max-line-length 256
  "Maximum character length of a candidate line.

nil           — no limit.
positive N    — exclude lines longer than N characters.
negative -N   — include but truncate lines to N characters.

Applies at read time: lines from the subprocess are filtered or
truncated before entering the candidate pool, so scoring never
sees the excess characters.

For the rg / ag / ugrep grep-style commands the cap is also pushed
upstream into the search tool itself (via `--max-columns' / `--width')."
  :type '(choice (const   :tag "No limit" nil)
                 (integer :tag "N (positive = exclude, negative = truncate)"))
  :group 'fzfa)

(defcustom fzfa-case-mode 'smart
  "Case-sensitivity mode propagated to `fzf-native-case-mode'.

Mirrors fzf-native's enum:
smart    Case-insensitive when the query is all lowercase; case-sensitive
         once it contains any uppercase character (fzf's default).
ignore   Always case-insensitive.
respect  Always case-sensitive."
  :type '(choice (const :tag "Smart case (default)" smart)
                 (const :tag "Ignore case"          ignore)
                 (const :tag "Respect case"         respect))
  :group 'fzfa)

(defcustom fzfa-fuzzy t
  "Whether to fuzzy match with `fzf-native'.

If t, use fuzzy matching, if nil, use exact/substring matching.

If t, prefixing a term with ' switches that term to exact matching.

If nil, prefixing a term with ' switches that term to fuzzy matching.

Read at the start of every scoring call.

Propagated to `fzf-native-fuzzy' via `:around' advice on the
`fzf-native' async entry points."
  :type 'boolean
  :group 'fzfa)

(defcustom fzfa-cache-size 40
  "Maximum number of scored snapshots cached per async session.

Each entry stores the top-K results and the full matched-candidate
index for one query, enabling exact-fresh hits (skip scoring) and
prefix-refinement hits (rescore only previously-matched candidates
plus deltas) without re-scanning the full pool.

A larger value keeps a longer typing trail in cache, which improves
backspace coverage — backspacing past N keystrokes will still hit
the LRU as long as those intermediate queries weren't evicted by
unrelated lookups.

Read at session start; changing it does not affect running sessions."
  :type 'integer
  :group 'fzfa)

;;;###autoload
(defconst fzfa-extension-registry
  '((ag        . "ag (the_silver_searcher)")
    (chrome    . "Chrome bookmarks + passwords")
    (company   . "company-mode completions")
    (eglot     . "Eglot workspace symbols")
    (emacs     . "Emacs built-in sources")
    (embark    . "Embark actions")
    (evil      . "Evil-mode marks + registers")
    (fd        . "fd (find alternative)")
    (find      . "POSIX find")
    (firefox   . "Firefox bookmarks")
    (flymake   . "Flymake diagnostics")
    (git       . "Git")
    (grep      . "POSIX grep")
    (helm      . "Helm frontend")
    (hg        . "Mercurial (hg)")
    (hungry    . "Hungry (buffer-derived dirs)")
    (imenu     . "Imenu (buffer index)")
    (info      . "Info manuals")
    (ivy       . "Ivy frontend")
    (locate    . "locate")
    (mail      . "macOS Mail.app")
    (make      . "make / ninja targets")
    (media-thumbnail . "Video thumbnails (media-thumbnail)")
    (music     . "macOS Music.app")
    (notmuch   . "notmuch mail search")
    (org       . "Org-mode headings")
    (pass      . "password-store (pass)")
    (posframe  . "Posframe preview pane")
    (project   . "project.el")
    (regexp    . "Regexp (buffer-line picker)")
    (replay    . "Persisted replay")
    (rg        . "ripgrep (rg)")
    (safari    . "Safari bookmarks + history (macOS)")
    (shell     . "Shell command + history")
    (spotlight . "macOS Spotlight (mdfind)")
    (tramp     . "TRAMP support")
    (transient . "Transient menus")
    (ugrep     . "ugrep")
    (vc        . "vc.el")
    (vertico   . "Vertico"))
  "Single source of truth for fzfa extensions.

Each entry is (SYMBOL . DESCRIPTION).  Adding a new extension is a
one-line change here — the default value and `:type' of
`fzfa-extensions', plus the prune set in `fzfa-sync-autoloads',
all derive from this alist.")

;;;###autoload
(defcustom fzfa-extensions
  (mapcar #'car fzfa-extension-registry)
  "List of fzfa extensions to load from `fzfa-setup'.

Each SYMBOL causes `fzfa-setup' to call `fzfa-SYMBOL-setup' if
defined.  The autoloaded commands inside each extension remain
callable regardless of this list.

To also hide commands from excluded extensions from `M-x',
`where-is', `describe-command', call `fzfa-sync-autoloads'
after setting this variable — typically in a
`use-package' `:init' clause.  See its docstring for the pattern."
  :type `(set ,@(mapcar (lambda (cell)
                          `(const :tag ,(cdr cell) ,(car cell)))
                        fzfa-extension-registry))
  :group 'fzfa)

(defcustom fzfa-project-backend 'project
  "How to resolve the root directory for fzfa commands.

project    Use `project.el' to find the project root (default).
projectile Use `projectile-project-root'.
nil        Use `default-directory' (no project detection).
function   Call the function with no arguments; Returns a directory string."
  :type '(choice (const :tag "project.el" project)
                 (const :tag "Projectile" projectile)
                 (const :tag "None (default-directory)" nil)
                 (function :tag "Custom function"))
  :group 'fzfa)

;;; Variables

(defvar fzfa--setup-done nil
  "Non-nil once `fzfa--ensure-setup' has installed registrations.

Reset to nil to force a re-setup on the next entry-point call.")

(defvar fzfa--multi-mode nil
  "Dispatch flag for `fzfa-completing-read'.

- `:extract'         — throw `fzfa-extracted' with the call's keyword args.
- (`:inject' . CAND) — return CAND directly without prompting.
Bound by `fzfa-multi-read' to derive multi-source sources from
existing single-source commands without modifying their definitions.")

(defvar fzfa--multi-narrowed-p nil
  "Non-nil when the active multi-source session is narrowed to one source.")

(defvar fzfa-directory nil
  "Per-call directory override for fzfa commands.

When non-nil, supersedes `fzfa-project-backend' and `default-directory'.
Intended for `let'-binding when extending built-in commands:

Priority: `fzfa-directory' > project backend > `default-directory'.")

;; Preview Variables

(defvar fzfa--preview-session nil
  "Active preview cell for the in-flight dispatch: (HANDLER . STATE-PLIST).

`let'-bound by the router's broadcast to the current source's cell so
`:setup', `:preview', `:exit', and `:return' handlers see per-source
state via `fzfa-preview-get' / `fzfa-preview-put'.")

(defvar-local fzfa--preview-timer nil
  "Buffer-local debounce timer; lives in the minibuffer only.")
(defvar-local fzfa--preview-last 'unset
  "Last previewed candidate in this minibuffer (for change detection).")
(defvar-local fzfa--minibuffer-marker nil
  "Buffer-local marker set on the fzfa minibuffer by `fzfa--preview-install'.

Extensions (notably `fzfa-posframe') use this to detect whether a
given minibuffer belongs to an fzfa session — needed because embark's
nested completing-read stacks a fresh minibuffer whose display-buffer
routing wants to peek at the parent-fzfa's context, not fire globally.")
(defvar-local fzfa--minibuffer-session nil
  "Buffer-local `fzfa-session' for the current fzfa minibuffer.

Set by `fzfa--preview-install' at session open, cleared on
`minibuffer-exit-hook'.  Read via `fzfa--current-session' by
fixed-arity integrations that can't take session by parameter (embark
transformer, `fzfa-apply-current' from a keybinding).")
(defvar-local fzfa--preview-run-fn nil
  "Buffer-local reference to the preview `run' closure.

Installed by `fzfa--preview-install' so `fzfa--frontend-exhibit' can
call the closure right after `vertico--exhibit' populates candidates.
Ties preview firing to actual candidate arrival instead of relying on
`post-command-hook' — which does not fire while the user is idle
waiting on a slow async producer.")

;; Tofu

(defconst fzfa--tofu-base #x100000
  "Base Unicode Private Use Area codepoint for source-disambiguation suffixes.

Each multi source's candidates carry a single trailing codepoint at
`fzfa--tofu-base' + source-idx, propertized `display \"\"' so it renders
invisibly while making cross-source duplicates `string='-unique.
See consult's `consult--tofu-encode' for the same trick.")

(defvar fzfa--tofu-cache (make-hash-table :test 'eql)
  "Cache of propertized tofu suffix strings, keyed by source index.")

;;; `completion-styles'

(defun fzfa--ensure-category-override (category)
  "Register a `(styles fzfa)' override for CATEGORY if not already set.

Called from `fzfa-completing-read' on every invocation so any fzfa
category (built-in or caller-defined) gets pinned to fzfa's
passthrough style without maintaining a hardcoded list.  Skips
categories the user has explicitly overridden (respects
customization) and any category already registered by an
extension (fzfa-chrome-history, fzfa-mail, etc.).

Rationale: without an override, `completion--nth-completion' falls
through to the user's global `completion-styles' (fussy,
orderless, ...) for our category, and those styles then drive
fzfa's pre-scored collection through basic/PCM machinery that
expects a different shape — visible as
`(wrong-type-argument listp 0)' in the debugger.  Fzfa's own
passthrough returns non-nil for its collection, so once it's the
first style tried, `seq-some' short-circuits on it every time."
  (when (and category (symbolp category)
             (not (assq category completion-category-overrides)))
    (push `(,category (styles fzfa)) completion-category-overrides)))

(defun fzfa-try-completion (string _table _pred _point)
  "Try-completion for the fzfa completion style.

Always accepts STRING as-is; scoring is done in C."
  (cons string (length string)))

(defun fzfa-all-completions (string table pred _point)
  "All-completions for the fzfa completion style.

Passes STRING through to the collection TABLE filtered by PRED.

When the frontend opts into variable `completion-lazy-hilit' (vertico, icomplete
on Emacs 28+), set `completion-lazy-hilit-fn' to a closure that
highlights one candidate at display time via `fzf-native-highlight-one'.
This means the frontend pays for face on only the actually-visible
candidates rather than the eager top-N picked by the C scorer.
Frontends that don't opt in (ivy, helm) fall back to the eager C-side
highlight already attached by the scorer."
  (when (and (boundp 'completion-lazy-hilit)
             completion-lazy-hilit
             (fboundp 'fzf-native-highlight-one))
    (let ((query string))
      (setq completion-lazy-hilit-fn
            (lambda (cand)
              (if (or (null query) (string-empty-p query))
                  cand
                (fzf-native-highlight-one cand query))))))
  (funcall table string pred t))

;;; Frontend abstraction

(defun fzfa--frontend-index ()
  "Return the active completion UI's selection index (0-based), or nil.

Returns nil for frontends without a selection index (e.g. icomplete)."
  (cond
   ((bound-and-true-p vertico-mode) (max 0 vertico--index))
   ((bound-and-true-p ivy-mode) (and (boundp 'ivy--index) (max 0 ivy--index)))
   (t nil)))

(defun fzfa--frontend-candidate ()
  "Return the currently highlighted candidate string in the active UI, or nil.

Used for live preview (e.g. `fzfa-theme') and persistent-action
\(`fzfa-apply-current')."
  ;; Ensure we're in the minibuffer because the user could've pressed away
  ;; by the time this function is called.
  ;; e.g. M-x `fzfa-find-any'
  ;; C-h k <key>
  ;; Focus switches to help window -> timer fires -> this function is called.
  (when-let* ((win (active-minibuffer-window)))
    (with-current-buffer (window-buffer win)
      (cond
       ((bound-and-true-p vertico-mode)
        (when (and (boundp 'vertico--candidates) vertico--candidates)
          (nth (max 0 vertico--index) vertico--candidates)))
       ((bound-and-true-p ivy-mode)
        (when (and (boundp 'ivy--all-candidates) ivy--all-candidates)
          (nth (max 0 ivy--index) ivy--all-candidates)))
       ((bound-and-true-p icomplete-mode)
        (car (completion-all-sorted-completions)))))))

(defun fzfa--icomplete-fit-mini-window ()
  "Grow the mini-window to fit `icomplete-overlay's `after-string'.

Mini-window auto-resize during redisplay treats the empty-input case
\(zero-length overlay) as needing 1 line, ignoring the multi-line
`after-string', so the initial display under icomplete-vertical never
grows for our completions.  Cap at `max-mini-window-height'.  Pair
with `resize-mini-windows' set buffer-locally to `grow-only' (in
`fzfa--minibuffer-format-reset') so subsequent redisplays can't shrink
the pane back."
  (when-let* ((win (and (bound-and-true-p icomplete-mode)
                        (active-minibuffer-window)))
              ((eq win (selected-window)))
              (after (and (bound-and-true-p icomplete-overlay)
                          (overlay-get icomplete-overlay 'after-string)))
              ((stringp after))
              (lines (1+ (cl-count ?\n after)))
              (max-lines
               (cond ((floatp max-mini-window-height)
                      (max 1 (floor (* max-mini-window-height
                                       (frame-height)))))
                     ((integerp max-mini-window-height) max-mini-window-height)
                     (t 25)))
              (target (min lines max-lines))
              ((> target (window-text-height win))))
    (ignore-errors
      (set-window-text-height win target))))

(defun fzfa--icomplete-exhibit ()
  "Refresh icomplete's candidate display after an async generation bump.

Flushes the sorted-completions cache (icomplete reads it via the
buffer-local variable `completion-all-sorted-completions'), runs the
standard `icomplete-exhibit' to repopulate the overlay's
`after-string', and fits the mini-window."
  (completion--flush-all-sorted-completions)
  ;; Belt-and-suspenders flush: the function above can short-circuit
  ;; based on region args, leaving the cache populated.
  (setq completion-all-sorted-completions nil)
  (icomplete-exhibit)
  (fzfa--icomplete-fit-mini-window))

(defvar-local fzfa--icomplete-cursor-mark nil
  "Last buffer position fzfa marked with `cursor t' text property.

Tracked buffer-locally so the next `post-command-hook' tick can
wipe it before re-marking at the new (point).  See
`fzfa--icomplete-cursor-override'.")

(defun fzfa--icomplete-cursor-override ()
  "Pin the visible cursor to (point) under `icomplete-mode'.

`icomplete-exhibit' sets `cursor t' on its after-string overlay
at `field-end' (icomplete.el:780), which jumps the visible cursor
away from mid-buffer point in fzfa's `compact' / `full' display.
Mark the char at (point) with the same property — the closest
`cursor t' to point wins."
  (when (and (bound-and-true-p icomplete-mode)
             (minibufferp))
    (with-silent-modifications
      (when (and fzfa--icomplete-cursor-mark
                 (< fzfa--icomplete-cursor-mark (point-max)))
        (remove-text-properties
         fzfa--icomplete-cursor-mark
         (1+ fzfa--icomplete-cursor-mark)
         '(cursor nil)))
      (setq fzfa--icomplete-cursor-mark nil)
      (when (< (point) (point-max))
        (put-text-property (point) (1+ (point)) 'cursor t)
        (setq fzfa--icomplete-cursor-mark (point))))))

(defun fzfa--frontend-exhibit ()
  "Trigger a display refresh in the active completion UI.

Handles vertico and icomplete.  `ivy' is handled separately.

After the frontend commits new candidates, invokes
`fzfa--preview-run-fn' if it is bound in the minibuffer buffer —
this is how the initial preview lands for slow async producers, since
`post-command-hook' does not fire while the user waits idly for the
first batch of results.  Subsequent typing then re-triggers preview
through the usual post-command-hook / idle-timer path."
  (when-let* ((win (active-minibuffer-window)))
    (with-selected-window win
      (cond
       ((bound-and-true-p vertico-mode)
        (setq vertico--input t)
        (vertico--exhibit))
       ((bound-and-true-p icomplete-mode)
        (fzfa--icomplete-exhibit)))
      (when (functionp fzfa--preview-run-fn)
        (funcall fzfa--preview-run-fn)))))

(defun fzfa--frontend-push (ivy-push-fn)
  "Refresh the active completion display.

Under `ivy-mode' (push model) `funcall' IVY-PUSH-FN; otherwise hand
off to `fzfa--frontend-exhibit' for the pull-model frontends.
Designed to be passed straight to `run-with-idle-timer' with
IVY-PUSH-FN as the trailing argument, or called directly inline."
  (if (bound-and-true-p ivy-mode)
      (funcall ivy-push-fn)
    (fzfa--frontend-exhibit)))

(defun fzfa--insert-prompt-if-ivy ()
  "Force ivy to redraw its prompt if `ivy-mode' is active.

Required because `ivy--exhibit' skips the prompt redraw when the
candidate body didn't change; our dynamic-state pre-prompt content
\(stats line, narrow indicator) would otherwise stay stale.  No-op
under non-ivy frontends."
  (when (and (bound-and-true-p ivy-mode)
             (active-minibuffer-window))
    (with-selected-window (active-minibuffer-window)
      (ivy--insert-prompt))))

;;; Visit / preview hooks

(defcustom fzfa-after-visit-hook
  '(recenter pulse-momentary-highlight-one-line)
  "Hook run after a fzfa command visits its selection.

Each command's action lambda wraps its body in `fzfa-with-visit', which
fires this hook once the visit completes (point is at the destination)."
  :type 'hook :group 'fzfa)

(defcustom fzfa-after-apply-hook
  '(recenter pulse-momentary-highlight-one-line)
  "Hook run after `fzfa-apply-current' displays a candidate."
  :type 'hook :group 'fzfa)

(defcustom fzfa-after-preview-hook
  '(recenter pulse-momentary-highlight-one-line)
  "Hook run after `fzfa-preview-show' displays a candidate.

Fires on every preview tick (point is at the previewed location in the
origin window)."
  :type 'hook :group 'fzfa)

(defmacro fzfa-with-visit (&rest body)
  "Run BODY as a visit action; fire `fzfa-after-visit-hook' on completion."
  (declare (indent 0) (debug t))
  `(prog1 (progn ,@body)
     (run-hooks 'fzfa-after-visit-hook)))

;;; File-visit strategy

(defcustom fzfa-external-extensions
  '(;; Video
    "mp4" "mkv" "webm" "mov" "avi" "mpg" "mpeg" "wmv" "flv" "m4v"
    "3gp" "ogv" "ts" "vob" "rmvb"
    ;; Audio
    "mp3" "m4a" "flac" "wav" "ogg" "aac" "wma" "opus" "ape" "alac"
    "aiff" "dsf"
    ;; Other multimedia containers / large binary blobs
    "iso" "dmg")
  "File extensions that `fzfa-smart-find-file' hands off to the OS handler.

Matched case-insensitively against `file-name-extension'.  `mailcap'
is intentionally not consulted — its static MIME table predates modern
formats (no `mp4', `mkv', `webm') and falling back across both sources
just produces inconsistent behavior."
  :type '(repeat string)
  :group 'fzfa)

(defcustom fzfa-external-open-command
  (cond ((eq system-type 'darwin)              "open")
        ((eq system-type 'gnu/linux)           "xdg-open")
        ((memq system-type '(windows-nt cygwin)) "start"))
  "Program used to open files matching `fzfa-external-extensions'.

Nil disables external dispatch — every selection falls back to
`find-file' regardless of extension.  Invoked with the absolute file
path as its single argument and detached from Emacs (`call-process'
with PROC=0), so Emacs doesn't block on the external viewer."
  :type '(choice string (const :tag "Disable external dispatch" nil))
  :group 'fzfa)

(defun fzfa--external-p (file)
  "Non-nil if FILE's extension is in `fzfa-external-extensions'."
  (when-let* ((ext (file-name-extension file)))
    (member (downcase ext) fzfa-external-extensions)))

(defun fzfa-smart-find-file (file)
  "Open FILE via the OS handler when its extension matches, else `find-file'.

Extension match (case-insensitive) against `fzfa-external-extensions'
plus a non-nil `fzfa-external-open-command' dispatches to that command
detached from Emacs (so the external player runs asynchronously).
`current-prefix-arg' of `(4)' (C-u) routes non-external files through
`find-file-other-window'.  Everything else falls through to `find-file'.

The extension check fires before `file-directory-p' so TRAMP-shaped
inputs (`/ssh:host:', `/sudo::') don't trigger a remote connection
just to verify they're directories — they have no extension at all,
so `fzfa--external-p' short-circuits to nil and we route straight to
`find-file' (which knows how to interpret the TRAMP path)."
  (cond
   ((and fzfa-external-open-command
         (fzfa--external-p file)
         (not (file-directory-p file)))
    (call-process fzfa-external-open-command nil 0 nil
                  (expand-file-name file)))
   ((equal current-prefix-arg '(4))
    (find-file-other-window file))
   (t (find-file file))))

(defun fzfa-smart-switch-to-buffer (buffer-or-name)
  "Switch to BUFFER-OR-NAME, other-window under `\\[universal-argument]'.

`current-prefix-arg' of `(4)' routes through
`switch-to-buffer-other-window'; anything else uses `switch-to-buffer'."
  (if (equal current-prefix-arg '(4))
      (switch-to-buffer-other-window buffer-or-name)
    (switch-to-buffer buffer-or-name)))

(defun fzfa-smart-bookmark-jump (bookmark)
  "Jump to BOOKMARK, other-window under `\\[universal-argument]'.

`current-prefix-arg' of `(4)' routes through `bookmark-jump-other-window';
anything else uses `bookmark-jump'."
  (if (equal current-prefix-arg '(4))
      (bookmark-jump-other-window bookmark)
    (bookmark-jump bookmark)))

(defcustom fzfa-action-config
  '((fzfa-file
     (nil  :action fzfa-smart-find-file)
     ((16) :directory (lambda () default-directory))
     ((64) :directory (lambda () (read-directory-name "In dir: "))))
    (fzfa-buffer
     (nil :action fzfa-smart-switch-to-buffer))
    (fzfa-bookmark
     (nil :action fzfa-smart-bookmark-jump))
    (fzfa-grep
     (nil  :action fzfa-grep-jump)
     ((16) :directory (lambda () default-directory))
     ((64) :directory (lambda () (read-directory-name "In dir: "))))
    (fzfa-location
     (nil :action fzfa-location-jump)))
  "Category-keyed prefix-arg dispatch for fzfa commands.

Each entry is `(CATEGORY (SLOT-KEY :action FN :directory FN) ...)'.
SLOT-KEY is `nil' (no prefix), `(4)' (C-u), `(16)' (C-u C-u), or
`(64)' (C-u C-u C-u).  `:action' is called with the picked candidate;
`:directory' is called with no args and returns the working directory.

Resolution: the `nil' slot's plist is the category baseline; the
matched slot's plist overlays it (matched wins).  Missing slot -> falls
to `nil' slot.  `:directory' returning `nil' falls back to
`fzfa--default-dir'."
  :type 'sexp
  :group 'fzfa)

(defun fzfa--plist-merge (base overlay)
  "Return BASE with OVERLAY's keys merged in; OVERLAY wins on collision."
  (let ((out (copy-sequence base)))
    (while overlay
      (setq out (plist-put out (car overlay) (cadr overlay))
            overlay (cddr overlay)))
    out))

(defun fzfa--resolve-action-slot (category prefix)
  "Return the effective plist for CATEGORY at PREFIX.

Merges the `nil' slot's baseline with the matched slot's delta.  Slot
key comparison uses `equal' — `(4)' is a cons, so `assq' won't match."
  (let* ((table (alist-get category fzfa-action-config))
         (nil-plist (cdr (assoc nil table)))
         (match-plist (and prefix (cdr (assoc prefix table)))))
    (fzfa--plist-merge nil-plist match-plist)))

(defun fzfa-visit-file (file)
  "Visit FILE via the `fzfa-file' category action and fire `fzfa-after-visit-hook'.

The action is resolved from `fzfa-action-config' against
`current-prefix-arg'."
  (let ((action (plist-get
                 (fzfa--resolve-action-slot 'fzfa-file current-prefix-arg)
                 :action)))
    (fzfa-with-visit (funcall action file))))

(defun fzfa-visit-buffer (buffer-or-name)
  "Switch to BUFFER-OR-NAME via the `fzfa-buffer' category action.

The action is resolved from `fzfa-action-config' against
`current-prefix-arg'."
  (let ((action (plist-get
                 (fzfa--resolve-action-slot 'fzfa-buffer current-prefix-arg)
                 :action)))
    (fzfa-with-visit (funcall action buffer-or-name))))

(defun fzfa-visit-bookmark (bookmark)
  "Jump to BOOKMARK via the `fzfa-bookmark' category action.

The action is resolved from `fzfa-action-config' against
`current-prefix-arg'."
  (let ((action (plist-get
                 (fzfa--resolve-action-slot 'fzfa-bookmark current-prefix-arg)
                 :action)))
    (fzfa-with-visit (funcall action bookmark))))

(defun fzfa-visit-grep (cand)
  "Jump to grep candidate CAND via the `fzfa-grep' category action.

CAND is a FILE:LINE:CONTENT string.  The action is resolved from
`fzfa-action-config' against `current-prefix-arg'."
  (let ((action (plist-get
                 (fzfa--resolve-action-slot 'fzfa-grep current-prefix-arg)
                 :action)))
    (fzfa-with-visit (funcall action cand))))

(defun fzfa-visit-location (cand)
  "Jump to location candidate CAND via the `fzfa-location' category action.

CAND carries an `fzfa-location' text property `(SOURCE . LINE)'.  The
action is resolved from `fzfa-action-config' against
`current-prefix-arg'."
  (let ((action (plist-get
                 (fzfa--resolve-action-slot 'fzfa-location current-prefix-arg)
                 :action)))
    (fzfa-with-visit (funcall action cand))))

;;; Apply (persistent-action)

(defcustom fzfa-apply-key "C-M-m"
  "Key string bound to `fzfa-apply-current' in `fzfa' minibuffer sessions.

This only applies to non-`ivy', non-`helm' sessions.

`ivy' uses `ivy-call' and `helm' uses `helm-execute-persistent-action'.

Matches `ivy''s default `ivy-call' binding."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'fzfa)

(defcustom fzfa-apply-functions
  '((fzfa-file     :apply find-file)
    (fzfa-buffer   :apply switch-to-buffer)
    (fzfa-bookmark :apply bookmark-jump)
    (fzfa-grep     :apply fzfa-grep-jump)
    (fzfa-location :apply fzfa-location-jump))
  "Per-category apply handlers.

Alist of (CATEGORY . PLIST), where PLIST recognizes the `:apply' slot
\(a function taking a single CANDIDATE string).  Used by
`fzfa--resolve-apply' when a session/source doesn't declare an explicit
`:apply' lambda.  Drop a category's entry (or its `:apply' slot) to
disable the fallback for that category."
  :type '(alist :key-type symbol
                :value-type (plist :options ((:apply function))))
  :group 'fzfa)

(defvar fzfa--session-apply nil
  "Apply function for the active single-source session.

Lambda taking a single CANDIDATE; invoked by `fzfa-apply-current'
without exiting the session.  `let'-bound by `fzfa-completing-read'
from the constructor's `:apply' keyword (falling back to
`fzfa-apply-functions' by category).  Multi sessions look up
`:apply' per-source via `fzfa--resolve-apply' instead.")

(defun fzfa--resolve-apply (cand)
  "Resolve the apply function for CAND in the active fzfa session.

Returns the function or nil when no apply is available.

Multi: route via `fzfa--multi-source-of', prefer the source's `:apply'
slot, fall back to `:action' (helm semantics) then to the
category default from `fzfa-apply-functions'.

Single: return `fzfa--session-apply' (already pre-resolved against
`fzfa-apply-functions' at constructor time)."
  (cond
   ((bound-and-true-p fzfa--active-sources)
    (when-let* ((src (fzfa--multi-source-of
                      cand fzfa--active-sources nil)))
      (or (plist-get src :apply)
          (plist-get src :action)
          (plist-get
           (alist-get (plist-get src :category) fzfa-apply-functions)
           :apply))))
   (t fzfa--session-apply)))

(defun fzfa--pin-window-buffer (window buffer)
  "Ensure WINDOW stays on BUFFER once the active minibuffer session exits.

Preview uses `display-buffer-same-window' to render candidates in
WINDOW; the minibuffer unwind path consults `quit-restore' and related
`display-buffer' bookkeeping to revert the window after exit.  Apply
\(`fzfa-apply-current') calls this helper so the user's deliberate
\\[fzfa-apply-current] visit survives that reversion.

Implementation: `minibuffer-exit-hook' fires BEFORE the unwind's
window restoration, so a synchronous `set-window-buffer' inside the
hook gets clobbered afterward.  Defer the re-assert via `run-at-time
0 nil' from within the exit hook so the set fires at the next event
loop tick, after the unwind has fully drained.

On commit, the calling command's post-`completing-read' body runs
before our deferred re-assert, but it places the candidate's buffer
in WINDOW anyway, so the re-assert is a harmless no-op when the two
agree.  On abort the body doesn't run and the re-assert sticks."
  (when-let* ((mb (active-minibuffer-window))
              ((window-live-p window))
              ((buffer-live-p buffer)))
    (with-current-buffer (window-buffer mb)
      (add-hook 'minibuffer-exit-hook
                (lambda ()
                  (run-at-time
                   0 nil
                   (lambda ()
                     (when (and (window-live-p window)
                                (buffer-live-p buffer))
                       (set-window-buffer window buffer)))))
                t t))))

(defun fzfa-apply-current ()
  "Invoke the current candidate's `:apply' function without exiting.

Looks up the apply via `fzfa--resolve-apply', resolves the candidate
to an absolute path via `fzfa-resolve-candidate' (which consults the
source's `:directory' + `:resolve-paths'), then runs the apply lambda
inside the origin window so file/buffer visits land there and the
picker keeps focus.

`fzfa--pin-window-buffer' makes the visit survive the minibuffer
unwind path's implicit window-state restoration.

Silently no-ops when no `:apply' is defined for the source/session."
  (interactive)
  (when-let* ((cand (fzfa--frontend-candidate))
              (apply (fzfa--resolve-apply cand))
              (resolved (fzfa-resolve-candidate cand (fzfa--current-session)))
              (origin (or (minibuffer-selected-window) (selected-window))))
    (condition-case err
        (with-selected-window origin
          (funcall apply resolved)
          (fzfa--pin-window-buffer origin (current-buffer))
          (run-hooks 'fzfa-after-apply-hook))
      (error (message "fzfa-apply: %s" (error-message-string err))))))

(defun fzfa--minibuffer-install-apply-key ()
  "Bind `fzfa-apply-key' to `fzfa-apply-current' in the active minibuffer.

Installed via a per-instance child of `current-local-map' so we don't
mutate the frontend's shared keymap.

`ivy' uses `ivy-call' instead of this key."
  (when (and fzfa-apply-key
             (not (bound-and-true-p ivy-mode)))
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map (kbd fzfa-apply-key) #'fzfa-apply-current)
      (use-local-map map))))

(defcustom fzfa-preview-key "C-c C-p"
  "Key string bound to `fzfa-preview-current' in `fzfa' minibuffer sessions.

Fires preview for the currently selected candidate on demand,
bypassing the `fzfa-preview-delay' idle timer.  Default behaviour
\(with `fzfa-preview-delay' nil) is no auto-preview — pressing this
key is the only way preview fires.  Set both `fzfa-preview-delay'
and this to nil to disable preview entirely.

Under helm, ignored — helm uses `C-j' (`helm-execute-persistent-action')
with `:follow' for preview firing."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'fzfa)

(defun fzfa-preview-current ()
  "Fire preview for the currently selected candidate, ignoring the idle timer.

Looks up the active session's preview handler and dispatches `:preview'
with the current candidate.  Silently no-ops when no handler is
registered for this session."
  (interactive)
  (when-let* ((cand (fzfa--frontend-candidate))
              (session (fzfa--current-session)))
    (setq fzfa--preview-last cand)
    (fzfa--preview-call :preview session cand)))

(defun fzfa--minibuffer-install-preview-key ()
  "Bind `fzfa-preview-key' to `fzfa-preview-current' in the active minibuffer.

Installed via a per-instance child of `current-local-map'.  No-op
when `fzfa-preview-key' is nil."
  (when fzfa-preview-key
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map (current-local-map))
      (define-key map (kbd fzfa-preview-key) #'fzfa-preview-current)
      (use-local-map map))))

;;; Live preview
;;
;; Categories declare per-action handlers in `fzfa-preview-functions'.
;; The lifecycle is:
;;
;;   :setup    ()             once at minibuffer entry
;;   :preview  (CAND)         each debounced candidate change; nil = reset
;;   :exit     ()             just before minibuffer closes (still live)
;;   :return   (CAND-OR-NIL)  after minibuffer closes; nil = aborted
;;
;; Handlers share state via `fzfa-preview-get' / `fzfa-preview-put',
;; backed by a dynamically-bound session cons whose cdr is a plist.
;; The session let-binding spans both `completing-read' and the
;; :return dispatch in `unwind-protect', so :return sees the same
;; state that :setup stashed even though the minibuffer is gone.
;; Only :preview is required.

(defcustom fzfa-preview-file-size-limit (* 10 1024 1024)
  "Maximum file size in bytes that `fzfa--file-preview' will open.

Files larger than this fall back to a size-readout placeholder
(`fzfa--file-preview-too-large') instead of loading — keeps selection
movement snappy even when the cursor lands on multi-megabyte binaries.
Set to nil to remove the cap (not recommended for large repositories).
Set to 0 to disable file preview entirely without dropping the
`fzfa-file' handler from `fzfa-preview-functions'."
  :type '(choice (const :tag "No cap" nil)
                 (integer :tag "Bytes"))
  :group 'fzfa)

(defcustom fzfa-file-preview-dispatch-functions nil
  "Abnormal hook consulted by `fzfa--file-preview' before the default handler.

Each function is called with `(PATH SESSION)'.  Return a live buffer
to use as the preview; return nil to defer to subsequent functions or
the built-in text buffer path.  The first non-nil return wins.

Extensions attach here to intercept specific file types — e.g.,
`fzfa-media-thumbnail-setup' routes video files to an ffmpeg-generated
JPEG instead of a raw-bytes buffer."
  :type 'hook
  :group 'fzfa)

(defcustom fzfa-preview-excluded-files
  '("\\`/[^/|:]+:"   ;; tramp-shaped paths (e.g. /ssh:host:/path)
    "\\.gpg\\'")      ;; gpg-encrypted (would prompt for passphrase)
  "Regexps matched against candidate paths; matches are not previewed.

The tramp regex bails on `/method:host:' paths before
`expand-file-name' would consult `file-name-handler-alist' and
autoload tramp.  The `.gpg' entry avoids function `epa-file-handler' firing
during preview, which would prompt for a passphrase."
  :type '(repeat regexp)
  :group 'fzfa)

(defun fzfa--preview-excluded-p (path)
  "Non-nil if PATH matches any entry in `fzfa-preview-excluded-files'."
  (and path (seq-find (lambda (re) (string-match-p re path))
                      fzfa-preview-excluded-files)))

(defcustom fzfa-preview-suppressed-functions
  '(;; Language servers — spawn subprocess and start network / stdio traffic.
    eglot-ensure
    eglot--maybe-activate-editing-mode
    lsp
    lsp-deferred
    lsp-mode
    ;; Syntax checkers — start background processes.
    flycheck-mode
    global-flycheck-mode
    flymake-mode
    flymake-mode-on
    flymake-start
    ;; Debuggers.
    dap-mode
    ;; VCS overlays / integration — probe git repository each open.
    git-gutter-mode
    git-gutter+-mode
    diff-hl-mode
    diff-hl-flydiff-mode
    diff-hl-dired-mode
    magit-file-mode
    magit-blob-mode
    ;; VCS state refresh (vc-mode-line + git status).  The prefix-based
    ;; filter in `fzfa--filter-find-file-hook' catches the auto-generated
    ;; `vc-refresh-<mode>' entries; this one covers the top-level function.
    vc-refresh-state
    ;; Session-tracking pollution — recentf logs every previewed file.
    recentf-track-opened-file
    ;; File watchers / periodic subprocess wake-ups.
    auto-revert-mode
    auto-revert-tail-mode
    global-auto-revert-mode
    ;; Snippet system — may load a large template set on activation.
    yas-minor-mode
    yas-global-mode)
  "Functions filtered out of every hook while a preview buffer loads.

Preview buffers open under `fzfa-with-quiet-find-file', which installs
an `:around' advice on `run-hooks' that shims each hook variable's
value with `cl-progv' — any function in this list is removed from a
hook's value before the hook dispatches, so it never fires in preview.

Filtering is by exact symbol match on hook contents.  For the
`vc-refresh-<mode>' family of functions that helm's `find-file-hook'
auto-generates, `fzfa--filter-find-file-hook' applies a separate
prefix-based filter."
  :type '(repeat (symbol :tag "Function"))
  :group 'fzfa)

(defun fzfa--filter-suppressed-hooks (orig-fn &rest hooks)
  "Around advice for `run-hooks': drop `fzfa-preview-suppressed-functions'.

Rebinds each named hook variable to a filtered copy for the duration
of the underlying `run-hooks' call, via `cl-progv' so the technique
works for arbitrary hook symbols without hard-coding them."
  (let ((filtered
         (mapcar
          (lambda (h)
            (if (boundp h)
                (cl-remove-if
                 (lambda (fn)
                   (memq fn fzfa-preview-suppressed-functions))
                 (symbol-value h))
              nil))
          hooks)))
    (cl-progv hooks filtered
      (apply orig-fn hooks))))

(defcustom fzfa-preview-functions
  '((fzfa-buffer   :preview fzfa--buffer-preview)
    (fzfa-file     :setup   fzfa--file-preview-setup
                   :preview fzfa--file-preview
                   :return  fzfa--file-preview-return)
    (fzfa-grep     :preview fzfa--grep-preview)
    (fzfa-location :preview fzfa--location-preview))
  "Per-category preview handlers.

Alist of (CATEGORY . PLIST), where PLIST recognizes :setup, :preview,
:exit, :return slots (see commentary in fzfa.el).  Only :preview is
required.  Categories without an entry get no preview.
Disable preview entirely by setting both `fzfa-preview-delay' and
`fzfa-preview-key' to nil.

Built-in handlers are listed in the default; redefine the entire
alist to opt out of a category, or extend it (e.g. via `customize'
or `setq') to add categories of your own."
  :type '(alist :key-type symbol
                :value-type (plist :options ((:setup function)
                                             (:preview function)
                                             (:exit function)
                                             (:return function))))
  :group 'fzfa)

(defun fzfa-preview-get (key &optional default)
  "Return KEY from the current preview cell's state plist, or DEFAULT.

Called by preview handlers to reach per-source state (`:opener',
`:origin-window', `:default-directory') that :setup stashed on the
cell.  Only meaningful during handler invocation."
  (let ((cell (plist-member (cdr fzfa--preview-session) key)))
    (if cell (cadr cell) default)))

(defun fzfa-preview-put (key value)
  "Set KEY to VALUE on the current preview cell's state plist."
  (setcdr fzfa--preview-session
          (plist-put (cdr fzfa--preview-session) key value)))

(defun fzfa--preview-handler (preview category)
  "Resolve the handler plist for this call, or nil when none apply.

PREVIEW is the explicit `:preview' keyword value (nil means \"fall back
to the registry\"):
  nil        — look up CATEGORY in `fzfa-preview-functions'.
  a function — treat as a `:preview'-only plist (shorthand for the
               common ad-hoc case with no lifecycle).
  a plist    — use as-is.

Resolution is independent of `fzfa-preview-delay'; the delay gates
auto-fire only.  `fzfa-preview-key' (manual fire) and helm's
persistent-action wiring both need the handler regardless of delay."
  (cond
   ((functionp preview) (list :preview preview))
   ((and (listp preview) preview) preview)
   (t (alist-get category fzfa-preview-functions))))

(defun fzfa--preview-call (action session &rest args)
  "Dispatch ACTION to the current cell's handler with ARGS and SESSION.

Handlers may declare `(cand)' or `(cand session)' — SESSION is
appended only when the handler's arity accepts it, so short lambdas
that don't care about session stay clean."
  (when-let* ((handler (car fzfa--preview-session))
              (fn (plist-get handler action)))
    (let* ((win (fzfa-preview-get :origin-window))
           (buf (fzfa-preview-get :origin-buffer))
           (dir (fzfa-preview-get :default-directory))
           (want (1+ (length args)))
           (max  (cdr (func-arity fn)))
           (call-args (if (or (eq max 'many) (>= max want))
                          (append args (list session))
                        args)))
      (condition-case err
          (if (and (window-live-p win) (buffer-live-p buf))
              (with-selected-window win
                (with-current-buffer buf
                  (let ((default-directory (or dir default-directory)))
                    (apply fn call-args))))
            (let ((default-directory (or dir default-directory)))
              (apply fn call-args)))
        (error
         (message "fzfa preview %s error: %s"
                  action (error-message-string err)))))))

(defun fzfa--preview-install (session &optional delay)
  "Install live preview in the current minibuffer for the active session.

Call from inside a `minibuffer-with-setup-hook' lambda.  Reads the
handler from `fzfa--preview-session', captures origin window/buffer
and `default-directory' into the session state, dispatches :setup, and
registers `minibuffer-exit-hook'.  Adds an auto-fire `post-command-hook'
only when an effective delay is positive.

DELAY defaults to `fzfa-preview-delay'.  When positive, scheduling
uses `run-with-idle-timer' so fast typing or arrow-key bursts suppress
intermediate previews until input settles.  A pending timer is reused
rather than reset; the callback re-reads the current candidate at fire
time so reuse never previews a stale selection.  DELAY of 0 previews
immediately on every selection change.  When both DELAY and
`fzfa-preview-delay' are nil, no auto-fire hook is installed —
preview only fires via `fzfa-preview-key' / `fzfa-preview-current'."
  (let* ((delay (or delay fzfa-preview-delay))
         (mb (current-buffer))
         (run (lambda ()
                ;; Suppress dispatch while a nested minibuffer is up
                ;; (embark's action prompter or similar) — otherwise
                ;; the idle timer would stomp embark's UI with a fresh
                ;; preview swap.  Depth = 1 means we're inside just
                ;; the fzfa session; > 1 means something nested.
                (when (<= (minibuffer-depth) 1)
                  (when-let* ((cand (fzfa--frontend-candidate)))
                    (unless (equal cand fzfa--preview-last)
                      (setq fzfa--preview-last cand)
                      (fzfa--preview-call :preview session cand)))))))
    (fzfa-preview-put :origin-window (minibuffer-selected-window))
    (fzfa-preview-put :origin-buffer (window-buffer
                                      (minibuffer-selected-window)))
    (fzfa-preview-put :default-directory default-directory)
    (setq fzfa--preview-last 'unset
          ;; Marker consulted by fzfa-posframe's embark-buffer routing
          ;; to distinguish "this is a fzfa minibuffer" from a random
          ;; other minibuffer that happens to be visible.
          fzfa--minibuffer-marker t
          ;; Session pointer for fixed-arity third-party integrations
          ;; (embark transformer, mostly) — looked up on the active
          ;; minibuffer via `fzfa--current-session'.
          fzfa--minibuffer-session session
          ;; Expose `run' to `fzfa--frontend-exhibit' so preview fires
          ;; the instant the frontend commits its first batch of
          ;; candidates.  A session that starts with a pre-set query
          ;; (replay) never re-enters `post-command-hook' otherwise:
          ;; the user is idle waiting on async results, and
          ;; timer-fires don't touch `post-command-hook'.
          ;;
          ;; Only wire it when auto-preview is enabled (DELAY set) —
          ;; otherwise the user opted out of hover-fired previews
          ;; entirely and only wants preview on explicit key press.
          fzfa--preview-run-fn (and delay run))
    (fzfa--preview-call :setup session)
    (when delay
      (add-hook
       'post-command-hook
       (if (<= delay 0)
           run
         ;; The idle-timer callback fires with whatever buffer is current
         ;; at fire time, not necessarily this minibuffer.  All state we
         ;; touch (`fzfa--preview-timer', `fzfa--preview-last') is
         ;; buffer-local here, so route the callback through MB or we
         ;; silently corrupt the wrong buffer's locals and leave a stale
         ;; timer object behind that blocks every subsequent preview.
         (lambda ()
           (unless (timerp fzfa--preview-timer)
             (setq fzfa--preview-timer
                   (run-with-idle-timer
                    delay nil
                    (lambda ()
                      (when (buffer-live-p mb)
                        (with-current-buffer mb
                          (when (timerp fzfa--preview-timer)
                            (cancel-timer fzfa--preview-timer))
                          (setq fzfa--preview-timer nil)
                          (funcall run)))))))))
       nil t))
    (add-hook
     'minibuffer-exit-hook
     (lambda ()
       (when (timerp fzfa--preview-timer)
         (cancel-timer fzfa--preview-timer)
         (setq fzfa--preview-timer nil))
       (setq fzfa--preview-run-fn nil
             fzfa--minibuffer-marker nil
             fzfa--minibuffer-session nil)
       (fzfa--preview-call :preview session nil)
       (fzfa--preview-call :exit session))
     nil t)))

(defun fzfa--preview-return (cand session)
  "Dispatch :return on SESSION with CAND (nil = aborted)."
  (fzfa--preview-call :return session cand))

;;; Built-in preview handlers

(defun fzfa--filter-find-file-hook (orig &rest hooks)
  "Advice for `run-hooks': filter `vc-refresh-*' from `find-file-hook'.

ORIG is the advised `run-hooks'; HOOKS are its arguments.
Active only while `fzfa-with-quiet-find-file' is on the stack.
Mutating both the default and current value via `cl-letf' so nested
`run-hooks' calls (the load path is not a single direct invocation)
see the same filtered list."
  (if (memq 'find-file-hook hooks)
      (cl-letf* (((default-value 'find-file-hook)
                  (cl-remove-if
                   (lambda (h)
                     (and (symbolp h)
                          (string-prefix-p "vc-refresh-" (symbol-name h))))
                   (default-value 'find-file-hook)))
                 (find-file-hook (default-value 'find-file-hook)))
        (apply orig hooks))
    (apply orig hooks)))

(defvar fzfa-loading-preview nil
  "Non-nil while a preview buffer is being loaded by fzfa.

Bound to `t' by `fzfa-with-quiet-find-file' around its body.  User
hooks can consult this to conditionalize behaviour on whether the
current mode-setup / find-file dispatch is happening for a preview
buffer or a real user visit:

  ;; Skip an expensive feature in preview:
  (add-hook \\='dired-mode-hook
            (lambda ()
              (unless fzfa-loading-preview
                (media-thumbnail-dired-mode))))

  ;; Or, only run something in preview:
  (add-hook \\='prog-mode-hook
            (lambda ()
              (when fzfa-loading-preview
                (setq-local truncate-lines t))))

Dynamic, not buffer-local — the binding is unwound when
`fzfa-with-quiet-find-file' returns, so nothing observed later can be
confused about whether a buffer is \"still\" a preview.")

(defmacro fzfa-with-quiet-find-file (&rest body)
  "Run BODY with file-loading noise and preview-hostile hooks suppressed.

`find-file-noselect' can trigger minibuffer prompts via file-local
variables, `find-file-hook', or warnings — inside an active completion
those signal \"Command attempted to use minibuffer while in minibuffer\".
Custom `:preview' handlers that load files should wrap the call in this
macro.

`fzfa-loading-preview' is dynamically bound to `t' around BODY so user
hooks can detect the preview context (see its docstring).

Mode hooks run normally; costly / stateful entries are filtered by
`fzfa--filter-suppressed-hooks' against `fzfa-preview-suppressed-functions'.
`fzfa--filter-find-file-hook' additionally strips `vc-refresh-*'
entries from `find-file-hook' via prefix match.  Both advices are
installed and removed under `unwind-protect' so a non-local exit can't
strand them."
  (declare (indent 0) (debug t))
  `(let ((enable-local-variables :safe)
         (enable-local-eval nil)
         (enable-dir-local-variables nil)
         (non-essential t)
         (inhibit-message t)
         (fzfa-loading-preview t))
     (advice-add 'run-hooks :around #'fzfa--filter-find-file-hook)
     (advice-add 'run-hooks :around #'fzfa--filter-suppressed-hooks)
     (unwind-protect
         (progn ,@body)
       (advice-remove 'run-hooks #'fzfa--filter-suppressed-hooks)
       (advice-remove 'run-hooks #'fzfa--filter-find-file-hook))))

(defun fzfa--disassociate (buf)
  "Schedule BUF to be disassociated from its file on the next command.

Setting variable `buffer-file-name' to nil makes the buffer invisible
to `find-buffer-visiting' / `get-file-buffer', so the post-selection
action on the candidate opens a fresh, fully-hooked buffer via
`find-file' instead of reusing the partial-init preview buffer.

Disassociation is delayed to `pre-command-hook' rather than done
immediately because some major modes (`pdf-view-mode', `doc-view-mode')
read variable `buffer-file-name' during their own setup."
  (let ((hook (make-symbol "fzfa--disassociate-hook")))
    (fset hook
          (lambda ()
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (remove-hook 'pre-command-hook hook)
                (setq-local buffer-read-only t
                            buffer-file-name nil)))))
    (add-hook 'pre-command-hook hook)))

(defun fzfa-preview-show (buffer &optional pos)
  "Show BUFFER (optionally moved to POS) in the originating window.

Does not steal the minibuffer's input focus.  POS may be a buffer
position number or a marker; when nil, point is left where it was.
Public helper for `:preview' handlers to call."
  (when (buffer-live-p buffer)
    (when pos
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (goto-char (if (markerp pos) (marker-position pos) pos)))))
    (let ((win (display-buffer buffer '(display-buffer-same-window))))
      (when (window-live-p win)
        (with-selected-window win
          (run-hooks 'fzfa-after-preview-hook)))
      win)))

(defun fzfa--grep-preview (cand session)
  "Open the FILE from a FILE:LINE:CONTENT grep CAND at LINE for preview.

Resolves FILE against CAND's source's :directory — the search root
grep ran under."
  (when (and cand
             (string-match "\\`\\(.+?\\):\\([0-9]+\\):" cand))
    (let* ((file (match-string 1 cand))
           (line (string-to-number (match-string 2 cand)))
           (dir  (or (fzfa-candidate-directory cand session)
                     default-directory))
           (path (expand-file-name file dir)))
      (when (file-readable-p path)
        (let ((buf (fzfa-with-quiet-find-file
                    (find-file-noselect path 'nowarn))))
          (with-current-buffer buf
            (save-restriction
              (widen)
              (goto-char (point-min))
              (forward-line (1- line))))
          (fzfa-preview-show buf))))))

(defun fzfa--location-preview (cand _session)
  "Preview SOURCE at LINE for an `fzfa-location' CAND.

Reads `(SOURCE . LINE)' off CAND's `fzfa-location' text property.
SOURCE is resolved as a file path when `file-readable-p', otherwise as
a live buffer name.  Computes the line's start position in the source
buffer and hands off to `fzfa-preview-show'.  No-op when the property
is missing or the target cannot be resolved."
  (when-let* ((loc (and (stringp cand) (> (length cand) 0)
                        (get-text-property 0 'fzfa-location cand)))
              (source (car loc))
              (line   (cdr loc))
              (buf    (cond
                       ((and (stringp source) (file-readable-p source))
                        (fzfa-with-quiet-find-file
                         (find-file-noselect source 'nowarn)))
                       ((get-buffer source)))))
    (let ((pos (with-current-buffer buf
                 (save-restriction
                   (widen)
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line (1- line))
                     (point))))))
      (fzfa-preview-show buf pos))))

(defun fzfa--buffer-preview (cand _session)
  "Show CAND (a buffer name) in a side window for preview."
  (when-let* ((buf (and cand (get-buffer cand))))
    (fzfa-preview-show buf)))

(defun fzfa--temporary-files ()
  "Return an opener closure for ephemeral preview buffers.

The closure has two call forms:

  (FN PATH) → return a buffer for PATH for preview, or nil if PATH
              matches `fzfa-preview-excluded-files'.  Reuses an
              already-open user buffer, or a buffer this session
              previously opened for preview.
  (FN)      → kill every still-ephemeral preview buffer.  Idempotent.

Preview buffers are disassociated from their file on the next
command (`buffer-file-name' set to nil) so the post-selection
`find-file' on the candidate opens a fresh, fully-hooked buffer
rather than reusing the partial-init preview buffer.
`font-lock-ensure' is called so previews are fontified even
though variable `delay-mode-hooks' suppresses `global-font-lock-mode'."
  (let (ephemerals)  ;; alist of (PATH . BUF)
    (lambda (&optional path)
      (cond
       ((null path)
        (pcase-dolist (`(,_ . ,b) ephemerals)
          (when (buffer-live-p b) (kill-buffer b)))
        (setq ephemerals nil))
       ((stringp path)
        (unless (fzfa--preview-excluded-p path)
          (let ((p (expand-file-name path)))
            (or (find-buffer-visiting p)
                (cdr (assoc p ephemerals))
                (let ((buf (fzfa-with-quiet-find-file
                            (find-file-noselect p 'nowarn))))
                  (with-current-buffer buf
                    (ignore-errors (font-lock-ensure)))
                  (push (cons p buf) ephemerals)
                  (fzfa--disassociate buf)
                  buf)))))))))

(defun fzfa--file-preview-setup (_session)
  "Initialize a fresh `fzfa--temporary-files' opener for this session."
  (fzfa-preview-put :opener (fzfa--temporary-files)))

(defvar fzfa--file-preview-too-large-buffer " *fzfa-preview-too-large*"
  "Name of the shared buffer used to render the \"file too large\" placeholder.")

(defun fzfa--file-preview-too-large (path size)
  "Return a shared placeholder buffer describing PATH being SIZE bytes.

Overwritten on every hover so the buffer content always matches the
current candidate — since preview buffers are per-buffer inside
`posframe' etc., the reused buffer works whether the previous preview
was another oversized candidate or a normal file."
  (let ((buf (get-buffer-create fzfa--file-preview-too-large-buffer)))
    (with-current-buffer buf
      (setq-local buffer-read-only nil)
      (erase-buffer)
      (insert
       (propertize "File too large for preview\n\n"
                   'face 'warning)
       "Path:  " path "\n"
       "Size:  " (file-size-human-readable size) "\n"
       "Limit: " (file-size-human-readable fzfa-preview-file-size-limit)
       "\n\nAdjust `fzfa-preview-file-size-limit' to raise the threshold, "
       "or press RET to open the file.")
      (goto-char (point-min))
      (setq-local buffer-read-only t))
    buf))

(defun fzfa--file-preview (cand session)
  "Open CAND (a file or directory path) for preview.

`fzfa-file-preview-dispatch-functions' gets first crack at each
readable, non-directory PATH; a non-nil return replaces the built-in
handler for that candidate.  Regular files that fall through are gated
by `fzfa-preview-file-size-limit' — files exceeding the limit render a
size-readout placeholder instead of loading, keeping selection movement
snappy on multi-megabyte binaries.  Directories are always previewed
via `dired-mode' (from `find-file-noselect')."
  (when (and cand fzfa-preview-file-size-limit
             (> fzfa-preview-file-size-limit 0))
    (let ((path (fzfa-resolve-candidate cand session)))
      (when (file-readable-p path)
        (cond
         ((file-directory-p path)
          (when-let* ((opener (fzfa-preview-get :opener))
                      (buf (funcall opener path)))
            (fzfa-preview-show buf)))
         ((when-let* ((buf (run-hook-with-args-until-success
                            'fzfa-file-preview-dispatch-functions
                            path session)))
            (fzfa-preview-show buf)
            t))
         (t
          (let* ((size (file-attribute-size (file-attributes path))))
            (cond
             ((and size (< size fzfa-preview-file-size-limit))
              (when-let* ((opener (fzfa-preview-get :opener))
                          (buf (funcall opener path)))
                (fzfa-preview-show buf)))
             (size
              (fzfa-preview-show
               (fzfa--file-preview-too-large path size)))))))))))

(defun fzfa--file-preview-return (cand session)
  "Promote CAND's buffer (if accepted) and kill the remaining ephemerals.

The promoted buffer survives so the caller's subsequent `find-file'
reuses it instead of re-loading from disk."
  (when-let* ((opener (fzfa-preview-get :opener)))
    (when cand
      (when-let* ((buf (find-buffer-visiting
                        (fzfa-resolve-candidate cand session))))
        (funcall opener buf)))
    (funcall opener)))

;;; Completing-read helpers

(defun fzfa--minibuffer-format-reset (&optional suppress-format)
  "Set up the active minibuffer for an fzfa session.

Always installs `fzfa-apply-key' and (under icomplete) pins
`resize-mini-windows' to `grow-only'.

When SUPPRESS-FORMAT is non-nil, also disables `vertico''s
`vertico-count-format' and icomplete's `icomplete-matches-format' so
they don't overwrite fzfa's own stats overlay / pre-prompt text.  Ivy's
analogous `ivy-count-format' is bound at the call site under the same
gating.  Pass nil to leave the frontend's native count rendering intact
\(useful for sync `:candidates' sources where fzfa doesn't install a
stats overlay — vertico shows its native \"N/M PROMPT\", ivy its
configured `ivy-count-format', icomplete its match list, etc.).

Under icomplete, the `grow-only' pin works around the empty-input state
hitting a zero-length-overlay resize blind spot: the overlay's
multi-line `after-string' isn't counted, so any redisplay collapses the
pane to 1 line.  Toggle the resize policy based on input state —
`grow-only' (plus an explicit fit) when empty so the pane stays
visible, user's original value otherwise so narrowing can shrink
naturally.  Covers initial entry and backspace-to-empty alike."
  (when suppress-format
    (when (boundp 'vertico-count-format)
      (setq-local vertico-count-format nil))
    (when (boundp 'icomplete-matches-format)
      (setq-local icomplete-matches-format nil)))
  ;; Pin fzfa's passthrough completion-style buffer-locally.  The
  ;; call-site let-binding around `completing-read' unwinds by the
  ;; time timer callbacks (preview idle-timer, icomplete's
  ;; `icomplete-exhibit', poll timer) fire, so those callbacks would
  ;; drive completion with the user's global styles (fussy, orderless,
  ;; etc.) against fzfa's passthrough table and blow up with
  ;; `(wrong-type-argument listp 0)' from the basic/PCM machinery.
  ;; A setq-local wins over the dynamic scope permanently for this
  ;; minibuffer.
  (setq-local completion-styles '(fzfa))
  (fzfa--minibuffer-install-apply-key)
  (fzfa--minibuffer-install-preview-key)
  (when (bound-and-true-p icomplete-mode)
    ;; Empty-input state hits the zero-length-overlay resize blind spot:
    ;; the overlay's multi-line `after-string' isn't counted, so any
    ;; redisplay collapses the pane to 1 line.  Toggle the resize policy
    ;; based on input state — `grow-only' (plus an explicit fit) when
    ;; empty so the pane stays visible, user's original value otherwise
    ;; so narrowing can shrink naturally.  Covers initial entry and
    ;; backspace-to-empty alike.
    (let ((orig resize-mini-windows))
      (setq-local resize-mini-windows 'grow-only)
      (add-hook 'after-change-functions
                (lambda (&rest _)
                  (if (= (minibuffer-prompt-end) (point-max))
                      (progn
                        (setq-local resize-mini-windows 'grow-only)
                        ;; `after-change-functions' fires during the
                        ;; edit, BEFORE icomplete's post-command-hook
                        ;; rebuilds the overlay.  Fitting now would
                        ;; read stale content.  Defer so the fit sees
                        ;; the freshly-rendered candidate list.
                        (run-at-time
                         0 nil #'fzfa--icomplete-fit-mini-window))
                    (setq-local resize-mini-windows orig)))
                nil t))
    ;; One-shot fit after icomplete's initial `post-command-hook'-driven
    ;; exhibit populates the overlay.
    (run-at-time 0 nil #'fzfa--icomplete-fit-mini-window)))

(defun fzfa--commas (n)
  "Format integer N with comma thousand-separators.

e.g., 1234567 → 1,234,567."
  (let ((s (number-to-string n))
        (out ""))
    (while (> (length s) 3)
      (setq out (concat "," (substring s -3) out)
            s   (substring s 0 -3)))
    (concat s out)))

(defun fzfa--format-stats (prefix idx filtered total)
  "Format the fzfa stats text \"PREFIX[N/][FILTERED](TOTAL) \".

PREFIX is the leading text — e.g. \"PROMPT DIR \", \"DIR \", or just
\"PROMPT\" — and is emitted verbatim.  IDX is the 0-based selection
index; when non-nil it's rendered as \"N/\", when nil that segment is
omitted (frontends like icomplete don't expose a selection index).
FILTERED and TOTAL are integer candidate counts, comma-formatted."
  (format "%s%s[%s](%s) "
          prefix
          (if idx (format "%d/" (1+ idx)) "")
          (fzfa--commas filtered)
          (fzfa--commas total)))

(defun fzfa-format-prompt (data)
  "Default value of `fzfa-prompt-function'.

`:command' sources render \"PROMPT DIR IDX/[FILTERED](TOTAL)\".
`:multi' sources render the same with the active narrow name in place
of DIR (when a narrow is active).
`:candidates' sources return nil, so the bare PROMPT shows with no
stats overlay — sync, in-memory pools don't surface meaningful totals.
Callers that want a dir-aware prompt over `:candidates' (e.g.,
`fzfa-browse-files') let-bind their own `fzfa-prompt-function'.

DATA is the plist documented at `fzfa-prompt-function'."
  (pcase (plist-get data :source-kind)
    (:candidates nil)
    (:command
     (fzfa--format-stats
      (concat (plist-get data :prompt) (plist-get data :directory) " ")
      (plist-get data :index)
      (plist-get data :filtered)
      (plist-get data :total)))
    (:multi
     (let* ((prompt (plist-get data :prompt))
            (narrow (plist-get data :narrow-name))
            (prefix (if narrow
                        (concat prompt
                                (propertize (format "{%s} " narrow)
                                            'face 'minibuffer-prompt))
                      prompt)))
       (fzfa--format-stats prefix
                           (plist-get data :index)
                           (plist-get data :filtered)
                           (plist-get data :total))))))

(defcustom fzfa-prompt-function #'fzfa-format-prompt
  "Function that renders the fzfa prompt-overlay text.

Called with a single plist DATA argument on every overlay refresh
\(roughly per keystroke under vertico/icomplete; from
`ivy-pre-prompt-function' under ivy).  Should return a string to display
in place of the minibuffer prompt, or nil to skip decoration (the bare
prompt passed to `completing-read' shows through unchanged — ivy renders
just its own prompt).

DATA plist keys:
  :source-kind   `:command', `:candidates', or `:multi'.  Branch on
                 this to render async sources differently from in-memory
                 ones.
  :prompt        Base prompt string passed to `completing-read'.
                 Returning this verbatim is equivalent to nil.
  :directory     Abbreviated working directory.  Always present for
                 single-source sessions; nil for `:multi' (sources may
                 each have their own dir).
  :command       Shell command string for `:command' sources, nil
                 otherwise.
  :index         Current selection index (0-based), or nil under
                 frontends without a selection cursor (e.g. icomplete).
  :filtered      Candidates matching the current query.
  :total         Total candidates collected so far.
  :narrow-name   Name of the currently-narrowed source (`:multi' only),
                 or nil."
  :type 'function
  :group 'fzfa)

(defun fzfa--normalize-candidates (cands)
  "Normalize CANDS to the (lambda (INPUT CALLBACK) ...) producer shape.

Accepted forms:
- list of strings → wrapped as (lambda (_ cb) (funcall cb LIST))
- zero-arg function returning a list → wrapped as
  (lambda (_ cb) (funcall cb (funcall FN)))
- 2-arg function (lambda (INPUT CALLBACK) ...) → returned as-is

Returns nil for nil input.  Signals on any other shape."
  (cond
   ((null cands) nil)
   ((functionp cands)
    (let ((min-args (car (func-arity cands))))
      (cond
       ((= min-args 0)
        (let ((fn cands))
          (lambda (_input callback) (funcall callback (funcall fn)))))
       ((>= min-args 1) cands)
       (t (error "fzfa: :candidates fn has unsupported arity %S" min-args)))))
   ((listp cands)
    (let ((lst cands))
      (lambda (_input callback) (funcall callback lst))))
   (t (error "fzfa: :candidates must be list, zero-arg fn, or 2-arg fn, got %S"
             cands))))

(defun fzfa--current-query (str)
  "Return the live query for a `completing-read' collection lambda.

STR is the string the collection function was called with.  Under
`ivy-mode' STR is reliably `ivy-text' (the user's input), so trust
it verbatim — ivy renders candidates as inserted minibuffer text
below the prompt, and falling back to `minibuffer-contents' would
pull in the rendered candidate display as a giant garbage query.

Other frontends (vertico, icomplete) sometimes pass an empty STR
even when the minibuffer holds a real query; they render candidates
via display overlays so `minibuffer-contents' stays clean.  Fall
back to that there.

Returns the empty string otherwise."
  (or (if (or (not (string-empty-p str))
              (bound-and-true-p ivy-mode))
          str
        (when-let* ((win (active-minibuffer-window)))
          (with-current-buffer (window-buffer win)
            (minibuffer-contents-no-properties))))
      ""))

(defun fzfa--candidate-limit ()
  "Return `fzfa-max-candidates' when it is a positive integer, else nil.

Nil disables the cap on the C side."
  (and fzfa-max-candidates
       (> fzfa-max-candidates 0)
       fzfa-max-candidates))

(defun fzfa--final-p (r handle query)
  "Return non-nil when R is the final answer for QUERY on HANDLE.

R is the value returned by `fzf-native-async-candidates' for QUERY.
The result is final in either of two cases:
  - R is non-nil (candidates to display); or
  - R is nil AND `fzf-native-async-result-fresh-p' reports the cache
    fresh for QUERY — scoring has completed at the current pool size,
    so zero matches is the authoritative answer.
A nil return means scoring for QUERY is still in flight: an empty R
should be treated as \"no information yet\", and callers should
preserve the previous display rather than blanking it.

Use this guard around any `setq' that updates a stored result, and
around any side-effect that commits R to the display.

When the loaded fzf-native predates `fzf-native-async-result-fresh-p',
fall back to treating any nil R as in-flight.  That loses the
authoritative-zero distinction (consult-style display will keep showing
stale candidates when scoring legitimately matched nothing), but it
keeps fzfa functional on older fzf-native builds."
  (or r (and (fboundp 'fzf-native-async-result-fresh-p)
             (fzf-native-async-result-fresh-p handle query))))

(defun fzfa--defer-async-stop (handles)
  "Stop each of HANDLES via `fzf-native-async-stop'.

HANDLES may be a single async handle, a list, or a vector; nil values
\(including nil HANDLES) are ignored."
  (cond
   ((null handles))
   ((vectorp handles)
    (cl-loop for h across handles when h do (fzf-native-async-stop h)))
   ((listp handles)
    (dolist (h handles) (when h (fzf-native-async-stop h))))
   (t (fzf-native-async-stop handles))))

;;; History

(defvar fzfa--hist-hash nil
  "Cached string→position hash for the active minibuffer history.

Lower positions are more recent.  Built lazily by `fzfa--history-hash'.")

(defvar fzfa--hist-hash-last-val nil
  "List `fzfa--hist-hash' was built from; `eq'-compared per call.

Any update to the history list (entries cons onto the head) breaks
identity and triggers a rebuild.")

(defun fzfa--history-hash ()
  "Return a cached string→position hash for the active minibuffer history.

Reads `minibuffer-history-variable', which `completing-read' dynamically
binds to the HIST argument supplied by the caller.  Returns nil when no
real history variable is in effect.  Lower positions are more recent.

Reuses `fzfa--hist-hash' as long as the underlying list is `eq' to the
value the hash was built from; new entries cons onto the head, so any
update invalidates the cache."
  (let* ((sym (and (not (eq minibuffer-history-variable t))
                   minibuffer-history-variable))
         (hist (and sym (symbol-value sym))))
    (cond
     ((eq hist fzfa--hist-hash-last-val) fzfa--hist-hash)
     (t
      (setq fzfa--hist-hash-last-val hist)
      (setq fzfa--hist-hash
            (when hist
              (let ((table (make-hash-table :test 'equal :size (length hist))))
                (cl-loop for index from 0
                         for item in hist
                         unless (gethash item table)
                         do (puthash item index table))
                table)))))))

(defun fzfa--build-history-hash (hist-sym)
  "Build candidate→index recency hash from HIST-SYM.

Lower indices are more recent.  Returns nil when HIST-SYM is nil,
unbound, or its value is empty.  Parameterised counterpart to
`fzfa--history-hash' (which always reads `minibuffer-history-variable')
— callers in the multi-source loop pass each source's own history
symbol so each block ranks against its own recency, not the outer
multi-read's nil history."
  (when-let* ((_ (and hist-sym (boundp hist-sym)))
              (hist (symbol-value hist-sym))
              ((not (null hist))))
    (let ((table (make-hash-table :test 'equal :size (length hist))))
      (cl-loop for index from 0
               for item in hist
               unless (gethash item table)
               do (puthash item index table))
      table)))

(defun fzfa--score-history-length-sort (completions hist-hash)
  "Order COMPLETIONS by score, then HIST-HASH recency, then length.

Sort keys, in order: `completion-score' (desc), HIST-HASH index
\(asc, lower index = more recent), candidate length (asc).

HIST-HASH may be nil — the history tiebreak is then skipped and ties
fall straight through to length.  Returns a fresh list; the input is
not mutated.

Building block shared by `fzfa--sort-by-history' (single-source) and
the multi-source per-source pass in the multi loop."
  (mapcar
   #'car
   (sort
    (mapcar
     (lambda (c)
       (list c
             (or (get-text-property 0 'completion-score c) 0)
             (or (and hist-hash (gethash c hist-hash)) most-positive-fixnum)
             (length c)))
     completions)
    (lambda (a b)
      (let ((s1 (nth 1 a)) (s2 (nth 1 b)))
        (if (= s1 s2)
            (let ((h1 (nth 2 a)) (h2 (nth 2 b)))
              (if (= h1 h2)
                  (< (nth 3 a) (nth 3 b))
                (< h1 h2)))
          (> s1 s2)))))))

(defun fzfa--rank-and-highlight (slot query hist-sym)
  "Per-source rank + highlight refresh for SLOT against QUERY.

Pass SLOT through unchanged when QUERY is empty, SLOT is empty, or
SLOT's head carries no `completion-score' (async path — C order is
canonical).  Otherwise sort SLOT by score/history/length and refresh
match face via `fzf-native-highlight-all'.

HIST-SYM is the source's :history variable symbol (or nil) — used for
the per-source recency tiebreak.  QUERY is the typed filter the C
scorer already ran against, used to recompute positions for the
highlight pass.

Shared by the vertico multi loop, ivy multi push, and the single +
multi helm dispatch.  Each call site previously inlined this body;
factor centralizes it so Chunk 6's batch-highlight suppression edits
one place instead of four."
  (cond
   ((null slot) slot)
   ((not (and (stringp (car slot))
              (not (string-empty-p query))
              (get-text-property 0 'completion-score (car slot))))
    slot)
   (t
    (let* ((hist (fzfa--build-history-hash hist-sym))
           (sorted (fzfa--score-history-length-sort slot hist)))
      (when (fboundp 'fzf-native-highlight-all)
        (fzfa--bridge-defcustoms #'fzf-native-highlight-all sorted query))
      sorted))))

(defun fzfa--tagged-p (cand)
  "Non-nil if CAND carries an invisible multi-source tofu suffix.

The suffix is added by `fzfa--tag' to make cross-source
duplicates `string='-distinct.  Its codepoint sits at
`fzfa--tofu-base' or higher (Unicode PUA range).  Used by
`fzfa--sort-by-history' to detect multi-source input — the multi
loop already applied per-source sort + highlight, so a global
re-sort here would trample the per-source ordering."
  (and (stringp cand)
       (let ((n (length cand)))
         (and (> n 0)
              (>= (aref cand (1- n)) fzfa--tofu-base)))))

(defun fzfa--sort-by-history (completions &optional history-sym)
  "Order COMPLETIONS by score, history recency, then length.

Primary key is the `completion-score' text property attached on
the sync path by `fzf-native-score-all'.  Ties break by position
in HISTORY-SYM (a history variable, more recent first), then by
candidate length (shorter first).

HISTORY-SYM is the source's `:history' variable symbol.  Nil
means \"this source opted out of history-based ordering\" — the
history branch is skipped entirely.  Notably, nil does NOT fall
back to the global `minibuffer-history' (which carries noise
from unrelated commands — `eval-expression', `read-string', etc.
— none of which overlap meaningfully with fzfa candidates).

Bound into per-session metadata as a closure by
`fzfa--completion-metadata' so the active source's `:history' is
captured at session-construction time.

Branches dispatched in order:
- nil COMPLETIONS                  → nil
- multi-source (tofu-tagged head)  → pass through; the multi loop
                                     already ranked each source
                                     internally + applied per-source
                                     highlights, so a global re-sort
                                     across source boundaries would
                                     trample that ordering
- empty query, HISTORY-SYM set     → rank by that source's history
- empty query, HISTORY-SYM nil     → pass through (no reorder)
- head lacks `completion-score'    → single-source async; C order is
                                     canonical, pass through
- otherwise                        → single-source sync: score
                                     + (history if HISTORY-SYM set)
                                     + length sort, then post-sort
                                     highlight refresh"
  (let ((query (fzfa--current-query "")))
    (cond
     ((null completions) nil)
     ((fzfa--tagged-p (car completions))
      completions)
     ((string-empty-p query)
      (if-let* ((hist (and history-sym
                           (fzfa--build-history-hash history-sym))))
          (mapcar
           #'car
           (sort
            (mapcar
             (lambda (c)
               (cons c (or (gethash c hist) most-positive-fixnum)))
             completions)
            (lambda (a b) (< (cdr a) (cdr b)))))
        completions))
     ((null (get-text-property 0 'completion-score (car completions)))
      completions)
     (t
      (let ((sorted (fzfa--score-history-length-sort
                     completions
                     (and history-sym
                          (fzfa--build-history-hash history-sym)))))
        (when (fboundp 'fzf-native-highlight-all)
          (fzfa--bridge-defcustoms #'fzf-native-highlight-all sorted query))
        sorted)))))

(defun fzfa--history-rank (candidates hist-sym)
  "Return CANDIDATES reordered by recency in HIST-SYM, tofu-stripped.

HIST-SYM is a history variable symbol (or nil).  Each candidate is
looked up via `fzfa--tofu-hide' so multi-source entries carrying an
invisible PUA suffix still match the bare strings stored in history.
Candidates absent from HIST-SYM keep their incoming relative order — a
stable `sort' preserves source order for ties at `most-positive-fixnum'.

Returns CANDIDATES unchanged when HIST-SYM is nil, unbound, or empty.
This helper is the multi-source analogue of `fzfa--sort-by-history',
where the active `minibuffer-history-variable' isn't meaningful because
multi's outer `completing-read' is intentionally called with HIST nil."
  (let ((hist (and hist-sym (boundp hist-sym) (symbol-value hist-sym))))
    (if (not hist)
        candidates
      (let ((table (make-hash-table :test 'equal :size (length hist))))
        (cl-loop for index from 0
                 for item in hist
                 unless (gethash item table)
                 do (puthash item index table))
        (mapcar
         #'car
         (sort
          (mapcar
           (lambda (c)
             (cons c (or (gethash (fzfa--tofu-hide c) table)
                         most-positive-fixnum)))
           candidates)
          (lambda (a b) (< (cdr a) (cdr b)))))))))

(cl-defun fzfa--completion-metadata (category &key annotate affix group history)
  "Return the `metadata' alist for fzfa's `completing-read' collection lambdas.

CATEGORY is the completion category symbol.  Optional ANNOTATE / AFFIX /
GROUP attach `annotation-function', `affixation-function', and
`group-function' when non-nil.  `display-sort-function' and
`cycle-sort-function' route through `fzfa--sort-by-history' so the empty
query surfaces recent picks first, while scored output produced by the C
scorer is preserved verbatim.

HISTORY is the active source's `:history' variable symbol; when non-nil,
the sort function is wrapped in a per-session closure so the empty-query
reorder and sync tiebreak use that specific history.  Nil keeps the bare
function — `fzfa--sort-by-history' then skips history entirely (does NOT
fall back to global `minibuffer-history')."
  (let ((sort-fn (if history
                     (lambda (cands) (fzfa--sort-by-history cands history))
                   #'fzfa--sort-by-history)))
    `(metadata
      (category . ,category)
      (display-sort-function . ,sort-fn)
      (cycle-sort-function . ,sort-fn)
      ,@(when annotate `((annotation-function . ,annotate)))
      ,@(when affix    `((affixation-function . ,affix)))
      ,@(when group    `((group-function      . ,group))))))

(defun fzfa--maybe-expand (result directory resolve-paths)
  "Return RESULT expanded against DIRECTORY when RESOLVE-PATHS is non-nil.

Low-level primitive used by internal call sites that have DIRECTORY
and RESOLVE-PATHS in explicit scope (e.g. the `:inject' replay path,
the helm dispatch return, the nested-multi entry action) — call sites
where no session dynvars are bound.  Session-aware consumers use
`fzfa-resolve-candidate' instead, which layers over this by resolving
`(source :directory)' and `(source :resolve-paths)' from the
candidate's emitting source.

For RESOLVE-PATHS=t the whole RESULT is passed through `expand-file-name'
— this works for both plain paths and FILE:LINE:CONTENT grep candidates,
since `expand-file-name' prepends DIRECTORY and leaves the suffix
untouched.  RESOLVE-PATHS=`auto' resolves to t when the source has a
`:command'; here (no session in scope) callers pre-resolve `auto' from
their own context or pass t/nil explicitly."
  (if (and resolve-paths (stringp result) (not (string-empty-p result)))
      (expand-file-name result directory)
    result))

(defun fzfa-candidate-directory (cand session)
  "Return CAND's emitting source's :directory, or nil.

Looks up CAND in SESSION's `cand->src' hash, gets that source's plist
from SESSION's `specs' vector, and returns its `:directory' — but only
when the source declares its candidates ARE paths, via `:resolve-paths':

  auto — treated as t when the source has a `:command' (fd / rg / etc
         emit paths), nil otherwise (candidates lists usually don't).
  t    — explicit path-shaped.
  nil  — explicit non-path-shaped.

Returns nil when SESSION is nil, CAND is not in the map, or the
source is non-path-shaped."
  (when-let* ((session)
              ((stringp cand))
              ((> (length cand) 0))
              (idx (gethash cand (fzfa-session-cand->src session)))
              (src (aref (fzfa-session-specs session) idx))
              (rp  (plist-get src :resolve-paths))
              ((if (eq rp 'auto) (plist-get src :command) rp)))
    (plist-get src :directory)))

(defun fzfa-resolve-candidate (cand session)
  "Return CAND expanded against its emitting source's :directory.

Returns CAND unchanged when the source is non-path-shaped or SESSION
is nil.  Any tofu suffix on multi-source candidates is stripped before
the expansion so absolute paths never carry disambiguation chars."
  (if-let* ((dir (fzfa-candidate-directory cand session)))
      (expand-file-name (fzfa--tofu-hide cand) dir)
    cand))

(defun fzfa--default-dir ()
  "Return the working directory for fzfa commands.

Priority: `fzfa-directory' >
          `fzfa-project-backend' >
          `default-directory'."
  (or fzfa-directory
      (pcase fzfa-project-backend
        ((pred functionp)
         (funcall fzfa-project-backend))
        ('project
         (when-let* ((pr (project-current)))
           (project-root pr)))
        ('projectile
         (when (bound-and-true-p projectile-mode)
           (projectile-project-root))))
      default-directory))

;;; Split-style + display infrastructure

(defcustom fzfa-separator ?#
  "Character delimiting the CMD region in async sessions.

In `compact' and `full' display, the minibuffer text has the shape
\"<sep>CMD<sep>FILTER\".  The two separators are pinned by self-
healing overlays — deleting one re-inserts it — so this is a pure
display choice.  Same character is used on both sides, so anything
symmetric reads well."
  :type 'character
  :group 'fzfa)

(defcustom fzfa-shell-command-debounce 0.2
  "Seconds of typing silence before a shell command restart fires.

Each keystroke that changes the command portion of the minibuffer
reschedules a fresh restart timer; the producer process is not
spawned until the user pauses for this long, so a burst of
keystrokes ends with exactly one restart on the final cmd value."
  :type 'float
  :group 'fzfa)

(defcustom fzfa-shell-command-throttle 0.5
  "Minimum seconds between shell command restarts."
  :type 'float
  :group 'fzfa)

(defun fzfa--split-input (str)
  "Split STR (\"<sep>CMD<sep>FILTER\" shape) into (CMD . FILTER).

SEP is `fzfa-separator'.  When STR doesn't start with SEP, the
whole STR is CMD and FILTER is empty.  With one SEP only, the
trailing text is CMD and FILTER is empty."
  (let ((sep (char-to-string fzfa-separator)))
    (cond
     ((not (string-prefix-p sep str)) (cons str ""))
     (t
      (let* ((tail (substring str 1))
             (close (string-match (regexp-quote sep) tail)))
        (if close
            (cons (substring tail 0 close)
                  (substring tail (1+ close)))
          (cons tail "")))))))

(defun fzfa--split (input display-state command)
  "Return (CMD . FILTER) for INPUT given the current session state.

In `hidden' DISPLAY-STATE, CMD lives outside the buffer (in COMMAND,
the closure variable in `fzfa-completing-read''s body or its
helm/multi analogue), so the split is trivial: CMD = COMMAND, FILTER
= INPUT.  In any other display state, `fzfa--split-input'
parses INPUT against `fzfa-separator'.

Frontend-agnostic — the table-lambda in `completing-read' sessions
calls it with `(fzfa--current-query str)' as INPUT; helm's
`:candidates' callback calls it with `helm-pattern'.  Same body for
both."
  (if (eq display-state 'hidden)
      (cons (or command "") input)
    (fzfa--split-input input)))

(defcustom fzfa-display-key ">"
  "Key string that toggles compact view of the CMD portion.

When compact, only the program name and the quoted-argument slot
\(if any) are visible; flags are hidden behind a `...' display.
Press the key again to expand and edit the full command.  The
session starts compact.  Set to nil to disable the feature entirely
\(no binding, no initial compaction)."
  :type '(choice (const :tag "Disabled" nil) string)
  :group 'fzfa)

(defun fzfa--display-cmd-bounds (sep)
  "Return (CMD-BEG . CMD-END) for the CMD region in the current minibuffer.

SEP is the separator character used by the active split style.  Returns
nil when the minibuffer does not begin with SEP or no closing SEP is
present yet."
  (save-excursion
    (goto-char (minibuffer-prompt-end))
    (when (eq (char-after) sep)
      (let ((beg (1+ (point))))
        (goto-char beg)
        (when (search-forward (char-to-string sep) nil t)
          (cons beg (1- (point))))))))

(defun fzfa--display-make-overlays (cmd-beg cmd-end)
  "Return overlays compacting flag regions between CMD-BEG and CMD-END.

Uses the *last* balanced `\\='…\\=' / \"…\" pair as anchor (so an
earlier quoted flag value like `-flag=\\='val\\='' is ignored in favor
of the trailing input slot): text from the end of the program name up
to the opening quote renders as \" ... \", and any tail after the
closing quote renders as \"\".  Without a quoted argument, everything
after the program name is hidden with an empty display.  Whitespace-
only spans are left alone."
  (let* ((cmd-text (buffer-substring-no-properties cmd-beg cmd-end))
         (prog-end (if (string-match "\\`[^ \t]+" cmd-text)
                       (+ cmd-beg (match-end 0))
                     cmd-end))
         (last-pair
          (save-match-data
            (let ((pos 0) last)
              (while (string-match "\\('[^']*'\\|\"[^\"]*\"\\)"
                                   cmd-text pos)
                (setq last (cons (match-beginning 0) (match-end 0))
                      pos  (match-end 0)))
              last)))
         overlays)
    (cl-flet ((mk (beg end display)
                (when (and (< beg end)
                           (string-match-p
                            "[^ \t]"
                            (buffer-substring-no-properties beg end)))
                  (let ((ov (make-overlay beg end nil t nil)))
                    (overlay-put ov 'display display)
                    (overlay-put ov 'fzfa-display t)
                    (push ov overlays)))))
      (cond
       (last-pair
        (let ((qb (+ cmd-beg (car last-pair)))
              (qe (+ cmd-beg (cdr last-pair))))
          (mk prog-end qb " ... ")
          (mk qe cmd-end "")))
       (t
        (mk prog-end cmd-end ""))))
    overlays))

(defun fzfa--display-next-state (state)
  "Return the display STATE cycled one step forward.

The cycle is `hidden' → `compact' → `full' → `hidden'.  Any unknown
input falls back to `hidden' (defensive default for an unknown
session state)."
  (cl-case state
    ((hidden) 'compact)
    ((compact) 'full)
    ((full) 'hidden)
    (otherwise 'hidden)))

(defun fzfa--display-materialize (cmd initial-char)
  "Materialize \"<sep>CMD<sep>\" at the start of the editable region.

Inserts INITIAL-CHAR + CMD + INITIAL-CHAR at `minibuffer-prompt-end'
in the current buffer.  Preserves point's offset within FILTER —
i.e. the distance from `minibuffer-prompt-end' before the call
equals the distance from the start of the new FILTER region after.

Installs two self-healing protective overlays on the separators and
returns them as a two-element list `(OPEN-OVERLAY CLOSE-OVERLAY)'.

Operates on `current-buffer'.  Inside an actual minibuffer
`minibuffer-prompt-end' returns the position past the prompt's
`field' boundary; in temp buffers it returns `(point-min)' so this
helper is testable in isolation."
  (let* ((mbe (minibuffer-prompt-end))
         (filter-offset (max 0 (- (point) mbe)))
         (cmd-str (or cmd ""))
         (cmd-text (concat (char-to-string initial-char)
                           cmd-str
                           (char-to-string initial-char))))
    (goto-char mbe)
    (insert cmd-text)
    (goto-char (+ mbe (length cmd-text) filter-offset))
    (let ((close-pos (+ mbe 1 (length cmd-str))))
      (list (fzfa--protect-separator mbe initial-char)
            (fzfa--protect-separator close-pos initial-char)))))

(defun fzfa--display-extract (separator-overlays)
  "Parse \"<sep>CMD<sep>FILTER\" at start of editable region, delete the prefix.

Reads the buffer contents from `minibuffer-prompt-end' to
`point-max', runs `fzfa--split-input' on it, and uses the
resulting CMD to delete the leading \"<sep>CMD<sep>\" prefix.

Removes SEPARATOR-OVERLAYS first so their `modification-hooks'
don't self-heal the very deletion we're about to perform.

Returns the extracted CMD string.  The caller is responsible for
storing it into the session's closure variable so the hidden-mode
trivial-split picks it up on the next tick.

Operates on `current-buffer'."
  (let* ((mbe (minibuffer-prompt-end))
         (input (buffer-substring-no-properties mbe (point-max)))
         (split (fzfa--split-input input))
         (cmd (car split)))
    (mapc #'delete-overlay separator-overlays)
    (delete-region mbe (min (point-max) (+ mbe 2 (length cmd))))
    cmd))
;;; Async `completing-read'

(defun fzfa--separator-heal-hook (overlay is-after _beg _end
                                          &optional _prelength)
  "Re-insert OVERLAY's separator char if the overlay's range collapsed.

IS-AFTER is non-nil when called after the modification.  _BEG, _END,
and _PRELENGTH are the standard `modification-hooks' args, ignored.

Used as a `modification-hooks' entry on the protective overlays placed
over the `#…#' splitter separators.  Fires for both content changes and
text-property changes; we self-heal only when the overlay's range
collapsed to zero, which is the signature of a deletion of the covered
character.  Face additions (e.g. vertico's `add-face-text-property')
leave the range intact, so they pass through unmodified.

`inhibit-modification-hooks' is bound during the re-insertion so it
doesn't recurse through this hook."
  (when (and is-after
             (overlay-buffer overlay)
             (= (overlay-start overlay) (overlay-end overlay)))
    (let ((inhibit-modification-hooks t)
          (p (overlay-start overlay))
          (sep (overlay-get overlay 'fzfa-separator-char)))
      (when (and p sep)
        (save-excursion
          (goto-char p)
          (insert (char-to-string sep)))
        (move-overlay overlay p (1+ p))))))

(defun fzfa--protect-separator (pos sep-char)
  "Place a self-healing protective overlay at POS covering SEP-CHAR.

Returns the overlay.  The overlay tracks its single character via
`front-advance' t / `rear-advance' nil so insertions adjacent to the
separator don't extend its range; if the covered char is deleted by
any path, `fzfa--separator-heal-hook' restores it."
  (let ((ov (make-overlay pos (1+ pos) nil t nil)))
    (overlay-put ov 'fzfa-separator-char sep-char)
    (overlay-put ov 'cursor-intangible t)
    (overlay-put ov 'modification-hooks
                 (list #'fzfa--separator-heal-hook))
    ov))

;;; Per-source runtime state

(cl-defstruct (fzfa-source (:constructor fzfa-source-create)
                           (:copier nil))
  "Per-source runtime state.

The bag of mutable state that exists once per source.  In
single-source sessions, one instance lives in the substrate's
let*; in multi-source sessions, a vector of these is indexed by
source idx.  Shared helpers (`fzfa-source--restart',
`fzfa-source--display-cycle', etc.) take this struct and operate
on it identically regardless of which container holds it."
  ;; Identity (set at construction, treated read-only).
  ;; `spec' carries the original :key plist consumed by
  ;; `fzfa-completing-read'; closures fetch non-hot keys via
  ;; `plist-get' on demand.  Hot-path fields are hoisted into
  ;; their own slots.
  spec
  name                          ; string
  directory                     ; absolute path or nil
  history                       ; history symbol or nil
  ;; Async handle (used when source was constructed from :command;
  ;; mutually exclusive with the producer-fn bundle below).
  handle                        ; fzf-native handle or nil
  command                       ; string — CMD the source wants
  current-cmd                   ; string — CMD the handle is running
  last-gen                      ; integer
  ;; Producer-fn (used when source was constructed from :candidates;
  ;; mutually exclusive with the async handle above).  `cands-fn' is
  ;; immutable; the rest is runtime state mutated by the producer
  ;; callback.
  cands-fn                      ; normalized 2-arg producer fn / nil
  snapshot                      ; list — latest producer-delivered list
  prod-token                    ; integer — staleness counter
  prod-input                    ; :unfetched | string
  ;; Display-state machinery (`>'-key cycle).
  display-state                 ; 'hidden | 'compact | 'full
  separator-overlays            ; list of overlays
  display-overlays              ; list of overlays
  ;; Throttle / timers.
  restart-timer                 ; timer or nil
  last-restart-time             ; float
  retry-timer                   ; timer or nil
  ;; Result / score.
  last-result                   ; list — cached candidate list
  last-query                    ; string — query that produced last-result
                                ; (nil before any result is cached).  Used by
                                ; the interrupt branch of helm/ivy :candidates
                                ; closures so a `while-no-input' bail-out
                                ; returns last-result only when the current
                                ; filter still matches — otherwise the user
                                ; sees stale candidates from the prior query
                                ; sitting under the new (often empty) header.
  rank                          ; integer (multi-only)
  total                         ; integer
  filtered                      ; integer
  ;; Preview (multi-only).
  preview-cell                  ; list or nil
  ;; Helm-only.
  source-name)                  ; string or nil

(cl-defun fzfa-make-source (&key spec command candidates directory
                                 history display name)
  "Build a `fzfa-source' from SPEC plus hoisted args.

SPEC is the keyword-args plist consumed by `fzfa-completing-read'
\(extracted via `:extract' mode, or built directly by the substrate).
COMMAND, CANDIDATES, DIRECTORY, HISTORY, and DISPLAY are pulled
from SPEC when not passed explicitly — the explicit-arg form lets
callers reuse already-normalized values.  NAME overrides the
spec's `:name' (used by multi-source name assignment).

The async handle is NOT started here — the caller decides whether
to eager-start via `fzf-native-async-start' or to defer."
  ;; All hoisted args default to nil so the `or' chain falls through
  ;; to the spec plist before the final default.  Using cl-defun
  ;; keyword defaults (e.g. (display 'hidden)) would short-circuit
  ;; the `or' and mask the spec value.
  (let* ((command    (or command (plist-get spec :command) ""))
         (candidates (or candidates (plist-get spec :candidates)))
         (directory  (or directory (plist-get spec :directory)))
         (history    (or history (plist-get spec :history)))
         (display    (or display (plist-get spec :display) 'hidden))
         (name       (or name (plist-get spec :name) "")))
    (fzfa-source-create
     :spec spec
     :name name
     :directory (expand-file-name (or directory default-directory))
     :history history
     :handle nil
     :command command
     :current-cmd nil
     :last-gen -1
     :cands-fn (and candidates
                    (fzfa--normalize-candidates candidates))
     :snapshot nil
     :prod-token 0
     :prod-input :unfetched
     :display-state display
     :separator-overlays nil
     :display-overlays nil
     :restart-timer nil
     :last-restart-time 0.0
     :retry-timer nil
     :last-result nil
     :rank 0
     :total 0
     :filtered 0
     :preview-cell nil
     :source-name nil)))

;;; Session

(cl-defstruct (fzfa-session (:constructor fzfa-session-create)
                            (:copier nil))
  "Runtime state of one in-flight fzfa completing-read.

Slots:

  SPECS           Vector of source plists (immutable).
  SOURCES         Vector of `fzfa-source' structs (mutable runtime).
  CAND->SRC       Hash table: candidate string → source index.
  DIRECTORY       Session's captured search root.
  ENTRY-COMMAND   `this-command' at session open; used by replay.
  NARROW-IDX      Current narrow selection or nil.
  ROUTER-CELLS    Per-source `(handler . state-plist)' cells for
                  preview dispatch, or nil when no source has a
                  registered preview handler.
  APPLY-FN        The `:apply' function for single-source sessions
                  (multi-source lookup is per-candidate via
                  `router-cells').
  SELECTED-IDX    Source idx picked at exit, captured by
                  `minibuffer-exit-hook' before completing-read
                  returns.
  LAST-QUERY      Final query string; captured for replay."
  specs
  sources
  cand->src
  directory
  entry-command
  narrow-idx
  router-cells
  apply-fn
  selected-idx
  last-query)

(defun fzfa--current-session ()
  "Return the `fzfa-session' for the active minibuffer, or nil.

Walks the minibuffer stack — the outer fzfa minibuffer carries the
session even when a nested minibuffer (embark's action prompter) sits
on top.  Used by fixed-arity integrations that can't take session by
parameter (`fzfa-apply-current' from a keybinding, embark transformer)."
  (or (when-let* ((mbwin (active-minibuffer-window)))
        (buffer-local-value 'fzfa--minibuffer-session (window-buffer mbwin)))
      (cl-loop for depth from 1 to (minibuffer-depth)
               for buf = (get-buffer (format " *Minibuf-%d*" depth))
               thereis (and buf (buffer-local-value
                                 'fzfa--minibuffer-session buf)))))

(defun fzfa-session-source-of (session cand)
  "Return the source plist responsible for CAND in SESSION, or nil."
  (when-let* ((hash (fzfa-session-cand->src session))
              (idx (gethash cand hash))
              (specs (fzfa-session-specs session)))
    (aref specs idx)))

(defun fzfa-source--display-clear (source)
  "Delete SOURCE's display-mode overlays."
  (mapc #'delete-overlay (fzfa-source-display-overlays source))
  (setf (fzfa-source-display-overlays source) nil))

(defun fzfa-source--display-apply (source initial-char)
  "Re-apply SOURCE's display-state visuals.

INITIAL-CHAR is the session's `fzfa-separator'.  Clears any
existing display-overlays and installs compact-mode overlays when
the current state is `compact'.  Hidden has no CMD region; full
shows it verbatim — both are pure no-ops after clear."
  (fzfa-source--display-clear source)
  (when (eq (fzfa-source-display-state source) 'compact)
    (when-let* ((bounds (fzfa--display-cmd-bounds initial-char)))
      (setf (fzfa-source-display-overlays source)
            (fzfa--display-make-overlays
             (car bounds) (cdr bounds))))))

(defun fzfa-source--display-force-hidden (source initial-char)
  "Force SOURCE's display-state back to `hidden'.

Extracts CMD from the buffer's `<sep>CMD<sep>' shape into SOURCE's
`command' slot, removes separator overlays, and clears the
compact-mode display overlays.  No-op when SOURCE is already
hidden.  Used by `fzfa-multi-read' on widen / source-switch so the
buffer's `#cmd#filter' shape doesn't linger past the narrow that
established it.

Afterwards display INITIAL-CHAR."
  (unless (eq (fzfa-source-display-state source) 'hidden)
    (setf (fzfa-source-command source)
          (fzfa--display-extract
           (fzfa-source-separator-overlays source)))
    (setf (fzfa-source-separator-overlays source) nil
          (fzfa-source-display-state source) 'hidden)
    (fzfa-source--display-apply source initial-char)))

(defun fzfa-source--display-cycle (source initial-char)
  "Advance SOURCE's display-state through hidden → compact → full → hidden.

INITIAL-CHAR is the session's `fzfa-separator'.  Mutates buffer
content on transitions across the hidden boundary via the
frontend-agnostic `fzfa--display-{materialize,extract}' primitives
\(operate on `current-buffer' from `minibuffer-prompt-end').

The compact / full visual overlay is then re-applied via
`fzfa-source--display-apply'.  Caller is responsible for any
post-cycle frontend sync (e.g. updating `helm-pattern' under helm)."
  (let* ((from (fzfa-source-display-state source))
         (to   (fzfa--display-next-state from)))
    (cond
     ((and (eq from 'hidden) (eq to 'compact))
      (setf (fzfa-source-separator-overlays source)
            (fzfa--display-materialize
             (fzfa-source-command source) initial-char)))
     ((eq to 'hidden)
      (setf (fzfa-source-command source)
            (fzfa--display-extract
             (fzfa-source-separator-overlays source)))
      (setf (fzfa-source-separator-overlays source) nil)))
    (setf (fzfa-source-display-state source) to)
    (fzfa-source--display-apply source initial-char)))

(defvar fzfa-source-spawn-transform-function nil
  "Function called with (CMD DIR) before each source shell-handle spawn.

Return a cons (NEW-CMD . NEW-DIR) to rewrite the pair, or nil to
skip the rewrite.  Used by `fzfa-tramp' to make sources spawn
transparently against TRAMP paths.  Local sources should leave
this nil so the spawn hot path stays a single nil test.")

(defun fzfa--spawn (cmd dir)
  "Start a `fzf-native' async handle for CMD in DIR.

Applies `fzfa-source-spawn-transform-function' first so extensions
can rewrite CMD/DIR before the shell fork."
  (let ((pair (or (and fzfa-source-spawn-transform-function
                       (funcall fzfa-source-spawn-transform-function
                                cmd dir))
                  (cons cmd dir))))
    (fzf-native-async-start (car pair) (cdr pair))))

(defun fzfa-source--restart (source new-cmd refresh-fn)
  "Restart SOURCE's producer with NEW-CMD.

For producer-fn sources (CANDS-FN non-nil), re-fires the producer
with NEW-CMD and discards stale callbacks via a per-source
incrementing token.  For shell command sources, stops the existing
fzf-native handle and starts a new one against SOURCE's DIRECTORY.

REFRESH-FN is a 0-arg function called after the restart (or after
the producer callback arrives) to push the new state to the
frontend — typically a closure over the session's
`fzfa--frontend-push' machinery.  Pass `#'ignore' if no refresh
is needed.

Updates CURRENT-CMD, LAST-GEN, and LAST-RESTART-TIME unconditionally."
  (cond
   ((fzfa-source-cands-fn source)
    (let ((token (cl-incf (fzfa-source-prod-token source))))
      (setf (fzfa-source-current-cmd source) new-cmd
            (fzfa-source-last-gen source) -1
            (fzfa-source-last-restart-time source) (float-time))
      (funcall (fzfa-source-cands-fn source) (or new-cmd "")
               (lambda (cands)
                 (when (= token (fzfa-source-prod-token source))
                   (let ((snap (or cands '())))
                     (setf (fzfa-source-snapshot source) snap
                           (fzfa-source-total source) (length snap)
                           (fzfa-source-filtered source) (length snap)
                           (fzfa-source-last-result source) snap))
                   (funcall refresh-fn))))))
   (t
    (when-let* ((old (fzfa-source-handle source)))
      (fzfa--defer-async-stop old)
      (setf (fzfa-source-handle source) nil))
    (setf (fzfa-source-current-cmd source) new-cmd
          (fzfa-source-last-gen source) -1
          (fzfa-source-filtered source) 0
          (fzfa-source-total source) 0
          (fzfa-source-last-restart-time source) (float-time))
    (when (and new-cmd (not (string-empty-p new-cmd)))
      (setf (fzfa-source-handle source)
            (fzfa--spawn new-cmd (fzfa-source-directory source))))
    (funcall refresh-fn))))

(defun fzfa-source--debounce-restart (source new-cmd refresh-fn)
  "Schedule a debounced restart of SOURCE's shell handle with NEW-CMD.

Cancels any pending `restart-timer' on SOURCE and queues a fresh
one.  Delay floors at `fzfa-shell-command-debounce' and respects
`fzfa-shell-command-throttle' since the source's last restart, so
a burst of keystrokes ends with exactly one spawn on the final
cmd.  When the timer fires it clears the slot and calls
`fzfa-source--restart' with REFRESH-FN."
  (when-let* ((tm (fzfa-source-restart-timer source)))
    (cancel-timer tm)
    (setf (fzfa-source-restart-timer source) nil))
  (let* ((elapsed (- (float-time)
                     (fzfa-source-last-restart-time source)))
         (delay (max fzfa-shell-command-debounce
                     (- fzfa-shell-command-throttle elapsed))))
    (setf (fzfa-source-restart-timer source)
          (run-with-timer
           (max 0.01 delay) nil
           (lambda ()
             (setf (fzfa-source-restart-timer source) nil)
             (fzfa-source--restart source new-cmd refresh-fn))))))

(defun fzfa-source--stop (source)
  "Tear down SOURCE: cancel timers, delete overlays, stop handle.

Idempotent.  Producer-fn token is also bumped so any in-flight
callback discards on arrival."
  (when-let* ((tm (fzfa-source-restart-timer source)))
    (cancel-timer tm)
    (setf (fzfa-source-restart-timer source) nil))
  (when-let* ((tm (fzfa-source-retry-timer source)))
    (cancel-timer tm)
    (setf (fzfa-source-retry-timer source) nil))
  (mapc #'delete-overlay (fzfa-source-separator-overlays source))
  (mapc #'delete-overlay (fzfa-source-display-overlays source))
  (setf (fzfa-source-separator-overlays source) nil
        (fzfa-source-display-overlays source) nil)
  (when-let* ((h (fzfa-source-handle source)))
    (fzfa--defer-async-stop h)
    (setf (fzfa-source-handle source) nil))
  (when (fzfa-source-cands-fn source)
    (cl-incf (fzfa-source-prod-token source))))

;;;###autoload
(cl-defun fzfa-completing-read (&key
                                prompt
                                command
                                candidates
                                (directory (fzfa--default-dir))
                                (category 'auto)
                                annotate
                                affix
                                group
                                history
                                (require-match 'auto)
                                default
                                initial-input
                                (resolve-paths 'auto)
                                (display 'hidden)
                                skip-executable-check
                                preview
                                apply)
  "Run `completing-read' with fzf-native scoring over COMMAND or CANDIDATES.

Exactly one of :COMMAND or :CANDIDATES selects the source kind:

  :COMMAND     Shell command whose stdout lines become candidates.
  :CANDIDATES  Static list, zero-arg function returning a list, or a
               2-arg producer fn `(lambda (INPUT CALLBACK) ...)' that
               populates a snapshot per-keystroke.  Lists and zero-arg
               fns are auto-wrapped into constant producers.

The minibuffer input is split into a CMD part and a FILTER part on
`fzfa-separator': the buffer text has shape \"<sep>CMD<sep>FILTER\".
Changing CMD restarts the subprocess (for COMMAND sources) or refires
the producer (for CANDIDATES sources); changing FILTER rescores in
place via fzf-native against the current snapshot.

:DISPLAY controls how much of the CMD region is visible.  Press
`fzfa-display-key' (default \">\") to cycle:
  hidden  — entire \"<sep>CMD<sep>\" prefix is invisible; only FILTER
            appears editable.  Default for shell COMMAND sources.
  compact — flags within CMD collapse behind ` ... '; program name
            and the trailing argument slot remain visible.
  full    — whole \"<sep>CMD<sep>FILTER\" string is shown verbatim.

:PROMPT                 Minibuffer prompt.  Derived from the first token of
                        COMMAND (e.g. \"find: \" for \"find .\") when omitted;
                        falls back to \"fzf > \" for CANDIDATES sources.
:COMMAND                Shell command whose stdout lines become candidates.
                        Pre-inserted into the minibuffer as
                        \"<sep>COMMAND<sep>\".  Mutually exclusive with
                        :CANDIDATES.
:CANDIDATES             List, zero-arg fn, or 2-arg producer (see above).
                        Mutually exclusive with :COMMAND.
:DIRECTORY              Working directory for COMMAND.  Defaults to
                        `fzfa--default-dir' (respects
                        `fzfa-project-backend').
:CATEGORY               Completion category symbol.  Defaults to
                        `fzfa-file' for :COMMAND, `fzfa-misc' for
                        :CANDIDATES.  Common explicit values: `fzfa-grep'
                        for FILE:LINE:CONTENT, `fzfa-buffer' for
                        `buffer-name' pickers, `fzfa-bookmark' for
                        bookmarks, etc.
:ANNOTATE               Optional (CANDIDATE) -> string function exposed as
                        `annotation-function' completion metadata.
                        Annotations render immediately after each candidate.
:AFFIX                  Optional (CANDIDATES) -> ((CAND PREFIX SUFFIX) ...)
                        function exposed as `affixation-function'.  Prefer
                        this over :ANNOTATE when column alignment matters.
:GROUP                  Optional (CANDIDATE TRANSFORM) -> string function
                        exposed as `group-function' completion metadata.
                        Renders section headers in vertico/icomplete.
:HISTORY                Optional history variable symbol passed to
                        `completing-read'.  Selected entries are pushed onto
                        this list; under ivy the empty-FILTER candidate
                        ordering also reflects this history's recency.
:REQUIRE-MATCH          Forwarded to `completing-read'.  Defaults to nil
                        for :COMMAND (free-form input — `fzfa-find'
                        accepts a new path), t for :CANDIDATES (must
                        pick a listed entry).  Pass an explicit nil or
                        t to override.
:DEFAULT                Forwarded to `completing-read'.  Returned when the
                        user submits empty input; also seeded into history.
:INITIAL-INPUT          Optional initial minibuffer text overriding the
                        auto-built \"<sep>COMMAND<sep>\".  Either a string
                        or (TEXT . POSITION) cons with 0-based cursor
                        offset.
:RESOLVE-PATHS          When non-nil, the returned candidate is passed
                        through `expand-file-name' against :DIRECTORY
                        before returning.  Defaults to t for :COMMAND
                        sources, nil for :CANDIDATES.  Pass an explicit
                        t or nil to override.
:DISPLAY                Initial display mode (`hidden', `compact', or
                        `full'); see above.
:SKIP-EXECUTABLE-CHECK  When non-nil, skip the `executable-find' guard
                        on the first token of COMMAND.  No-op for
                        :CANDIDATES sources (no command to check).
:PREVIEW                Per-call live-preview handler that bypasses the
                        `fzfa-preview-functions' registry lookup for
                        CATEGORY.  Pass a (CAND) -> any function for a
                        simple preview-only handler, or a full handler
                        plist with `:setup', `:preview', `:exit', `:return'
                        slots.  Nil falls back to the registry.  Set both
                        `fzfa-preview-delay' and `fzfa-preview-key' to
                        nil to disable previews.
:APPLY                  Lambda (CAND) -> any.  Runs without exiting the
                        session.  Invoked by `fzfa-apply-key' (under
                        vertico / icomplete), `ivy-call' (ivy), or
                        `helm-execute-persistent-action' (helm).  When
                        omitted, falls back to the category default in
                        `fzfa-apply-functions'.

The prompt overlay shows: DIR IDX/[FILTERED](TOTAL)
  DIR      — abbreviated working directory (omitted for CANDIDATES sources)
  IDX      — current selection index (omitted for frontends without one)
  FILTERED — candidates matching the current query
  TOTAL    — total candidates collected so far"
  (fzfa--ensure-setup)
  (when (and command candidates)
    (user-error
     "fzfa-completing-read: :command and :candidates are mutually exclusive"))
  ;; Smart defaults keyed on source kind.  :command sources behave like
  ;; the old async path (file-category, free-form input, path
  ;; resolution); :candidates sources behave like the old sync path
  ;; (misc category, require-match, no path resolution).  Callers
  ;; override by passing an explicit value.
  (when (eq resolve-paths 'auto)
    (setq resolve-paths (and command t)))
  (when (eq category 'auto)
    (setq category (if command 'fzfa-file 'fzfa-misc)))
  (when (eq require-match 'auto)
    (setq require-match (and candidates t)))
  (fzfa--ensure-category-override category)
  (when-let* ((slot-dir (plist-get
                         (fzfa--resolve-action-slot category current-prefix-arg)
                         :directory)))
    ;; Restore `this-command' / `real-this-command' after the funcall
    ;; — an interactive `:directory' like `read-directory-name' enters
    ;; a recursive minibuffer that leaves both set to `exit-minibuffer',
    ;; which would then be captured as `fzfa--read''s `entry-command'
    ;; (polluting the replay ring) and mislead `M-x repeat'.
    (let ((entry-cmd  this-command)
          (real-entry real-this-command)
          (d (funcall slot-dir)))
      (setq this-command      entry-cmd
            real-this-command real-entry)
      (when d (setq directory d))))
  (unless (or skip-executable-check candidates)
    (when-let* ((prog (and command (car (split-string command nil t)))))
      (unless (executable-find prog)
        (user-error "%s not found in exec-path" prog))))
  (let ((prompt (or prompt
                    (when command
                      (concat (car (split-string command nil t)) ": "))
                    ;; Match former sync default when no command/explicit
                    ;; prompt — used by list-shape callers and any fn-form
                    ;; candidates session that didn't set its own prompt.
                    (when candidates "fzf > "))))
    (cond
     ((eq fzfa--multi-mode :extract)
      (throw 'fzfa-extracted
             (list :prompt prompt :command command :candidates candidates
                   :directory directory :category category
                   :annotate annotate :affix affix :group group
                   :history history :require-match require-match
                   :default default
                   :initial-input initial-input
                   :resolve-paths resolve-paths
                   :display display
                   :preview preview
                   :apply apply)))
     ((eq (car-safe fzfa--multi-mode) :inject)
      ;; One-shot consume: mutate the outer action's `let' cell so the
      ;; rest of the caller's body (and any nested fzfa calls) run with
      ;; multi-mode = nil instead of replaying our inject value.
      (let ((cand (cdr fzfa--multi-mode)))
        (setq fzfa--multi-mode nil)
        (cl-return-from fzfa-completing-read
          (fzfa--maybe-expand cand directory resolve-paths)))))
    ;; Helm dispatch receives the RAW candidates value (list / zero-arg
    ;; fn / 2-arg fn).  Helm's own `fzfa-helm-make-sync-source' has
    ;; polymorphism that does the right thing per kind — in particular,
    ;; static-list candidates skip the #CMD#FILTER split so the user's
    ;; typing filters the snapshot rather than going to a no-op CMD
    ;; slot.  Normalization happens AFTER for the non-helm path, which
    ;; consumes the uniform producer shape.
    (when (and (bound-and-true-p helm-mode)
               (fboundp 'fzfa-helm--completing-read))
      (cl-return-from fzfa-completing-read
        (fzfa--maybe-expand
         (fzfa-helm--completing-read
          :prompt prompt :command command :candidates candidates
          :directory directory :category category
          :annotate annotate :affix affix :group group
          :history history :require-match require-match
          :default default :preview preview :apply apply
          :display display
          :skip-executable-check skip-executable-check)
         directory resolve-paths)))
    ;; Non-helm path: build a 1-source plist and dispatch through
    ;; `fzfa--read'.  N=1 fast paths inside `fzfa--read'
    ;; restore the legacy single-source UX (no narrow menu, no tofu
    ;; suffix on candidates, source-direct metadata, post-action
    ;; `fzfa--maybe-expand') while sharing the multi-source plumbing.
    (let ((completion-styles '(fzfa)))
      (fzfa--read
       (list (list :name "fzfa"
                   :prompt prompt
                   :command command
                   :candidates candidates
                   :directory directory
                   :category category
                   :annotate annotate
                   :affix affix
                   :group group
                   :history history
                   :require-match require-match
                   :default default
                   :initial-input initial-input
                   :resolve-paths resolve-paths
                   :display display
                   :preview preview
                   :apply apply
                   :action #'identity))
       :prompt prompt))))

(defun fzfa--extract-args (cmd)
  "Run CMD in `:extract' mode and return its keyword args plist.

Returns nil if CMD does not flow through a fzfa `completing-read'."
  (catch 'fzfa-extracted
    (let ((fzfa--multi-mode :extract))
      (funcall cmd))
    nil))

;;; Smart dispatch

(defun fzfa--smart-resolve (clauses)
  "Return the first CMD in CLAUSES whose conditions are satisfied, else nil.

Each CLAUSE is (CMD &key executable predicate).  A clause matches when
CMD is `fboundp', its `:executable' (if any) is on PATH, and its
`:predicate' (if any) returns non-nil.  Clauses without either keyword
are unconditional fallbacks."
  (catch 'fzfa-smart-found
    (dolist (clause clauses)
      (let* ((cmd  (car clause))
             (rest (cdr clause))
             (exe  (plist-get rest :executable))
             (pred (plist-get rest :predicate)))
        (when (and (fboundp cmd)
                   (or (null exe)  (executable-find exe))
                   (or (null pred) (funcall pred)))
          (throw 'fzfa-smart-found cmd))))
    nil))

(defun fzfa-smart-define (name clauses)
  "Define `fzfa-smart-NAME' that dispatches to the first available CLAUSE.

NAME is a symbol naming the command group (e.g. `find', `grep'); the
generated command is interned as `fzfa-smart-NAME'.

CLAUSES is a list of (CMD &key executable predicate) entries.  Each
clause is tried in order; the first whose `:executable' (if supplied)
is on PATH and whose `:predicate' (if supplied) returns non-nil wins,
provided CMD is `fboundp'.  A clause with neither key is an
unconditional fallback.

The generated command is interactive and calls the chosen CMD via
`funcall', so the active `fzfa--multi-mode' (if any) propagates and
the smart command transparently participates in multi-source dispatch
\(`fzfa-multi-read') without needing any special-casing.

Resolution runs on every invocation — no caching — so PATH changes,
TRAMP buffers, and containerized shells are handled naturally.

Returns the new symbol."
  (let ((fname (intern (format "fzfa-smart-%s" name))))
    (defalias fname
      (lambda ()
        (interactive)
        (let ((cmd (fzfa--smart-resolve clauses)))
          (unless cmd
            (user-error "`%s': no available backend among %s"
                        fname
                        (mapconcat (lambda (c) (symbol-name (car c)))
                                   clauses ", ")))
          (funcall cmd)))
      (format "Smart `%s' dispatcher.
Tries backends in order and invokes the first whose executable is on
PATH and whose command symbol is bound: %s."
              name
              (mapconcat (lambda (c) (format "`%s'" (car c))) clauses ", ")))
    fname))

(fzfa-smart-define
 'find
 '((fzfa-fd       :executable "fd")
   (fzfa-rg-files :executable "rg")
   (fzfa-ag-files :executable "ag")
   (fzfa-find     :executable "find"))) ;; -> `fzfa-smart-find'

(fzfa-smart-define
 'grep
 '((fzfa-ugrep :executable "ugrep")
   (fzfa-rg    :executable "rg")
   (fzfa-ag    :executable "ag")
   (fzfa-grep  :executable "grep"))) ;; -> `fzfa-smart-grep'

;;; Sync `completing-read'

;;; Multi-source `completing-read'

(defcustom fzfa-multi-narrow-key "<"
  "Key string that activates source narrowing in `fzfa-multi-read'.

Press this key in a multi-source minibuffer, then a source's narrow
character, to restrict candidates to that source.  Re-pressing the
same combination widens back to all sources.  Set to nil to disable
narrowing entirely."
  :type '(choice (const :tag "Disabled" nil)
                 (string :tag "Key string (passed to `kbd')"))
  :group 'fzfa)

(defvar fzfa--active-sources nil
  "Source vector bound across the active `fzfa--read' session.

Read by frontends that need per-source rendering — currently the
ivy display transformer in `fzfa-ivy.el', which decodes each
candidate's tofu suffix into its source name.")

(defvar fzfa--candidate->source nil
  "Candidate→source-idx hash bound across the active `fzfa--read' session.

Used by `fzfa--multi-source-of' / `fzfa--multi-source-idx' for dispatch
when callers don't have direct access to the session-local hash table.")

(defun fzfa--tofu-suffix (idx)
  "Return the cached invisible tofu suffix string for source IDX."
  (or (gethash idx fzfa--tofu-cache)
      (puthash idx
               (propertize (string (+ fzfa--tofu-base idx))
                           'invisible t 'display "")
               fzfa--tofu-cache)))

(defun fzfa--tofu-hide (s)
  "Return S without its trailing tofu codepoint, if any.

Returns S unchanged when there is no tofu suffix."
  (if (and (stringp s)
           (let ((n (length s)))
             (and (> n 0)
                  (>= (aref s (1- n)) fzfa--tofu-base))))
      (substring s 0 (1- (length s)))
    s))

(defun fzfa--tag (cand idx hash &optional multi-p source-action)
  "Return CAND associated with source IDX in HASH.

When MULTI-P is non-nil (the cross-source case), appends an
invisible tofu suffix so cross-source duplicates remain
`string='-distinct and records the *tagged* string in HASH.
Idempotent: strips any pre-existing tofu suffix from CAND first,
so calling on an already-tagged string yields the same content
\(safe to re-tag producer-path output after `fzf-native-score-all'
preserves the snapshot's tofu suffix).

SOURCE-ACTION, when non-nil (multi-p case only), is stamped on
the tofu char as an `fzfa-multi-action' text property.  Read by
`fzfa--multi-default-action' so `embark-collect' (and any other
post-session dispatcher) routes each candidate to its source's
own action instead of falling back to the entry command — which
would just re-open the picker.  Session-scoped dispatch state
\(`fzfa--candidate->source', `fzfa--multi-build-router') dies with
the minibuffer; the property survives anywhere the candidate
string goes.

When MULTI-P is nil (N=1 fast path), no suffix is appended — CAND
is hashed and returned verbatim, skipping the per-candidate
`propertize'+`concat' cost.  At N=1 there's no cross-source
ambiguity to disambiguate."
  (cond
   (multi-p
    (let* ((clean (fzfa--tofu-hide cand))
           (tagged (concat clean (fzfa--tofu-suffix idx))))
      (puthash tagged idx hash)
      (when source-action
        (add-text-properties (1- (length tagged)) (length tagged)
                             `(fzfa-multi-action ,source-action)
                             tagged))
      tagged))
   (t
    (puthash cand idx hash)
    cand)))

(defun fzfa--multi-source-of (cand sources-v hash)
  "Return the source plist responsible for CAND, or nil.

SOURCES-V is the vector of source plists.  HASH maps CAND to source index;
when nil, falls back to the session-bound `fzfa--candidate->source' dynvar."
  (and (stringp cand) (> (length cand) 0)
       (when-let* ((tbl (or hash fzfa--candidate->source))
                   (idx (gethash cand tbl)))
         (aref sources-v idx))))

(defun fzfa--multi-source-idx (cand hash)
  "Return the source index for CAND, or nil.

HASH maps CAND to source index; when nil, falls back to the session-bound
`fzfa--candidate->source' dynvar."
  (and (stringp cand) (> (length cand) 0)
       (when-let* ((tbl (or hash fzfa--candidate->source)))
         (gethash cand tbl))))

(defun fzfa--multi-build-router (sources-v candidate->source)
  "Build a synthetic preview handler that dispatches per source.

SOURCES-V is the vector of source plists; CAND->SRC is the
candidate→source-idx hash table.  Returns nil when no source has a
registered handler (preview wiring then no-ops); otherwise returns a
plist with `:setup' / `:preview' / `:exit' / `:return' slots plus a
`:multi-cells' slot exposing the per-source session cell vector —
callers stash this in the outer preview session so per-candidate
dispatch outside the router can reach the right source's `:opener'.

CANDIDATE->SOURCE is the source of the candidate.

For each source, a fresh handler plist is resolved via
`fzfa--preview-handler' using the source's own `:preview' override
and `:category'.  Per-source state is stored in its own session
cell, so an `:opener' stashed by one source's `:setup' never
collides with another's.


Lifecycle:
  :setup    Broadcast to every source; propagates origin
            window/buffer/`default-directory' from the parent
            session into each per-source cell first.
  :preview  Routes to the source of CAND only.
  :exit     Broadcast to every source.
  :return   Broadcast: the source containing CAND receives CAND,
            every other source receives nil (\"aborted from its
            perspective\")."
  (let* ((n (length sources-v))
         (cells (make-vector n nil))
         (any nil))
    (dotimes (i n)
      (let* ((src (aref sources-v i))
             (handler (fzfa--preview-handler
                       (plist-get src :preview)
                       (plist-get src :category))))
        (when handler
          (aset cells i (cons handler nil))
          (setq any t))))
    (when any
      (cl-flet ((broadcast (action session &optional cand cand-i)
                  (dotimes (i n)
                    (when-let* ((cell (aref cells i)))
                      (let ((fzfa--preview-session cell))
                        (fzfa--preview-call
                         action session
                         (when (and cand (eql i cand-i))
                           (fzfa--tofu-hide cand))))))))
        (list
         :setup
         (lambda (session)
           (let ((win (fzfa-preview-get :origin-window))
                 (buf (fzfa-preview-get :origin-buffer))
                 (dir (fzfa-preview-get :default-directory)))
             (dotimes (i n)
               (when-let* ((cell (aref cells i)))
                 ;; Each source's cell gets its OWN :default-directory
                 ;; — the search root the source's command ran under.
                 ;; Handlers expand relative candidates against it.
                 (let* ((src (aref sources-v i))
                        (src-dir (or (plist-get src :directory) dir))
                        (fzfa--preview-session cell))
                   (fzfa-preview-put :origin-window    win)
                   (fzfa-preview-put :origin-buffer    buf)
                   (fzfa-preview-put :default-directory src-dir)
                   (fzfa--preview-call :setup session))))))
         :preview
         (lambda (cand session)
           (if-let* ((i (and cand
                             (fzfa--multi-source-idx cand candidate->source)))
                     (cell (aref cells i)))
               (let ((fzfa--preview-session cell))
                 (fzfa--preview-call :preview session (fzfa--tofu-hide cand)))
             ;; cand=nil (reset) — broadcast.
             (unless cand (broadcast :preview session nil nil))))
         :exit  (lambda (session) (broadcast :exit session))
         :return
         (lambda (cand session)
           (let ((i (and cand (fzfa--multi-source-idx cand candidate->source))))
             (broadcast :return session cand i)))
         :multi-cells cells)))))

(defun fzfa--multi-rank (results query async-p)
  "Top fzf score for RESULTS under QUERY.

For async sources (ASYNC-P non-nil) the C async path does not attach
`completion-score' text properties, so we re-score the top candidate
once via `fzf-native-score'.  For sync sources the score is read off
the property set by `fzf-native-score-all'.  Returns 0 on empty input."
  (cond
   ((or (null results) (string-empty-p query)) 0)
   (async-p
    (or (car (fzfa--bridge-defcustoms
              #'fzf-native-score (car results) query))
        0))
   (t (or (get-text-property 0 'completion-score (car results)) 0))))

(defun fzfa--source-fetch (source query &optional refresh-fn on-deliver)
  "Refetch SOURCE's producer for QUERY iff QUERY changed.

Protocol-only: updates SOURCE's `prod-input', `prod-token',
`snapshot', `total' slots.  Discards stale callbacks via the
`prod-token' check (a newer fetch bumps the token; the old
callback's `(= my-token …)' test fails and it no-ops).

ON-DELIVER, when non-nil, is `(lambda (CANDS) → VALUE)' invoked
inside the callback with the producer's output before snapshot
write.  Returns the value to store as the snapshot.  Used by
`fzfa--multi-candidates-fetch' to stamp each candidate with the
source→idx mapping via `fzfa--tag'; nil callers (helm) take the
producer output verbatim.

REFRESH-FN, when non-nil, is a 0-arg closure that fires the
frontend's redraw.  Sync-vs-async detection: a local SYNC-CALL
flag is t during the funcall, flipped to nil afterwards.  The
callback consults it — sync producers don't schedule a refresh
\(the caller's pass already sees the new snapshot); async
producers (callback fires post-funcall) schedule
`(run-with-idle-timer 0 nil refresh-fn)' so the frontend
re-renders.

Mirrors `fzfa-source--restart''s callback-driven refresh pattern
\(restart unconditionally fires REFRESH-FN because its caller
never reads the snapshot in the same tick; fetch only fires when
async).

Returns non-nil iff a fetch was actually issued."
  (unless (equal query (fzfa-source-prod-input source))
    (setf (fzfa-source-prod-input source) query)
    (let ((my-token (cl-incf (fzfa-source-prod-token source)))
          (sync-call t))
      (funcall (fzfa-source-cands-fn source) (or query "")
               (lambda (cands)
                 (when (= my-token (fzfa-source-prod-token source))
                   (let* ((delivered (or cands '()))
                          (value (if on-deliver
                                     (funcall on-deliver delivered)
                                   delivered)))
                     (setf (fzfa-source-snapshot source) value
                           (fzfa-source-total source) (length value))
                     (when (and (not sync-call) refresh-fn)
                       (run-with-idle-timer 0 nil refresh-fn))))))
      (setq sync-call nil))
    t))

(defun fzfa--multi-candidates-fetch (source idx query candidate->source
                                            &optional multi-p refresh-fn)
  "Tagging wrapper over `fzfa--source-fetch' for the multi path.

IDX is the source's index in the multi-read session — used by
`fzfa--tag' to stamp the candidate→source mapping.
CANDIDATE->SOURCE is the candidate→source-idx hash table.  MULTI-P
is forwarded to `fzfa--tag' — non-nil applies the tofu suffix
\(cross-source case), nil keeps candidates verbatim (N=1).

See `fzfa--source-fetch' for the underlying producer protocol +
REFRESH-FN semantics.  Helm callers skip this wrapper — they
have no candidate→source hash and don't need the tofu tagging,
so they call `fzfa--source-fetch' directly with their own
refresh closure (helm-force-update guarded by helm-alive-p).

Returns non-nil iff a fetch was actually issued."
  (let ((action (plist-get (fzfa-source-spec source) :action)))
    (fzfa--source-fetch
     source query refresh-fn
     (lambda (cands)
       (mapcar (lambda (s)
                 (fzfa--tag s idx candidate->source multi-p action))
               cands)))))

(defun fzfa--multi-poll-bumped-p (sources-v)
  "Non-nil iff any source in SOURCES-V has a fresh generation.

Walks each source's CURRENT handle (not a frozen snapshot — so
post-restart handles get polled) and compares the live generation
to the source's `last-gen' slot.  Does not commit; pair with
`fzfa--multi-poll-commit' inside the same poll tick to keep the
detect/push throttle correct (commit only when actually pushing,
otherwise the bump signal is silently dropped)."
  (cl-loop for src across sources-v
           thereis (when-let* ((h (fzfa-source-handle src))
                               (g (fzf-native-async-generation h)))
                     (/= g (fzfa-source-last-gen src)))))

(defun fzfa--multi-poll-commit (sources-v)
  "Commit each SOURCES-V source's current generation to its `last-gen' slot.

Call after `fzfa--multi-poll-bumped-p' returned non-nil and you've
decided to fire the frontend push, so the next tick starts from
the published baseline."
  (cl-loop for src across sources-v do
           (when-let* ((h (fzfa-source-handle src))
                       (g (fzf-native-async-generation h)))
             (setf (fzfa-source-last-gen src) g))))

(defun fzfa--make-poll-fn (sources-v alive-p refresh-fn first-shown-p)
  "Return a 0-arg poll closure for SOURCES-V.

ALIVE-P / REFRESH-FN / FIRST-SHOWN-P are 0-arg functions.  Each tick
the closure checks ALIVE-P, `fzfa--multi-poll-bumped-p', and
`input-pending-p', then a `fzfa-input-throttle' gate that is
bypassed while FIRST-SHOWN-P returns nil — so cold sessions paint
as soon as the producer + scoring delivers, without sitting through
a throttle window on the empty buffer.  When all checks pass, commit
the generations and call REFRESH-FN."
  (let ((last-exhibit 0.0))
    (lambda ()
      (when (and (funcall alive-p)
                 (fzfa--multi-poll-bumped-p sources-v)
                 (not (input-pending-p))
                 (or (not (funcall first-shown-p))
                     (>= (- (float-time) last-exhibit)
                         fzfa-input-throttle)))
        (fzfa--multi-poll-commit sources-v)
        (setq last-exhibit (float-time))
        (funcall refresh-fn)))))

(defun fzfa--multi-narrow->string (k)
  "Coerce narrow key value K (symbol, character, or string) to length-1 string."
  (let ((s (cond
            ((stringp k) k)
            ((characterp k) (string k))
            ((symbolp k) (symbol-name k))
            (t (error "Bad fzfa narrow key %S" k)))))
    (cl-assert (= (length s) 1) nil
               "fzfa narrow key must be a single character, got %S" k)
    s))

(defun fzfa--multi-derive-narrow-key (name used)
  "Return a free single-character narrow key for source NAME.

USED is a hash table of already-allocated length-1 strings.  Tries
each word's first character (case preserved) from NAME split on
\"-\"; falls back to a-z / A-Z / 0-9.  Errors when the pool is
exhausted."
  (or
   (cl-loop for word in (split-string name "-" t)
            for s = (substring word 0 1)
            unless (gethash s used)
            return s)
   (cl-loop for c across (concat "abcdefghijklmnopqrstuvwxyz"
                                 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                 "0123456789")
            for s = (string c)
            unless (gethash s used)
            return s)
   (error "Fzfa narrow key pool exhausted")))

(defun fzfa--format-narrow-hint (sources-v narrow-idx
                                           &optional width prefix-key)
  "Format the narrow-menu hint.

SOURCES-V is the source vector; NARROW-IDX is the active narrow
index or nil.  Shows every source as `KEY:NAME', separated by two
spaces, wrapping between entries when a line would exceed WIDTH
\(defaults to the active minibuffer body width, or 200 in batch).
When NARROW-IDX is non-nil, that source is highlighted with the
`minibuffer-prompt' face.  When PREFIX-KEY is non-nil, a trailing
`PREFIX-KEY:widen' marker (faced `shadow') is appended to
indicate the prefix widens.  Multi-line output relies on
`resize-mini-windows' (the Emacs default) to grow the minibuffer."
  (let* ((entries
          (append
           (mapcar
            (lambda (i)
              (let* ((src (aref sources-v i))
                     (k (or (plist-get src :narrow) "?"))
                     (n (or (plist-get src :name) ""))
                     (s (format "%s:%s" k n)))
                (if (eql i narrow-idx)
                    (propertize s 'face 'minibuffer-prompt)
                  s)))
            (number-sequence 0 (1- (length sources-v))))
           (when prefix-key
             (list (propertize (format "%s:widen" prefix-key)
                               'face 'shadow)))))
         (width (or width
                    (max 20 (1- (or (when-let*
                                        ((w (active-minibuffer-window)))
                                      (window-body-width w))
                                    200)))))
         (sep "  ")
         (sep-w 2)
         lines cur (cur-w 0))
    (dolist (e entries)
      (let ((ew (string-width e)))
        (cond
         ((null cur)
          (setq cur (list e) cur-w ew))
         ((<= (+ cur-w sep-w ew) width)
          (push e cur)
          (setq cur-w (+ cur-w sep-w ew)))
         (t
          (push (mapconcat #'identity (nreverse cur) sep) lines)
          (setq cur (list e) cur-w ew)))))
    (when cur
      (push (mapconcat #'identity (nreverse cur) sep) lines))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun fzfa--multi-allocate-narrow-keys (sources)
  "Return SOURCES with each plist augmented with a length-1 :narrow key.

Two passes: explicit :narrow values are coerced and reserved first
\(duplicates signal an error); remaining sources derive keys from
their :name via `fzfa--multi-derive-narrow-key'."
  (let ((used (make-hash-table :test 'equal))
        (with-explicit (make-vector (length sources) nil))
        (sources-v (vconcat sources))
        result)
    (dotimes (i (length sources-v))
      (let* ((src (aref sources-v i))
             (k (plist-get src :narrow)))
        (when k
          (let ((s (fzfa--multi-narrow->string k)))
            (when (gethash s used)
              (error "Duplicate fzfa narrow key %S for source %S"
                     s (plist-get src :name)))
            (puthash s t used)
            (aset with-explicit i s)))))
    (dotimes (i (length sources-v))
      (let* ((src (aref sources-v i))
             (s (or (aref with-explicit i)
                    (let ((d (fzfa--multi-derive-narrow-key
                              (or (plist-get src :name) "") used)))
                      (puthash d t used)
                      d))))
        (push (plist-put (copy-sequence src) :narrow s) result)))
    (nreverse result)))

(cl-defun fzfa--read (sources &key (prompt "fzf-multi: ") narrow-idx)
  "Run `completing-read' across multiple SOURCES, fzfa style.

PROMPT is shown in the minibuffer.

NARROW-IDX, when non-nil, seeds the active narrow at session start
to that source index — used by `fzfa-replay' to replay a session
that exited with a source narrowed.  Nil restores the default
\(N=1 always narrows to 0, N>1 starts widened).

Internal — users should call `fzfa-multi-read' which derives sources
from existing single-source commands.  This function takes pre-built
source plists directly.

SOURCES is a list of plists.  Each source contributes a labeled group of
candidates; group order is recomputed on every keystroke from each
group's top fzf score, so the strongest-matching group floats to the
top.  Within a group, candidates stay in fzf order.  An empty query
falls back to declared source order.

Per-source plist keys:
  :name        Group header (required).
  :command     Shell command string.  Mutually exclusive with
               :candidates.
  :candidates  List of strings, a zero-arg fn returning one, or a
               2-arg `(lambda (INPUT CALLBACK) ...)' producer.  Lists
               and zero-arg fns are wrapped as constant producers via
               `fzfa--normalize-candidates'.  Mutually exclusive with
               :command.
  :directory   Working directory for :command (default
               `default-directory').
  :annotate    Optional (CAND) -> string annotation function.
  :action      Optional (CAND) -> any.  Called with the selection.
               When omitted, the raw selection string is returned.
  :history     Optional history variable symbol.  When set, the cleaned
               selection (tofu suffix stripped) is pushed via
               `add-to-history' on exit — mirroring the HIST push the
               source's own `completing-read' would have done.  On
               empty input, the source's candidate slot is additionally
               reordered by this history so recent picks surface first.
               Static-list sources extracted from existing `fzfa-*'
               commands inherit this from each source's
               `fzfa-completing-read' :history argument."
  (cond
   ;; Composability: when this multi is being extracted by an outer
   ;; `fzfa-multi-read', throw our merged SOURCES so the
   ;; outer can flatten them into its own source list.  Each source
   ;; already carries its own :action closure, so dispatch from the
   ;; outer multi routes back to the correct underlying command.
   ((eq fzfa--multi-mode :extract)
    (throw 'fzfa-extracted (list :multi-sources sources)))
   ((eq (car-safe fzfa--multi-mode) :inject)
    (let ((cand (cdr fzfa--multi-mode)))
      (setq fzfa--multi-mode nil)
      (cl-return-from fzfa--read cand))))
  (when (bound-and-true-p helm-mode)
    (if (fboundp 'fzfa-helm--read)
        (cl-return-from fzfa--read
          (fzfa-helm--read sources
                           :prompt prompt
                           :narrow-idx narrow-idx))
      (user-error "Fzfa--read does not yet support helm-mode")))
  (cl-assert (> (length sources) 0) nil
             "fzfa--read: SOURCES must contain at least one source")
  ;; Pin fzfa's style for every source's category so timer callbacks /
  ;; out-of-band completion calls (which don't inherit our
  ;; `let'-bound `completion-styles') still route through fzfa's
  ;; passthrough.  `fzfa-completing-read' also calls this, but
  ;; `fzfa-multi-read' and `fzfa-replay' reach `fzfa--read' directly.
  (dolist (spec sources)
    (fzfa--ensure-category-override (plist-get spec :category)))
  (fzfa--ensure-category-override 'fzfa-multi)
  ;; Bind here so every caller (including `fzfa-replay', which doesn't
  ;; go through the shim wrappers) routes candidates through fzfa's
  ;; passthrough style.  Without this, the user's default completion
  ;; styles (orderless, flex, etc.) re-filter our pre-scored
  ;; candidates against their own substring / fuzzy logic and silently
  ;; drop sources whose content doesn't match — that's how replay of
  ;; `fzfa-find-any' lost every source except the few whose pool
  ;; happened to substring-match the seeded filter.
  (let ((completion-styles '(fzfa)))
  (let* ((specs        sources)              ; cl-defun arg renamed
         (n            (length specs))
         (multi-p      (> n 1))              ; gates tofu tagging, narrow
                                             ; menu, source-name headers
         (specs-v      (vconcat specs))      ; plist vector (helper callsites)
         ;; Capture the user-facing entry command for session records.
         ;; `this-command' is bound by the dispatcher and stays stable
         ;; across `fzfa--read''s body — nested fzfa calls via
         ;; `:inject' mode flip it only inside their own execution.
         (entry-command this-command)
         ;; All per-source runtime state — handle, current-cmd, snapshot,
         ;; prod-token, prod-input, last-result, rank, total, filtered,
         ;; last-gen — lives on each struct.  Shared helpers
         ;; (`fzfa-source--restart', `fzfa--multi-candidates-fetch', etc.)
         ;; mutate slots via setf; this loop replaces the 10 parallel
         ;; vectors with one.
         (sources      (vconcat
                        (mapcar (lambda (spec)
                                  (fzfa-make-source :spec spec))
                                specs)))
         (limit        (fzfa--candidate-limit))
         (candidate->source    (make-hash-table :test 'equal :size 1024))
         ;; Flipped to t the first time any source's per-tick computation
         ;; produces a non-empty result.  The poll-timer gate skips
         ;; `fzfa-input-throttle' while this is nil so the empty
         ;; minibuffer doesn't sit through two throttle windows waiting
         ;; for the producer + scoring round-trip on the cold session.
         (first-cands-shown nil)
         ;; Session-level plist keys live on source 0 — the original
         ;; `fzfa-completing-read' contract is that these are
         ;; session-wide settings, and the shim that wraps it builds a
         ;; single source carrying them.  At N>1 (true multi) most
         ;; default sensibly to nil/t and the user picks them up via
         ;; `fzfa-multi-read''s own conventions.
         (s0           (aref specs-v 0))
         (require-match (if multi-p t
                          ;; Honor an explicit nil — `(or nil …)' would
                          ;; treat "unset" and "explicitly nil" the
                          ;; same and force `t' for `:candidates'
                          ;; sources, which breaks callers (e.g.
                          ;; `fzfa-browse-files') that need free-form
                          ;; input against a static list.
                          (if (plist-member s0 :require-match)
                              (plist-get s0 :require-match)
                            (and (plist-get s0 :candidates) t))))
         (default-val   (and (not multi-p) (plist-get s0 :default)))
         ;; `:initial-input' is read off the narrow-target spec (or
         ;; source 0 when widened / at N=1) regardless of multi-p so
         ;; `fzfa-replay' can replay the filter for multi sessions
         ;; too — that's where the captured filter lives.
         (initial-input
          (plist-get (aref specs-v (or narrow-idx 0)) :initial-input))
         ;; `:apply' is consumed via `fzfa--apply-resolve' which
         ;; reads it off the source plist through `candidate->source'
         ;; dispatch — no local binding needed.
         ;; Compute `init-text' + `init-point' from the source plist
         ;; (mirrors the legacy `fzfa-completing-read' setup).  At N=1
         ;; with a shell `:command' source whose `:display' starts
         ;; non-hidden, pre-seed `<sep>CMD<sep>' so the user can edit
         ;; CMD immediately.  Static `:candidates' sources in
         ;; compact/full get `<sep><sep>' (cursor lands inside an
         ;; empty CMD slot).  At N>1 these stay nil — `fzfa-multi-read'
         ;; starts widened with an empty buffer.
         (initial-char  fzfa-separator)
         (s0-command    (and (not multi-p) (plist-get s0 :command)))
         (s0-candidates (and (not multi-p) (plist-get s0 :candidates)))
         (s0-display    (and (not multi-p)
                             (or (plist-get s0 :display) 'hidden)))
         (init-text
          (cond
           ((consp initial-input) (car initial-input))
           ((stringp initial-input) initial-input)
           ((and s0-command (memq s0-display '(compact full)))
            (concat (char-to-string initial-char) s0-command
                    (char-to-string initial-char)))
           ((and s0-candidates (not s0-command)
                 (memq s0-display '(compact full)))
            (concat (char-to-string initial-char)
                    (char-to-string initial-char)))
           (t nil)))
         (init-point
          (cond
           ((consp initial-input) (cdr initial-input))
           ((and s0-candidates (not s0-command) init-text
                 (memq s0-display '(compact full)))
            1)
           (init-text (length init-text))
           (t nil)))
         ;; Prompt-fn arg builder.  Single source of truth for the
         ;; `:source-kind' / `:directory' / `:command' triple so the
         ;; refresh-overlay tick, the ivy pre-prompt closure, and the
         ;; wants-decoration probe all agree.  At N=1 the args
         ;; reflect the lone source (matches the historical
         ;; single-source prompt: DIR + filtered/total); at N>1 they
         ;; describe the aggregate multi session.  Caller passes EXTRA
         ;; — a plist with `:index', `:filtered', `:total',
         ;; `:narrow-name'.
         (prompt-fn-args
          (lambda (extra)
            (let* ((s0 (aref specs-v 0)))
              (append
               (list :source-kind
                     (cond (multi-p :multi)
                           ((plist-get s0 :command) :command)
                           (t :candidates))
                     :prompt prompt
                     :directory
                     ;; Set for single-source pickers whenever the
                     ;; source has a working dir — includes
                     ;; `:candidates' pickers that opt in by passing
                     ;; `:directory' (e.g., `fzfa-browse-files' under
                     ;; its sync path).  Multi keeps nil (each source
                     ;; has its own dir).
                     (and (not multi-p)
                          (or (plist-get s0 :command)
                              (plist-get s0 :directory))
                          (let ((d (or (plist-get s0 :directory)
                                       default-directory)))
                            (and d (abbreviate-file-name
                                    (expand-file-name d)))))
                     :command
                     (and (not multi-p)
                          (fzfa-source-command (aref sources 0))))
               extra))))
         ;; See the single-source `wants-decoration' probe for the rule.
         (wants-decoration
          (and (funcall fzfa-prompt-function
                        (funcall prompt-fn-args
                                 (list :index nil
                                       :filtered 0
                                       :total 0
                                       :narrow-name nil)))
               t))
         (stats-overlay nil)
         ;; Captured by `minibuffer-exit-hook' from the propertized text
         ;; in the minibuffer before `completing-read' returns and strips
         ;; properties.  Reliable per-instance source dispatch even when
         ;; the same string appears in multiple sources.
         (selected-idx nil)
         ;; Index into specs-v / sources of the currently-narrowed source,
         ;; or nil for "all sources" (multi-source widened state).  At
         ;; N=1 there is no "widened" — the lone source is permanently
         ;; the narrow target, so initialize to 0 so the rest of the
         ;; code paths (display-cycle gate, candidate-fetch CMD split,
         ;; metadata source resolution, etc.) behave as if narrowed
         ;; from the first frame onward.  Caller-supplied NARROW-IDX
         ;; \(`fzfa-replay') wins so a saved narrow target is restored.
         (narrow-idx (or narrow-idx (unless multi-p 0)))
         ;; Dynamic mirror of `narrow-idx' restricted to multi-source
         ;; sessions.
         (fzfa--multi-narrowed-p (and multi-p narrow-idx t))
         ;; Most recent user filter, updated on every table / ivy push.
         ;; Read by the replay-snapshot block to persist the filter as
         ;; the narrow target's `:initial-input' for the next replay.
         (last-query "")
         ;; When the narrow menu is on screen (during the
         ;; `narrow-handler''s `read-char') we must NOT overwrite the
         ;; overlay with the stats line on every tick — otherwise async
         ;; sources streaming new generations (a 50ms cadence) erase
         ;; the menu before the user has had a chance to read it.
         (menu-active nil)
         (refresh-overlay
          (lambda ()
            (when (and stats-overlay
                       (not menu-active)
                       (active-minibuffer-window))
              (with-selected-window (active-minibuffer-window)
                (overlay-put
                 stats-overlay 'display
                 (funcall
                  fzfa-prompt-function
                  (funcall
                   prompt-fn-args
                   (list :index (fzfa--frontend-index)
                         :filtered (cl-loop for s across sources
                                            sum (fzfa-source-filtered s))
                         :total (cl-loop for s across sources
                                         sum (fzfa-source-total s))
                         :narrow-name
                         (and multi-p narrow-idx
                              (or (plist-get
                                   (aref specs-v narrow-idx)
                                   :name)
                                  "?"))))))))
            (fzfa--insert-prompt-if-ivy)))
         ;; Forward placeholders — these closures reference each other
         ;; in a cycle (`ivy-push-multi' → `narrow-do-restart' →
         ;; `multi-refresh-fn' → `ivy-push-multi'), so they can't be
         ;; created left-to-right.  Bound via `setq' in the body once
         ;; all four cells exist.
         multi-refresh-fn
         narrow-do-restart
         narrow-display-cycle
         ;; Ivy push closure: ivy doesn't re-call the collection on
         ;; timer ticks (push model), so async sources would stay
         ;; stuck on the initial pattern.  Mirrors the per-source
         ;; iterate/score/rank/concat from the `'t' collection
         ;; action but reads `ivy-text' and pushes via
         ;; `ivy--set-candidates' + `ivy--exhibit'.  Mutates the
         ;; same per-source state vectors, so state stays consistent
         ;; whether updated via the collection (vertico/icomplete)
         ;; or this closure (ivy).
         (ivy-push-multi
          (lambda ()
            (when-let* ((win   (active-minibuffer-window))
                        ((or (not (bound-and-true-p ivy-last))
                             (ivy-state-dynamic-collection ivy-last)))
                        (input (and (boundp 'ivy-text) ivy-text)))
              ;; Run the ivy ops with the minibuffer buffer current —
              ;; the closure can fire from `run-with-idle-timer'
              ;; whose buffer context is whatever was current at idle
              ;; time, and `ivy--insert-prompt' / `ivy--exhibit'
              ;; silently write to the wrong buffer otherwise.
              (with-selected-window win
                (let* ((interrupted nil)
                       ;; When narrowed to a source whose display is
                       ;; `compact' / `full', split the buffer at
                       ;; `fzfa-separator' so CMD drives the producer /
                       ;; shell handle and FILTER drives the fzf score.
                       ;; Otherwise the whole input flows through
                       ;; unchanged.
                       (narrow-src (and narrow-idx (aref sources narrow-idx)))
                       (narrow-active
                        (and narrow-src
                             (not (eq (fzfa-source-display-state narrow-src)
                                      'hidden))))
                       (narrow-split
                        (and narrow-active
                             (fzfa--split
                              input
                              (fzfa-source-display-state narrow-src)
                              (fzfa-source-command narrow-src))))
                       (narrow-cmd    (car narrow-split))
                       (narrow-filter (cdr narrow-split))
                       (query (if narrow-active narrow-filter input)))
                  (setq last-query query)
                  (when (and narrow-active
                             (not (equal narrow-cmd
                                         (fzfa-source-current-cmd
                                          narrow-src))))
                    (funcall narrow-do-restart narrow-src narrow-cmd))
                  (dotimes (i n)
                    (let* ((src (aref sources i))
                           (this-narrow (and narrow-active
                                             (eql i narrow-idx)))
                           (prod-input (if this-narrow narrow-cmd input)))
                      (if (and narrow-idx (/= narrow-idx i))
                          (progn
                            (setf (fzfa-source-last-result src) nil
                                  (fzfa-source-filtered src) 0
                                  (fzfa-source-rank src) 0))
                        (let* ((h     (fzfa-source-handle src))
                               (prod  (fzfa-source-cands-fn src))
                               (out
                                (cond
                                 (h (while-no-input
                                      (fzf-native-async-candidates
                                       h query limit)))
                                 (prod
                                  (fzfa--multi-candidates-fetch
                                   src i prod-input candidate->source
                                   multi-p multi-refresh-fn)
                                  (let ((snap (fzfa-source-snapshot src)))
                                    (cond
                                     ((null snap) '())
                                     ((string-empty-p query) snap)
                                     (t (let ((fzfa-batch-highlight nil))
                                          (while-no-input
                                            (fzfa--bridge-defcustoms
                                             #'fzf-native-score-all
                                             snap query))))))))))
                          (cond
                           ((eq out t) (setq interrupted t))
                           ((and h (not (fzfa--final-p out h query)))
                            (when-let* ((stats (fzf-native-async-stats h)))
                              (setf (fzfa-source-total src) (cdr stats))))
                           (t
                            ;; Re-tag async candidates only.  Async handles
                            ;; emit fresh elisp strings each call that have
                            ;; never been tagged.  Producer-path output is
                            ;; already tagged at fetch time: `fzf-native-
                            ;; score-all' returns input strings unchanged
                            ;; for the rank tail, and content-equal copies
                            ;; for the highlighted top-N — both dispatch
                            ;; through `candidate->source' via content-equality.
                            (when h
                              (let ((src-action
                                     (plist-get (fzfa-source-spec src)
                                                :action)))
                                (setq out
                                      (mapcar
                                       (lambda (c)
                                         (fzfa--tag c i candidate->source
                                                    multi-p src-action))
                                       out))))
                            (when (and out (not first-cands-shown))
                              (setq first-cands-shown t))
                            (setf (fzfa-source-last-result src) out
                                  (fzfa-source-rank src)
                                  (fzfa--multi-rank out query h))
                            (cond
                             (h (when-let* ((stats (fzf-native-async-stats h)))
                                  (setf (fzfa-source-filtered src) (car stats)
                                        (fzfa-source-total src) (cdr stats))))
                             (t (setf (fzfa-source-filtered src)
                                      (length out))))))))))
                  (unless interrupted
                    (let* ((order (number-sequence 0 (1- n)))
                           (empty-q (string-empty-p query))
                           (sorted
                            (if empty-q
                                order
                              (sort order
                                    (lambda (a b)
                                      (> (fzfa-source-rank (aref sources a))
                                         (fzfa-source-rank (aref sources b)))))))
                           ;; Per-source sorted + highlighted candidate lists,
                           ;; one entry per source-index in `sorted' order.
                           (per-source
                            (mapcar
                             (lambda (i)
                               (let* ((src (aref sources i))
                                      (slot (fzfa-source-last-result src)))
                                 (cond
                                  ;; Empty query → per-source history rank
                                  ;; only, no scoring ran.
                                  ((and empty-q
                                        (fzfa-source-history src))
                                   (fzfa--history-rank
                                    slot (fzfa-source-history src)))
                                  ;; Otherwise: rank + highlight (sync) or
                                  ;; passthrough (async / empty).
                                  (t
                                   (fzfa--rank-and-highlight
                                    slot query
                                    (fzfa-source-history src))))))
                             sorted))
                           (cands
                            (if empty-q
                                ;; No scoring → flat concat preserves
                                ;; source-rank order.
                                (apply #'append per-source)
                              ;; Non-empty: ivy is flat (no group headers),
                              ;; so each source's leader gets pulled into a
                              ;; leader pool first — preventing a strong
                              ;; group's mid-rank tail from outranking
                              ;; another group's top candidate.  Then each
                              ;; source's remaining candidates flat-concat
                              ;; in source-rank order.
                              (let (leaders tails)
                                (dolist (slot per-source)
                                  (when slot
                                    (push (car slot) leaders)
                                    (when (cdr slot)
                                      (push (cdr slot) tails))))
                                (append (nreverse leaders)
                                        (apply #'append (nreverse tails)))))))
                      (ivy--set-candidates cands)
                      (ivy--exhibit)
                      ;; `ivy--exhibit' skips the prompt redraw when the
                      ;; candidate body didn't change.  Force it so our
                      ;; `ivy-pre-prompt-function' lambda runs again with
                      ;; the freshest stats.
                      (ivy--insert-prompt))))))))
         ;; Ivy action list for narrow dispatch.  One entry per
         ;; source's :narrow key (mutates `narrow-idx' and refreshes
         ;; via `ivy-push-multi'), plus a widen entry on
         ;; `fzfa-multi-narrow-key' so pressing the prefix key twice
         ;; widens — matching the existing `<<' muscle memory.  Bound
         ;; into `ivy--actions-list' across `completing-read' below;
         ;; `ivy-dispatching-call' triggers the action menu via
         ;; `fzfa-multi-narrow-key' in the keymap install.
         (ivy-multi-actions
          (when (and multi-p (bound-and-true-p ivy-mode))
            (let (acts)
              (dotimes (i n)
                (when-let* ((spec    (aref specs-v i))
                            (narrow (plist-get spec :narrow)))
                  (let ((idx i)
                        (name (or (plist-get spec :name) "?")))
                    (push (list narrow
                                (lambda (_cand)
                                  (let ((before narrow-idx))
                                    (setq narrow-idx idx
                                          fzfa--multi-narrowed-p t)
                                    ;; `ivy-call' restores the
                                    ;; pre-minibuffer buffer before
                                    ;; dispatching the action, so any
                                    ;; mutation done here lands in
                                    ;; the wrong buffer.  Switch back
                                    ;; to the minibuffer so
                                    ;; `force-hidden' → `extract'
                                    ;; touches the right text.
                                    (when-let* ((win (active-minibuffer-window)))
                                      (with-current-buffer (window-buffer win)
                                        (when (and before (/= before idx))
                                          (fzfa-source--display-force-hidden
                                           (aref sources before)
                                           fzfa-separator))))
                                    ;; Defer so `ivy-text' is fresh
                                    ;; after `force-hidden' mutates the
                                    ;; buffer.  See narrow-handler.
                                    (run-with-idle-timer
                                     0 nil #'fzfa--frontend-push
                                     ivy-push-multi)))
                                (format "narrow → %s" name))
                          acts))))
              (when fzfa-multi-narrow-key
                (push (list fzfa-multi-narrow-key
                            (lambda (_cand)
                              (let ((before narrow-idx))
                                (setq narrow-idx nil
                                      fzfa--multi-narrowed-p nil)
                                (when-let* ((win (active-minibuffer-window)))
                                  (with-current-buffer (window-buffer win)
                                    (when before
                                      (fzfa-source--display-force-hidden
                                       (aref sources before)
                                       fzfa-separator))))
                                (run-with-idle-timer
                                 0 nil #'fzfa--frontend-push
                                 ivy-push-multi)))
                            "widen")
                      acts))
              (nreverse acts))))
         (narrow-handler
          (lambda ()
            (interactive)
            ;; Three states:
            ;;   1. widened (narrow-idx nil) — no menu, query freely
            ;;   2. narrow menu — this handler is running
            ;;   3. narrowed (narrow-idx set) — no menu, query freely
            ;;
            ;; From the menu: source letter narrows/switches; the
            ;; prefix key widens (-> 1); any other key cancels the
            ;; menu and returns to the prior state (1 or 3).
            (let* ((seq (and fzfa-multi-narrow-key
                             (listify-key-sequence
                              (kbd fzfa-multi-narrow-key))))
                   (prefix-event (car (last seq)))
                   (before narrow-idx))
              ;; Suspend `refresh-overlay' (which otherwise restores the
              ;; stats line on every async tick) so the menu stays put
              ;; until `read-char' returns.  `unwind-protect' guarantees
              ;; the flag clears on `C-g' / error too.
              (setq menu-active t)
              (unwind-protect
                  (progn
                    (when-let* ((win (active-minibuffer-window)))
                      (with-selected-window win
                        ;; Lazily create the stats overlay if the table
                        ;; arm hasn't ticked through yet (`fzfa-replay'
                        ;; on a seeded filter only fires `(t …)' once
                        ;; before the user presses `<', and depending on
                        ;; timing the overlay may not have been
                        ;; installed when the menu opens).  Without
                        ;; this, the `when (and stats-overlay …)' guard
                        ;; silently dropped the hint and the menu came
                        ;; up blank.
                        (unless stats-overlay
                          (setq stats-overlay
                                (make-overlay (point-min)
                                              (minibuffer-prompt-end))))
                        (overlay-put stats-overlay 'display
                                     (concat (fzfa--format-narrow-hint
                                              specs-v narrow-idx nil
                                              fzfa-multi-narrow-key)
                                             " "))
                        (redisplay)))
                    (let* ((c (read-char))
                           (target
                            (cl-position-if
                             (lambda (spec)
                               (when-let* ((k (plist-get spec :narrow)))
                                 (and (stringp k)
                                      (= (length k) 1)
                                      (= (string-to-char k) c))))
                             specs-v)))
                      (cond
                       ((and prefix-event (eql c prefix-event))
                        (setq narrow-idx nil
                              fzfa--multi-narrowed-p nil))
                       (target (setq narrow-idx target
                                     fzfa--multi-narrowed-p t))
                       (t nil))
                      (setq menu-active nil)
                      (unless (eql before narrow-idx)
                        ;; Leaving a non-hidden source — extract its
                        ;; `#cmd#' shape back onto the source struct so
                        ;; widened / next-narrowed mode doesn't inherit
                        ;; a stale `#cmd#filter' buffer.
                        (when before
                          (fzfa-source--display-force-hidden
                           (aref sources before) fzfa-separator))
                        ;; Defer the push: when `force-hidden' mutates
                        ;; the minibuffer, ivy's `ivy-text' doesn't sync
                        ;; until `post-command-hook' fires, so an inline
                        ;; push reads stale input.  An idle-0 timer
                        ;; lands after the hook and reads fresh text.
                        (run-with-idle-timer
                         0 nil #'fzfa--frontend-push ivy-push-multi))
                      ;; Restore the normal overlay now that the menu
                      ;; is dismissed (the 't action's own refresh path
                      ;; only fires on candidate computations).
                      (funcall refresh-overlay)
                      ;; Under ivy, force a prompt redraw so the
                      ;; `ivy-pre-prompt-function' lambda runs again
                      ;; with `menu-active' = nil and swaps the menu
                      ;; hint back to the stats line.  Cheap if
                      ;; `ivy-push-multi' already redrew.
                      (when (bound-and-true-p ivy-mode)
                        (ivy--exhibit))))
                (setq menu-active nil)))))
         (router      (fzfa--multi-build-router specs-v candidate->source))
         (session
          (fzfa-session-create
           :specs specs-v
           :sources sources
           :cand->src candidate->source
           :directory (or (fzfa--default-dir)
                          (and s0 (plist-get s0 :directory)))
           :entry-command entry-command
           :narrow-idx narrow-idx
           :router-cells (and router (plist-get router :multi-cells))
           :apply-fn (and (not multi-p) (plist-get s0 :apply))))
         (fzfa--preview-session
          (and router
               ;; Stash the candidate→source-idx table for per-candidate
               ;; routing outside the router; cells exposed via
               ;; `:multi-cells' on ROUTER itself.
               (list router :multi-candidate->source candidate->source)))
         retry-timer timer result)
    ;; Bind the forward-declared closures.  Order: `multi-refresh-fn'
    ;; references `ivy-push-multi' (which was bound in let*);
    ;; `narrow-do-restart' references `multi-refresh-fn';
    ;; `narrow-display-cycle' references `ivy-push-multi'.
    (setq multi-refresh-fn
          (lambda () (fzfa--frontend-push ivy-push-multi)))
    (setq narrow-do-restart
          (lambda (src cmd)
            (if (fzfa-source-cands-fn src)
                ;; Cands-fn producer: re-fire immediately on cmd change.
                (fzfa-source--restart src cmd multi-refresh-fn)
              ;; Shell handle: debounce to coalesce fast keystrokes.
              (fzfa-source--debounce-restart src cmd multi-refresh-fn))))
    (setq narrow-display-cycle
          (lambda ()
            (interactive)
            (cond
             ;; Widened → let `>' self-insert; in multi-source mode
             ;; there's no single source to cycle.
             ((null narrow-idx) (call-interactively #'self-insert-command))
             ;; Mutate the buffer (hidden↔compact materializes /
             ;; extracts `#CMD#') and let the frontend's
             ;; post-command-hook pick it up.  A manual
             ;; `fzfa--frontend-push' here runs *before* ivy syncs
             ;; `ivy-text' from the minibuffer, so it would parse the
             ;; stale pre-insert input and restart the source with
             ;; garbage CMD.
             (t (fzfa-source--display-cycle
                 (aref sources narrow-idx) fzfa-separator)))))
    ;; Eager-start fzf-native handles for command sources.  Producer-fn
    ;; sources (`cands-fn' slot) need no eager work — first dispatch
    ;; tick fires the initial fetch via `fzfa--multi-candidates-fetch'.
    ;; Seed `current-cmd' so the narrow-source restart-check (later in
    ;; the candidate loop) sees the source as "already running its
    ;; initial cmd" — otherwise the first split-driven tick would fire
    ;; a redundant restart of a freshly-started handle.
    (dotimes (i n)
      (let* ((src (aref sources i))
             (cmd (plist-get (fzfa-source-spec src) :command)))
        (when cmd
          (setf (fzfa-source-current-cmd src) cmd
                (fzfa-source-handle src)
                (fzfa--spawn cmd (fzfa-source-directory src))))))
    (unwind-protect
        (progn
          (setq timer
                (run-with-timer
                 0 fzfa-refresh-delay
                 (fzfa--make-poll-fn
                  sources
                  (lambda () (active-minibuffer-window))
                  (lambda ()
                    (run-with-idle-timer
                     0 nil #'fzfa--frontend-push ivy-push-multi))
                  (lambda () first-cands-shown))))
          (add-hook 'post-command-hook refresh-overlay)
          (sit-for fzfa-refresh-delay)
          (setq result
                (minibuffer-with-setup-hook
                    (lambda ()
                      (fzfa--minibuffer-format-reset wants-decoration)
                      (when (bound-and-true-p icomplete-mode)
                        (add-hook 'post-command-hook
                                  #'fzfa--icomplete-cursor-override
                                  nil t))
                      (when router
                        (fzfa--preview-install session fzfa-preview-delay))
                      ;; Capture source idx from the propertized minibuffer
                      ;; text before completing-read returns and strips text
                      ;; properties from its return value.  Reliable
                      ;; per-instance dispatch even for cross-source
                      ;; duplicate strings.
                      (add-hook 'minibuffer-exit-hook
                                (lambda ()
                                  (let ((s (buffer-substring
                                            (minibuffer-prompt-end)
                                            (point-max))))
                                    (when (> (length s) 0)
                                      (setq selected-idx
                                            (gethash s candidate->source)))))
                                nil 'local)
                      ;; Install narrow-key binding as a per-instance
                      ;; child of the active completion keymap so we
                      ;; don't mutate vertico/icomplete/ivy's shared
                      ;; map.  Under ivy, hand off to its native
                      ;; action dispatch (`ivy-dispatching-call' +
                      ;; per-source entries in `ivy--actions-list');
                      ;; under other frontends, run the in-house
                      ;; `narrow-handler' that does its own read-char
                      ;; menu.  Also override SPC / `?' from
                      ;; `minibuffer-local-completion-map' — those are
                      ;; `minibuffer-complete-word' / `minibuffer-
                      ;; completion-help' by default, which silently
                      ;; eat the keypress when editing the `#cmd#'
                      ;; shell command region.
                      (let ((map (make-sparse-keymap)))
                        (set-keymap-parent map (current-local-map))
                        (define-key map " " #'self-insert-command)
                        (define-key map "?" #'self-insert-command)
                        ;; `<' (narrow-switch) only meaningful when
                        ;; there are multiple sources to switch
                        ;; between.
                        (when (and multi-p fzfa-multi-narrow-key)
                          (define-key map (kbd fzfa-multi-narrow-key)
                                      (if (bound-and-true-p ivy-mode)
                                          #'ivy-dispatching-call
                                        narrow-handler)))
                        (when fzfa-display-key
                          (define-key map (kbd fzfa-display-key)
                                      narrow-display-cycle))
                        (use-local-map map))
                      ;; N=1 with `:initial-input' OR a compact / full
                      ;; default seeded `<sep>CMD<sep>' (init-text /
                      ;; init-point are nil at N>1; the legacy
                      ;; single-source seeding lives here now).
                      (when init-point
                        (goto-char (+ (minibuffer-prompt-end) init-point))))
                  (let ((fzfa--active-sources specs-v)
                        (fzfa--candidate->source candidate->source)
                        (ivy-completing-read-dynamic-collection t)
                        (ivy-count-format
                         (if (and (bound-and-true-p ivy-mode) wants-decoration)
                             ""
                           (bound-and-true-p ivy-count-format)))
                        (ivy--actions-list
                         (if (bound-and-true-p ivy-mode)
                             (plist-put (cl-copy-list
                                         (or ivy--actions-list '()))
                                        t ivy-multi-actions)
                           (bound-and-true-p ivy--actions-list)))
                        (ivy-pre-prompt-function
                         (when (and (bound-and-true-p ivy-mode) wants-decoration)
                           (lambda ()
                             ;; Ivy's `ivy--insert-prompt' always renders
                             ;; the base PROMPT next to (or after) our
                             ;; pre-prompt string.  Passing `:prompt ""'
                             ;; keeps our stats line from carrying the
                             ;; same PROMPT again — otherwise the user
                             ;; sees e.g. `browse: DIR N/[F](T) browse:'.
                             ;; Under other frontends the stats overlay
                             ;; REPLACES the prompt, so `prompt-fn-args'
                             ;; keeps `:prompt' intact for them.
                             (let ((args (funcall
                                          prompt-fn-args
                                          (list :index (fzfa--frontend-index)
                                                :filtered
                                                (cl-loop for s across sources
                                                         sum (fzfa-source-filtered s))
                                                :total
                                                (cl-loop for s across sources
                                                         sum (fzfa-source-total s))
                                                :narrow-name
                                                (and multi-p narrow-idx
                                                     (or (plist-get
                                                          (aref specs-v
                                                                narrow-idx)
                                                          :name)
                                                         "?"))))))
                               (setq args (plist-put args :prompt ""))
                               (or (funcall fzfa-prompt-function args)
                                   ""))))))
                    (completing-read
                     prompt
                     (lambda (str _pred action)
                       (pcase action
                         ('metadata
                          (if (not multi-p)
                              ;; N=1: surface the lone source's
                              ;; category + annotation hooks directly so
                              ;; downstream tools (embark targets,
                              ;; marginalia, vertico/icomplete
                              ;; affixation) see the same metadata they
                              ;; got from the legacy single-source path.
                              (let ((s0 (aref specs-v 0)))
                                (fzfa--completion-metadata
                                 (or (plist-get s0 :category) 'fzfa-multi)
                                 :history  (plist-get s0 :history)
                                 :annotate (plist-get s0 :annotate)
                                 :affix    (plist-get s0 :affix)
                                 :group    (plist-get s0 :group)))
                            (fzfa--completion-metadata
                             'fzfa-multi
                             :group
                             (lambda (cand transform)
                               (let* ((src (fzfa--multi-source-of
                                            cand specs-v candidate->source))
                                      (g   (plist-get src :group)))
                                 (if transform
                                     ;; Per-source :group transform — lets a
                                     ;; source strip an internal "IDX:" prefix
                                     ;; or otherwise reshape its display string
                                     ;; while keeping the raw value as the
                                     ;; lookup/match key.  Falls back to the raw
                                     ;; candidate when a source has no :group
                                     ;; function (or its transform returns nil).
                                     ;; The tofu suffix is hidden via its
                                     ;; `display ""' text property, so the raw
                                     ;; CAND fallback renders cleanly without an
                                     ;; explicit strip.
                                     (or (and g (funcall
                                                 g (fzfa--tofu-hide cand) t))
                                         cand)
                                   ;; Section header.  When narrowed to a single
                                   ;; source, delegate to the per-source :group's
                                   ;; nil branch so any internal sub-grouping
                                   ;; (e.g. per-file headers for grep-style
                                   ;; sources) takes over — matching the
                                   ;; standalone command's layout.  Across
                                   ;; sources, the source name is the only
                                   ;; header that meaningfully separates them.
                                   (or (and narrow-idx g
                                            (funcall g (fzfa--tofu-hide cand) nil))
                                       (plist-get src :name) ""))))
                             :affix
                             ;; Pin annotations to window-relative column
                             ;; maxw+1 via a `(space :align-to ...)' display
                             ;; spec.  Vertico just concatenates suffixes
                             ;; verbatim (no padding of its own) so it needs
                             ;; the spec; icomplete's own slice-relative
                             ;; padding stacks badly with literal spaces, so
                             ;; the spec wins there too.
                             (lambda (cands)
                               (let* ((displays
                                       (mapcar
                                        (lambda (c)
                                          (let* ((src (fzfa--multi-source-of
                                                       c specs-v candidate->source))
                                                 (g (and src
                                                         (plist-get src :group))))
                                            (or (and g (funcall
                                                        g (fzfa--tofu-hide c) t))
                                                c)))
                                        cands))
                                      (maxw (apply #'max 0
                                                   (mapcar #'string-width
                                                           displays))))
                                 (cl-mapcar
                                  (lambda (cand _display)
                                    (let* ((src (fzfa--multi-source-of
                                                 cand specs-v candidate->source))
                                           (ann (and src
                                                     (plist-get src :annotate)))
                                           (s   (and ann (funcall
                                                          ann
                                                          (fzfa--tofu-hide cand)))))
                                      (list cand ""
                                            (if s
                                                (concat
                                                 (propertize
                                                  " " 'display
                                                  `(space :align-to
                                                          (+ left ,(1+ maxw))))
                                                 s)
                                              ""))))
                                  cands displays))))))
                         (`(boundaries . ,_) (cons 0 0))
                         ('lambda t)
                         ('t
                          (let* ((input (fzfa--current-query str))
                                 (interrupted nil)
                                 (narrow-src
                                  (and narrow-idx (aref sources narrow-idx)))
                                 (narrow-active
                                  (and narrow-src
                                       (not (eq (fzfa-source-display-state
                                                 narrow-src)
                                                'hidden))))
                                 (narrow-split
                                  (and narrow-active
                                       (fzfa--split
                                        input
                                        (fzfa-source-display-state narrow-src)
                                        (fzfa-source-command narrow-src))))
                                 (narrow-cmd    (car narrow-split))
                                 (narrow-filter (cdr narrow-split))
                                 (query (if narrow-active narrow-filter input)))
                            (setq last-query query)
                            (when (and narrow-active
                                       (not (equal narrow-cmd
                                                   (fzfa-source-current-cmd
                                                    narrow-src))))
                              (funcall narrow-do-restart
                                       narrow-src narrow-cmd))
                            (dotimes (i n)
                              (let* ((src (aref sources i))
                                     (this-narrow
                                      (and narrow-active (eql i narrow-idx)))
                                     (prod-input
                                      (if this-narrow narrow-cmd input)))
                                (if (and narrow-idx (/= narrow-idx i))
                                    ;; Source filtered out by narrow — drop
                                    ;; its prior results and zero its filtered
                                    ;; count so the overlay reflects the
                                    ;; narrowed pool.  `total' is preserved
                                    ;; so re-widening shows the full size.
                                    (setf (fzfa-source-last-result src) nil
                                          (fzfa-source-filtered src) 0
                                          (fzfa-source-rank src) 0)
                                  (let* ((h     (fzfa-source-handle src))
                                         (prod  (fzfa-source-cands-fn src))
                                         (out
                                          (cond
                                           (h (while-no-input
                                                (fzf-native-async-candidates
                                                 h query limit)))
                                           (prod
                                            (fzfa--multi-candidates-fetch
                                             src i prod-input candidate->source
                                             multi-p multi-refresh-fn)
                                            (let ((snap (fzfa-source-snapshot src)))
                                              (cond
                                               ((null snap) '())
                                               ((string-empty-p query) snap)
                                               (t (let ((fzfa-batch-highlight nil))
                                                    (while-no-input
                                                      (fzfa--bridge-defcustoms
                                                       #'fzf-native-score-all
                                                       snap query))))))))))
                                    (cond
                                     ((eq out t) (setq interrupted t))
                                     ;; Async source whose result is not yet
                                     ;; final — keep the prior per-source slot;
                                     ;; refresh `total' so the overlay still
                                     ;; reflects the live pool.
                                     ((and h (not (fzfa--final-p
                                                   out h query)))
                                      (when-let* ((stats (fzf-native-async-stats h)))
                                        (setf (fzfa-source-total src) (cdr stats))))
                                     (t
                                      ;; Re-tag async candidates only.
                                      ;; Producer-path output is already
                                      ;; tagged at fetch time and the
                                      ;; sync scorer preserves identity
                                      ;; (tail) or content-equality
                                      ;; (highlighted top-N) — both
                                      ;; dispatch through `candidate->source'.
                                      ;; out may be nil (zero matches) — ok.
                                      (when h
                                        (let ((src-action
                                               (plist-get
                                                (fzfa-source-spec src)
                                                :action)))
                                          (setq out
                                                (mapcar
                                                 (lambda (c)
                                                   (fzfa--tag c i candidate->source
                                                              multi-p src-action))
                                                 out))))
                                      (when (and out (not first-cands-shown))
                                        (setq first-cands-shown t))
                                      (setf (fzfa-source-last-result src) out
                                            (fzfa-source-rank src)
                                            (fzfa--multi-rank out query h))
                                      (cond
                                       (h (when-let*
                                              ((stats (fzf-native-async-stats h)))
                                            (setf (fzfa-source-filtered src)
                                                  (car stats)
                                                  (fzfa-source-total src)
                                                  (cdr stats))))
                                       (t (setf (fzfa-source-filtered src)
                                                (length out))))))))))
                            (when interrupted
                              (when retry-timer (cancel-timer retry-timer))
                              (setq retry-timer
                                    (run-with-idle-timer
                                     fzfa-input-debounce nil
                                     (lambda ()
                                       (setq retry-timer nil)
                                       (fzfa--frontend-push ivy-push-multi)))))
                            (when-let* ((win (active-minibuffer-window)))
                              (with-selected-window win
                                (unless stats-overlay
                                  (setq stats-overlay
                                        (make-overlay (point-min)
                                                      (minibuffer-prompt-end))))
                                (funcall refresh-overlay)))
                            (let* ((order (number-sequence 0 (1- n)))
                                   (empty-q (string-empty-p query))
                                   ;; `sort' is stable since Emacs 25, so equal
                                   ;; ranks preserve declared source order.
                                   (sorted
                                    (if empty-q
                                        order
                                      (sort order
                                            (lambda (a b)
                                              (> (fzfa-source-rank
                                                  (aref sources a))
                                                 (fzfa-source-rank
                                                  (aref sources b))))))))
                              (apply #'append
                                     (mapcar
                                      (lambda (i)
                                        (let* ((src (aref sources i))
                                               (slot (fzfa-source-last-result src)))
                                          (cond
                                           ;; Empty query → per-source history rank
                                           ;; only, no scoring ran.
                                           ((and empty-q
                                                 (fzfa-source-history src))
                                            (fzfa--history-rank
                                             slot (fzfa-source-history src)))
                                           ;; Otherwise: rank + highlight (sync) or
                                           ;; passthrough (async / empty).
                                           (t
                                            (fzfa--rank-and-highlight
                                             slot query
                                             (fzfa-source-history src))))))
                                      sorted)))))
                         (_ t)))
                     nil require-match init-text
                     (and (not multi-p) (fzfa-source-history (aref sources 0)))
                     default-val)))))
      (when timer (cancel-timer timer))
      (when retry-timer (cancel-timer retry-timer))
      (remove-hook 'post-command-hook refresh-overlay)
      (when stats-overlay (delete-overlay stats-overlay))
      ;; Snapshot BEFORE `fzfa-source--stop' so per-source
      ;; `current-cmd' / `display-state' (set by the table arm and
      ;; the display-cycle command) are still readable.  Fires on
      ;; both normal exit and C-g abort.
      (fzfa--sessions-push specs sources prompt narrow-idx
                           last-query entry-command)
      (mapc #'fzfa-source--stop sources)
      (when router (fzfa--preview-return result session)))
    (when result
      (let* ((src-idx (or selected-idx
                           (fzfa--multi-source-idx
                            result candidate->source)))
             (src     (and src-idx (aref specs-v src-idx)))
             ;; Property recovery: the canonical candidate lives on
             ;; the source's snapshot with all in-band metadata the
             ;; caller attached (e.g. `fzfa-location' on
             ;; `fzfa-swiper').  `read-from-minibuffer' may strip
             ;; text properties under frontends that don't honor
             ;; `minibuffer-allow-text-properties' — recover via
             ;; `member' (content equality) so downstream consumers
             ;; always see the original propertized string.  No-op
             ;; for async sources (snapshot is nil there).
             (snap    (and src-idx
                           (fzfa-source-snapshot (aref sources src-idx))))
             (result  (or (and snap (car (member result snap))) result))
             (action (and src (plist-get src :action)))
             (hist   (and src (plist-get src :history)))
             (expanded (fzfa-resolve-candidate result session)))
        ;; Multi bypasses each source's inner `completing-read', so the
        ;; source's natural HIST push never fires.  Mirror it here so
        ;; recency-aware sources (e.g. `extended-command-history') stay
        ;; consistent whether picked directly or via a multi.
        (when (and hist (symbolp hist) (not (eq hist t)))
          (add-to-history hist expanded))
        (if action (funcall action expanded) expanded))))))

;;; Replay

(defcustom fzfa-sessions-max 16
  "Maximum number of `fzfa--read' snapshots retained for `fzfa-replay'.

Older snapshots are dropped once the in-memory list grows past this
limit.  Set higher if you want a deeper resume ring."
  :type 'natnum
  :group 'fzfa)

(defcustom fzfa-sessions-exclude-commands
  '(fzfa-replay-from-memory
    fzfa-replay-from-file
    fzfa-replay-any
    fzfa-replay
    fzfa-browse-files
    helm-maybe-exit-minibuffer
    matcha-me-mx)
  "Commands whose `fzfa--read' invocations are NOT captured for replay."
  :type '(repeat function)
  :group 'fzfa)

(defvar fzfa--sessions nil
  "List of recent `fzfa--read' session snapshots, most recent first.

Each entry is a plist with session-global state plus an inner
vector of per-source records:

  :prompt      Prompt string the call ran with.
  :narrow-idx  Active narrow index at exit (nil = widened).
  :sources     Vector of source-records, one per source.

Each source-record is a plist:

  :spec           Source spec as originally passed in.
  :command        Current command at exit — may differ from the
                  spec's `:command' if the user edited the CMD
                  region during the session.
  :display        Display-state at exit (`hidden' / `compact' /
                  `full').
  :initial-input  Last filter the user typed (persisted only on
                  the narrow target; resume threads it into that
                  source's `:initial-input').

`fzfa-replay' pops the head and replays it; the rest of the list
is reserved for a future filesystem-backed resume picker that
spans past Emacs sessions plus the in-memory ring.")

(defun fzfa--session-dedup-key (session)
  "Identity key for SESSION used by `fzfa--sessions-push' to suppress dupes.

Sessions that share (command, directory, narrow-idx, narrow
target's filter) are considered the same interaction; running
the same command from the same directory with the same query
five times leaves one entry in the ring (the most recent)."
  (let* ((sources (plist-get session :sources))
         (target (or (plist-get session :narrow-idx) 0))
         (filter (when (and sources (< target (length sources)))
                   (plist-get (aref sources target) :initial-input))))
    (list (plist-get session :command)
          (plist-get session :directory)
          (plist-get session :narrow-idx)
          filter)))

(defun fzfa--sessions-push (specs sources prompt narrow-idx last-query
                                  entry-command)
  "Snapshot the just-completed `fzfa--read' call onto `fzfa--sessions'.

SPECS is the input source plist list.  SOURCES is the runtime
`fzfa-source' struct vector — read for current-cmd and
display-state.  PROMPT is the call's prompt.  NARROW-IDX is the
active narrow at exit (nil = widened).  LAST-QUERY is the most
recent filter the user typed, captured by the table arm / ivy
push closure.  ENTRY-COMMAND is `this-command' at `fzfa--read'
entry — the user-facing command name shown in the picker and used
as part of the dedup key.

Each per-source record carries its `:snapshot' — the captured
producer output — so disk persistence (`fzfa-replay-save-list')
can substitute function-shaped `:candidates' with their actual
list at save time.

Dedup: before pushing, any existing session with the same
\(command, directory, narrow-idx, filter) key is removed.  Five
identical \\[fzfa-fd]'s from the same dir with the same query
collapse into one — keeping the in-memory ring meaningful and
preventing the on-disk file from accumulating duplicates.

Exclusion: when ENTRY-COMMAND is in `fzfa-sessions-exclude-commands'
the push is skipped entirely.  Used to keep the replay pickers
themselves out of the ring (replaying a replay is confusing) and
to drop helm's exit dispatcher (`helm-maybe-exit-minibuffer')
which would otherwise mask every `helm-mode' pick."
  (unless (memq entry-command fzfa-sessions-exclude-commands)
  (let* ((n (length sources))
         (target (or narrow-idx 0))
         (records
          (cl-loop
           for i below n
           for src = (aref sources i)
           for spec = (nth i specs)
           collect
           (list :spec spec
                 :command (fzfa-source-command src)
                 :display (fzfa-source-display-state src)
                 ;; Captured producer output — preserves the
                 ;; exit-time candidate list so the persisted form
                 ;; can substitute non-serializable function
                 ;; `:candidates' on save.  Nil for shell sources.
                 :snapshot (fzfa-source-snapshot src)
                 ;; Persist the last filter only on the narrow
                 ;; target — that's where it logically belongs.
                 ;; Sources outside the narrow weren't user-edited
                 ;; in this session, so their input stays nil.
                 :initial-input (when (eql i target) last-query))))
         (new (list :prompt prompt
                    :narrow-idx narrow-idx
                    ;; Session-level stamps — feed the picker's
                    ;; annotation / group functions, the dedup key,
                    ;; and the persistence layer's mtime-keyed sort.
                    :timestamp (float-time)
                    :directory default-directory
                    :command entry-command
                    :sources (vconcat records)))
         (key (fzfa--session-dedup-key new)))
    (setq fzfa--sessions
          (cons new (cl-remove-if
                     (lambda (s)
                       (equal (fzfa--session-dedup-key s) key))
                     fzfa--sessions)))
    (when (> (length fzfa--sessions) fzfa-sessions-max)
      (setq fzfa--sessions
            (cl-subseq fzfa--sessions 0 fzfa-sessions-max))))))

(defun fzfa--session-restore-spec (record)
  "Return RECORD's spec with captured runtime state overlaid.

Walks the captured `:command' / `:display' / `:initial-input'
slots and writes them onto a copy of the original spec so
`fzfa--read' sees the exit-state values.

`:command' is dropped when empty — the runtime slot defaults to
\"\" on `:candidates'-only sources, and overlaying that onto a
spec that has no `:command' would trick the eager-start loop into
spawning a shell handle with an empty command (kills the sync
path's result set)."
  (let ((spec (cl-copy-list (plist-get record :spec))))
    (let ((cmd (plist-get record :command)))
      (when (and (stringp cmd) (not (string-empty-p cmd)))
        (setq spec (plist-put spec :command cmd))))
    (let ((display (plist-get record :display)))
      (when display
        (setq spec (plist-put spec :display display))))
    (let ((input (plist-get record :initial-input)))
      (when input
        (setq spec (plist-put spec :initial-input input))))
    spec))

;;;###autoload
(defun fzfa-replay (&optional replay-session)
  "Replay REPLAY-SESSION, or the most recent session interactively.

When REPLAY-SESSION is non-nil, restore its captured specs and
run the session; otherwise default to `(car fzfa--sessions)' —
the most recent capture.  Errors when no session is available.

The picker-route commands (`fzfa-replay-from-memory',
`fzfa-replay-from-file', `fzfa-replay-any') feed their selection
through here so all replay paths share one dispatch."
  (interactive)
  (let ((session (or replay-session (car fzfa--sessions))))
    (unless session
      (user-error "No fzfa session to replay"))
    (let* ((specs (cl-map 'list #'fzfa--session-restore-spec
                          (plist-get session :sources)))
           (entry-cmd (plist-get session :command))
           ;; Let `fzfa--read''s capture site see the *replayed*
           ;; command (e.g. `fzfa-fd') instead of `fzfa-replay'.
           (this-command (or entry-cmd this-command))
           ;; Restore the ambient dir the session was captured under.
           ;; `fzfa--sessions-push' snapshots `default-directory' into
           ;; the session's dedup key AND its `:directory' slot; without
           ;; this rebind the replay always looks like a fresh session
           ;; from wherever the picker was invoked from, so dedup fails
           ;; and the picker keeps accumulating identical-looking
           ;; entries every time you replay one.
           (default-directory (or (plist-get session :directory)
                                  default-directory))
           (result (fzfa--read specs
                               :prompt (plist-get session :prompt)
                               :narrow-idx (plist-get session :narrow-idx))))
      ;; `fzfa--read' returns the picked candidate but the original
      ;; command's body (e.g. `(fzfa-visit-file result)' for
      ;; `fzfa-fd') ran outside it.  Re-invoke the entry command in
      ;; :inject mode so its body fires on replay too.
      (when (and result entry-cmd (fboundp entry-cmd))
        (let ((fzfa--multi-mode (cons :inject result)))
          (funcall entry-cmd))))))

;;;###autoload
(defun fzfa-multi-read (commands &rest options)
  "Run a multi-source completing-read over COMMANDS.

Each entry in COMMANDS is either a bare command symbol or a list
\(COMMAND :narrow KEY) overriding the auto-derived narrow key for
that source (KEY is a single character — symbol, ?char, or string).

Each command is funcalled twice per multi session — once in
`:extract' mode (capture keyword args, abort), once in `:inject' mode after
the user picks (so the command's post-action runs).  OPTIONS is forwarded
to `fzfa--read'.  Commands whose body does not reach
`fzfa-completing-read' are skipped.
Commands must be arg-less (no interactive `read-*' prompts in their body).

Composes: if a command in COMMANDS itself calls `fzfa--read'
\(e.g. `fzfa-find-any'), its inner sources are flattened in alongside
the other commands' sources, with each inner source keeping its own
:action.  Explicit :narrow on a nested-multi entry is ignored —
inner sources receive auto-derived keys from their own :name."
  (fzfa--ensure-setup)
  (let* ((completion-styles '(fzfa))
         (normalized
          (mapcar (lambda (entry)
                    (cond
                     ((symbolp entry) (cons entry nil))
                     ((and (consp entry) (symbolp (car entry)))
                      (cons (car entry) (cdr entry)))
                     (t (error "Bad fzfa multi entry: %S" entry))))
                  commands))
         (source-lists
          (mapcar
           (lambda (pair)
             (let* ((cmd (car pair))
                    (spec (cdr pair))
                    (args (condition-case nil
                              (catch 'fzfa-extracted
                                (let ((fzfa--multi-mode :extract))
                                  (funcall cmd))
                                nil)
                            (error nil))))
               (when args
                 (if-let* ((nested (plist-get args :multi-sources)))
                     ;; Flatten: nested multi command's sources are
                     ;; already fully built with :action closures.
                     ;; A user-provided :narrow here can't sensibly
                     ;; pick one inner source over another, so skip it.
                     nested
                   (let* ((cat (plist-get args :category))
                          (default-annotate
                           (cond
                            ((memq cat '(fzfa-buffer buffer))
                             (lambda (c)
                               (when (fboundp 'marginalia-annotate-buffer)
                                 (marginalia-annotate-buffer c))))
                            ((memq cat '(fzfa-file file))
                             #'fzfa--annotate-file))))
                     ;; Wrap a single source in a list so `append'
                     ;; below treats single and nested cases uniformly.
                     ;; The :action closure captures each source's own
                     ;; :directory / :resolve-paths so post-session
                     ;; callers (e.g. `embark-collect' acting from a
                     ;; buffer whose `default-directory' differs from
                     ;; the picker's) still resolve grep-style paths
                     ;; against the original source's directory.  The
                     ;; cleanup is idempotent on the live `fzfa--read'
                     ;; path: it already passes a clean+expanded CAND.
                     (let ((dir     (plist-get args :directory))
                           (resolve (plist-get args :resolve-paths)))
                       (list
                        (append
                         (list :name (replace-regexp-in-string
                                      "^fzfa-" "" (symbol-name cmd))
                               :annotate (or (plist-get args :annotate)
                                             default-annotate)
                               :action (lambda (cand)
                                         (let ((fzfa--multi-mode
                                                (cons :inject
                                                      (fzfa--maybe-expand
                                                       (fzfa--tofu-hide cand)
                                                       dir resolve))))
                                           (funcall cmd)))
                               :narrow (plist-get spec :narrow))
                         args))))))))
           normalized))
         (sources (apply #'append (delq nil source-lists))))
    ;; Allocation must happen at the OUTERMOST multi level — when we're
    ;; being extracted by an outer `fzfa-multi-read', our inner sources
    ;; will be flattened into its source list and allocated there.  If
    ;; we allocated here first, those keys arrive at the outer as fixed
    ;; explicit reservations that can collide with the outer's own
    ;; explicit `:narrow' annotations.
    (unless (eq fzfa--multi-mode :extract)
      (setq sources (fzfa--multi-allocate-narrow-keys sources)))
    (apply #'fzfa--read sources options)))

(defcustom fzfa-find-any-commands
  '((fzfa-frames :narrow F)
    (fzfa-tabs :narrow t)
    fzfa-imenu
    (fzfa-vc-modified-locally :narrow l)
    fzfa-vc-added-files
    (fzfa-vc-staged-for-commit :narrow c)
    fzfa-buffer
    fzfa-recent-file
    (fzfa-hungry-find :narrow f)
    (fzfa-imenu-all-but-current :narrow I)
    (fzfa-M-x :narrow x)
    (fzfa-swiper-all :narrow s)
    (fzfa-hungry-swiper :narrow S)
    fzfa-locate)
  "Commands shown by `fzfa-find-any'.

Each entry is either a bare command symbol or a list
\(COMMAND :narrow KEY) overriding the auto-derived narrow key."
  :type '(repeat (choice function (cons function plist)))
  :group 'fzfa)

(defcustom fzfa-find-some-commands
  '((fzfa-frames :narrow F)
    (fzfa-tabs :narrow t)
    fzfa-imenu
    (fzfa-vc-modified-locally :narrow l)
    fzfa-vc-added-files
    (fzfa-vc-staged-for-commit :narrow c)
    fzfa-buffer
    fzfa-recent-file
    (fzfa-smart-find :narrow f)
    (fzfa-M-x-for-buffer :narrow x)
    (fzfa-swiper :narrow s)
    (fzfa-smart-grep :narrow g))
  "Commands shown by `fzfa-find-some'.

Each entry is either a bare command symbol or a list
\(COMMAND :narrow KEY) overriding the auto-derived narrow key."
  :type '(repeat (choice function (cons function plist)))
  :group 'fzfa)

;;;###autoload
(defun fzfa-find-any ()
  "Multi-source fuzzy completion over `fzfa-find-any-commands'."
  (interactive)
  (fzfa-multi-read fzfa-find-any-commands :prompt "any?: "))

;;;###autoload
(defun fzfa-find-some ()
  "Multi-source fuzzy completion over `fzfa-find-some-commands'."
  (interactive)
  (fzfa-multi-read fzfa-find-some-commands :prompt "some?: "))

;;;###autoload
(defun fzfa-passwords ()
  "Multi-source fuzzy completion over `pass' and Chrome's password manager.

Selecting an entry copies its password to the kill ring via the
originating source's command (`fzfa-pass-copy' or
`fzfa-chrome-pass-copy')."
  (interactive)
  (fzfa-multi-read
   '(fzfa-pass-copy fzfa-chrome-pass-copy)
   :prompt "passwords: "))

;;; Grep candidate machinery

(defun fzfa--max-columns-flag (tool)
  "Return a max-line-length CLI flag string for grep-style TOOL.

Note: rg's `--max-columns' DROPS the line; ag's and ugrep's
`--width' TRUNCATE display.  Practical effect for our use case
is similar (bounded line length into our pipe), but the
underlying semantics differ slightly.  Either way the
reader-side cap still runs as a backstop."
  (let ((mll fzfa-max-line-length))
    (if (not (and (integerp mll) (> mll 0)))
        ""                                ; nil / 0 / negative → no flag
      (pcase tool
        ('rg    (format "--max-columns=%d" mll))
        ('ugrep (format "--width=%d" mll))
        ('ag    (format "--width=%d" mll))
        (_      "")))))

(defconst fzfa--grep-line-regexp "\\`\\(.*?\\):\\([0-9]+\\):"
  "Lazy parser for `fzfa-grep' category candidates.

Group 1 is SOURCE (file path or buffer name).  Group 2 is LINE.
Lazy match anchors on the first `:DIGITS:' boundary so a colon-bearing
buffer name does not corrupt the split.")

(defun fzfa--goto-source (source line)
  "Open SOURCE and jump to LINE.

SOURCE is a file path when `file-exists-p'; otherwise it is treated as
a buffer name.  `current-prefix-arg' of `(4)' (C-u) routes through
`find-file-other-window' / `switch-to-buffer-other-window'; anything
else uses `find-file' / `switch-to-buffer'."
  (let ((other-window-p (equal current-prefix-arg '(4))))
    (cond
     ((file-exists-p source)
      (if other-window-p
          (find-file-other-window source)
        (find-file source)))
     ((get-buffer source)
      (if other-window-p
          (switch-to-buffer-other-window source)
        (switch-to-buffer source)))
     (t (user-error "Source not found: %s" source))))
  (goto-char (point-min))
  (forward-line (1- line)))

(defun fzfa-grep-jump (cand)
  "Open the SOURCE and jump to the LINE referenced by grep candidate CAND.

CAND is a FILE:LINE:CONTENT string as produced by `grep', `rg', `ag',
etc."
  (when (string-match fzfa--grep-line-regexp cand)
    (fzfa--goto-source
     (match-string 1 cand)
     (string-to-number (match-string 2 cand)))))

(defvar-keymap fzfa-grep-map
  :doc "Embark keymap for `fzfa-grep' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'.")

(defun fzfa--grep-group (cand transform)
  "Group function for FILE:LINE:CONTENT grep candidate CAND.

TRANSFORM nil  → return the filename as the section header.
TRANSFORM non-nil → strip the filename prefix; display LINE:CONTENT only."
  (if (string-match fzfa--grep-line-regexp cand)
      (if transform
          (substring cand (match-beginning 2))
        (match-string 1 cand))
    cand))

;;; Location candidates (in-Emacs line search)

(defun fzfa--location-candidate (cand source line)
  "Tag CAND with SOURCE and LINE as an `fzfa-location' text property.

Modifies CAND in place and returns it.  The property is attached at index
0 only so the interval tree stays one node per candidate.  SOURCE is a
file path or buffer name; LINE is the 1-based line number.

Use this when building candidates for the `fzfa-location' category — line
search commands like `fzfa-swiper' where the source should be carried
in-band for jump but never enter fzf's scoring."
  (when (> (length cand) 0)
    (add-text-properties 0 1 `(fzfa-location (,source . ,line)) cand))
  cand)

(defun fzfa-location-jump (cand)
  "Open SOURCE and jump to LINE recorded on CAND's `fzfa-location' property."
  (when-let* ((loc (and (stringp cand) (> (length cand) 0)
                        (get-text-property 0 'fzfa-location cand))))
    (fzfa--goto-source (car loc) (cdr loc))))

(defvar-keymap fzfa-location-map
  :doc "Embark keymap for `fzfa-location' candidates.
Composed with `embark-general-map' via `embark-keymap-alist'.")

;;; Multi-source default action (post-session dispatch)

(defun fzfa--multi-default-action (cand)
  "Dispatch CAND to its source's `:action' stamped at tag time.

Each multi-source candidate's tofu char carries an `fzfa-multi-action'
text property — the per-source action closure from `fzfa-multi-read'.
This lets `embark-collect' route per-candidate even though the
minibuffer (and its `fzfa--candidate->source' dispatch hash) has
already exited.  Without it embark falls back to `embark--command'
\(the entry command, e.g. `fzfa-find-any'), which would just reopen
the picker."
  (when (stringp cand)
    (let* ((n (length cand))
           (action (and (> n 0)
                        (get-text-property (1- n) 'fzfa-multi-action cand))))
      (when action
        (funcall action cand)))))

(defun fzfa--location-group (cand transform)
  "Group function for `fzfa-location' candidate CAND.

TRANSFORM nil  → return the source (file or buffer) as the section header.
TRANSFORM non-nil → return CAND unchanged.
Reads the source off CAND's `fzfa-location' text property; returns CAND
when the property is missing so candidates without locations still
render."
  (if transform
      cand
    (or (when-let* ((loc (and (> (length cand) 0)
                              (get-text-property 0 'fzfa-location cand))))
          (car loc))
        cand)))

;;; Setup

(defun fzfa--bridge-defcustoms (orig-fn &rest args)
  "Wrap fzf-native call ORIG-FN with ARGS; bridge fzfa-* into C scorer."
  (let ((fzf-native-async-highlight  fzfa-highlight)
        (fzf-native-batch-highlight  fzfa-batch-highlight)
        (fzf-native-max-line-length  fzfa-max-line-length)
        (fzf-native-async-cache-size fzfa-cache-size)
        (fzf-native-case-mode        fzfa-case-mode)
        (fzf-native-fuzzy            fzfa-fuzzy))
    (apply orig-fn args)))

(defun fzfa--annotate-file (cand)
  "Marginalia file annotator; skip when `default-directory' is remote.

Wraps `marginalia-annotate-file' with a `file-remote-p' short
circuit so annotating a flood of file candidates under a TRAMP
`default-directory' does not fire a `file-attributes' round-trip
per visible candidate."
  (unless (file-remote-p default-directory)
    (when (fboundp 'marginalia-annotate-file)
      (marginalia-annotate-file cand))))

(defun fzfa--ensure-setup ()
  "Install fzfa's registrations exactly once.

 e.g.
 `fzfa-completing-read', `fzfa-multi-read'."
  (unless fzfa--setup-done
    (setq fzfa--setup-done t)
    (when (fboundp 'fzf-native-ensure-loaded)
      (fzf-native-ensure-loaded))
    (add-to-list 'completion-styles-alist
                 '(fzfa
                   fzfa-try-completion fzfa-all-completions
                   "Passthrough style for pre-scored async fzf completions."))

    (advice-add 'fzf-native-async-start      :around #'fzfa--bridge-defcustoms)
    (advice-add 'fzf-native-async-candidates :around #'fzfa--bridge-defcustoms)

    (with-eval-after-load 'embark
      (dolist (entry '((fzfa-file     . embark-file-map)
                       (fzfa-buffer   . embark-buffer-map)
                       (fzfa-bookmark . embark-bookmark-map)
                       (fzfa-grep     fzfa-grep-map     embark-general-map)
                       (fzfa-location fzfa-location-map embark-general-map)))
        (add-to-list 'embark-keymap-alist entry))
      (setf (alist-get 'fzfa-grep     embark-default-action-overrides)
            (lambda (cand) (fzfa-visit-grep cand)))
      (setf (alist-get 'fzfa-location embark-default-action-overrides)
            (lambda (cand) (fzfa-visit-location cand)))
      (setf (alist-get 'fzfa-multi    embark-default-action-overrides)
            #'fzfa--multi-default-action))

    (with-eval-after-load 'marginalia
      (dolist (entry '((fzfa-file     fzfa--annotate-file          none)
                       (fzfa-buffer   marginalia-annotate-buffer   none)
                       (fzfa-bookmark marginalia-annotate-bookmark none)
                       (fzfa-theme    marginalia-annotate-theme    none)
                       (fzfa-imenu    marginalia-annotate-imenu    none)))
        (add-to-list 'marginalia-annotators entry)))

    (when fzfa-extensions
      (dolist (ext fzfa-extensions)
        (let ((setup-fn (intern (format "fzfa-%s-setup" ext))))
          (when (fboundp setup-fn) (funcall setup-fn)))))))

(provide 'fzfa)
;;; fzfa.el ends here
