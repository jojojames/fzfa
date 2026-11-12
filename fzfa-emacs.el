;;; fzfa-emacs.el --- Built-in completion sources for `fzfa' -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Version: 1.0
;; Homepage: https://github.com/jojojames/fzfa
;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Emacs-internal sources for fzfa: recent files, buffers, the kill
;; ring, bookmarks, themes, and current-buffer / all-buffer line
;; search.
;;
;; Loaded automatically when `emacs' is in `fzfa-extensions' and
;; `fzfa-setup' has been called.  No setup function is registered —
;; the commands are usable immediately.
;;
;; Commands:
;;   `fzfa-recent-file'              Find a recently visited file
;;   `fzfa-buffer'                   Switch to an open buffer
;;   `fzfa-yank-pop'                 Yank from `kill-ring' with fzf
;;   `fzfa-bookmark'                 Jump to a bookmark
;;   `fzfa-theme'                    Enable a theme with live preview
;;   `fzfa-font'                     Pick a font family with live preview
;;   `fzfa-man'                      Pick a man page and open it
;;   `fzfa-swiper'                   Search lines of the current buffer
;;   `fzfa-swiper-all'               Search lines across all open buffers
;;   `fzfa-M-x'                      Run an extended command
;;   `fzfa-M-x-for-buffer'           Run an extended command for current mode
;;   `fzfa-apropos'                  Pick a symbol and describe it
;;   `fzfa-descbinds'                Pick a keybinding and describe / execute
;;   `fzfa-complex-command'          Pick from `command-history' and eval
;;   `fzfa-history'                  Pick from comint / eshell input ring
;;   `fzfa-unicode-char'             Pick a Unicode character by name
;;   `fzfa-minor-mode-menu'          Toggle a minor mode (on/off annotated)
;;   `fzfa-mark'                     Jump to a position in this buffer's marks
;;   `fzfa-global-mark'              Jump to a position in `global-mark-ring'
;;   `fzfa-register'                 Use a register (jump-to or insert)
;;   `fzfa-outline'                  Jump to an outline heading in this buffer
;;   `fzfa-compile-error'            Jump to an error from a compilation buffer
;;   `fzfa-ffap-menu'                Pick a file or URL mentioned in this buffer
;;   `fzfa-frames'                   Switch focus to another live frame
;;   `fzfa-tabs'                     Switch to a tab on the current frame

;;; Code:

(require 'fzfa)
(require 'cl-lib)

(declare-function bookmark-all-names "bookmark")
(declare-function bookmark-maybe-load-default-file "bookmark")
(declare-function outline-next-heading "outline")
(declare-function compilation-next-error "compile" (n &optional types start))
(declare-function compile-goto-error "compile" (&optional event))
(declare-function ffap-menu-rescan "ffap")
(declare-function ffap-guesser "ffap")
(declare-function ffap-url-p "ffap" (string))
(declare-function find-file-at-point "ffap" (&optional filename))
(defvar ffap-menu-alist)
(defvar recentf-list)
;; `ext:' prefix — check-declare skips file existence when the source
;; lives in a C file that may not ship with the installed Emacs (nix
;; builds, some Windows distributions).
(declare-function x-family-fonts "ext:xfaces.c" (&optional family frame))

;;;###autoload
(defun fzfa-recent-file ()
  "Find a recently visited file using `recentf'."
  (interactive)
  (require 'recentf)
  (recentf-mode 1)
  (unless recentf-list
    (user-error "No recent files"))
  (when-let* ((result (fzfa-completing-read :candidates recentf-list
                                                :prompt "recent: "
                                                :category 'fzfa-file)))
    (fzfa-visit-file result)))

;;;###autoload
(defun fzfa-buffer ()
  "Switch to an open buffer."
  (interactive)
  (let* ((names (cl-loop for b in (buffer-list)
                         unless (or (minibufferp b)
                                    (string-prefix-p " " (buffer-name b)))
                         collect (buffer-name b))))
    (when-let* ((result (fzfa-completing-read
                         :candidates names :prompt "buffer: "
                         :category 'fzfa-buffer)))
      (fzfa-visit-buffer result))))

;;;###autoload
(defun fzfa-yank-pop ()
  "Yank from `kill-ring' using fzf for selection.

When invoked immediately after a yank command, replaces the previously
yanked text with the selection (mirroring `yank-pop' / `consult-yank-pop')."
  (interactive "*")
  (unless kill-ring
    (user-error "Kill ring is empty"))
  (let* ((seen (make-hash-table :test 'equal))
         (lookup (make-hash-table :test 'equal))
         (entries
          (cl-loop
           for s in kill-ring
           for clean = (substring-no-properties s)
           unless (or (string-empty-p clean) (gethash clean seen))
           do (puthash clean t seen)
           and collect
           (let ((display
                  (replace-regexp-in-string
                   "\n" (propertize "⏎" 'face 'shadow)
                   (if (> (length clean) 200)
                       (concat (substring clean 0 200) "…")
                     clean))))
             ;; Disambiguate displays collapsed to the same string
             ;; (e.g. entries differing only in stripped properties).
             (while (gethash display lookup)
               (setq display (concat display " ")))
             (puthash display s lookup)
             display))))
    (when-let* ((result (fzfa-completing-read
                         :candidates entries
                         :prompt "yank-pop: "))
                (text (gethash result lookup)))
      (cond
       ((eq last-command 'yank)
        (let ((inhibit-read-only t)
              (pt (point))
              (mk (mark t)))
          (funcall (or yank-undo-function #'delete-region)
                   (min pt mk) (max pt mk))
          (setq yank-undo-function nil)
          (set-marker (mark-marker) pt (current-buffer))
          (insert-for-yank text)))
       (t
        (push-mark)
        (insert-for-yank text)))
      (setq this-command 'yank))))

(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))
(declare-function bookmark-get-position "bookmark" (bookmark-name-or-record))

;;;###autoload
(defun fzfa-bookmark ()
  "Jump to a bookmark."
  (interactive)
  (require 'bookmark)
  (bookmark-maybe-load-default-file)
  (let ((names (bookmark-all-names)))
    (unless names
      (user-error "No bookmarks defined"))
    (when-let* ((result
                 (fzfa-completing-read
                  :candidates names :prompt "bookmark: "
                  :category 'fzfa-bookmark
                  :preview
                  `(:setup
                    ,(lambda ()
                       (fzfa-preview-put :opener (fzfa--temporary-files)))
                    :preview
                    ,(lambda (cand)
                       (when-let* ((file (ignore-errors
                                           (bookmark-get-filename cand)))
                                   ((stringp file))
                                   (path (expand-file-name file))
                                   ((file-readable-p path))
                                   ((not (file-directory-p path)))
                                   (attrs (file-attributes path))
                                   (size (file-attribute-size attrs))
                                   (limit fzfa-preview-file-size-limit)
                                   ((or (null limit) (zerop limit)
                                        (< size limit)))
                                   (opener (fzfa-preview-get :opener))
                                   (buf (funcall opener path))
                                   (pos (or (ignore-errors
                                              (bookmark-get-position cand))
                                            1)))
                         (fzfa-preview-show buf pos)))
                    :return
                    ,(lambda (cand)
                       (when-let* ((opener (fzfa-preview-get :opener)))
                         (when cand
                           (when-let* ((file (ignore-errors
                                               (bookmark-get-filename cand)))
                                       ((stringp file))
                                       (buf (get-file-buffer
                                             (expand-file-name file))))
                             (funcall opener buf)))
                         (funcall opener)))))))
      (fzfa-visit-bookmark result))))

(defun fzfa--theme-switch (sym)
  "Disable currently enabled themes (except SYM) and enable SYM, if any.

SYM nil means leave nothing enabled."
  (dolist (th custom-enabled-themes)
    (unless (eq th sym) (disable-theme th)))
  (when (and sym (not (memq sym custom-enabled-themes)))
    (if (and (custom-theme-p sym) (get sym 'theme-settings))
        (enable-theme sym)
      (load-theme sym :no-confirm))))

(defun fzfa--theme-symbol (cand)
  "Return the theme symbol for CAND, or nil for the \"default\" sentinel."
  (and cand (not (equal cand "default")) (intern cand)))

;;;###autoload
(defun fzfa-theme ()
  "Prompt for a theme to enable, previewing each candidate live.

Aborting (e.g. \\[keyboard-quit]) restores the themes that were enabled
when the command was invoked.  Selecting \"default\" disables all themes."
  (interactive)
  (fzfa-completing-read
   :candidates (cons "default"
                     (mapcar #'symbol-name (custom-available-themes)))
   :prompt "theme: "
   :category 'fzfa-theme
   :apply (lambda (cand)
            (fzfa--theme-switch (fzfa--theme-symbol cand)))
   :preview `(:setup
              ,(lambda ()
                 (fzfa-preview-put :saved
                                   (copy-sequence custom-enabled-themes)))
              :preview
              ,(lambda (cand)
                 (when cand
                   (fzfa--theme-switch (fzfa--theme-symbol cand))))
              :return
              ,(lambda (cand)
                 (if cand
                     (fzfa--theme-switch (fzfa--theme-symbol cand))
                   (mapc #'disable-theme custom-enabled-themes)
                   (mapc #'enable-theme
                         (reverse (fzfa-preview-get :saved))))))))

;;;###autoload
(defun fzfa-font ()
  "Prompt for a font family, previewing each candidate live.

Applies the picked family to the `default' face across all frames via
`set-face-attribute'.  Only `:family' is touched — `:height' and other
attributes are preserved.  Aborting (e.g. \\[keyboard-quit]) restores
the family that was active when the command was invoked.

With `\\[universal-argument]' prefix, filters to monospaced families
via `x-family-fonts's FIXED-P field (index 5); FAMILY (index 0) is a
symbol on some builds, so coerce via `format' to keep the candidate
list uniformly string-shaped."
  (interactive)
  (let* ((mono-p   (equal current-prefix-arg '(4)))
         (families (if mono-p
                       (or (sort (delete-dups
                                  (cl-loop for v in (x-family-fonts)
                                           when (aref v 5)
                                           collect (format "%s" (aref v 0))))
                                 #'string<)
                           (user-error "No monospaced fonts available"))
                     (delete-dups (font-family-list)))))
    (unless families
      (user-error "No fonts available"))
    (fzfa-completing-read
     :candidates families
     :prompt (if mono-p "mono font: " "font: ")
     :category 'fzfa-font
     :apply (lambda (cand)
              (when cand
                (set-face-attribute 'default nil :family cand)))
     :preview `(:setup
                ,(lambda ()
                   (fzfa-preview-put :saved
                                     (face-attribute 'default :family)))
                :preview
                ,(lambda (cand)
                   (when cand
                     (set-face-attribute 'default nil :family cand)))
                :return
                ,(lambda (cand)
                   (if cand
                       (set-face-attribute 'default nil :family cand)
                     (set-face-attribute 'default nil :family
                                         (fzfa-preview-get :saved))))))))

(defcustom fzfa-man-command "apropos ."
  "Shell command used by `fzfa-man' to list man page entries.

Output is expected to be one entry per line in the standard
\"NAME (SECTION) - DESCRIPTION\" format that `apropos' produces.
Selection extracts the first token as the page name and passes it to
`man'."
  :type 'string
  :group 'fzfa)

;;;###autoload
(defun fzfa-man ()
  "Pick a man page and open it with `man'.

Streams entries from `fzfa-man-command' (default `apropos .') as
NAME (SECTION) - DESCRIPTION lines; selection extracts NAME and
funcalls `man'."
  (interactive)
  (when-let* ((r (fzfa-completing-read
                  :command fzfa-man-command
                  :prompt "man: "
                  :category 'fzfa-man)))
    (when (string-match "\\`\\([^ (]+\\)" r)
      (man (match-string 1 r)))))

;;;###autoload
(defun fzfa-swiper ()
  "Search lines of the current buffer using fzf."
  (interactive)
  (let* ((source (or (buffer-file-name) (buffer-name)))
         (candidates
          (let (lines)
            (save-excursion
              (goto-char (point-min))
              (let ((i 1))
                (while (not (eobp))
                  (let ((content (buffer-substring-no-properties
                                  (line-beginning-position)
                                  (line-end-position))))
                    (unless (string-empty-p content)
                      (push (fzfa--location-candidate
                             (format "%d:%s" i content) source i)
                            lines)))
                  (forward-line 1)
                  (cl-incf i))))
            (nreverse lines))))
    (fzfa-visit-location
     (fzfa-completing-read :candidates candidates
                                :prompt "swiper: "
                                :category 'fzfa-location))))

(defcustom fzfa-swiper-all-buffer-exclude-regexp nil
  "Regexp matching buffer names to omit from `fzfa-swiper-all'.
Nil excludes no additional buffers.  Matching buffers are skipped before
their lines are collected.  Minibuffers and buffers whose names begin
with a space are always excluded."
  :type '(choice (const :tag "No additional exclusions" nil) regexp)
  :group 'fzfa)

;;;###autoload
(defun fzfa-swiper-all ()
  "Search lines across all open buffers using fzf.

Candidates display LINE:CONTENT; the originating buffer (file path or
buffer name) is carried on each candidate as an `fzfa-location' text
property and shown as the group header.  fzf scores only against
LINE:CONTENT — buffer names never enter the search input.
See `fzfa-swiper-all-buffer-exclude-regexp' to skip buffers by name."
  (interactive)
  (let* ((buffers (cl-remove-if
                   (lambda (b)
                     (or (minibufferp b)
                         (string-prefix-p " " (buffer-name b))
                         (and fzfa-swiper-all-buffer-exclude-regexp
                              (string-match-p
                               fzfa-swiper-all-buffer-exclude-regexp
                               (buffer-name b)))))
                   (buffer-list)))
         (used (make-hash-table :test 'equal))
         (candidates
          (cl-loop
           for buf in buffers
           for source = (or (buffer-file-name buf) (buffer-name buf))
           append (with-current-buffer buf
                    (let (lines)
                      (save-excursion
                        (goto-char (point-min))
                        (let ((j 1))
                          (while (not (eobp))
                            (let ((content (buffer-substring-no-properties
                                            (line-beginning-position)
                                            (line-end-position))))
                              (unless (string-empty-p content)
                                (let ((display (format "%d:%s" j content)))
                                  (while (gethash display used)
                                    (setq display (concat display " ")))
                                  (puthash display t used)
                                  (push (fzfa--location-candidate
                                         display source j)
                                        lines))))
                            (forward-line 1)
                            (cl-incf j))))
                      (nreverse lines))))))
    (fzfa-visit-location
     (fzfa-completing-read
      :candidates candidates
      :prompt "swiper-all: "
      :category 'fzfa-location
      :group #'fzfa--location-group))))

(defun fzfa--command-not-obsolete-p (sym)
  "Return non-nil if SYM should not be hidden as an obsolete command.
Mirrors the obsolete-command filtering done by
`read-extended-command-1': hide obsolete commands except those that
name a replacement and were obsoleted in this Emacs major version
or later."
  (let ((obsolete (get sym 'byte-obsolete-info)))
    (or (not obsolete)
        (and (functionp (car obsolete))
             (condition-case nil
                 (>= (car (version-to-list (caddr obsolete)))
                     emacs-major-version)
               (error t))))))

(defun fzfa--commands (&optional predicate)
  "Return a sorted list of command names as strings.

When PREDICATE is non-nil, only include commands for which
\(funcall PREDICATE SYMBOL CURRENT-BUFFER) returns non-nil.  This
matches the signature used by `read-extended-command-predicate'.

Obsolete commands are filtered the same way as
`read-extended-command-1'."
  (let ((buffer (current-buffer))
        commands)
    (mapatoms
     (lambda (sym)
       (when (and (commandp sym)
                  (fzfa--command-not-obsolete-p sym)
                  (or (not predicate)
                      (condition-case-unless-debug err
                          (funcall predicate sym buffer)
                        (error
                         (message "fzfa--commands predicate: %s: %s"
                                  sym (error-message-string err))
                         nil))))
         (push (symbol-name sym) commands))))
    (sort commands #'string<)))

(defun fzfa--run-command (name)
  "Execute the command named NAME.

Records it like \\[execute-extended-command]."
  (let ((cmd (intern name)))
    (setq this-command cmd
          real-this-command cmd)
    (command-execute cmd 'record)))

(defcustom fzfa-commands-chunk-size 5000
  "Number of symbols processed per `obarray' chunk in `fzfa-M-x' producers.

Smaller values reduce per-tick blocking time at the cost of more
timer scheduling overhead.  At 5000 each chunk takes roughly
0.5-1ms on a typical commandp+predicate filter, well under one
frame."
  :type 'integer
  :group 'fzfa)

(defun fzfa--commands-producer (buffer predicate)
  "Return a 2-arg `:candidates' producer that walks `obarray' non-blockingly.

BUFFER is the originating buffer; PREDICATE (or nil) is called as
\(funcall PREDICATE SYM BUFFER) like `read-extended-command-predicate'.

The candidate set is query-independent — fzf does the filtering —
so the obarray walk runs exactly once per producer (i.e. once per
`fzfa-completing-read' session), chunked across timer ticks of
`fzfa-commands-chunk-size' symbols each.  Subsequent producer
calls (which `fzfa--source-fetch' fires on every keystroke) reuse
the cached result via the producer's closure state:

  - Before build completes: call is a no-op; fzf keeps filtering
    against the partial snapshot delivered by the last chunk.
  - After build completes: callback fires synchronously with the
    cached sorted list.

The latest pending callback wins — earlier callbacks are replaced
inside the closure when fzf re-invokes the producer, and the
consumer's `prod-token' check makes the replaced ones harmless."
  (let ((symbols nil)
        (results nil)
        (final nil)
        (latest-cb nil)
        (started nil)
        (chunk fzfa-commands-chunk-size))
    (cl-labels
        ((process ()
           (let ((n 0))
             (while (and symbols (< n chunk))
               (let ((sym (pop symbols)))
                 (when (and (commandp sym)
                            (fzfa--command-not-obsolete-p sym)
                            (or (not predicate)
                                (condition-case-unless-debug err
                                    (funcall predicate sym buffer)
                                  (error
                                   (message
                                    "fzfa M-x predicate: %s: %s"
                                    sym (error-message-string err))
                                   nil))))
                   (push (symbol-name sym) results)))
               (cl-incf n)))
           (cond
            (symbols
             (when latest-cb (funcall latest-cb results))
             (run-with-timer 0 nil #'process))
            (t
             (setq final (sort (copy-sequence results) #'string<)
                   ;; Drop the working list so it can be GC'd.
                   results nil)
             (when latest-cb (funcall latest-cb final))))))
      (lambda (_input callback)
        (setq latest-cb callback)
        (cond
         (final
          (funcall callback final))
         (started
          ;; Build in progress.  Don't deliver here; the next chunk
          ;; completion will call `latest-cb' (now CALLBACK).
          nil)
         (t
          (setq started t
                symbols (let (acc)
                          (mapatoms (lambda (s) (push s acc)))
                          acc))
          (run-with-timer 0 nil #'process)))))))

;;;###autoload
(defun fzfa-unicode-char ()
  "Pick a Unicode character by name and insert (or copy) it.

Candidates are NAME<space>GLYPH lines from `ucs-names'.  Selection
inserts the character at point when the buffer is writable, otherwise
copies it to the kill ring."
  (interactive)
  (let* ((table (ucs-names))
         (entries
          (cl-loop
           for name being the hash-keys of table
           using (hash-values code)
           when (and (integerp code) (not (string-empty-p name)))
           collect (propertize (format "%s  %c" name code)
                               'fzfa-unicode-code code))))
    (unless entries
      (user-error "No Unicode names available"))
    (when-let* ((r (fzfa-completing-read
                    :candidates entries
                    :prompt "unicode: "
                    :category 'fzfa-unicode))
                (code (get-text-property 0 'fzfa-unicode-code r)))
      (if (or buffer-read-only (minibufferp))
          (progn (kill-new (string code))
                 (message "Copied: %c" code))
        (insert-char code 1 t)))))

(defvar comint-input-ring)
(defvar eshell-history-ring)
(declare-function ring-elements "ring" (ring))
(declare-function comint-line-beginning-position "comint" ())

;;;###autoload
(defun fzfa-history ()
  "Pick from the current buffer's input history and insert at prompt.

M-r replacement for shell / eshell / any comint-derived mode.  Reads
`eshell-history-ring' under `eshell-mode', otherwise
`comint-input-ring' under `comint-mode' (or derived).  Selection
replaces the current input line."
  (interactive)
  (let ((ring (cond
               ((and (derived-mode-p 'eshell-mode)
                     (bound-and-true-p eshell-history-ring))
                eshell-history-ring)
               ((and (derived-mode-p 'comint-mode)
                     (bound-and-true-p comint-input-ring))
                comint-input-ring)
               (t (user-error "No history ring in %s" major-mode)))))
    (require 'ring)
    (let ((entries (delete-dups (ring-elements ring))))
      (unless entries
        (user-error "History is empty"))
      (when-let* ((r (fzfa-completing-read
                      :candidates entries
                      :prompt "history: "
                      :category 'fzfa-history)))
        (cond
         ((derived-mode-p 'eshell-mode)
          (beginning-of-line)
          (delete-region (point) (point-max))
          (insert r))
         ((derived-mode-p 'comint-mode)
          (goto-char (comint-line-beginning-position))
          (delete-region (point) (line-end-position))
          (insert r)))))))

;;;###autoload
(defun fzfa-complex-command ()
  "Pick from `command-history' and re-eval the selected sexp.

`repeat-complex-command' picker: entries in `command-history' are
Lisp forms already scheduled by the command loop as executable.
Selection `eval's the form (lexical), re-running the command with
its exact arguments."
  (interactive)
  (unless command-history
    (user-error "command-history is empty"))
  (let ((entries
         (cl-loop for sexp in command-history
                  collect (propertize (prin1-to-string sexp)
                                      'fzfa-command-sexp sexp))))
    (when-let* ((r (fzfa-completing-read
                    :candidates entries
                    :prompt "complex-command: "
                    :category 'fzfa-complex-command
                    :history 'command-history)))
      (eval (get-text-property 0 'fzfa-command-sexp r) t))))

;;;###autoload
(defun fzfa-M-x ()
  "Run an extended command using fzf, like \\[execute-extended-command].

Honors `read-extended-command-predicate' so the candidate set
matches what plain \\[execute-extended-command] would show.

Candidates are computed off the main thread via cooperative
timer-chunking — extract returns immediately, and the obarray
walk happens in slices that yield to the input loop between
chunks (see `fzfa--commands-producer')."
  (interactive)
  (when-let* ((result (fzfa-completing-read
                       :candidates (fzfa--commands-producer
                                    (current-buffer)
                                    read-extended-command-predicate)
                       :prompt "M-x: "
                       :category 'command
                       :history 'extended-command-history)))
    (fzfa--run-command result)))

;;;###autoload
(defun fzfa-M-x-for-buffer ()
  "Run an extended command applicable to the current buffer's mode.

Uses the same predicate as `execute-extended-command-for-buffer':
commands marked for the current major/minor modes, plus commands
bound in the buffer's active keymaps.

Candidates are computed off the main thread via cooperative
timer-chunking — extract returns immediately, and the obarray
walk happens in slices that yield to the input loop between
chunks.  The predicate is constructed once at command entry and
captures the originating buffer's keymaps, so each per-symbol
filter call is cheap."
  (interactive)
  (let* ((buf (current-buffer))
         (predicate
          (cond
           ((fboundp 'command-completion--command-for-this-buffer-function)
            (command-completion--command-for-this-buffer-function))
           ((fboundp 'command-completion-default-include-p)
            #'command-completion-default-include-p))))
    (when-let* ((result (fzfa-completing-read
                         :candidates (fzfa--commands-producer buf predicate)
                         :prompt (format "M-x [%s]: " major-mode)
                         :category 'command
                         :history 'extended-command-history)))
      (fzfa--run-command result))))

(defun fzfa--apropos-producer ()
  "Return a 2-arg `:candidates' producer walking `obarray' non-blockingly.

Collects every interned symbol that names a function (`fboundp'),
variable (`boundp'), or face (`facep').  Same cooperative-chunking
pattern as `fzfa--commands-producer' — chunk size is
`fzfa-commands-chunk-size' — so the walk yields between slices."
  (let ((symbols nil)
        (results nil)
        (final nil)
        (latest-cb nil)
        (started nil)
        (chunk fzfa-commands-chunk-size))
    (cl-labels
        ((process ()
           (let ((n 0))
             (while (and symbols (< n chunk))
               (let ((sym (pop symbols)))
                 (when (or (fboundp sym) (boundp sym) (facep sym))
                   (push (symbol-name sym) results)))
               (cl-incf n)))
           (cond
            (symbols
             (when latest-cb (funcall latest-cb results))
             (run-with-timer 0 nil #'process))
            (t
             (setq final (sort (copy-sequence results) #'string<)
                   results nil)
             (when latest-cb (funcall latest-cb final))))))
      (lambda (_input callback)
        (setq latest-cb callback)
        (cond
         (final
          (funcall callback final))
         (started nil)
         (t
          (setq started t
                symbols (let (acc)
                          (mapatoms (lambda (s) (push s acc)))
                          acc))
          (run-with-timer 0 nil #'process)))))))

;;;###autoload
(defun fzfa-apropos ()
  "Pick a symbol (function, variable, or face) and describe it.

Presents every interned symbol that names a function (`fboundp'),
variable (`boundp'), or face (`facep').  Selection calls
`describe-symbol', which routes to `describe-function' /
`describe-variable' / `describe-face' based on what the symbol names."
  (interactive)
  (when-let* ((r (fzfa-completing-read
                  :candidates (fzfa--apropos-producer)
                  :prompt "apropos: "
                  :category 'fzfa-apropos)))
    (describe-symbol (intern r))))

;;;###autoload
(defun fzfa-descbinds ()
  "Pick a keybinding and describe (or execute) its command.

Lists every binding currently active in the origin buffer (as
reported by \\[describe-bindings]).  Selecting a candidate calls
`describe-function' on the bound command; with
`\\[universal-argument]' prefix, calls `command-execute' in the
origin buffer instead."
  (interactive)
  (let* ((origin   (current-buffer))
         (executep (equal current-prefix-arg '(4)))
         (pairs
          (with-temp-buffer
            (let ((standard-output (current-buffer)))
              (describe-buffer-bindings origin))
            (goto-char (point-min))
            (let (out)
              (while (not (eobp))
                (let* ((line (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position)))
                       (tab (string-match "\t" line)))
                  (when tab
                    (let ((key (string-trim (substring line 0 tab)))
                          (cmd (string-trim (substring line (1+ tab)))))
                      (unless (or (string-empty-p key)
                                  (string-empty-p cmd)
                                  (member cmd '("Prefix Command" "??"))
                                  (string-match-p "\\`key\\'" key))
                        (push (cons key cmd) out)))))
                (forward-line 1))
              (nreverse out))))
         (max-key
          (min 40 (apply #'max 0 (mapcar (lambda (p) (length (car p))) pairs))))
         (fmt (format "%%-%ds  %%s" max-key))
         (candidates
          (mapcar (lambda (p)
                    (propertize
                     (format fmt (car p) (cdr p))
                     'fzfa-descbinds-command (cdr p)))
                  pairs)))
    (unless candidates
      (user-error "No bindings collected"))
    (when-let* ((r (fzfa-completing-read
                    :candidates candidates
                    :prompt (if executep "descbinds (execute): " "descbinds: ")
                    :category 'fzfa-descbinds))
                (cmd-name (get-text-property 0 'fzfa-descbinds-command r)))
      (if-let* ((sym (intern-soft cmd-name)))
          (if executep
              (with-current-buffer origin (call-interactively sym))
            (describe-function sym))
        (user-error "Not a resolvable command: %s" cmd-name)))))

;;;###autoload
(defun fzfa-minor-mode-menu ()
  "Toggle a minor mode via fzf.

Candidates are every command-bound symbol in `minor-mode-list'.
Enabled modes sort to the top with an `[on]' prefix; disabled modes
follow with `[off]'.  Selecting calls the mode function via
`call-interactively' — toggling it for buffer-local modes, flipping
the global state for global modes."
  (interactive)
  (cl-labels ((active-p (m) (and (boundp m) (symbol-value m))))
    (let* ((modes (cl-remove-if-not
                   (lambda (m) (and (boundp m) (commandp m)))
                   minor-mode-list))
           (sorted (sort (copy-sequence modes)
                         (lambda (a b)
                           (let ((av (active-p a)) (bv (active-p b)))
                             (cond ((and av (not bv)) t)
                                   ((and bv (not av)) nil)
                                   (t (string< (symbol-name a)
                                               (symbol-name b))))))))
           (cands (mapcar #'symbol-name sorted))
           (affix
            (lambda (cs)
              (mapcar (lambda (c)
                        (let* ((sym (intern-soft c))
                               (on  (and sym (active-p sym))))
                          (list c
                                (propertize (if on "[on]  " "[off] ")
                                            'face (if on 'success 'shadow))
                                "")))
                      cs))))
      (unless cands
        (user-error "No minor modes available"))
      (when-let* ((sel (fzfa-completing-read
                        :candidates cands
                        :prompt "minor mode: "
                        :category 'fzfa-minor-mode
                        :affix affix)))
        (call-interactively (intern sel))))))

(defun fzfa--mark-candidates (markers)
  "Build `fzfa-location' candidates from MARKERS.

Each marker contributes one candidate showing LINE:CONTENT of the
target line, with the originating buffer (file path or buffer name)
and the line number carried as an `fzfa-location' text property.
Disambiguates duplicate display strings with a trailing space so
fzf-native's distinct-candidate guarantee holds."
  (let ((used (make-hash-table :test 'equal)))
    (cl-loop
     for m in markers
     for buf = (and (markerp m) (marker-buffer m))
     for pos = (and (markerp m) (marker-position m))
     when (and buf (buffer-live-p buf) pos)
     collect
     (with-current-buffer buf
       (save-excursion
         (when (and (<= (point-min) pos) (<= pos (point-max)))
           (goto-char pos)
           (let* ((source (or (buffer-file-name) (buffer-name)))
                  (line (line-number-at-pos))
                  (content (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))
                  (display (format "%d:%s" line content)))
             (while (gethash display used)
               (setq display (concat display " ")))
             (puthash display t used)
             (fzfa--location-candidate display source line))))))))

;;;###autoload
(defun fzfa-mark ()
  "Jump to a position in the current buffer's `mark-ring' using fzf.

Includes the buffer's current `mark-marker' as the first candidate.
Selection pushes point onto the mark ring before moving so the prior
position is recoverable with \\[set-mark-command] \\[set-mark-command]."
  (interactive)
  (let* ((markers (cl-remove-if-not
                   (lambda (m) (eq (and (markerp m) (marker-buffer m))
                                   (current-buffer)))
                   (cons (mark-marker) mark-ring)))
         (candidates (cl-remove-if-not #'identity
                                       (fzfa--mark-candidates markers))))
    (unless candidates
      (user-error "No marks in this buffer"))
    (when-let* ((r (fzfa-completing-read
                    :candidates candidates
                    :prompt "mark: "
                    :category 'fzfa-location
                    :group #'fzfa--location-group)))
      (push-mark nil t)
      (fzfa-visit-location r))))

;;;###autoload
(defun fzfa-global-mark ()
  "Jump to a position in `global-mark-ring' using fzf.

Selection switches buffers when needed and moves point to the marked
line.  Dead markers (buffer killed or position out of range) are
silently skipped."
  (interactive)
  (unless global-mark-ring
    (user-error "Global mark ring is empty"))
  (let ((candidates (cl-remove-if-not #'identity
                                      (fzfa--mark-candidates
                                       global-mark-ring))))
    (unless candidates
      (user-error "No live global marks"))
    (when-let* ((r (fzfa-completing-read
                    :candidates candidates
                    :prompt "global-mark: "
                    :category 'fzfa-location
                    :group #'fzfa--location-group)))
      (push-mark nil t)
      (fzfa-visit-location r))))

;;;###autoload
(defun fzfa-register ()
  "Select and use a register from `register-alist' via fzf.

Dispatches by type: position-class registers (markers, frame and
window configurations, framesets, file/file-query, bookmark) jump;
remaining types (text, number, rectangle, …) insert at point.
Falls back to `insert-register' when `jump-to-register' signals."
  (interactive)
  (unless register-alist
    (user-error "No registers set"))
  (let* ((lookup (make-hash-table :test 'equal))
         (used   (make-hash-table :test 'equal))
         (candidates
          (cl-loop
           for (name . val) in register-alist
           for label = (cond
                        ((characterp name) (single-key-description name))
                        ((symbolp name)    (symbol-name name))
                        (t                  (format "%S" name)))
           for preview = (or (and (fboundp 'register-describe-oneline)
                                  (ignore-errors
                                    (substring-no-properties
                                     (register-describe-oneline name))))
                             (replace-regexp-in-string
                              "\n" (propertize "⏎" 'face 'shadow)
                              (format "%S" val)))
           for display = (format "[%s] %s" label preview)
           collect
           (progn
             (while (gethash display used)
               (setq display (concat display " ")))
             (puthash display t used)
             (puthash display name lookup)
             display))))
    (when-let* ((r    (fzfa-completing-read
                       :candidates candidates
                       :prompt "register: "
                       :category 'fzfa-misc))
                (name (gethash r lookup)))
      (fzfa-with-visit
        (condition-case _
            (jump-to-register name)
          (error (insert-register name)))))))

;;;###autoload
(defun fzfa-outline ()
  "Jump to an outline heading in the current buffer using fzf.

Walks every line matching `outline-regexp' (set by `outline-mode',
`outline-minor-mode', or the major mode's own definition — most
programming modes set one).  Selection pushes point onto the mark
ring and moves to the heading line.

Candidates display LINE:CONTENT; the source buffer/file is carried as
an `fzfa-location' text property so fzf scores only the heading text
and line number."
  (interactive)
  (require 'outline)
  (unless (and (boundp 'outline-regexp) outline-regexp)
    (user-error "No `outline-regexp' set in this buffer"))
  (let* ((source (or (buffer-file-name) (buffer-name)))
         (used (make-hash-table :test 'equal))
         (candidates
          (save-excursion
            (goto-char (point-min))
            (let (out)
              (while (outline-next-heading)
                (let* ((line    (line-number-at-pos))
                       (content (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                       (display (format "%d:%s" line content)))
                  (while (gethash display used)
                    (setq display (concat display " ")))
                  (puthash display t used)
                  (push (fzfa--location-candidate display source line)
                        out)))
              (nreverse out)))))
    (unless candidates
      (user-error "No outline headings in buffer"))
    (when-let* ((r (fzfa-completing-read
                    :candidates candidates
                    :prompt "outline: "
                    :category 'fzfa-location)))
      (push-mark nil t)
      (fzfa-visit-location r))))

;;;###autoload
(defun fzfa-compile-error ()
  "Jump to an error from a compilation buffer using fzf.

Walks the current buffer when it derives from `compilation-mode',
otherwise the buffer returned by `next-error-find-buffer' (typically
the most recent compile/grep buffer).  Each line carrying a
`compilation-message' text property contributes a candidate showing
that line's text verbatim.

Selection pops to the compile buffer at the chosen error and
invokes `compile-goto-error', which resolves the source location
via the compile buffer's own `default-directory' and error
regexp — so relative paths and unconventional formats work as long
as `compile' itself can navigate them."
  (interactive)
  (require 'compile)
  (let ((buffer (or (and (derived-mode-p 'compilation-mode) (current-buffer))
                    (next-error-find-buffer))))
    (unless (and buffer (buffer-live-p buffer))
      (user-error "No compilation buffer"))
    (let* ((lookup (make-hash-table :test 'equal))
           (used   (make-hash-table :test 'equal))
           (candidates
            (with-current-buffer buffer
              (save-excursion
                (goto-char (point-min))
                (let (out)
                  (while (condition-case nil
                             (progn (compilation-next-error 1) t)
                           (error nil))
                    (let* ((content (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))
                           (display (if (string-empty-p content)
                                        (format "<line %d>"
                                                (line-number-at-pos))
                                      content)))
                      (while (gethash display used)
                        (setq display (concat display " ")))
                      (puthash display t used)
                      (puthash display (point) lookup)
                      (push display out)))
                  (nreverse out))))))
      (unless candidates
        (user-error "No errors in %s" (buffer-name buffer)))
      (when-let* ((r   (fzfa-completing-read
                        :candidates candidates
                        :prompt (format "compile-error[%s]: "
                                        (buffer-name buffer))
                        :category 'fzfa-misc))
                  (pos (gethash r lookup)))
        (fzfa-with-visit
          (pop-to-buffer buffer)
          (goto-char pos)
          (compile-goto-error))))))

;;;###autoload
(defun fzfa-ffap-menu (&optional rescan)
  "Pick a file or URL mentioned in this buffer using fzf, then visit it.

Scans the buffer with `ffap-menu-rescan' (cached buffer-locally in
`ffap-menu-alist'), then prompts via `fzfa-completing-read'.  Selection
pushes point onto the mark ring, jumps to the candidate's position, and
visits the guess: URLs go through `find-file-at-point' (i.e. the user's
`ffap-url-fetcher'), and file paths go through `fzfa-visit-file' so the
`fzfa-file' category action in `fzfa-action-config' is honored.

Candidates display as LINE:GUESS so fzf can score against either the
file/URL or the line number.  Previews scroll the originating buffer to
the candidate's position.

With prefix RESCAN, force a rebuild of the menu cache."
  (interactive "P")
  (require 'ffap)
  (when (or (not ffap-menu-alist) rescan
            (let ((first (car ffap-menu-alist)))
              (save-excursion
                (goto-char (cdr first))
                (not (equal (car first) (ffap-guesser))))))
    (ffap-menu-rescan))
  (unless ffap-menu-alist
    (user-error "No files or URLs in this buffer"))
  (let* ((buf (current-buffer))
         (lookup (make-hash-table :test 'equal))
         (used   (make-hash-table :test 'equal))
         (candidates
          (cl-loop
           for (item . pos) in ffap-menu-alist
           for line = (line-number-at-pos pos t)
           for display = (format "%d:%s" line item)
           do (while (gethash display used)
                (setq display (concat display " ")))
           do (puthash display t used)
           do (puthash display (cons item pos) lookup)
           collect display)))
    (when-let* ((result (fzfa-completing-read
                         :candidates candidates
                         :prompt "ffap-menu: "
                         :category 'fzfa-misc
                         :preview
                         (lambda (cand)
                           (when-let* ((hit (gethash cand lookup)))
                             (fzfa-preview-show buf (cdr hit))))))
                (hit (gethash result lookup)))
      (push-mark nil t)
      (goto-char (cdr hit))
      (let ((target (car hit)))
        (if (ffap-url-p target)
            (fzfa-with-visit (find-file-at-point target))
          (fzfa-visit-file target))))))

;;;###autoload
(defun fzfa-frames ()
  "Pick a live Emacs frame other than the current one and switch focus to it.

Candidates are visible and iconified frames excluding the
currently-selected frame.  Selecting via RET calls
`select-frame-set-input-focus' (deiconifying first when needed).
The apply key (`fzfa-apply-key', default \\[fzfa-apply-current])
focuses without exiting the picker — useful for visually
confirming which frame the candidate refers to before committing.

Previews show the candidate frame's `selected-window' buffer in the
originating window, so the picker keeps focus while you browse."
  (interactive)
  (let* ((current (selected-frame))
         (all (frame-list))
         (frames (cl-remove-if
                  (lambda (f)
                    (or (eq f current)
                        (frame-parameter f 'no-accept-focus)
                        (not (memq (frame-visible-p f) '(t icon)))))
                  all))
         (lookup (make-hash-table :test 'equal))
         (used   (make-hash-table :test 'equal))
         (candidates
          (cl-loop
           for f in frames
           for name = (or (frame-parameter f 'name)
                          (format "Frame-%d"
                                  (1+ (cl-position f all))))
           for buf  = (buffer-name (window-buffer (frame-selected-window f)))
           for vis  = (if (eq (frame-visible-p f) 'icon)
                          "iconified"
                        "visible")
           for display = (format "[%s]  %s  %dx%d  %s"
                                 name buf
                                 (frame-width f) (frame-height f)
                                 vis)
           do (progn
                (while (gethash display used)
                  (setq display (concat display " ")))
                (puthash display t used)
                (puthash display f lookup))
           collect display)))
    (unless candidates
      (user-error "No other frames"))
    (cl-labels
        ((select-cand-frame (cand)
           (when-let* ((f (gethash cand lookup)) ((frame-live-p f)))
             (when (eq (frame-visible-p f) 'icon)
               (make-frame-visible f))
             (fzfa-with-visit (select-frame-set-input-focus f))))
         (preview-frame (cand)
           (when-let* ((f (gethash cand lookup))
                       ((frame-live-p f))
                       (win (frame-selected-window f))
                       ((window-live-p win))
                       (buf (window-buffer win))
                       ((buffer-live-p buf)))
             (fzfa-preview-show buf (window-point win)))))
      (when-let* ((result (fzfa-completing-read
                           :candidates candidates
                           :prompt "frame: "
                           :category 'fzfa-frame
                           :apply #'select-cand-frame
                           :preview #'preview-frame)))
        (select-cand-frame result)))))

;;;###autoload
(defun fzfa-tabs ()
  "Pick a tab on the current frame and switch to it.

Candidates include every tab on the selected frame, marking the
active one.  Selecting via RET calls `tab-bar-select-tab' with
the chosen tab's 1-based index.  The apply key (`fzfa-apply-key',
default \\[fzfa-apply-current]) switches without exiting the picker,
so you can hop between tabs visually before committing.

Previews show a representative buffer for the candidate tab — the
current buffer for the active tab, or the head of the tab's saved
buffer list for the others."
  (interactive)
  (require 'tab-bar)
  (let* ((tabs (tab-bar-tabs))
         (lookup (make-hash-table :test 'equal))
         (used   (make-hash-table :test 'equal))
         (candidates
          (cl-loop
           for tab in tabs
           for i from 1
           unless (eq (car tab) 'current-tab)
           collect
           (let* ((name (or (alist-get 'name tab) (format "Tab-%d" i)))
                  (b (car (alist-get 'wc-bl tab)))
                  (buf (and (buffer-live-p b) (buffer-name b)))
                  (display (format "[%d]  %s%s"
                                   i name
                                   (if (and buf (not (equal buf name)))
                                       (format "  %s" buf)
                                     ""))))
             (while (gethash display used)
               (setq display (concat display " ")))
             (puthash display t used)
             (puthash display i lookup)
             display))))
    (unless candidates
      (user-error "No other tabs"))
    (cl-labels
        ((tab-buffer (i)
           (car (alist-get 'wc-bl (nth (1- i) tabs))))
         (switch-tab (cand)
           (when-let* ((i (gethash cand lookup)))
             (fzfa-with-visit (tab-bar-select-tab i))))
         (preview-tab (cand)
           (when-let* ((i (gethash cand lookup))
                       (buf (tab-buffer i))
                       ((buffer-live-p buf)))
             (fzfa-preview-show buf))))
      (when-let* ((result (fzfa-completing-read
                           :candidates candidates
                           :prompt "tab: "
                           :category 'fzfa-tab
                           :apply #'switch-tab
                           :preview #'preview-tab)))
        (switch-tab result)))))

;;; Browse files (directory-navigating file picker)

(defvar fzfa-browse-files--outermost-dir nil
  "Non-nil while an outermost `fzfa-browse-files' invocation is in flight.
Bound to the initial directory by the first call; recursive
descents (dir → dir) leave it untouched so the whole loop counts
as one browse for the caller's mental model.")

(defun fzfa-browse-files--parent (dir)
  "Return DIR's parent directory as a slash-terminated path."
  (file-name-as-directory
   (file-name-directory (directory-file-name dir))))

(defvar fzfa-browse-files--parent-nav-requested nil
  "Set by `fzfa-browse-files--parent-nav' when DEL requests parent nav.
`fzfa-browse-files--pick' reads this in its `quit' handler to
distinguish the parent-nav abort from a user C-g abort.")

(defun fzfa-browse-files--filter ()
  "Return the user's typed filter, ignoring frontend-injected candidates.

Under ivy the minibuffer buffer contains the candidate list too, so
`minibuffer-contents-no-properties' overreports.  `ivy-text' holds
just the user's typed line — gate on `ivy-mode' (the symbol may be
bound with a stale value under other frontends).

Under vertico / icomplete / helm the user's input lives in the
minibuffer buffer directly; take everything from
`minibuffer-prompt-end' to the next newline (or `point-max').  Do
NOT use `helm-pattern' — helm updates it lazily on its own hooks
so it lags user keystrokes by a tick, and a DEL fired between the
keystroke and the update would misread the filter as empty."
  (cond
   ((and (bound-and-true-p ivy-mode) (boundp 'ivy-text)) ivy-text)
   (t
    (let* ((beg (minibuffer-prompt-end))
           (end (save-excursion
                  (goto-char beg)
                  (if (search-forward "\n" nil t) (1- (point))
                    (point-max)))))
      (buffer-substring-no-properties beg end)))))

(defun fzfa-browse-files--parent-nav ()
  "DEL handler in the browse picker.

Empty filter → set the parent-nav flag and `abort-recursive-edit'
so `completing-read' unwinds cleanly; the outer `--pick' catches
the quit and dispatches back to `--loop' for a parent recursion.
Non-empty filter → default backward delete."
  (interactive)
  (if (string-empty-p (fzfa-browse-files--filter))
      (progn
        (setq fzfa-browse-files--parent-nav-requested t)
        (abort-recursive-edit))
    (call-interactively #'delete-backward-char)))

(defun fzfa-browse-files--install-parent-nav-key ()
  "Bind backspace to `fzfa-browse-files--parent-nav' in the browse minibuffer.
Installs via a per-instance child of `current-local-map' so the
frontend's shared keymap isn't mutated.  Uses `[remap
delete-backward-char]' (plus `[remap ivy-backward-delete-char]'
for ivy's own remap) so our binding wins over ivy's DEL remap
regardless of resolution order."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map (current-local-map))
    (define-key map (kbd "DEL") #'fzfa-browse-files--parent-nav)
    (define-key map [remap delete-backward-char]
                #'fzfa-browse-files--parent-nav)
    (define-key map [remap ivy-backward-delete-char]
                #'fzfa-browse-files--parent-nav)
    (use-local-map map)))

(defun fzfa-browse-files--entries (dir)
  "Return the browse candidates for DIR: `../', `./', then DIR's entries.

Directory entries get a `/' suffix (matching `ls -p`).  Symlinks
inherit the target's kind — a symlink to a directory gets `/'.
Read synchronously via `directory-files-and-attributes' — one
syscall + stat per entry, no shell fork.  Sub-millisecond for
typical browse dirs; browsing multi-thousand-entry dirs is
uncommon enough that we keep the simpler single-code-path here."
  (append '("../" "./")
          (delq nil
                (mapcar
                 (lambda (item)
                   (let ((name (car item))
                         (type (file-attribute-type (cdr item))))
                     (unless (or (equal name ".") (equal name ".."))
                       (cond
                        ((eq type t) (concat name "/"))
                        ((stringp type)
                         (if (file-directory-p
                              (expand-file-name name dir))
                             (concat name "/")
                           name))
                        (t name)))))
                 (directory-files-and-attributes dir nil nil t)))))

(defconst fzfa-browse-files--prompt-label "browse: "
  "Fixed label rendered by `fzfa-browse-files--format-prompt'.

Baked into the formatter rather than passed through fzfa's
`:prompt' plumbing: under ivy, fzfa's pre-prompt code strips
`:prompt' to avoid double-rendering it against ivy's own trailing
prompt insertion.  Passing an empty `:prompt' to
`fzfa-completing-read' makes ivy insert nothing extra, and the
label sits where we want it — between the counts and the DIR.")

(defun fzfa-browse-files--format-prompt (data)
  "Prompt formatter for `fzfa-browse-files'.

Renders \"IDX/[FILTERED](TOTAL) browse: DIR\" so DIR sits
immediately before the cursor — the user sees the current path
as the input line's prefix, matching helm/vertico find-file
conventions.  The label is hardcoded (see
`fzfa-browse-files--prompt-label')."
  (concat
   (fzfa--format-stats "" (plist-get data :index)
                       (plist-get data :filtered)
                       (plist-get data :total))
   fzfa-browse-files--prompt-label
   (plist-get data :directory)))

(defun fzfa-browse-files--completing-read-prompt (dir)
  "Return the `:prompt' string to pass to `fzfa-completing-read' for DIR.

Ivy → empty: ivy inserts <pre-prompt><head><tail> at the top of
the minibuffer, so an empty prompt makes ivy add nothing after
our formatter output — cursor lands right after DIR.

Vertico / icomplete → single space: fzfa's stats overlay needs a
non-empty range to anchor its `display' property; a lone space
gets replaced by the formatter output and the cursor sits at
`minibuffer-prompt-end' (visually right after DIR).

Helm → \"browse: DIR\" bakeform: helm doesn't use fzfa's overlay
\(prompt is set once via `helm :prompt' and stays static), so we
inline the label + dir into the prompt itself.  Counts don't
appear (helm shows candidate counts in its mode line already)."
  (cond
   ((bound-and-true-p ivy-mode) "")
   ((bound-and-true-p helm-mode)
    (concat fzfa-browse-files--prompt-label
            (abbreviate-file-name dir)))
   (t " ")))

(defvar helm-map)

(defun fzfa-browse-files--helm-map ()
  "Return a copy of `helm-map' with our parent-nav DEL bindings.

Helm sets its minibuffer keymap via `read-from-minibuffer''s keymap
argument (using the dynamically-scoped `helm-map'), and its own setup
does not surface through `minibuffer-with-setup-hook' the way
`fzfa--read' does — so our `--install-parent-nav-key' hook doesn't
reliably win under helm.  Let-bind `helm-map' to a pre-augmented copy
around the `fzfa-completing-read' call so the bindings are baked in
before helm reads the keymap."
  (let ((m (copy-keymap helm-map)))
    (define-key m (kbd "DEL") #'fzfa-browse-files--parent-nav)
    (define-key m [remap delete-backward-char]
                #'fzfa-browse-files--parent-nav)
    (define-key m [remap helm-delete-minibuffer-contents]
                #'fzfa-browse-files--parent-nav)
    m))

(defun fzfa-browse-files--pick (dir)
  "Show the browse picker anchored at DIR; return the chosen entry.

Returns the selection string on normal exit, `:parent' when the
DEL handler requested parent nav, or re-signals `quit' when the
user aborted."
  (setq fzfa-browse-files--parent-nav-requested nil)
  (let ((result
         (condition-case _
             ;; `:append' so our hook runs LAST — under fido-mode /
             ;; icomplete-fido-mode, `icomplete--fido-mode-setup' does
             ;; a late `(use-local-map (make-composed-keymap
             ;; icomplete-fido-mode-map ...))' that composes fido's
             ;; DEL binding on top of the local map.  Running after it
             ;; keeps our binding on top.
             (minibuffer-with-setup-hook
                 (:append #'fzfa-browse-files--install-parent-nav-key)
               (let* ((fzfa-prompt-function
                       #'fzfa-browse-files--format-prompt)
                      ;; Force `this-command' so nested picker
                      ;; invocations (each --pick opens a fresh
                      ;; minibuffer) capture `fzfa-browse-files' as
                      ;; their `entry-command'.  Otherwise every
                      ;; subdir-nav gets whatever key the user pressed
                      ;; (vertico-directory-enter, icomplete-fido-ret,
                      ;; ...) as entry-command — bypassing
                      ;; `fzfa-sessions-exclude-commands' and
                      ;; polluting the replay ring with sessions that
                      ;; call frontend-exit commands at replay time
                      ;; and error with `(no-catch exit nil)'.
                      (this-command 'fzfa-browse-files)
                      (real-this-command 'fzfa-browse-files)
                      ;; Under helm-mode, layer DEL onto a copy of
                      ;; `helm-map' so helm's `:keymap' argument sees
                      ;; our bindings.
                      (helm-map (if (and (bound-and-true-p helm-mode)
                                         (boundp 'helm-map))
                                    (fzfa-browse-files--helm-map)
                                  (bound-and-true-p helm-map))))
                 (fzfa-completing-read
                  :candidates (fzfa-browse-files--entries dir)
                  :directory dir
                  :prompt (fzfa-browse-files--completing-read-prompt dir)
                  :category 'fzfa-file
                  :resolve-paths nil
                  :require-match nil)))
           ;; Under vertico/icomplete, `abort-recursive-edit' from our
           ;; DEL handler signals `quit' — catch and inspect the flag.
           ;; Helm swallows the quit internally and returns nil, so we
           ;; also check the flag on the normal return below.
           (quit :fzfa-browse-files-quit))))
    (cond
     (fzfa-browse-files--parent-nav-requested :parent)
     ((eq result :fzfa-browse-files-quit) (signal 'quit nil))
     (t result))))

(defun fzfa-browse-files--dispatch (dir entry)
  "Dispatch ENTRY (from the picker) relative to DIR.

`../' recurses into the parent, `./' opens DIR as dired, an entry
suffixed with `/' recurses into that subdirectory, and everything
else is passed to `find-file' (creates the file if it doesn't
exist, matching C-x C-f semantics)."
  (cond
   ((string= entry "../")
    (fzfa-browse-files--loop (fzfa-browse-files--parent dir)))
   ((string= entry "./")
    (dired dir))
   ((string-suffix-p "/" entry)
    (fzfa-browse-files--loop (expand-file-name entry dir)))
   (t
    (find-file (expand-file-name entry dir)))))

(defun fzfa-browse-files--loop (dir)
  "Pick from DIR; dispatch or recurse per the choice.

DEL-on-empty in the picker returns `:parent' via
`abort-recursive-edit' + `fzfa-browse-files--parent-nav-requested'
— that branch recurses into the parent (equivalent to picking
`../') without needing frontend-specific bindings."
  (let ((result (fzfa-browse-files--pick dir)))
    (cond
     ((eq result :parent)
      (fzfa-browse-files--loop (fzfa-browse-files--parent dir)))
     ((null result) nil)
     (t (fzfa-browse-files--dispatch dir result)))))

;;;###autoload
(defun fzfa-browse-files (&optional dir)
  "Browse DIR (default `default-directory') via fzfa; open the pick.

The picker loops: selecting a subdirectory recurses into it,
selecting a file opens it via `find-file'.  Also:
- `../' recurses into the parent (also bound to `DEL' on empty
  filter).
- `./' opens the current directory as dired.
- Hidden files are included (matches C-x C-f).

Excluded from the replay ring per-recursion to avoid dedup-key
pollution.  Every dispatch call inside the loop counts as one
fzfa session; excluding at the command level keeps browsing out
of memory / on-disk replay entirely."
  (interactive)
  (let* ((outermost-p (null fzfa-browse-files--outermost-dir))
         (dir (file-name-as-directory
               (expand-file-name (or dir default-directory))))
         (fzfa-browse-files--outermost-dir
          (or fzfa-browse-files--outermost-dir dir)))
    (unwind-protect
        (fzfa-browse-files--loop dir)
      (when outermost-p
        (setq fzfa-browse-files--outermost-dir nil)))))

(provide 'fzfa-emacs)

;; Local Variables:
;; package-lint-main-file: "fzfa.el"
;; End:

;;; fzfa-emacs.el ends here
