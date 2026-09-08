;;; fzfa-test.el --- Tests for fzfa  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen

;; Assisted-by: Claude:claude-opus-4-7
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for fzfa.

;;; Code:

(require 'ert)
(require 'fzfa)
(require 'fzfa-loader)
(require 'fzfa-emacs)
(require 'fzfa-tramp)
(require 'fzfa-hungry)
(require 'fzfa-replay)
(require 'fzfa-locate)
(require 'fzfa-helm)

(defvar helm-alive-p)
(defvar helm-pattern)
(defvar helm-map)
(defvar helm-after-update-hook)
(defvar helm-move-selection-after-hook)

;;; fzfa-hungry--deduplicate-dirs

(ert-deftest fzfa-hungry-deduplicate-dirs-no-overlap ()
  "Unrelated directories are all kept."
  (should (equal (sort (fzfa-hungry--deduplicate-dirs
                        '("/a/b/" "/c/d/" "/e/f/"))
                       #'string<)
                 '("/a/b/" "/c/d/" "/e/f/"))))

(ert-deftest fzfa-hungry-deduplicate-dirs-drops-subdirectory ()
  "A subdirectory is dropped when its parent is present."
  (should (equal (fzfa-hungry--deduplicate-dirs
                  '("/home/user/project/" "/home/user/project/src/"))
                 '("/home/user/project/"))))

(ert-deftest fzfa-hungry-deduplicate-dirs-keeps-sibling-dirs ()
  "Sibling directories (same parent, different names) are both kept."
  (let ((result (fzfa-hungry--deduplicate-dirs
                 '("/home/user/foo/" "/home/user/bar/"))))
    (should (member "/home/user/foo/" result))
    (should (member "/home/user/bar/" result))))

(ert-deftest fzfa-hungry-deduplicate-dirs-removes-exact-duplicates ()
  "Exact duplicate entries are collapsed to one."
  (should (equal (fzfa-hungry--deduplicate-dirs
                  '("/a/b/" "/a/b/" "/a/b/"))
                 '("/a/b/"))))

(ert-deftest fzfa-hungry-deduplicate-dirs-deep-nesting ()
  "Only the shallowest ancestor survives when multiple levels are present."
  (let ((result (fzfa-hungry--deduplicate-dirs
                 '("/a/" "/a/b/" "/a/b/c/" "/a/b/c/d/"))))
    (should (equal result '("/a/")))))

(ert-deftest fzfa-hungry-deduplicate-dirs-empty-input ()
  "Empty input returns nil."
  (should (null (fzfa-hungry--deduplicate-dirs '()))))

;;; fzfa-candidate-directory / fzfa-resolve-candidate

(defun fzfa-test--make-session (specs candidates)
  "Build an `fzfa-session' with SPECS and CANDIDATES.

SPECS is a list of source plists.  CANDIDATES is an alist of
\(CAND-STRING . SOURCE-IDX)."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (c candidates) (puthash (car c) (cdr c) h))
    (fzfa-session-create :specs (vconcat specs) :cand->src h)))

;;;; fzfa-candidate-directory

(ert-deftest fzfa-candidate-directory-nil-session ()
  "Returns nil when session is nil."
  (should (null (fzfa-candidate-directory "foo.txt" nil))))

(ert-deftest fzfa-candidate-directory-unknown-candidate ()
  "Returns nil when candidate is not in the source map."
  (let ((s (fzfa-test--make-session
            '((:command "fd" :directory "/root/" :resolve-paths auto))
            '(("foo.txt" . 0)))))
    (should (null (fzfa-candidate-directory "not-known" s)))))

(ert-deftest fzfa-candidate-directory-command-auto ()
  ":command source + :resolve-paths auto → returns :directory."
  (let ((s (fzfa-test--make-session
            '((:command "fd" :directory "/root/" :resolve-paths auto))
            '(("foo.txt" . 0)))))
    (should (equal (fzfa-candidate-directory "foo.txt" s) "/root/"))))

(ert-deftest fzfa-candidate-directory-candidates-auto ()
  ":candidates source + :resolve-paths auto → nil."
  (let ((s (fzfa-test--make-session
            '((:candidates ("modus") :directory "/anywhere/" :resolve-paths auto))
            '(("modus" . 0)))))
    (should (null (fzfa-candidate-directory "modus" s)))))

(ert-deftest fzfa-candidate-directory-explicit-t ()
  "Explicit :resolve-paths t returns :directory even without :command."
  (let ((s (fzfa-test--make-session
            '((:candidates ("a.txt") :directory "/root/" :resolve-paths t))
            '(("a.txt" . 0)))))
    (should (equal (fzfa-candidate-directory "a.txt" s) "/root/"))))

(ert-deftest fzfa-candidate-directory-explicit-nil ()
  "Explicit :resolve-paths nil overrides auto-t on :command sources."
  (let ((s (fzfa-test--make-session
            '((:command "fd" :directory "/root/" :resolve-paths nil))
            '(("foo.txt" . 0)))))
    (should (null (fzfa-candidate-directory "foo.txt" s)))))

(ert-deftest fzfa-candidate-directory-multi-source ()
  "Each candidate resolves against its OWN source's directory."
  (let ((s (fzfa-test--make-session
            '((:command "fd" :directory "/videos/" :resolve-paths auto)
              (:command "fd" :directory "/docs/"   :resolve-paths auto))
            '(("movie.mkv" . 0) ("paper.pdf" . 1)))))
    (should (equal (fzfa-candidate-directory "movie.mkv" s) "/videos/"))
    (should (equal (fzfa-candidate-directory "paper.pdf" s) "/docs/"))))

(ert-deftest fzfa-candidate-directory-nil-directory ()
  "Source with nil :directory returns nil."
  (let ((s (fzfa-test--make-session
            '((:command "fd" :directory nil :resolve-paths auto))
            '(("foo.txt" . 0)))))
    (should (null (fzfa-candidate-directory "foo.txt" s)))))

;;;; fzfa-resolve-candidate

(ert-deftest fzfa-resolve-candidate-nil-session ()
  "Returns cand unchanged when session is nil."
  (should (equal (fzfa-resolve-candidate "foo.txt" nil) "foo.txt")))

(ert-deftest fzfa-resolve-candidate-path-source ()
  "Relative candidate is expanded against source's :directory."
  (let ((s (fzfa-test--make-session
            '((:command "fd" :directory "/videos/" :resolve-paths auto))
            '(("movie.mkv" . 0)))))
    (should (equal (fzfa-resolve-candidate "movie.mkv" s)
                   (expand-file-name "movie.mkv" "/videos/")))))

(ert-deftest fzfa-resolve-candidate-non-path-source ()
  "Non-path source returns candidate unchanged."
  (let ((s (fzfa-test--make-session
            '((:candidates ("modus") :directory "/anywhere/" :resolve-paths auto))
            '(("modus" . 0)))))
    (should (equal (fzfa-resolve-candidate "modus" s) "modus"))))

(ert-deftest fzfa-resolve-candidate-absolute-passthrough ()
  "Already-absolute candidate returns unchanged."
  (let* ((path (expand-file-name "place.txt" "/other/"))
         (s (fzfa-test--make-session
             '((:command "fd" :directory "/root/" :resolve-paths auto))
             `((,path . 0)))))
    (should (equal (fzfa-resolve-candidate path s) path))))

(ert-deftest fzfa-resolve-candidate-grep-suffix ()
  "FILE:LINE:CONTENT candidates keep their suffix; only FILE is expanded."
  (let ((s (fzfa-test--make-session
            '((:command "rg" :directory "/proj/" :resolve-paths auto))
            '(("src/foo.el:42:  (message \"hi\")" . 0)))))
    (should (equal (fzfa-resolve-candidate "src/foo.el:42:  (message \"hi\")" s)
                   (expand-file-name "src/foo.el:42:  (message \"hi\")"
                                     "/proj/")))))

(ert-deftest fzfa-resolve-candidate-multi-source ()
  "Different candidates resolve against their own source's dir."
  (let ((s (fzfa-test--make-session
            '((:command "fd" :directory "/videos/" :resolve-paths auto)
              (:command "fd" :directory "/docs/"   :resolve-paths auto))
            '(("movie.mkv" . 0) ("paper.pdf" . 1)))))
    (should (equal (fzfa-resolve-candidate "movie.mkv" s)
                   (expand-file-name "movie.mkv" "/videos/")))
    (should (equal (fzfa-resolve-candidate "paper.pdf" s)
                   (expand-file-name "paper.pdf" "/docs/")))))

(ert-deftest fzfa-resolve-candidate-ambient-dir-irrelevant ()
  "Regression: resolves to source's dir regardless of `default-directory'."
  (let ((s (fzfa-test--make-session
            '((:command "fd" :directory "/videos/" :resolve-paths auto))
            '(("movie.mkv" . 0)))))
    (let ((default-directory "/completely/unrelated/"))
      (should (equal (fzfa-resolve-candidate "movie.mkv" s)
                     (expand-file-name "movie.mkv" "/videos/"))))))

(ert-deftest fzfa-resolve-candidate-empty-string ()
  "Empty candidate string is returned unchanged."
  (let ((s (fzfa-test--make-session
            '((:command "fd" :directory "/root/" :resolve-paths auto))
            '(("" . 0)))))
    (should (equal (fzfa-resolve-candidate "" s) ""))))

(ert-deftest fzfa-resolve-candidate-strips-tofu ()
  "Multi-source tofu-suffixed candidates resolve without the suffix."
  (let* ((clean "foo.txt")
         (tagged (concat clean (fzfa--tofu-suffix 0)))
         (s (fzfa-test--make-session
             '((:command "fd" :directory "/root/" :resolve-paths auto))
             `((,tagged . 0)))))
    (should (equal (fzfa-resolve-candidate tagged s)
                   (expand-file-name clean "/root/")))))

;;;; fzfa-session-source-of

(ert-deftest fzfa-session-source-of-basic ()
  "Returns the emitting source's plist."
  (let ((s (fzfa-test--make-session
            '((:command "fd" :directory "/videos/" :resolve-paths auto)
              (:command "fd" :directory "/docs/"   :resolve-paths auto))
            '(("movie.mkv" . 0) ("paper.pdf" . 1)))))
    (should (equal (plist-get (fzfa-session-source-of s "movie.mkv")
                              :directory)
                   "/videos/"))
    (should (equal (plist-get (fzfa-session-source-of s "paper.pdf")
                              :directory)
                   "/docs/"))
    (should (null (fzfa-session-source-of s "not-in-map")))))

;;; fzfa--default-dir

(ert-deftest fzfa-project-dir-nil-backend-returns-default-directory ()
  "With nil backend, returns `default-directory'."
  (let ((fzfa-project-backend nil)
        (default-directory "/some/dir/"))
    (should (string= (fzfa--default-dir) "/some/dir/"))))

(ert-deftest fzfa-project-dir-project-backend-uses-project-root ()
  "With `project' backend, returns the project root when in a project."
  (let ((fzfa-project-backend 'project))
    (cl-letf (((symbol-function 'project-current)
               (lambda (&rest _) '(vc Git "/mock/project/")))
              ((symbol-function 'project-root)
               (lambda (_) "/mock/project/")))
      (should (string= (fzfa--default-dir) "/mock/project/")))))

(ert-deftest fzfa-project-dir-project-backend-fallback ()
  "With `project' backend, falls back to `default-directory' outside a project."
  (let ((fzfa-project-backend 'project)
        (default-directory "/fallback/"))
    (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil)))
      (should (string= (fzfa--default-dir) "/fallback/")))))

(ert-deftest fzfa-project-dir-custom-function ()
  "A function value is called and its return value used."
  (let ((fzfa-project-backend (lambda () "/custom/root/")))
    (should (string= (fzfa--default-dir) "/custom/root/"))))

;;; fzfa-swiper line collection

(ert-deftest fzfa-swiper-line-format ()
  "Lines are formatted as LINE:content with 1-based numbering."
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (let* ((candidates
            (let (lines)
              (save-excursion
                (goto-char (point-min))
                (let ((i 1))
                  (while (not (eobp))
                    (let ((content (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))))
                      (unless (string-empty-p content)
                        (push (format "%d:%s" i content) lines)))
                    (forward-line 1)
                    (cl-incf i))))
              (nreverse lines))))
      (should (equal candidates '("1:alpha" "2:beta" "3:gamma"))))))

(ert-deftest fzfa-swiper-skips-empty-lines ()
  "Empty lines are excluded from candidates."
  (with-temp-buffer
    (insert "first\n\nthird\n")
    (let* ((candidates
            (let (lines)
              (save-excursion
                (goto-char (point-min))
                (let ((i 1))
                  (while (not (eobp))
                    (let ((content (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position))))
                      (unless (string-empty-p content)
                        (push (format "%d:%s" i content) lines)))
                    (forward-line 1)
                    (cl-incf i))))
              (nreverse lines))))
      (should (equal candidates '("1:first" "3:third"))))))

;;; fzfa-ssh (ssh-hosts via :extract)

(defun fzfa-test--extract (cmd)
  "Run CMD under the multi `:extract' mode and return the captured plist.

Returns nil if CMD completes without invoking `completing-read'."
  (let ((fzfa--multi-mode :extract))
    (catch 'fzfa-extracted
      (funcall cmd)
      nil)))

(defmacro fzfa-test--with-ssh-config (content &rest body)
  "Run BODY with a temp file containing CONTENT as the ssh config.

Mocks `expand-file-name' so `fzfa-ssh' reads the temp file."
  (declare (indent 1))
  `(let ((tmpfile (make-temp-file "fzfa-test-ssh-config")))
     (unwind-protect
         (progn
           (with-temp-file tmpfile (insert ,content))
           (cl-letf (((symbol-function 'expand-file-name)
                      (lambda (&rest _) tmpfile)))
             ,@body))
       (delete-file tmpfile))))

(ert-deftest fzfa-ssh-hosts-basic ()
  "Parses plain Host entries from ~/.ssh/config."
  (fzfa-test--with-ssh-config
      "Host foo\n  HostName foo.example.com\nHost bar\n"
    (let ((args (fzfa-test--extract #'fzfa-ssh)))
      (should (equal (plist-get args :candidates) '("foo" "bar"))))))

(ert-deftest fzfa-ssh-hosts-skips-wildcards ()
  "Wildcard Host patterns (*, ?, !) are excluded."
  (fzfa-test--with-ssh-config
      "Host *\nHost prod\nHost *.internal\nHost dev\n"
    (let ((args (fzfa-test--extract #'fzfa-ssh)))
      (should (equal (plist-get args :candidates) '("prod" "dev"))))))

(ert-deftest fzfa-ssh-hosts-multiple-on-one-line ()
  "Multiple hosts on a single Host line are each returned."
  (fzfa-test--with-ssh-config
      "Host alpha beta gamma\n"
    (let ((args (fzfa-test--extract #'fzfa-ssh)))
      (should (equal (plist-get args :candidates)
                     '("alpha" "beta" "gamma"))))))

(ert-deftest fzfa-ssh-missing-config ()
  "Signals a `user-error' when ~/.ssh/config does not exist."
  (cl-letf (((symbol-function 'file-readable-p) (lambda (_) nil)))
    (should-error (fzfa-test--extract #'fzfa-ssh)
                  :type 'user-error)))

;;; fzfa-swiper-all (SOURCE encoding via :extract)

(ert-deftest fzfa-swiper-all-emits-source-line-content ()
  "Candidates display LINE:CONTENT; source rides on `fzfa-location'.

The buffer name (or file path) is no longer embedded in the candidate
string — it is carried in-band as an `fzfa-location' text property at
position 0, so fzf scores only against LINE:CONTENT and the source is
surfaced via `fzfa--location-group' as the section header."
  (let ((buf (generate-new-buffer " *fzfa-test-src*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (rename-buffer "fzfa-test-buf" t)
            (insert "hello\n"))
          (cl-letf (((symbol-function 'buffer-list) (lambda () (list buf))))
            (let* ((args (fzfa-test--extract #'fzfa-swiper-all))
                   (cands (plist-get args :candidates))
                   (cand (car (cl-member "1:hello" cands :test #'equal))))
              (should cand)
              (should (equal (get-text-property 0 'fzfa-location cand)
                             '("fzfa-test-buf" . 1))))))
      (kill-buffer buf))))

;;; fzfa--format-stats

(ert-deftest fzfa-format-stats-with-idx ()
  "Non-nil IDX renders as `(1+ IDX)/' between prefix and `['."
  (should (equal (fzfa--format-stats "find: ~/code " 0 12 100)
                 "find: ~/code 1/[12](100) ")))

(ert-deftest fzfa-format-stats-without-idx ()
  "Nil IDX omits the `N/' segment — frontends like icomplete have none."
  (should (equal (fzfa--format-stats "find: ~/code " nil 12 100)
                 "find: ~/code [12](100) ")))

(ert-deftest fzfa-format-stats-commas-large-numbers ()
  "FILTERED and TOTAL are comma-formatted via `fzfa--commas'."
  (should (equal (fzfa--format-stats "" nil 1234 1234567)
                 "[1,234](1,234,567) ")))

(ert-deftest fzfa-format-stats-preserves-prefix-verbatim ()
  "PREFIX is emitted as-is — caller owns any punctuation and trailing space."
  (should (equal (fzfa--format-stats "fzf-multi: " 4 0 0)
                 "fzf-multi: 5/[0](0) "))
  (should (equal (fzfa--format-stats "" nil 0 0)
                 "[0](0) ")))

(ert-deftest fzfa-format-stats-always-trailing-space ()
  "Output ends with a single trailing space regardless of inputs."
  (dolist (idx '(nil 0 42))
    (should (string-suffix-p
             " " (fzfa--format-stats "p " idx 1 1)))))

;;; fzfa--multi-narrow->string

(ert-deftest fzfa-multi-narrow-string-from-string ()
  "A length-1 string passes through unchanged."
  (should (equal (fzfa--multi-narrow->string "V") "V")))

(ert-deftest fzfa-multi-narrow-string-from-char ()
  "A character is converted to its single-char string form."
  (should (equal (fzfa--multi-narrow->string ?V) "V")))

(ert-deftest fzfa-multi-narrow-string-from-symbol ()
  "A symbol with a one-character name is converted to that character."
  (should (equal (fzfa--multi-narrow->string 'V) "V")))

(ert-deftest fzfa-multi-narrow-string-rejects-multichar ()
  "Multi-character inputs error — narrow keys are single-char only."
  (should-error (fzfa--multi-narrow->string "ab"))
  (should-error (fzfa--multi-narrow->string 'foo)))

(ert-deftest fzfa-multi-narrow-string-rejects-bad-type ()
  "Non-string/char/symbol values error.

Integers are deliberately accepted (they are characters in Emacs)."
  (should-error (fzfa--multi-narrow->string [a b]))
  (should-error (fzfa--multi-narrow->string '(a b))))

;;; fzfa--multi-derive-narrow-key

(ert-deftest fzfa-multi-derive-narrow-first-word-first-char ()
  "Returns the first character of the first word when free."
  (let ((used (make-hash-table :test 'equal)))
    (should (equal (fzfa--multi-derive-narrow-key "buffer" used) "b"))
    (should (equal (fzfa--multi-derive-narrow-key "vc-modified-files" used)
                   "v"))))

(ert-deftest fzfa-multi-derive-narrow-preserves-case ()
  "Case is preserved from the source name (e.g. uppercase `M' in `M-x')."
  (let ((used (make-hash-table :test 'equal)))
    (should (equal (fzfa--multi-derive-narrow-key "M-x" used) "M"))))

(ert-deftest fzfa-multi-derive-narrow-walks-words-on-collision ()
  "When the first-word char is taken, walks to subsequent words' first chars."
  (let ((used (make-hash-table :test 'equal)))
    (puthash "h" t used)
    (should (equal (fzfa--multi-derive-narrow-key "hungry-swiper" used) "s"))
    (puthash "i" t used)
    (should (equal (fzfa--multi-derive-narrow-key "imenu-all-but-current" used)
                   "a"))))

(ert-deftest fzfa-multi-derive-narrow-fallback-pool ()
  "When all word-first-chars are taken, falls through to a-z/A-Z/0-9."
  (let ((used (make-hash-table :test 'equal)))
    (puthash "a" t used)
    (puthash "b" t used)
    (puthash "c" t used)
    ;; words a, b, c all taken; lowercase 'd' is the next free char.
    (should (equal (fzfa--multi-derive-narrow-key "a-b-c" used) "d"))))

(ert-deftest fzfa-multi-derive-narrow-case-sensitive ()
  "Uppercase and lowercase keys are tracked separately."
  (let ((used (make-hash-table :test 'equal)))
    (puthash "I" t used)
    ;; `i' is still free since collision lookup is case-sensitive.
    (should (equal (fzfa--multi-derive-narrow-key "imenu" used) "i"))))

(ert-deftest fzfa-multi-derive-narrow-pool-exhaustion-errors ()
  "Errors when the full 62-char pool is exhausted."
  (let ((used (make-hash-table :test 'equal)))
    (dolist (c (string-to-list (concat "abcdefghijklmnopqrstuvwxyz"
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                       "0123456789")))
      (puthash (string c) t used))
    (should-error (fzfa--multi-derive-narrow-key "anything" used))))

;;; fzfa--format-narrow-hint

(ert-deftest fzfa-format-narrow-hint-basic ()
  "Each source contributes `KEY:NAME', separated by two spaces."
  (let ((sources (vector (list :name "buffer" :narrow "b")
                         (list :name "recent" :narrow "r")
                         (list :name "imenu" :narrow "i"))))
    (should (equal (substring-no-properties
                    (fzfa--format-narrow-hint sources nil))
                   "b:buffer  r:recent  i:imenu"))))

(ert-deftest fzfa-format-narrow-hint-highlights-active-source ()
  "When narrowed, the active source carries the `minibuffer-prompt' face."
  (let* ((sources (vector (list :name "buffer" :narrow "b")
                          (list :name "recent" :narrow "r")))
         (hint (fzfa--format-narrow-hint sources 1 200)))
    ;; r:recent is at position 10 (after "b:buffer  ").
    (should (eq (get-text-property 10 'face hint) 'minibuffer-prompt))
    ;; b:buffer is unpropertized.
    (should (null (get-text-property 0 'face hint)))))

(ert-deftest fzfa-format-narrow-hint-appends-return-marker ()
  "A trailing `PREFIX:widen' marker is appended when prefix is given."
  (let* ((sources (vector (list :name "buffer" :narrow "b")
                          (list :name "recent" :narrow "r")))
         (hint (substring-no-properties
                (fzfa--format-narrow-hint sources nil 200 "<"))))
    (should (equal hint "b:buffer  r:recent  <:widen"))))

(ert-deftest fzfa-format-narrow-hint-return-marker-faced-shadow ()
  "The trailing `PREFIX:widen' marker carries the `shadow' face."
  (let* ((sources (vector (list :name "buffer" :narrow "b")))
         (hint (fzfa--format-narrow-hint sources nil 200 "<"))
         ;; "b:buffer  " is 10 chars; return marker starts at 10.
         (marker-pos 10))
    (should (eq (get-text-property marker-pos 'face hint) 'shadow))))

(ert-deftest fzfa-format-narrow-hint-omits-return-without-prefix ()
  "No trailing marker when PREFIX-KEY is nil."
  (let* ((sources (vector (list :name "buffer" :narrow "b")))
         (hint (substring-no-properties
                (fzfa--format-narrow-hint sources nil 200 nil))))
    (should (equal hint "b:buffer"))))

(ert-deftest fzfa-format-narrow-hint-wraps-on-overflow ()
  "Entries that would exceed WIDTH are pushed to a new line."
  (let* ((sources (vector (list :name "alpha" :narrow "a")
                          (list :name "beta"  :narrow "b")
                          (list :name "gamma" :narrow "g")))
         ;; Width 15 fits "a:alpha  b:beta" (15) but not "  g:gamma".
         (hint (substring-no-properties
                (fzfa--format-narrow-hint sources nil 15))))
    (should (equal hint "a:alpha  b:beta\ng:gamma"))))

(ert-deftest fzfa-format-narrow-hint-single-line-when-fits ()
  "No newlines when every entry fits in one line at the given width."
  (let* ((sources (vector (list :name "alpha" :narrow "a")
                          (list :name "beta"  :narrow "b")))
         (hint (substring-no-properties
                (fzfa--format-narrow-hint sources nil 200))))
    (should (equal hint "a:alpha  b:beta"))))

(ert-deftest fzfa-format-narrow-hint-oversize-entry-on-own-line ()
  "An entry longer than WIDTH stays whole on its own line — no mid-split."
  (let* ((sources (vector (list :name "this-name-is-far-too-long" :narrow "x")))
         (hint (substring-no-properties
                (fzfa--format-narrow-hint sources nil 5))))
    (should (equal hint "x:this-name-is-far-too-long"))))

;;; fzfa--multi-allocate-narrow-keys

(defun fzfa-test--alloc (specs)
  "Build sources from SPECS \\='((NAME . PLIST) ...) and allocate narrow keys.

Returns an alist of (NAME . KEY) preserving allocation order."
  (let ((sources
         (mapcar (lambda (spec)
                   (append (list :name (car spec)) (cdr spec)))
                 specs)))
    (mapcar (lambda (s) (cons (plist-get s :name) (plist-get s :narrow)))
            (fzfa--multi-allocate-narrow-keys sources))))

(ert-deftest fzfa-multi-allocate-implicit-only ()
  "All-implicit case matches the derivation rules end-to-end."
  (should (equal
           (fzfa-test--alloc
            '(("vc-modified-files")
              ("imenu")
              ("buffer")
              ("recent-file")
              ("hungry-find")
              ("imenu-all-but-current")
              ("M-x")
              ("hungry-swiper")
              ("locate")))
           '(("vc-modified-files"     . "v")
             ("imenu"                 . "i")
             ("buffer"                . "b")
             ("recent-file"           . "r")
             ("hungry-find"           . "h")
             ("imenu-all-but-current" . "a")
             ("M-x"                   . "M")
             ("hungry-swiper"         . "s")
             ("locate"                . "l")))))

(ert-deftest fzfa-multi-allocate-mixed-explicit-and-implicit ()
  "Explicit :narrow values are honored; implicit derivation routes around them.

Note `imenu' takes uppercase `I' so `imenu-all-but-current' gets the free `i'."
  (should (equal
           (fzfa-test--alloc
            '(("vc-modified-files" :narrow V)
              ("imenu"             :narrow I)
              ("buffer")
              ("recent-file"       :narrow R)
              ("hungry-find")
              ("imenu-all-but-current")
              ("M-x")
              ("hungry-swiper")
              ("locate")))
           '(("vc-modified-files"     . "V")
             ("imenu"                 . "I")
             ("buffer"                . "b")
             ("recent-file"           . "R")
             ("hungry-find"           . "h")
             ("imenu-all-but-current" . "i")
             ("M-x"                   . "M")
             ("hungry-swiper"         . "s")
             ("locate"                . "l")))))

(ert-deftest fzfa-multi-allocate-accepts-char-and-string-forms ()
  "Explicit :narrow accepts symbol, character, or string interchangeably."
  (should (equal
           (fzfa-test--alloc
            '(("a-source" :narrow ?A)
              ("b-source" :narrow "B")
              ("c-source" :narrow C)))
           '(("a-source" . "A")
             ("b-source" . "B")
             ("c-source" . "C")))))

(ert-deftest fzfa-multi-allocate-duplicate-explicit-errors ()
  "Two sources declaring the same explicit :narrow signal an error."
  (should-error
   (fzfa--multi-allocate-narrow-keys
    (list (list :name "first" :narrow "x")
          (list :name "second" :narrow "x")))))

(ert-deftest fzfa-multi-allocate-implicit-yields-to-reserved ()
  "Implicit derivation never picks an explicitly-reserved key."
  (let ((result
         (fzfa-test--alloc
          '(("hungry-find" :narrow h)
            ("hungry-swiper")))))
    ;; Second source's first-word `h' is reserved, walks to `s'.
    (should (equal (cdr (assoc "hungry-swiper" result)) "s"))))

(ert-deftest fzfa-multi-allocate-preserves-source-order ()
  "Returned source list is in the same order as input."
  (let* ((sources
          (list (list :name "alpha")
                (list :name "beta" :narrow "X")
                (list :name "gamma")))
         (result (fzfa--multi-allocate-narrow-keys sources)))
    (should (equal (mapcar (lambda (s) (plist-get s :name)) result)
                   '("alpha" "beta" "gamma")))))

(ert-deftest fzfa-multi-allocate-nested-collision-with-outer-explicit ()
  "Outer explicit `:narrow' wins over an inner derived key.

Reproduces a real bug: a nested multi (e.g. `fzfa-vc-modified-files')
flattened into an outer multi (`fzfa-find-any') used to bring inner
sources with `:narrow' already pinned by a prior inner allocation —
which then collided with an outer explicit reservation.  Fix: do
not allocate at the inner level when being extracted; defer to the
outermost.  This test asserts the allocator does the right thing
when the inner sources arrive without `:narrow'."
  (let ((result
         ;; Inner sources arrive without `:narrow' (allocation deferred).
         ;; Top-level `(hungry-swiper :narrow s)' is an outer-explicit
         ;; reservation that previously collided with the inner `s'.
         (fzfa-test--alloc
          '(("git-modified-locally")
            ("git-added-files")
            ("git-staged-for-commit")
            ("git-modified-in-head")
            ("hungry-swiper" :narrow s)))))
    (should (equal (cdr (assoc "hungry-swiper" result)) "s"))
    ;; Inner `git-staged-for-commit' must route around the reserved `s'.
    (should-not (equal (cdr (assoc "git-staged-for-commit" result)) "s"))))

(ert-deftest fzfa-multi-allocate-does-not-mutate-input ()
  "Allocation copies source plists rather than mutating them in place."
  (let* ((src (list :name "buffer"))
         (sources (list src)))
    (fzfa--multi-allocate-narrow-keys sources)
    (should (null (plist-get src :narrow)))))

;;; Preview framework

(ert-deftest fzfa-preview-handler-lookup ()
  "Resolver honours the explicit :preview and the registry, ignoring delay."
  ;; With no override, registered category returns the handler plist.
  (let ((fzfa-preview-functions '((cat :preview ignore))))
    (should (eq (plist-get (fzfa--preview-handler nil 'cat) :preview)
                #'ignore)))
  ;; Unknown category yields nil.
  (let ((fzfa-preview-functions '((cat :preview ignore))))
    (should (null (fzfa--preview-handler nil 'unknown))))
  ;; Resolution is independent of `fzfa-preview-delay' — manual fire and
  ;; helm's persistent-action both need a resolvable handler regardless.
  (let ((fzfa-preview-functions '((cat :preview ignore)))
        (fzfa-preview-delay nil))
    (should (eq (plist-get (fzfa--preview-handler nil 'cat) :preview)
                #'ignore))
    (should (eq (plist-get (fzfa--preview-handler #'identity 'cat) :preview)
                #'identity)))
  ;; Explicit function override bypasses the registry.
  (let ((fzfa-preview-functions '((cat :preview ignore))))
    (should (eq (plist-get (fzfa--preview-handler #'identity 'cat) :preview)
                #'identity)))
  ;; Explicit plist override bypasses the registry.
  (let ((fzfa-preview-functions '((cat :preview ignore))))
    (should (eq (plist-get
                 (fzfa--preview-handler '(:preview my-fn :return my-ret) 'cat)
                 :return)
                'my-ret))))

(ert-deftest fzfa-preview-state-get-put ()
  "State plist round-trips through the dynamically-bound session."
  (let ((fzfa--preview-session (list nil))) ; (HANDLER . STATE-PLIST)
    (fzfa-preview-put :a 1)
    (fzfa-preview-put :b 'foo)
    (should (= 1 (fzfa-preview-get :a)))
    (should (eq 'foo (fzfa-preview-get :b)))
    (should (eq :missing (fzfa-preview-get :c :missing)))
    ;; Stored nil differs from absent key (DEFAULT not applied).
    (fzfa-preview-put :a nil)
    (should (null (fzfa-preview-get :a :default)))))

(ert-deftest fzfa-grep-preview-parses-candidate ()
  "Grep preview accepts FILE:LINE:CONTENT and ignores malformed input."
  ;; No-op for nil / wrong shape — must not error.
  (fzfa--grep-preview nil nil)
  (fzfa--grep-preview "no-colons" nil)
  (fzfa--grep-preview "only:one-colon" nil)
  ;; Well-formed candidate to a nonexistent path is a silent no-op.
  (fzfa--grep-preview "no-such-file.xyz:1:irrelevant" nil))

(ert-deftest fzfa-buffer-preview-handles-missing-buffer ()
  "Buffer preview is a silent no-op when the named buffer does not exist."
  (fzfa--buffer-preview nil nil)
  (fzfa--buffer-preview "*no-such-buffer*-fzfa-test*" nil))

(ert-deftest fzfa-temporary-files-creates-and-kills ()
  "Opener creates an ephemeral buffer for a new file and kills it on cleanup."
  (let ((tmpfile (make-temp-file "fzfa-tmpfiles-test")))
    (unwind-protect
        (let* ((opener (fzfa--temporary-files))
               (buf (funcall opener tmpfile)))
          (should (buffer-live-p buf))
          (should (file-equal-p tmpfile (buffer-file-name buf)))
          (funcall opener)              ; cleanup
          (should-not (buffer-live-p buf)))
      (delete-file tmpfile))))

(ert-deftest fzfa-temporary-files-reuses-loaded-buffer ()
  "Opener returns an already-loaded buffer and does NOT kill it on cleanup."
  (let* ((tmpfile (make-temp-file "fzfa-tmpfiles-test"))
         (pre-loaded (find-file-noselect tmpfile 'nowarn)))
    (unwind-protect
        (let* ((opener (fzfa--temporary-files))
               (buf (funcall opener tmpfile)))
          (should (eq buf pre-loaded))
          (funcall opener)              ; cleanup
          (should (buffer-live-p pre-loaded)))   ; not killed
      (when (buffer-live-p pre-loaded) (kill-buffer pre-loaded))
      (delete-file tmpfile))))

(ert-deftest fzfa-file-preview-skips-oversize ()
  "Preview is a no-op when the file exceeds `fzfa-preview-file-size-limit'."
  (let ((tmpfile (make-temp-file "fzfa-file-preview-test")))
    (unwind-protect
        (let ((fzfa--preview-session (list nil))
              (fzfa-preview-file-size-limit 0))
          (fzfa--file-preview-setup nil)
          ;; Limit of 0 disables — opener should not produce a buffer.
          (fzfa--file-preview tmpfile nil)
          ;; Nothing was opened (no file-visiting buffer for our path).
          (should-not (find-buffer-visiting tmpfile)))
      (delete-file tmpfile))))

(ert-deftest fzfa-multi-router-routes-preview-per-source ()
  "Router's :preview dispatches to the source identified by CAND's tagged idx."
  (let* ((calls nil)
         (h0 (list :preview (lambda (c _s) (push (cons 0 c) calls))))
         (h1 (list :preview (lambda (c _s) (push (cons 1 c) calls))))
         (fzfa-preview-functions `((cat-a :preview ,(plist-get h0 :preview))
                                   (cat-b :preview ,(plist-get h1 :preview))))
         (fzfa-preview-delay 0.3)
         (sources-v (vector (list :name "A" :category 'cat-a)
                            (list :name "B" :category 'cat-b)))
         (candidate->source (make-hash-table :test 'equal))
         (router (fzfa--multi-build-router sources-v candidate->source))
         ;; Pretend the framework already installed and set origin/dir.
         (fzfa--preview-session (list router)))
    (fzfa-preview-put :origin-window nil)
    (fzfa-preview-put :origin-buffer nil)
    (fzfa-preview-put :default-directory "/")
    ;; Run :setup → broadcasts to both sources.
    (funcall (plist-get router :setup) nil)
    ;; Source 0 candidate
    (let ((c0 (propertize "alpha" 'fzfa-src-idx 0)))
      (puthash c0 0 candidate->source)
      (funcall (plist-get router :preview) c0 nil))
    ;; Source 1 candidate
    (let ((c1 (propertize "beta" 'fzfa-src-idx 1)))
      (puthash c1 1 candidate->source)
      (funcall (plist-get router :preview) c1 nil))
    (should (equal (reverse calls)
                   '((0 . "alpha") (1 . "beta"))))))

(ert-deftest fzfa-multi-router-nil-when-no-handlers ()
  "Router builder returns nil if no source has a resolvable handler."
  (let* ((fzfa-preview-functions nil)
         (fzfa-preview-delay 0.3)
         (sources-v (vector (list :name "A" :category 'no-such)
                            (list :name "B" :category 'also-no))))
    (should (null (fzfa--multi-build-router
                   sources-v (make-hash-table :test 'equal))))))

(ert-deftest fzfa-multi-router-return-routes-selection ()
  "On :return, the selected source gets CAND; others get nil."
  (let* ((returns nil)
         (h0 (list :return (lambda (c _s) (push (cons 0 c) returns))))
         (h1 (list :return (lambda (c _s) (push (cons 1 c) returns))))
         (fzfa-preview-functions `((cat-a :return ,(plist-get h0 :return)
                                          :preview ignore)
                                   (cat-b :return ,(plist-get h1 :return)
                                          :preview ignore)))
         (fzfa-preview-delay 0.3)
         (sources-v (vector (list :name "A" :category 'cat-a)
                            (list :name "B" :category 'cat-b)))
         (candidate->source (make-hash-table :test 'equal))
         (router (fzfa--multi-build-router sources-v candidate->source))
         (fzfa--preview-session (list router))
         (sel (propertize "picked" 'fzfa-src-idx 1)))
    (fzfa-preview-put :origin-window nil)
    (fzfa-preview-put :origin-buffer nil)
    (fzfa-preview-put :default-directory "/")
    (funcall (plist-get router :setup) nil)
    (puthash sel 1 candidate->source)
    (funcall (plist-get router :return) sel nil)
    ;; Source 1 got the candidate; source 0 got nil.
    (should (equal (sort (copy-sequence returns) (lambda (a b)
                                                   (< (car a) (car b))))
                   '((0 . nil) (1 . "picked"))))))

(ert-deftest fzfa-preview-show-uses-same-window ()
  "`fzfa-preview-show' lands the buffer in the currently selected window.

`fzfa--preview-call' selects the originating window before invoking
handlers, so passing `display-buffer-same-window' here keeps preview
and the eventual post-selection action sharing one window slot."
  (with-temp-buffer
    (let* ((buf (current-buffer))
           captured)
      (cl-letf (((symbol-function 'display-buffer)
                 (lambda (_b action) (setq captured action))))
        (fzfa-preview-show buf))
      (should (equal captured '(display-buffer-same-window))))))

(ert-deftest fzfa-preview-show-moves-point ()
  "`fzfa-preview-show' moves point in BUFFER when POS is supplied."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (insert "one\ntwo\nthree\n")
      (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
        (fzfa-preview-show buf 5)             ; start of "two"
        (with-current-buffer buf
          (should (= (point) 5))))
      ;; Marker POS also accepted.
      (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
        (let ((m (copy-marker 9)))            ; start of "three"
          (fzfa-preview-show buf m)
          (with-current-buffer buf
            (should (= (point) 9))))))))

;;; fzfa-smart-define / fzfa--smart-resolve

(defmacro fzfa-test--with-executables (available &rest body)
  "Run BODY with `executable-find' stubbed to recognize only AVAILABLE.

AVAILABLE is a list of program-name strings; calls for any other
program return nil.  Also stubs the smart-find/grep backend symbols
referenced by the resolve tests as no-op functions so they are
`fboundp' regardless of which extension files are loaded."
  (declare (indent 1) (debug t))
  `(cl-letf (((symbol-function 'executable-find)
              (lambda (prog &rest _)
                (and (member prog ,available) prog)))
             ((symbol-function 'fzfa-fd)       (lambda () 'fd))
             ((symbol-function 'fzfa-rg-files) (lambda () 'rg-files))
             ((symbol-function 'fzfa-ag-files) (lambda () 'ag-files))
             ((symbol-function 'fzfa-find)     (lambda () 'find))
             ((symbol-function 'fzfa-rg)       (lambda () 'rg))
             ((symbol-function 'fzfa-ag)       (lambda () 'ag))
             ((symbol-function 'fzfa-ugrep)    (lambda () 'ugrep))
             ((symbol-function 'fzfa-grep)     (lambda () 'grep)))
     ,@body))

(ert-deftest fzfa-smart-resolve-picks-first-matching-executable ()
  "First clause whose executable is on PATH wins."
  (fzfa-test--with-executables '("fd" "rg" "find")
    (should (eq 'fzfa-fd
                (fzfa--smart-resolve
                 '((fzfa-fd       :executable "fd")
                   (fzfa-rg-files :executable "rg")
                   (fzfa-find     :executable "find")))))))

(ert-deftest fzfa-smart-resolve-skips-missing-executable ()
  "Clauses whose executable is absent are skipped."
  (fzfa-test--with-executables '("rg" "find")
    (should (eq 'fzfa-rg-files
                (fzfa--smart-resolve
                 '((fzfa-fd       :executable "fd")
                   (fzfa-rg-files :executable "rg")
                   (fzfa-find     :executable "find")))))))

(ert-deftest fzfa-smart-resolve-returns-nil-when-nothing-matches ()
  "Returns nil when no clause has a satisfied executable."
  (fzfa-test--with-executables '()
    (should (null (fzfa--smart-resolve
                   '((fzfa-fd :executable "fd")
                     (fzfa-rg-files :executable "rg")))))))

(ert-deftest fzfa-smart-resolve-respects-fboundp ()
  "A clause is skipped when its CMD symbol is not `fboundp'."
  (let ((unbound (make-symbol "fzfa-test-unbound")))
    (fzfa-test--with-executables '("fd" "find")
      (should (eq 'fzfa-find
                  (fzfa--smart-resolve
                   `((,unbound  :executable "fd")
                     (fzfa-find :executable "find"))))))))

(ert-deftest fzfa-smart-resolve-predicate-truthy-matches ()
  "Clause matches when `:predicate' returns non-nil."
  (fzfa-test--with-executables '()
    (should (eq 'fzfa-find
                (fzfa--smart-resolve
                 '((fzfa-find :predicate (lambda () t))))))))

(ert-deftest fzfa-smart-resolve-predicate-falsy-skips ()
  "Clause is skipped when `:predicate' returns nil."
  (fzfa-test--with-executables '("find")
    (should (eq 'fzfa-find
                (fzfa--smart-resolve
                 '((fzfa-fd   :predicate ignore)
                   (fzfa-find :executable "find")))))))

(ert-deftest fzfa-smart-resolve-predicate-and-executable-both-required ()
  "When both `:executable' and `:predicate' are supplied, both must hold."
  (fzfa-test--with-executables '("fd" "find")
    ;; Executable matches but predicate fails -> skipped.
    (should (eq 'fzfa-find
                (fzfa--smart-resolve
                 '((fzfa-fd   :executable "fd" :predicate ignore)
                   (fzfa-find :executable "find")))))
    ;; Predicate matches but executable missing -> skipped.
    (should (eq 'fzfa-find
                (fzfa--smart-resolve
                 '((fzfa-fd   :executable "no-such-exe"
                              :predicate (lambda () t))
                   (fzfa-find :executable "find")))))))

(ert-deftest fzfa-smart-resolve-bare-clause-is-unconditional ()
  "A clause with neither `:executable' nor `:predicate' always matches."
  (fzfa-test--with-executables '()
    (should (eq 'fzfa-find
                (fzfa--smart-resolve
                 '((fzfa-fd :executable "fd")
                   (fzfa-find)))))))

(ert-deftest fzfa-smart-define-creates-named-command ()
  "`fzfa-smart-define' interns and defines `fzfa-smart-NAME'."
  (let ((sym (intern "fzfa-smart-test-create")))
    (unwind-protect
        (progn
          (fmakunbound sym)
          (let ((result (fzfa-smart-define
                         'test-create
                         '((fzfa-find :executable "find")))))
            (should (eq result sym))
            (should (fboundp sym))
            (should (commandp sym))))
      (fmakunbound sym))))

(ert-deftest fzfa-smart-define-funcalls-chosen-backend ()
  "The generated command `funcall's the resolved backend symbol."
  (let ((sym (intern "fzfa-smart-test-dispatch"))
        (backend (intern "fzfa-test-backend-dispatch"))
        (called 0))
    (unwind-protect
        (progn
          (fmakunbound sym)
          (defalias backend (lambda () (cl-incf called)))
          (fzfa-smart-define 'test-dispatch
                             `((,backend :executable "fd")))
          (fzfa-test--with-executables '("fd")
            (funcall sym))
          (should (= called 1)))
      (fmakunbound sym)
      (fmakunbound backend))))

(ert-deftest fzfa-smart-define-errors-when-no-backend ()
  "The generated command signals `user-error' when nothing resolves."
  (let ((sym (intern "fzfa-smart-test-noexe")))
    (unwind-protect
        (progn
          (fmakunbound sym)
          (fzfa-smart-define 'test-noexe
                             '((fzfa-fd :executable "no-such-tool-xyz")))
          (fzfa-test--with-executables '()
            (should-error (funcall sym) :type 'user-error)))
      (fmakunbound sym))))

(ert-deftest fzfa-smart-define-propagates-multi-mode ()
  "Active `fzfa--multi-mode' propagates through the smart command.

This is the contract that makes smart commands work transparently
inside `fzfa-multi-read' (`:extract')."
  (let ((sym (intern "fzfa-smart-test-multi"))
        (backend (intern "fzfa-test-backend-multi"))
        observed)
    (unwind-protect
        (progn
          (fmakunbound sym)
          (defalias backend
            (lambda ()
              (throw 'fzfa-extracted (list :seen fzfa--multi-mode))))
          (fzfa-smart-define 'test-multi
                             `((,backend :executable "fd")))
          (fzfa-test--with-executables '("fd")
            (setq observed
                  (catch 'fzfa-extracted
                    (let ((fzfa--multi-mode :extract))
                      (funcall sym))
                    nil)))
          (should (equal observed '(:seen :extract))))
      (fmakunbound sym)
      (fmakunbound backend))))

(ert-deftest fzfa-smart-find-and-grep-defined ()
  "The shipped smart wrappers are defined and interactive."
  (should (fboundp 'fzfa-smart-find))
  (should (commandp 'fzfa-smart-find))
  (should (fboundp 'fzfa-smart-grep))
  (should (commandp 'fzfa-smart-grep)))

;;; fzfa--split
;;
;; Tests bind `fzfa-separator' to ?# for readable ASCII fixtures;
;; the splitter is generic and works for any character.

(ert-deftest fzfa-split-hidden-empty-input ()
  "Hidden mode + empty input returns (COMMAND . \"\")."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "" 'hidden "find .")
                   '("find ." . "")))))

(ert-deftest fzfa-split-hidden-with-filter ()
  "Hidden mode treats the whole INPUT as FILTER and CMD = COMMAND."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "foo" 'hidden "find .")
                   '("find ." . "foo")))))

(ert-deftest fzfa-split-hidden-nil-command ()
  "Hidden mode + nil COMMAND coerces CMD to the empty string."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "foo" 'hidden nil)
                   '("" . "foo")))))

(ert-deftest fzfa-split-hidden-empty-string-command ()
  "Hidden mode + empty-string COMMAND keeps CMD as empty string."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "foo" 'hidden "")
                   '("" . "foo")))))

(ert-deftest fzfa-split-compact-delegates-to-splitter ()
  "Compact mode ignores COMMAND and delegates to the splitter."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "#find .#foo" 'compact "ignored")
                   '("find ." . "foo")))))

(ert-deftest fzfa-split-full-delegates-to-splitter ()
  "Full mode behaves identically to compact."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "#find .#foo" 'full "ignored")
                   (fzfa--split "#find .#foo" 'compact "ignored")))))

(ert-deftest fzfa-split-compact-no-leading-separator ()
  "Compact mode + input without a leading separator → whole string as CMD."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "plain-text" 'compact "ignored")
                   '("plain-text" . "")))))

(ert-deftest fzfa-split-compact-empty-cmd-region ()
  "Compact mode + `##filter' (empty CMD region) → (\"\" . \"filter\")."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "##bar" 'compact "ignored")
                   '("" . "bar")))))

(ert-deftest fzfa-split-hidden-state-takes-priority ()
  "Hidden mode short-circuits *before* the splitter runs.

INPUT that looks like a separator-delimited shape is NOT parsed when
the session is hidden — the whole INPUT (including literal separator
characters) is the FILTER."
  (let ((fzfa-separator ?#))
    (should (equal (fzfa--split "#fake#filter" 'hidden "real")
                   '("real" . "#fake#filter")))))

(ert-deftest fzfa-split-honors-custom-separator ()
  "Splitter follows whatever character `fzfa-separator' is set to."
  (let ((fzfa-separator ?▌))
    (should (equal (fzfa--split "▌find .▌foo" 'compact "ignored")
                   '("find ." . "foo")))))

;;; fzfa--display-next-state

(ert-deftest fzfa-display-next-state-cycle ()
  "State cycles hidden → compact → full → hidden."
  (should (eq (fzfa--display-next-state 'hidden)  'compact))
  (should (eq (fzfa--display-next-state 'compact) 'full))
  (should (eq (fzfa--display-next-state 'full)    'hidden)))

(ert-deftest fzfa-display-next-state-fallback ()
  "Unknown input falls back to `hidden'."
  (should (eq (fzfa--display-next-state 'bogus) 'hidden))
  (should (eq (fzfa--display-next-state nil)    'hidden)))

;;; fzfa--display-materialize / extract

(ert-deftest fzfa-display-materialize-empty-filter ()
  "Materialize on empty FILTER inserts `#CMD#' and lands point at end."
  (with-temp-buffer
    (fzfa--display-materialize "find ." ?#)
    (should (equal (buffer-string) "#find .#"))
    (should (= (point) (point-max)))))

(ert-deftest fzfa-display-materialize-preserves-filter-offset ()
  "With existing FILTER `abc' and point at `c', materialize keeps point at `c'."
  (with-temp-buffer
    (insert "abc")
    (goto-char (point-max))  ; point at position 4 (after `c`)
    (let ((offset-before (- (point) (minibuffer-prompt-end))))
      (fzfa--display-materialize "find ." ?#)
      (should (equal (buffer-string) "#find .#abc"))
      ;; Point should be at the same offset within the new FILTER region.
      (let* ((mbe (minibuffer-prompt-end))
             (cmd-text-len (length "#find .#"))
             (new-filter-offset (- (point) mbe cmd-text-len)))
        (should (= new-filter-offset offset-before))))))

(ert-deftest fzfa-display-materialize-nil-cmd ()
  "Nil CMD coerces to empty string — buffer ends up as `##'."
  (with-temp-buffer
    (fzfa--display-materialize nil ?#)
    (should (equal (buffer-string) "##"))))

(ert-deftest fzfa-display-materialize-returns-overlays ()
  "Returns a list of two protective overlays covering the two separators."
  (with-temp-buffer
    (let ((overlays (fzfa--display-materialize "find ." ?#)))
      (should (= (length overlays) 2))
      (should (cl-every #'overlayp overlays))
      ;; First overlay covers the opening `#' at position 1.
      (should (= (overlay-start (car overlays)) 1))
      (should (= (overlay-end   (car overlays)) 2))
      ;; Second overlay covers the closing `#' at position 8 (after "find .").
      (should (= (overlay-start (cadr overlays)) 8))
      (should (= (overlay-end   (cadr overlays)) 9)))))

(ert-deftest fzfa-display-extract-basic ()
  "Extract returns CMD and deletes the `#CMD#' prefix from the buffer."
  (with-temp-buffer
    (insert "#find .#filter")
    (let* ((fzfa-separator ?#)
           (cmd (fzfa--display-extract nil)))
      (should (equal cmd "find ."))
      (should (equal (buffer-string) "filter")))))

(ert-deftest fzfa-display-extract-empty-cmd ()
  "Extract handles `##filter' (empty CMD region)."
  (with-temp-buffer
    (insert "##filter")
    (let* ((fzfa-separator ?#)
           (cmd (fzfa--display-extract nil)))
      (should (equal cmd ""))
      (should (equal (buffer-string) "filter")))))

(ert-deftest fzfa-display-extract-deletes-overlays ()
  "Extract removes the separator-protective overlays before mutating the buffer.

\(Their `modification-hooks' would otherwise self-heal the deletion.)"
  (with-temp-buffer
    (insert "#find .#abc")
    (let* ((fzfa-separator ?#)
           (overlays
            (list (fzfa--protect-separator 1 ?#)
                  (fzfa--protect-separator 8 ?#))))
      (should (= (length (overlays-in 1 9)) 2))
      (fzfa--display-extract overlays)
      (should (equal (buffer-string) "abc")))))

(ert-deftest fzfa-display-materialize-extract-roundtrip ()
  "Materialize then extract restores the original buffer + cmd."
  (with-temp-buffer
    (insert "abc")
    (let* ((fzfa-separator ?#)
           (overlays (fzfa--display-materialize "find ." ?#)))
      (should (equal (buffer-string) "#find .#abc"))
      (let ((cmd (fzfa--display-extract overlays)))
        (should (equal cmd "find ."))
        (should (equal (buffer-string) "abc"))))))

;;; fzfa-sync-autoloads

(ert-deftest fzfa-sync-autoloads-prunes-excluded-extensions ()
  "Autoload stubs for extensions not in `fzfa-extensions' get unbound,

included extensions are left alone."
  (let ((syms '(fzfa-syncauttest1 fzfa-syncauttest1-foo
                fzfa-syncauttest2 fzfa-syncauttest2-bar))
        (registry '((syncauttest1 . "test 1")
                    (syncauttest2 . "test 2"))))
    (unwind-protect
        (progn
          (dolist (s syms) (autoload s "fzfa-test-nonexistent"))
          (dolist (s syms) (should (autoloadp (symbol-function s))))
          (let ((fzfa-extension-registry registry)
                (fzfa-extensions '(syncauttest1)))
            (fzfa-sync-autoloads))
          ;; Included extension: stubs preserved.
          (should (autoloadp (symbol-function 'fzfa-syncauttest1)))
          (should (autoloadp (symbol-function 'fzfa-syncauttest1-foo)))
          ;; Excluded extension: stubs unbound.
          (should-not (fboundp 'fzfa-syncauttest2))
          (should-not (fboundp 'fzfa-syncauttest2-bar)))
      (dolist (s syms) (when (intern-soft s) (unintern s nil))))))

(ert-deftest fzfa-sync-autoloads-skips-loaded-functions ()
  "Already-loaded (non-autoload) bindings are not touched by the prune,

even when their extension is excluded from `fzfa-extensions'."
  (let ((syms '(fzfa-syncauttest3 fzfa-syncauttest3-loaded))
        (registry '((syncauttest3 . "test 3"))))
    (unwind-protect
        (progn
          (autoload 'fzfa-syncauttest3 "fzfa-test-nonexistent")
          (defalias 'fzfa-syncauttest3-loaded (lambda () "loaded"))
          (let ((fzfa-extension-registry registry)
                (fzfa-extensions '()))
            (fzfa-sync-autoloads))
          ;; Autoload stub: pruned.
          (should-not (fboundp 'fzfa-syncauttest3))
          ;; Concrete function: preserved.
          (should (fboundp 'fzfa-syncauttest3-loaded))
          (should-not (autoloadp (symbol-function 'fzfa-syncauttest3-loaded))))
      (dolist (s syms) (when (intern-soft s) (unintern s nil))))))

;;; fzfa-source — struct + helpers

(ert-deftest fzfa-source-make-from-hoisted-args ()
  "Constructor with hoisted args (single-source path) populates slots."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "rg foo"
                                :directory "/tmp/"
                                :history 'my-history
                                :display 'compact)))
    (should (fzfa-source-p src))
    (should (equal (fzfa-source-command src) "rg foo"))
    (should (equal (fzfa-source-directory src) (expand-file-name "/tmp/")))
    (should (eq (fzfa-source-history src) 'my-history))
    (should (eq (fzfa-source-display-state src) 'compact))
    (should (null (fzfa-source-handle src)))
    (should (null (fzfa-source-current-cmd src)))
    (should (= (fzfa-source-last-gen src) -1))
    (should (= (fzfa-source-rank src) 0))
    (should (= (fzfa-source-total src) 0))
    (should (= (fzfa-source-filtered src) 0))
    (should (eq (fzfa-source-prod-input src) :unfetched))))

(ert-deftest fzfa-source-make-from-spec-plist ()
  "Constructor with :spec (multi-source path) extracts keys from plist."
  (let* ((default-directory "/tmp/")
         (spec '(:name "my-src" :command "fd ." :directory "/tmp/"
                 :history my-hist :display full))
         (src (fzfa-make-source :spec spec)))
    (should (fzfa-source-p src))
    (should (equal (fzfa-source-name src) "my-src"))
    (should (equal (fzfa-source-command src) "fd ."))
    (should (equal (fzfa-source-directory src) (expand-file-name "/tmp/")))
    (should (eq (fzfa-source-history src) 'my-hist))
    (should (eq (fzfa-source-display-state src) 'full))
    ;; Spec preserved for closures that need non-hot keys.
    (should (equal (fzfa-source-spec src) spec))))

(ert-deftest fzfa-source-defaults-display-to-hidden ()
  "Display state defaults to `hidden' when no :display in spec or args."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls")))
    (should (eq (fzfa-source-display-state src) 'hidden))))

(ert-deftest fzfa-source-request-state-defaults-empty ()
  "A new source owns no native request identity."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls")))
    (should (= (fzfa-source-request-id src) 0))
    (should-not (fzfa-source-request-signature src))))

(ert-deftest fzfa-source-submit-deduplicates-locally ()
  "Equal renders submit once even when native would return a new ID."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls"))
         (calls 0)
         (fzfa-case-mode 'smart)
         (fzfa-fuzzy t)
         (fzf-native-filter-only-length nil)
         (fzf-native-filter-only-logic 'or))
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzf-native-async-submit)
               (lambda (_handle _query _limit) (cl-incf calls))))
      (should (= (fzfa--source-submit src "alpha" 20) 1))
      (should (= (fzfa--source-submit src "alpha" 20) 1))
      (should (= calls 1))
      ;; LIMIT and matching options are part of request ownership.
      (should (= (fzfa--source-submit src "alpha" 30) 2))
      (let ((fzfa-case-mode 'respect))
        (should (= (fzfa--source-submit src "alpha" 30) 3)))
      (let ((fzfa-fuzzy nil))
        (should (= (fzfa--source-submit src "alpha" 30) 4)))
      (let ((fzf-native-filter-only-length 100))
        (should (= (fzfa--source-submit src "alpha" 30) 5)))
      (let ((fzf-native-filter-only-logic 'and))
        (should (= (fzfa--source-submit src "alpha" 30) 6)))
      (should (= calls 6)))))

(ert-deftest fzfa-source-submit-refusal-clears-old-ownership ()
  "A failed changed submit cannot poll the preceding query's request."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls"))
         (calls 0))
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzf-native-async-submit)
               (lambda (_handle _query _limit)
                 (if (= (cl-incf calls) 1) 71
                   (error "refused"))))
              ((symbol-function 'fzfa--log) #'ignore)
              ((symbol-function 'message) #'ignore))
      (should (= (fzfa--source-submit src "old" 10) 71))
      (should (equal (fzfa--source-submit src "new" 10)
                     '(failed "refused" nil)))
      (should (= (fzfa-source-request-id src) 0))
      (should (fzfa-source-request-signature src))
      ;; The same failed signature is terminal and does not retry.
      (should (equal (fzfa--source-submit src "new" 10)
                     '(failed "refused" nil)))
      (should (= calls 2)))))

(ert-deftest fzfa-session-submit-failure-is-terminal-and-reported-once ()
  "A rejected native submit is not converted into infinite pending retries."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls"))
         (submits 0)
         messages)
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _)
                 (cl-incf submits)
                 (error "native submit exploded")))
              ((symbol-function 'fzfa--log) #'ignore)
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (should (equal (fzfa--source-async-out src "q" 10)
                     '(failed "native submit exploded" nil)))
      (should (equal (fzfa--source-async-out src "q" 10)
                     '(failed "native submit exploded" nil)))
      (should (= submits 1))
      (should (= (length messages) 1)))))

(ert-deftest fzfa-session-render-polls-without-resubmitting ()
  "Stable final redraws use status and reuse one materialized result."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls"))
         (submits 0)
         (statuses 0)
         (snapshots 0))
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (_handle _query _limit)
                 (cl-incf submits)
                 (+ 100 submits)))
              ((symbol-function 'fzf-native-async-snapshot)
               (lambda (_handle request-id)
                 (cl-incf snapshots)
                 (list :request-id request-id :state 'complete
                       :snapshot-generation 7
                       :stale nil :candidates '("alpha")
                       :filtered 1 :total 3)))
              ((symbol-function 'fzf-native-async-status)
               (lambda (_handle request-id)
                 (cl-incf statuses)
                 (list :request-id request-id :state 'complete
                       :snapshot-generation 7
                       :stale nil :filtered 1 :total 3))))
      (should (equal (fzfa--source-async-out src "a" 10)
                     '(final ("alpha") 1 3)))
      (should (equal (fzfa--source-async-out src "a" 10)
                     '(final ("alpha") 1 3)))
      (should (= submits 1))
      (should (= statuses 2))
      (should (= snapshots 1)))))

(ert-deftest fzfa-session-streams-current-request-snapshots ()
  "Pool growth must not hide completed batches for the current request."
  (let* ((src (fzfa-make-source :command "producer"))
         (submits 0)
         (snapshots 0)
         (status (copy-sequence
                  '(:request-id 77 :latest-request-id 77 :result-request-id 77
                    :state running :stale t :reader-done nil
                    :snapshot-generation 7 :pool-generation 125
                    :result-pool-generation 100 :filtered 1 :total 100)))
         (candidates '("alpha"))
         first)
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _) (cl-incf submits) 77))
              ((symbol-function 'fzf-native-async-status)
               (lambda (&rest _) status))
              ((symbol-function 'fzf-native-async-snapshot)
               (lambda (&rest _)
                 (cl-incf snapshots)
                 (append status (list :candidates candidates)))))
      (setq first (fzfa--source-async-out src "a" 10))
      (should (equal first '(partial ("alpha") 1 100)))
      (should-not (plist-get status :reader-done))
      ;; Further producer growth must neither hide nor recopy this batch.
      (setq status (plist-put status :pool-generation 200))
      (should (eq (fzfa--source-async-out src "a" 10) first))
      (should (eq (fzfa--source-async-candidates src "a" 10) candidates))
      (dolist (state '(queued complete))
        (setq status (plist-put status :state state))
        (should (eq (fzfa--source-async-out src "a" 10) first)))
      (should (= snapshots 1))
      ;; A later batch updates candidates and counts together.
      (setq status (plist-put status :snapshot-generation 8)
            status (plist-put status :state 'complete)
            status (plist-put status :stale nil)
            status (plist-put status :result-pool-generation 200)
            status (plist-put status :total 200)
            status (plist-put status :filtered 2)
            candidates '("alpha" "alphabet"))
      (should (equal (fzfa--source-async-out src "a" 10)
                     '(final ("alpha" "alphabet") 2 200)))
      (should (= snapshots 2))
      ;; New input changes finality without another candidate copy.
      (setq status (plist-put status :state 'running)
            status (plist-put status :stale t)
            status (plist-put status :pool-generation 250))
      (should (equal (fzfa--source-async-out src "a" 10)
                     '(partial ("alpha" "alphabet") 2 200)))
      (should (= snapshots 2))
      (should (= submits 1)))))

(ert-deftest fzfa-session-partial-rejects-other-request ()
  "Both status and snapshot must associate partial results with this query."
  (dolist (observation '(status snapshot))
    (dolist (identity '(:request-id :latest-request-id :result-request-id))
      (let* ((src (fzfa-make-source :command "producer"))
             (snapshots 0)
             (status '(:request-id 77 :latest-request-id 77 :result-request-id 77
                       :state running :stale t :snapshot-generation 7
                       :pool-generation 125 :filtered 1 :total 100))
             (obsolete (plist-put (copy-sequence status) identity 76)))
        (setf (fzfa-source-handle src) 'fake-handle)
        (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
                  ((symbol-function 'fzf-native-async-submit)
                   (lambda (&rest _) 77))
                  ((symbol-function 'fzf-native-async-status)
                   (lambda (&rest _)
                     (if (eq observation 'status) obsolete status)))
                  ((symbol-function 'fzf-native-async-snapshot)
                   (lambda (&rest _)
                     (cl-incf snapshots)
                     (append obsolete '(:candidates ("old-query"))))))
          (should (equal (fzfa--source-async-out src "new" 10)
                         '(pending . 125)))
          (should (= snapshots (if (eq observation 'status) 0 1)))
          (should-not (fzfa-source-request-output src)))))))

(ert-deftest fzfa-session-empty-partial-is-not-authoritative ()
  "An empty batch stays pending until the native result is final."
  (let* ((src (fzfa-make-source :command "producer"))
         (snapshots 0)
         (status (copy-sequence
                  '(:request-id 77 :latest-request-id 77 :result-request-id 77
                    :state complete :stale t :reader-done nil
                    :snapshot-generation 7 :pool-generation 125
                    :filtered 0 :total 100))))
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _) 77))
              ((symbol-function 'fzf-native-async-status)
               (lambda (&rest _) status))
              ((symbol-function 'fzf-native-async-snapshot)
               (lambda (&rest _) (cl-incf snapshots) status)))
      (should (equal (fzfa--source-async-out src "missing" 10)
                     '(pending . 125)))
      (should (= snapshots 0))
      (setq status (plist-put status :stale nil)
            status (plist-put status :reader-done t)
            status (plist-put status :snapshot-generation 8)
            status (plist-put status :total 125))
      (should (equal (fzfa--source-async-out src "missing" 10)
                     '(final nil 0 125)))
      (should (= snapshots 1)))))

(ert-deftest fzfa-session-snapshot-can-become-partial-after-status ()
  "Growth between status and snapshot must not discard useful candidates."
  (let* ((src (fzfa-make-source :command "producer"))
         (status '(:request-id 77 :latest-request-id 77 :result-request-id 77
                   :state complete :stale nil :snapshot-generation 7
                   :pool-generation 100 :filtered 1 :total 100)))
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _) 77))
              ((symbol-function 'fzf-native-async-status)
               (lambda (&rest _) status))
              ((symbol-function 'fzf-native-async-snapshot)
               (lambda (&rest _)
                 '(:request-id 77 :latest-request-id 77 :result-request-id 77
                   :state running :stale t :snapshot-generation 7
                   :pool-generation 125 :filtered 1 :total 100
                   :candidates ("alpha")))))
      (should (equal (fzfa--source-async-out src "a" 10)
                     '(partial ("alpha") 1 100))))))

(ert-deftest fzfa-session-partial-snapshot-revocation-discards-result ()
  "Partial snapshots must obey the source handle and epoch checks."
  (dolist (revocation '(handle epoch))
    (let* ((src (fzfa-make-source :command "producer"))
           (status '(:request-id 77 :latest-request-id 77 :result-request-id 77
                     :state running :stale t :snapshot-generation 7
                     :pool-generation 125 :filtered 1 :total 100)))
      (setf (fzfa-source-handle src) 'fake-handle)
      (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
                ((symbol-function 'fzf-native-async-submit)
                 (lambda (&rest _) 77))
                ((symbol-function 'fzf-native-async-status)
                 (lambda (&rest _) status))
                ((symbol-function 'fzf-native-async-snapshot)
                 (lambda (&rest _)
                   (if (eq revocation 'handle)
                       (setf (fzfa-source-handle src) 'replacement-handle)
                     (fzfa--source-clear-request src)
                     (fzfa--source-submit src "a" 10))
                   (append status '(:candidates ("obsolete"))))))
        (should (eq (fzfa--source-async-out src "a" 10) t))
        (should-not (fzfa-source-request-output src))))))

(ert-deftest fzfa-session-presentation-change-rematerializes-without-rescore ()
  "Highlight policy and hook changes rebuild Lisp output, not native work."
  (let* ((default-directory "/tmp/")
         (fzfa-highlight nil)
         (fzf-native-highlight-fn 'highlight-a)
         (src (fzfa-make-source :command "ls"))
         (submits 0)
         (snapshots 0))
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _)
                 (cl-incf submits)))
              ((symbol-function 'fzf-native-async-status)
               (lambda (&rest _)
                 '(:state complete :snapshot-generation 7
                   :stale nil :filtered 1 :total 1)))
              ((symbol-function 'fzf-native-async-snapshot)
               (lambda (&rest _)
                 (cl-incf snapshots)
                 (list :state 'complete :snapshot-generation 7 :stale nil
                       :candidates
                       (list (format "%S/%S"
                                     fzfa-highlight
                                     fzf-native-highlight-fn))
                       :filtered 1 :total 1))))
      (should (equal (fzfa--source-async-out src "a" 10)
                     '(final ("nil/highlight-a") 1 1)))
      ;; Stable presentation reuses the already materialized object.
      (should (equal (fzfa--source-async-out src "a" 10)
                     '(final ("nil/highlight-a") 1 1)))
      (setq fzfa-highlight t)
      (should (equal (fzfa--source-async-out src "a" 10)
                     '(final ("t/highlight-a") 1 1)))
      (setq fzf-native-highlight-fn 'highlight-b)
      (should (equal (fzfa--source-async-out src "a" 10)
                     '(final ("t/highlight-b") 1 1)))
      (should (= submits 1))
      (should (= snapshots 3)))))

(ert-deftest fzfa-session-helm-adapter-does-not-copy-pending-snapshot ()
  "Helm keeps its Lisp cache instead of copying stale native candidates."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls"))
         (submits 0)
         (statuses 0)
         (snapshots 0))
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _)
                 (cl-incf submits)
                 77))
              ((symbol-function 'fzf-native-async-status)
               (lambda (&rest _)
                 (cl-incf statuses)
                 '(:state running :snapshot-generation 3 :total 100)))
              ((symbol-function 'fzf-native-async-snapshot)
               (lambda (&rest _)
                 (cl-incf snapshots)
                 (error "pending request must not materialize candidates"))))
      (should (eq (fzfa--source-async-candidates src "q" 10) t))
      (should (eq (fzfa--source-async-candidates src "q" 10) t))
      (should (= submits 1))
      (should (= statuses 2))
      (should (= snapshots 0)))))

(ert-deftest fzfa-session-helm-adapter-preserves-terminal-failure-state ()
  "Helm must distinguish matcher failure from a final empty result."
  (let ((src (fzfa-make-source :command "ls")))
    (cl-letf (((symbol-function 'fzfa--source-async-out)
               (lambda (&rest _) '(failed "matcher OOM" 42))))
      (should (equal (fzfa--source-async-candidates src "q" 10)
                     '(failed "matcher OOM" 42))))))

(ert-deftest fzfa-helm-session-stats-use-live-pool-boundary ()
  "Helm's header total must track the growing session pool."
  (require 'fzfa-helm)
  (cl-letf (((symbol-function 'fzf-native-async-status)
             (lambda (&rest _)
               '(:filtered 5 :total 100 :pool-generation 125))))
    (should (equal (fzfa-helm--async-stats-suffix 'fake-handle)
                   " (5/125)"))))

(ert-deftest fzfa-minibuffer-owner-rejects-nested-session ()
  "A callback belongs only to the minibuffer session that created it."
  (let* ((window (selected-window))
         (original (window-buffer window))
         (outer (generate-new-buffer " *fzfa owner outer*"))
         (nested (generate-new-buffer " *fzfa owner nested*"))
         (outer-session (list 'outer)))
    (unwind-protect
        (cl-letf (((symbol-function 'active-minibuffer-window)
                   (lambda () window)))
          (set-window-buffer window outer)
          (with-current-buffer outer
            (setq fzfa--minibuffer-session outer-session))
          (should (fzfa--minibuffer-owner-p
                   outer window outer-session))
          (set-window-buffer window nested)
          (with-current-buffer nested
            (setq fzfa--minibuffer-session (list 'nested)))
          (should-not (fzfa--minibuffer-owner-p
                       outer window outer-session)))
      (set-window-buffer window original)
      (kill-buffer outer)
      (kill-buffer nested))))

(ert-deftest fzfa-frontend-exhibit-acknowledges-supported-refresh ()
  "The poller receives success only after a supported frontend refreshes."
  (let* ((window (selected-window))
         (previews 0)
         (exhibits 0)
         (fzfa--preview-run-fn (lambda () (cl-incf previews))))
    (cl-progv '(vertico-mode icomplete-mode) '(t nil)
      (cl-letf (((symbol-function 'active-minibuffer-window)
                 (lambda () window))
                ((symbol-function 'vertico--exhibit)
                 (lambda () (cl-incf exhibits))))
        (should (eq (fzfa--frontend-exhibit) t))
        (should (= exhibits 1))
        (should (= previews 1))
        (set 'vertico-mode nil)
        (should-not (fzfa--frontend-exhibit))
        (should (= exhibits 1))
        (should (= previews 1))))))

(ert-deftest fzfa-manual-actions-recheck-minibuffer-owner ()
  "Candidate lookup must not let old preview or apply code enter a new picker."
  (let* ((window (selected-window))
         (original (window-buffer window))
         (outer (generate-new-buffer " *fzfa action outer*"))
         (nested (generate-new-buffer " *fzfa action nested*"))
         (outer-session (list 'outer))
         previews applies)
    (unwind-protect
        (cl-letf (((symbol-function 'active-minibuffer-window)
                   (lambda () window))
                  ((symbol-function 'fzfa--frontend-candidate)
                   (lambda ()
                     (set-window-buffer window nested)
                     "old candidate"))
                  ((symbol-function 'fzfa--preview-call)
                   (lambda (&rest args) (push args previews)))
                  ((symbol-function 'fzfa--resolve-apply)
                   (lambda (_candidate)
                     (lambda (candidate) (push candidate applies))))
                  ((symbol-function 'fzfa-resolve-candidate)
                   (lambda (candidate _session) candidate))
                  ((symbol-function 'minibuffer-selected-window)
                   (lambda () window)))
          (with-current-buffer outer
            (setq fzfa--minibuffer-session outer-session))
          (with-current-buffer nested
            (setq fzfa--minibuffer-session (list 'nested)))
          (dolist (command '(fzfa-preview-current fzfa-apply-current))
            (set-window-buffer window outer)
            (funcall command))
          (should-not previews)
          (should-not applies))
      (set-window-buffer window original)
      (kill-buffer outer)
      (kill-buffer nested))))

(ert-deftest fzfa-helm-single-source-preserves-last-result-on-failure ()
  "A terminal matcher failure must not blank a single Helm source."
  (require 'fzfa-helm)
  (let* ((original-maker (symbol-function 'fzfa-make-source))
         (outcomes '(("last-good") (failed "matcher OOM" 1)))
         (helm-alive-p t)
         (helm-pattern "")
         (helm-map (make-sparse-keymap))
         runtime-source first second)
    (cl-letf (((symbol-function 'fzfa-helm--ensure-loaded) #'ignore)
              ((symbol-function 'fzfa--spawn)
               (lambda (&rest _) 'fake-handle))
              ((symbol-function 'fzfa-make-source)
               (lambda (&rest args)
                 (setq runtime-source (apply original-maker args))))
              ((symbol-function 'helm-make-source)
               (lambda (_name _class &rest args) args))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _) 'fake-timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'fzfa-source--stop) #'ignore)
              ((symbol-function 'fzfa--source-async-candidates)
               (lambda (&rest _) (pop outcomes))))
      (pcase-let* ((`(,source-plist . ,stop)
                     (fzfa-helm--async-source-and-stop
                      "test" "printf x" "/tmp/" #'identity 10))
                    (callback (plist-get source-plist :candidates)))
        (unwind-protect
            (setq first (funcall callback)
                  second (funcall callback))
          (funcall stop)))
      (should (equal first '("last-good")))
      (should (equal second '("last-good")))
      (should (equal (fzfa-source-last-result runtime-source)
                     '("last-good")))
      (should-not (fzfa-source-retry-timer runtime-source)))))

(ert-deftest fzfa-helm-multi-source-preserves-last-result-on-failure ()
  "A terminal matcher failure must not blank a Helm multi source."
  (require 'fzfa-helm)
  (let ((outcomes '(("last-good") (failed "matcher OOM" 1)))
        (helm-alive-p t)
        (helm-pattern "")
        (helm-map (make-sparse-keymap))
        (helm-after-update-hook nil)
        (helm-move-selection-after-hook nil)
        first second)
    (cl-letf (((symbol-function 'fzfa-helm--ensure-loaded) #'ignore)
              ((symbol-function 'fzfa--spawn)
               (lambda (&rest _) 'fake-handle))
              ((symbol-function 'fzfa-source--stop) #'ignore)
              ((symbol-function 'fzfa--preview-handler)
               (lambda (&rest _) nil))
              ((symbol-function 'fzfa--sessions-push) #'ignore)
              ((symbol-function 'helm-make-source)
               (lambda (_name _class &rest args) args))
              ((symbol-function 'run-with-timer)
               (lambda (&rest _) 'fake-timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'fzfa--source-async-candidates)
               (lambda (&rest _)
                 (or (pop outcomes) '(failed "unexpected call" 1))))
              ((symbol-function 'helm)
               (lambda (&rest args)
                 (let* ((helm-sources (plist-get args :sources))
                        (callback (plist-get (car helm-sources)
                                             :candidates)))
                   (setq first (funcall callback)
                         second (funcall callback)))
                 nil)))
      (fzfa-helm--read
       '((:name "one" :command "printf one" :action identity)
         (:name "two" :command "printf two" :action identity))
       :prompt "test: ")
      (should (equal first '("last-good")))
      (should (equal second '("last-good"))))))

(ert-deftest fzfa-session-pending-total-uses-live-pool-boundary ()
  "A running request reports candidates collected now, not an older result."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls")))
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _) 77))
              ((symbol-function 'fzf-native-async-status)
               (lambda (&rest _)
                 '(:state running :stale t :snapshot-generation 3
                   :pool-generation 125 :total 100))))
      (should (equal (fzfa--source-async-out src "q" 10)
                     '(pending . 125))))))

(ert-deftest fzfa-session-running-status-reports-producer-failure-once ()
  "Producer failure is visible even before its matcher request completes."
  (let* ((default-directory "/tmp/")
         (fzfa--async-failed-producers nil)
         (src (fzfa-make-source :command "bad-producer"))
         messages)
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _) 77))
              ((symbol-function 'fzf-native-async-status)
               (lambda (&rest _)
                 '(:state running :stale t :snapshot-generation 3
                   :pool-generation 125 :total 100
                   :producer-state failed :producer-error "read failed")))
              ((symbol-function 'fzfa--log) #'ignore)
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (should (equal (fzfa--source-async-out src "q" 10)
                     '(pending . 125)))
      (should (equal (fzfa--source-async-out src "q" 10)
                     '(pending . 125)))
      (should (= (length messages) 1))
      (should (string-match-p "source command failed" (car messages)))
      (should (memq 'fake-handle fzfa--async-failed-producers)))))

(ert-deftest fzfa-session-failed-request-is-terminal-and-reported-once ()
  "A persistent matcher failure is terminal, visible, and does not retry-loop."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls"))
         (submits 0)
         messages)
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p)
               (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _)
                 (+ 16 (cl-incf submits))))
              ((symbol-function 'fzf-native-async-status)
               (lambda (_handle request-id)
                 (list :request-id request-id
                       :state 'failed
                       :snapshot-generation 7
                       :error "matcher OOM"
                       :total 42)))
              ((symbol-function 'fzfa--log) #'ignore)
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages))))
      (should (equal (fzfa--source-async-out src "q" 10)
                     '(failed "matcher OOM" 42)))
      (should (equal (fzfa--source-async-out src "q" 10)
                     '(failed "matcher OOM" 42)))
      (should (= submits 1))
      (should (= (fzfa-source-request-id src) 17))
      (should (= (length messages) 1))
      (should (string-match-p "matcher OOM" (car messages))))))

(ert-deftest fzfa-session-nonpublishing-request-releases-ownership ()
  "Superseded, cancelled, unknown, or idle requests retry immediately."
  (dolist (terminal-state '(superseded cancelled unknown idle))
    (let* ((default-directory "/tmp/")
           (src (fzfa-make-source :command "ls"))
           (submits 0)
           (statuses 0)
           (snapshots 0))
      (setf (fzfa-source-handle src) 'fake-handle)
      (cl-letf (((symbol-function 'fzfa--session-api-p)
                 (lambda () t))
                ((symbol-function 'fzf-native-async-submit)
                 (lambda (&rest _)
                   (+ 16 (cl-incf submits))))
                ((symbol-function 'fzf-native-async-status)
                 (lambda (_handle request-id)
                   (cl-incf statuses)
                   (if (= request-id 17)
                       (list :request-id request-id :state terminal-state
                             :snapshot-generation 1 :total 42)
                     (list :request-id request-id :state 'complete
                           :snapshot-generation 2 :stale nil
                           :filtered 1 :total 42))))
                ((symbol-function 'fzf-native-async-snapshot)
                 (lambda (_handle request-id)
                   (cl-incf snapshots)
                   (list :request-id request-id :state 'complete
                         :snapshot-generation 2 :stale nil
                         :candidates '("alpha") :filtered 1 :total 42))))
        (should (equal (fzfa--source-async-out src "q" 10)
                       '(pending . 42)))
        (should (= (fzfa-source-request-id src) 18))
        (should (fzfa-source-request-signature src))
        (should (equal (fzfa--source-async-out src "q" 10)
                       '(final ("alpha") 1 42)))
        (should (= submits 2))
        (should (= statuses 2))
        (should (= snapshots 1))
        (should (= (fzfa-source-request-id src) 18))))))

(ert-deftest fzfa-session-poller-replaces-nonpublishing-request-in-one-refresh ()
  "A committed terminal generation must not strand replacement work."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls"))
         (sources (vector src))
         (submits 0)
         (refreshes 0)
         (fzfa-input-throttle 0)
         poll)
    (setf (fzfa-source-handle src) 'fake-handle
          (fzfa-source-last-gen src) 0
          (fzfa-source-request-id src) 17
          (fzfa-source-request-signature src)
          (fzfa--source-request-signature "q" 10))
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzfa--command-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-status)
               (lambda (_handle &optional request-id)
                 (if request-id
                     (list :request-id request-id :state 'superseded
                           :snapshot-generation 1 :total 42)
                   '(:snapshot-generation 1))))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _)
                 (+ 17 (cl-incf submits))))
              ((symbol-function 'input-pending-p) (lambda () nil)))
      (setq poll
            (fzfa--make-poll-fn
             sources (lambda () t)
             (lambda ()
               (cl-incf refreshes)
               (fzfa--source-async-out src "q" 10))
             (lambda () nil)))
      (funcall poll)
      (should (= refreshes 1))
      (should (= submits 1))
      (should (= (fzfa-source-last-gen src) 1))
      (should (= (fzfa-source-request-id src) 18))
      (should (fzfa-source-request-signature src)))))

(ert-deftest fzfa-session-poller-commits-after-scheduled-publication ()
  "A scheduled refresh must publish before its generation is committed."
  (let* ((src (fzfa-make-source :command "producer"))
         (sources (vector src))
         (fzfa-input-throttle 0)
         transaction poll)
    (setf (fzfa-source-handle src) 'handle
          (fzfa-source-last-gen src) 0)
    (cl-letf (((symbol-function 'fzfa--poll-generation) (lambda (_) 1))
              ((symbol-function 'input-pending-p) (lambda () nil)))
      (setq poll
            (fzfa--make-poll-fn
             sources (lambda () t) (lambda () t) (lambda () nil)
             (lambda (callback) (setq transaction callback))))
      (funcall poll)
      (should transaction)
      (should (= (fzfa-source-last-gen src) 0))
      (funcall transaction)
      (should (= (fzfa-source-last-gen src) 1)))))

(ert-deftest fzfa-session-poller-rejects-revoked-publication ()
  "A refresh must keep its generation pending after owner or handle changes."
  (dolist (revoked '(owner handle))
    (let* ((src (fzfa-make-source :command "producer"))
           (sources (vector src))
           (alive t)
           (fzfa-input-throttle 0)
           poll)
      (setf (fzfa-source-handle src) 'old-handle
            (fzfa-source-last-gen src) 0)
      (cl-letf (((symbol-function 'fzfa--poll-generation) (lambda (_) 1))
                ((symbol-function 'input-pending-p) (lambda () nil)))
        (setq poll
              (fzfa--make-poll-fn
               sources
               (lambda () alive)
               (lambda ()
                 (if (eq revoked 'owner)
                     (setq alive nil)
                   (setf (fzfa-source-handle src) 'new-handle))
                 t)
               (lambda () nil)
               (lambda (callback) (funcall callback))))
        (funcall poll)
        (should (= (fzfa-source-last-gen src) 0))))))

(ert-deftest fzfa-session-generation-change-rematerializes-result ()
  "A new native snapshot generation invalidates fzfa's candidate cache."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls"))
         (generation 1)
         (snapshots 0))
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _) 1))
              ((symbol-function 'fzf-native-async-status)
               (lambda (&rest _)
                 (list :state 'complete :stale nil
                       :snapshot-generation generation :total generation)))
              ((symbol-function 'fzf-native-async-snapshot)
               (lambda (&rest _)
                 (cl-incf snapshots)
                 (list :state 'complete :stale nil
                       :snapshot-generation generation
                       :candidates (list (format "result-%d" generation))
                       :filtered 1 :total generation))))
      (should (equal (fzfa--source-async-out src "q" 10)
                     '(final ("result-1") 1 1)))
      (should (equal (fzfa--source-async-out src "q" 10)
                     '(final ("result-1") 1 1)))
      (setq generation 2)
      (should (equal (fzfa--source-async-out src "q" 10)
                     '(final ("result-2") 1 2)))
      (should (= snapshots 2)))))

(ert-deftest fzfa-session-api-requires-complete-surface ()
  "A partial dynamic module cannot activate request-aware sessions."
  (cl-letf (((symbol-function 'fzf-native-async-submit) #'ignore)
            ((symbol-function 'fzf-native-async-snapshot) #'ignore)
            ((symbol-function 'fzf-native-async-status) #'ignore))
    (should (fzfa--session-api-p)))
  (cl-letf (((symbol-function 'fzf-native-async-submit) #'ignore)
            ((symbol-function 'fzf-native-async-snapshot) #'ignore)
            ((symbol-function 'fzf-native-async-status) nil))
    (should-not (fzfa--session-api-p))))

(ert-deftest fzfa-command-api-requires-session-start-and-stop ()
  "Command sources require the complete fzf-native 2.7 session contract."
  (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
            ((symbol-function 'fzf-native-async-start) #'ignore)
            ((symbol-function 'fzf-native-async-stop) #'ignore))
    (should (fzfa--command-api-p)))
  (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
            ((symbol-function 'fzf-native-async-start) nil)
            ((symbol-function 'fzf-native-async-stop) #'ignore))
    (should-not (fzfa--command-api-p)))
  (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () nil))
            ((symbol-function 'fzf-native-async-start) #'ignore)
            ((symbol-function 'fzf-native-async-stop) #'ignore))
    (should-not (fzfa--command-api-p))))

(ert-deftest fzfa-command-source-rejects-batch-only-module ()
  "A batch-only module must fail before fzfa calls an async native function."
  (cl-letf (((symbol-function 'fzfa--command-api-p) (lambda () nil)))
    (should-error (fzfa--spawn "printf alpha" default-directory)
                  :type 'user-error)))

(ert-deftest fzfa-session-source-does-not-use-the-combined-api ()
  "Missing session support is an error instead of a compatibility fallback."
  (let ((src (fzfa-make-source :command "printf alpha")))
    (setf (fzfa-source-handle src) 'fake-handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () nil)))
      (should-error (fzfa--source-async-out src "a" 10)
                    :type 'error))))

(ert-deftest fzfa-async-submit-preserves-raw-byte-query ()
  "Raw pathname bytes reach fzf-native unchanged."
  (let ((query (unibyte-string #x80 ?a))
        seen)
    (cl-letf (((symbol-function 'fzf-native-async-submit)
               (lambda (_handle value _limit)
                 (setq seen value)
                 1)))
      (should (= (fzfa--async-submit 'fake-handle query 10) 1))
      (should (eq seen query))
      (should (equal seen query)))))

(ert-deftest fzfa-producer-failure-with-partial-output-reports-once ()
  "A partial candidate list must not conceal a failed producer."
  (let ((fzfa--async-failed-producers nil)
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) messages)))
              ((symbol-function 'fzfa--log) #'ignore)
              ((symbol-function 'minibufferp) (lambda (&rest _) nil)))
      (let ((snapshot '(:candidates ("partial")
                        :producer-error "producer exited with status 7"
                        :producer-exit-status 7)))
        (fzfa--async-note-producer-failure 'fake-handle snapshot)
        (fzfa--async-note-producer-failure 'fake-handle snapshot))
      (should (= (length messages) 1))
      (should (string-match-p "source command failed" (car messages))))))

(ert-deftest fzfa-session-end-to-end-polls-one-native-request ()
  "Repeated renders poll one real native request until it is final."
  (skip-unless (and (fboundp 'fzf-native-async-start)
                    (fboundp 'fzf-native-async-submit)))
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "printf '%s\\n' alpha beta"))
         (handle (fzf-native-async-start
                  "printf '%s\\n' alpha beta" default-directory))
         (native-submit (symbol-function 'fzf-native-async-submit))
         (native-snapshot (symbol-function 'fzf-native-async-snapshot))
         (submits 0)
         (snapshots 0)
         result)
    (setf (fzfa-source-handle src) handle)
    (unwind-protect
        (progn
          (let ((deadline (+ (float-time) 3.0)))
            (while (and (< (float-time) deadline)
                        (not (plist-get (fzf-native-async-status handle)
                                        :reader-done)))
              (sleep-for 0.01)))
          (cl-letf (((symbol-function 'fzf-native-async-submit)
                     (lambda (&rest args)
                       (cl-incf submits)
                       (apply native-submit args)))
                    ((symbol-function 'fzf-native-async-snapshot)
                     (lambda (&rest args)
                       (cl-incf snapshots)
                       (apply native-snapshot args))))
            (let ((deadline (+ (float-time) 3.0)))
              (while (and (< (float-time) deadline)
                          (not (eq (car-safe
                                    (setq result
                                          (fzfa--source-async-out
                                           src "a" 10)))
                                   'final)))
                (sleep-for 0.01)))
            ;; A stable redraw must reuse the candidate list just built.
            (should (equal (fzfa--source-async-out src "a" 10) result)))
          (should (equal result '(final ("alpha" "beta") 2 2)))
          (should (= submits 1))
          (should (= snapshots 1)))
      (fzfa-source--stop src))))

(ert-deftest fzfa-session-end-to-end-reports-partial-producer-failure ()
  "Real partial output remains visible while its producer failure is shown."
  (skip-unless (and (fboundp 'fzf-native-async-start)
                    (fboundp 'fzf-native-async-submit)))
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source
               :command "printf '%s\\n' partial; exit 7"))
         (handle (fzf-native-async-start
                  "printf '%s\\n' partial; exit 7" default-directory))
         (fzfa--async-failed-producers nil)
         messages
         result)
    (setf (fzfa-source-handle src) handle)
    (unwind-protect
        (progn
          (let ((deadline (+ (float-time) 3.0)))
            (while (and (< (float-time) deadline)
                        (not (plist-get (fzf-native-async-status handle)
                                        :reader-done)))
              (sleep-for 0.01)))
          (cl-letf (((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages)))
                    ((symbol-function 'fzfa--log) #'ignore)
                    ((symbol-function 'minibufferp) (lambda (&rest _) nil)))
            (let ((deadline (+ (float-time) 3.0)))
              (while (and (< (float-time) deadline)
                          (not (eq (car-safe
                                    (setq result
                                          (fzfa--source-async-out
                                           src "part" 10)))
                                   'final)))
                (sleep-for 0.01))))
          (should (equal result '(final ("partial") 1 1)))
          (should (= (length messages) 1))
          (should (string-match-p "source command failed"
                                  (car messages))))
      (fzfa-source--stop src))))

(ert-deftest fzfa-source-cands-fn-normalized ()
  "Producer-fn constructor normalizes a list into a 2-arg producer."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :candidates '("a" "b" "c"))))
    (should (functionp (fzfa-source-cands-fn src)))
    ;; Firing the normalized producer should yield the list back.
    (let (got)
      (funcall (fzfa-source-cands-fn src) "ignored"
               (lambda (cands) (setq got cands)))
      (should (equal got '("a" "b" "c"))))))

(ert-deftest fzfa-candidates-kind-preserves-existing-function-classes ()
  "Candidate classification must preserve normalization's existing split."
  (let ((calls 0))
    (should
     (eq (fzfa--candidates-kind (lambda () (cl-incf calls))) 'zero))
    (should
     (eq (fzfa--candidates-kind (lambda (_input) (cl-incf calls)))
         'producer))
    (should
     (eq (fzfa--candidates-kind
          (lambda (_input _callback _extra) (cl-incf calls)))
         'producer))
    (should (zerop calls))))

(ert-deftest fzfa-source-directory-falls-back-to-default ()
  "Constructor falls back to `default-directory' when none provided."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "ls")))
    (should (equal (fzfa-source-directory src) (expand-file-name "/tmp/")))))

(ert-deftest fzfa-source-display-clear-removes-overlays ()
  "`fzfa-source--display-clear' deletes display-overlays and clears slot."
  (with-temp-buffer
    (insert "hello")
    (let* ((default-directory "/tmp/")
           (src (fzfa-make-source :command "ls"))
           (ov1 (make-overlay 1 2))
           (ov2 (make-overlay 3 5)))
      (setf (fzfa-source-display-overlays src) (list ov1 ov2))
      (fzfa-source--display-clear src)
      (should (null (fzfa-source-display-overlays src)))
      ;; Overlays should be detached.
      (should (null (overlay-buffer ov1)))
      (should (null (overlay-buffer ov2))))))

(ert-deftest fzfa-source-stop-is-idempotent ()
  "`fzfa-source--stop' is safe to call twice."
  (with-temp-buffer
    (let* ((default-directory "/tmp/")
           (src (fzfa-make-source :command "ls")))
      (setf (fzfa-source-request-id src) 42
            (fzfa-source-request-signature src) '("q" 10 smart t))
      ;; Call twice; should not error and must drop request ownership.
      (fzfa-source--stop src)
      (fzfa-source--stop src)
      (should (null (fzfa-source-handle src)))
      (should (null (fzfa-source-restart-timer src)))
      (should (null (fzfa-source-retry-timer src)))
      (should (= (fzfa-source-request-id src) 0))
      (should-not (fzfa-source-request-signature src)))))

(ert-deftest fzfa-source-stop-cancels-timers ()
  "`fzfa-source--stop' cancels restart-timer and retry-timer slots."
  (with-temp-buffer
    (let* ((default-directory "/tmp/")
           (src (fzfa-make-source :command "ls"))
           (rt (run-with-timer 3600 nil #'ignore))
           (yt (run-with-timer 3600 nil #'ignore)))
      (setf (fzfa-source-restart-timer src) rt
            (fzfa-source-retry-timer src) yt)
      (fzfa-source--stop src)
      (should (null (fzfa-source-restart-timer src)))
      (should (null (fzfa-source-retry-timer src)))
      ;; Cancelled timers are no longer in timer-list.
      (should-not (memq rt timer-list))
      (should-not (memq yt timer-list)))))

(ert-deftest fzfa-source-stop-bumps-producer-token ()
  "For producer-kind sources, `fzfa-source--stop' bumps prod-token."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :candidates '("a" "b")))
         (initial (fzfa-source-prod-token src)))
    (fzfa-source--stop src)
    (should (> (fzfa-source-prod-token src) initial))))

(ert-deftest fzfa-source-restart-producer-fires-callback ()
  "`fzfa-source--restart' on a producer source fires the producer and
updates snapshot, total, filtered, last-result."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :candidates '("x" "y" "z")))
         (refresh-fired 0))
    (setf (fzfa-source-request-id src) 42
          (fzfa-source-request-signature src) '("old" 10 smart t))
    (fzfa-source--restart src "ignored"
                          (lambda () (cl-incf refresh-fired)))
    (should (equal (fzfa-source-snapshot src) '("x" "y" "z")))
    (should (= (fzfa-source-total src) 3))
    (should (= (fzfa-source-filtered src) 3))
    (should (equal (fzfa-source-last-result src) '("x" "y" "z")))
    (should (= refresh-fired 1))
    (should (equal (fzfa-source-current-cmd src) "ignored"))
    (should (= (fzfa-source-request-id src) 0))
    (should-not (fzfa-source-request-signature src))))

(ert-deftest fzfa-source-restart-producer-stale-callback-dropped ()
  "Later prod-token bump invalidates an in-flight producer callback."
  ;; Use an async-firing producer (captures the callback for deferred firing).
  (let* ((default-directory "/tmp/")
         (deferred-cb nil)
         (src (fzfa-make-source
               :candidates (lambda (_input cb)
                             (setq deferred-cb cb)))))
    ;; Fire restart; producer captures cb but doesn't call it.
    (fzfa-source--restart src "first" #'ignore)
    (let ((stashed-cb deferred-cb))
      ;; Bump token so the next restart invalidates the stashed callback.
      (fzfa-source--restart src "second" #'ignore)
      ;; Invoke the stashed (now-stale) callback.
      (funcall stashed-cb '("stale"))
      ;; Snapshot should not have updated from the stale callback.
      (should-not (equal (fzfa-source-snapshot src) '("stale"))))))

(ert-deftest fzfa-source-display-cycle-hidden-to-compact ()
  "Hidden→compact transition materializes #CMD# in the buffer."
  (with-temp-buffer
    ;; Set up minibuffer-like environment: prompt-end is the buffer start.
    (let* ((default-directory "/tmp/")
           (fzfa-separator ?#)
           (src (fzfa-make-source :command "ls -la")))
      ;; Stub `minibuffer-prompt-end' for the helper.
      (cl-letf (((symbol-function 'minibuffer-prompt-end) #'point-min))
        (fzfa-source--display-cycle src ?#)
        (should (eq (fzfa-source-display-state src) 'compact))
        ;; Buffer now contains "#ls -la#".
        (should (string-match-p "^#ls -la#" (buffer-string)))
        (should (= 2 (length (fzfa-source-separator-overlays src))))))))

(ert-deftest fzfa-source-display-cycle-compact-to-full ()
  "Compact→full transition keeps separators and clears display-overlays."
  (with-temp-buffer
    (let* ((default-directory "/tmp/")
           (fzfa-separator ?#)
           (src (fzfa-make-source :command "ls -la")))
      (cl-letf (((symbol-function 'minibuffer-prompt-end) #'point-min))
        (fzfa-source--display-cycle src ?#)   ; hidden → compact
        (let ((sep-ovs-before (fzfa-source-separator-overlays src)))
          (fzfa-source--display-cycle src ?#) ; compact → full
          (should (eq (fzfa-source-display-state src) 'full))
          ;; Separator overlays survive compact→full.
          (should (equal (fzfa-source-separator-overlays src)
                         sep-ovs-before))
          ;; Display-overlays cleared (compact-mode only).
          (should (null (fzfa-source-display-overlays src))))))))

(ert-deftest fzfa-source-display-cycle-full-to-hidden ()
  "Full→hidden transition extracts CMD and removes separators."
  (with-temp-buffer
    (let* ((default-directory "/tmp/")
           (fzfa-separator ?#)
           (src (fzfa-make-source :command "ls -la")))
      (cl-letf (((symbol-function 'minibuffer-prompt-end) #'point-min))
        (fzfa-source--display-cycle src ?#)   ; hidden → compact
        (fzfa-source--display-cycle src ?#)   ; compact → full
        ;; Simulate user editing the CMD region.
        (goto-char (point-min))
        (search-forward "ls -la")
        (insert " --color")
        (fzfa-source--display-cycle src ?#)   ; full → hidden
        (should (eq (fzfa-source-display-state src) 'hidden))
        ;; Edited CMD captured back into the source.
        (should (equal (fzfa-source-command src) "ls -la --color"))
        ;; Separator overlays removed.
        (should (null (fzfa-source-separator-overlays src)))
        ;; Buffer no longer has #...# prefix.
        (should-not (string-match-p "^#" (buffer-string)))))))

;;; fzfa--sort-by-history

(ert-deftest fzfa-sort-by-history-score-ties-break-by-length ()
  "Score ties break by candidate length, shorter first.

Regression: M-x visuallinemo against three commands that all tie
at fzf-native score 290 used to surface `visual-line-mode' last
because the source order was alphabetical."
  (let* ((candidates '("global-visual-line-mode"
                       "menu-bar--visual-line-mode-enable"
                       "visual-line-mode"))
         (scored (mapcar (lambda (c)
                           (let ((s (copy-sequence c)))
                             (put-text-property 0 1 'completion-score 290 s)
                             s))
                         candidates)))
    (cl-letf (((symbol-function 'fzfa--current-query)
               (lambda (&rest _) "visuallinemo"))
              ((symbol-function 'fzfa--history-hash)
               (lambda () nil)))
      (should (equal (fzfa--sort-by-history scored)
                     (list (nth 2 scored)
                           (nth 0 scored)
                           (nth 1 scored)))))))

(ert-deftest fzfa-sort-by-history-honors-fzf-native-scores ()
  "End-to-end: fzf-native scoring + sort puts the shortest tied match first."
  (skip-unless (fboundp 'fzf-native-score-all))
  (let* ((candidates '("global-visual-line-mode"
                       "menu-bar--visual-line-mode-enable"
                       "visual-line-mode"))
         (scored (fzfa--bridge-defcustoms
                  #'fzf-native-score-all candidates "visuallinemo")))
    (cl-letf (((symbol-function 'fzfa--current-query)
               (lambda (&rest _) "visuallinemo"))
              ((symbol-function 'fzfa--history-hash)
               (lambda () nil)))
      (should (equal (substring-no-properties
                      (car (fzfa--sort-by-history scored)))
                     "visual-line-mode")))))

(ert-deftest fzfa-sort-by-history-higher-score-wins ()
  "Higher `completion-score' beats both history and length tiebreaks."
  (let* ((a (copy-sequence "aaaa"))
         (b (copy-sequence "bb")))
    (put-text-property 0 1 'completion-score 500 a)
    (put-text-property 0 1 'completion-score 100 b)
    (cl-letf (((symbol-function 'fzfa--current-query)
               (lambda (&rest _) "x"))
              ((symbol-function 'fzfa--history-hash)
               (lambda () nil)))
      (should (equal (fzfa--sort-by-history (list b a))
                     (list a b))))))

(ert-deftest fzfa-sort-by-history-async-preserves-order ()
  "Unscored candidates (async path) round-trip unchanged."
  (cl-letf (((symbol-function 'fzfa--current-query)
             (lambda (&rest _) "vis"))
            ((symbol-function 'fzfa--history-hash)
             (lambda () nil)))
    (should (equal (fzfa--sort-by-history '("zeta" "alpha" "beta"))
                   '("zeta" "alpha" "beta")))))

;;; fzfa--sort-by-history — Mode A / Mode B branches (Chunk 3)

(ert-deftest fzfa-sort-by-history-empty-input-nil ()
  "Null input yields nil."
  (cl-letf (((symbol-function 'fzfa--current-query)
             (lambda (&rest _) "anything"))
            ((symbol-function 'fzfa--history-hash)
             (lambda () nil)))
    (should-not (fzfa--sort-by-history nil))))

(ert-deftest fzfa-sort-by-history-single-sync-sorted ()
  "Sync input (each candidate carries `completion-score') reorders by
score, history, length.  Shorter higher-scoring candidate wins."
  (let* ((a (copy-sequence "find-file"))
         (b (copy-sequence "Buffer-menu-toggle-files-only"))
         (c (copy-sequence "ftp")))
    (put-text-property 0 1 'completion-score 32 a)
    (put-text-property 0 1 'completion-score 32 b)
    (put-text-property 0 1 'completion-score 32 c)
    (cl-letf (((symbol-function 'fzfa--current-query)
               (lambda (&rest _) "f"))
              ((symbol-function 'fzfa--history-hash)
               (lambda () nil)))
      (let ((sorted (fzfa--sort-by-history (list b a c))))
        ;; Shortest first when score+history tied.
        (should (equal (substring-no-properties (nth 0 sorted)) "ftp"))
        (should (equal (substring-no-properties (nth 1 sorted)) "find-file"))
        (should (equal (substring-no-properties (nth 2 sorted))
                       "Buffer-menu-toggle-files-only"))))))

(ert-deftest fzfa-sort-by-history-single-async-passthrough ()
  "Async input (no `completion-score') passes through verbatim — C
order is canonical."
  (cl-letf (((symbol-function 'fzfa--current-query)
             (lambda (&rest _) "f"))
            ((symbol-function 'fzfa--history-hash)
             (lambda () nil)))
    (let* ((input (list "find-file" "f90-mode" "ftp"))
           (out   (fzfa--sort-by-history input)))
      (should (equal out input)))))

(ert-deftest fzfa-sort-by-history-multi-passthrough ()
  "Multi-source input (tofu-tagged head) passes through verbatim — the
multi loop already applied per-source sort + highlight; a global
re-sort would trample the source-block ordering."
  (skip-unless (fboundp 'fzfa--tag))
  (let* ((hash    (make-hash-table :test 'equal))
         ;; Tag candidates as if they came from two different sources.
         (a (fzfa--tag (copy-sequence "alpha")  0 hash))
         (b (fzfa--tag (copy-sequence "beta")   1 hash))
         (c (fzfa--tag (copy-sequence "gamma")  0 hash))
         (input (list a b c)))
    (cl-letf (((symbol-function 'fzfa--current-query)
               (lambda (&rest _) "a"))
              ((symbol-function 'fzfa--history-hash)
               (lambda () nil)))
      (should (equal (fzfa--sort-by-history input) input)))))

;;; fzfa--score-history-length-sort + fzfa--build-history-hash (Chunk 4 helpers)

(ert-deftest fzfa-score-history-length-sort-shortest-wins-on-tied-score ()
  "Equal `completion-score' breaks by length (asc) when no history hash."
  (let* ((a (copy-sequence "alphabet"))
         (b (copy-sequence "alpha"))
         (c (copy-sequence "al")))
    (put-text-property 0 1 'completion-score 32 a)
    (put-text-property 0 1 'completion-score 32 b)
    (put-text-property 0 1 'completion-score 32 c)
    (let ((out (fzfa--score-history-length-sort (list a b c) nil)))
      (should (equal (mapcar #'substring-no-properties out)
                     '("al" "alpha" "alphabet"))))))

(ert-deftest fzfa-score-history-length-sort-history-beats-length ()
  "Equal score, distinct history positions → recency wins over length."
  (let* ((a (copy-sequence "long-name"))
         (b (copy-sequence "x"))
         (hash (make-hash-table :test 'equal)))
    (put-text-property 0 1 'completion-score 32 a)
    (put-text-property 0 1 'completion-score 32 b)
    ;; "long-name" is more recent than "x".
    (puthash "long-name" 0 hash)
    (puthash "x" 5 hash)
    (let ((out (fzfa--score-history-length-sort (list b a) hash)))
      (should (equal (mapcar #'substring-no-properties out)
                     '("long-name" "x"))))))

(ert-deftest fzfa-build-history-hash-nil-hist-sym ()
  "nil HIST-SYM returns nil."
  (should-not (fzfa--build-history-hash nil)))

(defvar fzfa--test-history-fixture nil
  "Test fixture for `fzfa-build-history-hash-recency-encoded'.")

(ert-deftest fzfa-build-history-hash-recency-encoded ()
  "Lower index = more recent.  Duplicates in the history list keep their
first \(= most-recent\) index."
  (let ((fzfa--test-history-fixture
         '("recent" "older" "oldest" "recent")))
    (let ((h (fzfa--build-history-hash 'fzfa--test-history-fixture)))
      (should (= (gethash "recent" h) 0))
      (should (= (gethash "older" h) 1))
      (should (= (gethash "oldest" h) 2)))))

;;; Multi-loop per-source sort + highlight (Chunk 4)
;;;
;;; The multi-loop site itself is hard to drive in isolation (it's
;;; inside a closure invoked by `completing-read').  These tests cover
;;; the per-source pipeline that the loop now calls: sort + history +
;;; highlight refresh, gated by completion-score presence.

(ert-deftest fzfa-multi-per-source-sort-skips-async-source ()
  "An async source's `last-result' (no `completion-score') round-trips
unchanged through the per-source pipeline."
  (let ((async-result '("zeta" "alpha" "beta")))
    (should-not (get-text-property 0 'completion-score (car async-result)))
    ;; The multi-loop branch for async sources is the `t' fall-through:
    ;; (t slot) — returns the slot verbatim.
    (should (equal async-result async-result))))

;;; Eager C-side highlight suppression — Chunk 6 (fussy pattern port)

(ert-deftest fzfa-setup-does-not-change-direct-native-matching-options ()
  "Initializing fzfa must not reconfigure unrelated fzf-native callers."
  (skip-unless (fboundp 'fzf-native-score-all))
  (let ((fzfa--setup-done nil)
        (fzfa-extensions nil)
        (completion-styles-alist (copy-tree completion-styles-alist))
        (fzf-native-case-mode 'respect)
        (fzfa-case-mode 'ignore)
        (fzf-native-batch-highlight nil)
        (fzfa-batch-highlight nil))
    (fzfa--ensure-setup)
    ;; The direct call keeps its caller-owned respect-case binding.
    (should-not (fzf-native-score-all '("Foo") "foo"))
    ;; fzfa's explicit bridge still applies its ignore-case policy.
    (should (equal (fzfa--bridge-defcustoms
                    #'fzf-native-score-all '("Foo") "foo")
                   '("Foo")))
    (dolist (fn '(fzf-native-async-start
                  fzf-native-async-submit
                  fzf-native-async-snapshot
                  fzf-native-async-status))
      (should-not (advice-member-p #'fzfa--bridge-defcustoms fn)))))

(ert-deftest fzfa-batch-highlight-nil-suppresses-c-highlight ()
  "Binding `fzfa-batch-highlight' to nil makes `fzf-native-score-all'
return faceless candidates — the bridge propagates the nil onto
`fzf-native-batch-highlight'.

This is the pre-condition Chunk 6 relies on: each score-all call site
binds nil locally so the C scorer skips its eager top-N face pass.
The post-sort `fzf-native-highlight-all' (inside
`fzfa--rank-and-highlight') is the sole face source for ivy / helm
\(vertico/icomplete also have the lazy fn from Chunk 2)."
  (skip-unless (fboundp 'fzf-native-score-all))
  (let* ((cands '("find-file" "format" "f90-mode"))
         (fzfa-batch-highlight nil)
         (out (fzfa--bridge-defcustoms #'fzf-native-score-all
                                       (copy-sequence cands) "f")))
    (dolist (c out)
      (should-not (text-property-not-all 0 (length c) 'face nil c)))))

(ert-deftest fzfa-batch-highlight-keeps-completion-score ()
  "Suppressing C-side highlight must NOT drop `completion-score'.  The
score property is what `fzfa--sort-by-history' /
`fzfa--score-history-length-sort' use for the score → history →
length tiebreak."
  (skip-unless (fboundp 'fzf-native-score-all))
  (let* ((cands '("find-file" "format"))
         (fzfa-batch-highlight nil)
         (out (fzfa--bridge-defcustoms #'fzf-native-score-all
                                       (copy-sequence cands) "f")))
    (dolist (c out)
      (should (get-text-property 0 'completion-score c)))))

;;; fzfa--rank-and-highlight (refactor of Chunk 4/5 per-source pipeline)

(ert-deftest fzfa-rank-and-highlight-passes-through-empty-query ()
  "Empty QUERY returns SLOT unchanged (no sort, no face)."
  (let ((slot '("alpha" "beta")))
    (should (eq (fzfa--rank-and-highlight slot "" nil) slot))))

(ert-deftest fzfa-rank-and-highlight-passes-through-empty-slot ()
  "Empty SLOT returns unchanged."
  (should (null (fzfa--rank-and-highlight nil "f" nil)))
  (should (equal (fzfa--rank-and-highlight '() "f" nil) '())))

(ert-deftest fzfa-rank-and-highlight-passes-through-async-slot ()
  "SLOT whose head lacks `completion-score' (async) round-trips
unchanged — C order is canonical for async."
  (let ((slot '("zeta" "alpha" "beta")))
    (should (eq (fzfa--rank-and-highlight slot "z" nil) slot))))

(ert-deftest fzfa-rank-and-highlight-sorts-sync-slot ()
  "SLOT with `completion-score' on the head gets score → history →
length sort.  Without history, length tiebreaks tied scores."
  (skip-unless (fboundp 'fzf-native-highlight-all))
  (let* ((a (copy-sequence "alphabet"))
         (b (copy-sequence "alpha"))
         (c (copy-sequence "al")))
    (put-text-property 0 1 'completion-score 32 a)
    (put-text-property 0 1 'completion-score 32 b)
    (put-text-property 0 1 'completion-score 32 c)
    (let ((out (fzfa--rank-and-highlight (list a b c) "al" nil)))
      (should (equal (mapcar #'substring-no-properties out)
                     '("al" "alpha" "alphabet"))))))

(ert-deftest fzfa-ivy-multi-leader-interleave-matches-design ()
  "Reference implementation for the ivy multi-source leader interleave.

Each source's per-source-sorted list contributes its top candidate to a
leader pool (source-rank order); remaining candidates flat-concat after
in the same source-rank order.  Empty sources contribute nothing.

This is the algorithm wired into the ivy multi push at fzfa.el:~3322;
this test exercises the algorithm in isolation so a regression in the
splice logic surfaces without standing up an ivy session."
  (let* ((g1 '("a1" "a2" "a3"))                ; source-rank 1
         (g3 '("c1" "c2"))                       ; source-rank 2
         (g2 '("b1" "b2"))                       ; source-rank 3 (async)
         (per-source (list g1 g3 g2))
         (expected '("a1" "c1" "b1" "a2" "a3" "c2" "b2"))
         leaders tails)
    (dolist (slot per-source)
      (when slot
        (push (car slot) leaders)
        (when (cdr slot)
          (push (cdr slot) tails))))
    (should (equal (append (nreverse leaders)
                           (apply #'append (nreverse tails)))
                   expected))))

(ert-deftest fzfa-ivy-multi-leader-interleave-skips-empty-source ()
  "Empty per-source slot contributes nothing to leaders or tails."
  (let* ((per-source '(("a1" "a2") nil ("c1" "c2")))
         leaders tails)
    (dolist (slot per-source)
      (when slot
        (push (car slot) leaders)
        (when (cdr slot)
          (push (cdr slot) tails))))
    (should (equal (append (nreverse leaders)
                           (apply #'append (nreverse tails)))
                   '("a1" "c1" "a2" "c2")))))

(ert-deftest fzfa-multi-per-source-sort-orders-sync-source ()
  "A sync source's `last-result' (with `completion-score') sorts by
score → length when no history is in effect."
  (skip-unless (fboundp 'fzf-native-highlight-all))
  (let* ((a (copy-sequence "alphabet"))
         (b (copy-sequence "al")))
    (put-text-property 0 1 'completion-score 32 a)
    (put-text-property 0 1 'completion-score 32 b)
    (let ((out (fzfa--score-history-length-sort (list a b) nil)))
      (should (equal (substring-no-properties (car out)) "al"))
      (should (equal (substring-no-properties (cadr out)) "alphabet")))))

(ert-deftest fzfa-sort-by-history-mixed-no-mode-b-dump ()
  "Mixed sync + async single-source input: async candidates must NOT
be silently dumped below sync.  Pre-fix Mode B treated `nil'
`completion-score' as 0 and pushed all async candidates to the bottom.

In the new design, mixed mid-flight state is rare in a single-source
session — but if it does occur, the head-of-list async branch fires
when the first candidate has no score, returning the list unchanged
rather than zero-scoring the async candidates."
  (let* ((s1 (copy-sequence "find-file")))
    (put-text-property 0 1 'completion-score 32 s1)
    (cl-letf (((symbol-function 'fzfa--current-query)
               (lambda (&rest _) "f"))
              ((symbol-function 'fzfa--history-hash)
               (lambda () nil)))
      ;; Async-first head: passes through (no score-based reorder).
      (let* ((input (list "ftp" s1))
             (out (fzfa--sort-by-history input)))
        (should (equal out input))))))

;;; fzfa-all-completions — completion-lazy-hilit plumbing (Chunk 2)

(ert-deftest fzfa-all-completions-sets-lazy-fn-when-bound ()
  "When `completion-lazy-hilit' is bound non-nil and
`fzf-native-highlight-one' is available, `fzfa-all-completions'
captures `completion-lazy-hilit-fn' so vertico/icomplete apply face per
visible candidate at render time."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((table (lambda (_str _pred _flag) '("alpha" "beta")))
         (completion-lazy-hilit t)
         (completion-lazy-hilit-fn nil))
    (fzfa-all-completions "a" table nil 0)
    (should (functionp completion-lazy-hilit-fn))))

(ert-deftest fzfa-all-completions-no-lazy-fn-when-unbound ()
  "When `completion-lazy-hilit' is nil the variable is left alone — ivy
and helm rely on the existing eager-highlight path."
  (let* ((table (lambda (_str _pred _flag) '("alpha")))
         (completion-lazy-hilit nil)
         (completion-lazy-hilit-fn nil))
    (fzfa-all-completions "a" table nil 0)
    (should-not completion-lazy-hilit-fn)))

(ert-deftest fzfa-all-completions-lazy-fn-highlights-correctly ()
  "Invoking the captured lazy fn produces a faced copy of the input
candidate at the matched position."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((table (lambda (_str _pred _flag) '("find-file")))
         (completion-lazy-hilit t)
         (completion-lazy-hilit-fn nil))
    (fzfa-all-completions "f" table nil 0)
    (let* ((ret (funcall completion-lazy-hilit-fn "find-file"))
           (face (get-text-property 0 'face ret)))
      (should (or (eq face 'completions-common-part)
                  (and (listp face)
                       (memq 'completions-common-part face)))))))

(ert-deftest fzfa-all-completions-lazy-highlight-uses-fzfa-policy ()
  "Lazy highlighting must use the same case policy as fzfa scoring."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((table (lambda (_str _pred _flag) '("Foo")))
         (completion-lazy-hilit t)
         (completion-lazy-hilit-fn nil)
         (fzfa-case-mode 'ignore)
         (fzf-native-case-mode 'respect))
    (fzfa-all-completions "foo" table nil 0)
    (let* ((ret (funcall completion-lazy-hilit-fn "Foo"))
           (face (get-text-property 0 'face ret)))
      (should (or (eq face 'completions-common-part)
                  (and (listp face)
                       (memq 'completions-common-part face)))))))

(ert-deftest fzfa-all-completions-empty-query-passes-through-lazy-fn ()
  "Empty query → the captured lazy fn returns the candidate unchanged
\(no face manipulation).  Avoids spurious clear-only work on the
backspace-to-empty frame."
  (skip-unless (fboundp 'fzf-native-highlight-one))
  (let* ((table (lambda (_str _pred _flag) '("alpha")))
         (completion-lazy-hilit t)
         (completion-lazy-hilit-fn nil))
    (fzfa-all-completions "" table nil 0)
    (let ((ret (funcall completion-lazy-hilit-fn "alpha")))
      (should (equal ret "alpha"))
      (should-not (text-property-not-all 0 (length ret) 'face nil ret)))))

(ert-deftest fzfa-all-completions-passes-through ()
  "Candidates returned by the table are not modified by `fzfa-all-completions'."
  (let* ((cands '("alpha" "beta" "gamma"))
         (table (lambda (_str _pred _flag) cands))
         (completion-lazy-hilit nil)
         (ret (fzfa-all-completions "a" table nil 0)))
    (should (equal ret cands))))

;;; fzfa--sort-by-history HISTORY-SYM threading

(ert-deftest fzfa-sort-by-history-nil-history-skips-history-on-empty-query ()
  "Nil HISTORY-SYM keeps input order on empty query — no global-history leak."
  (cl-letf (((symbol-function 'fzfa--current-query) (lambda (&rest _) ""))
            (minibuffer-history '("zeta" "alpha" "gamma")))
    (should (equal (fzfa--sort-by-history '("alpha" "beta" "gamma") nil)
                   '("alpha" "beta" "gamma")))))

(ert-deftest fzfa-sort-by-history-with-history-reorders-empty-query ()
  "Non-nil HISTORY-SYM reorders by that history's recency."
  (let ((my-hist '("gamma" "alpha")))
    (cl-letf (((symbol-function 'fzfa--current-query) (lambda (&rest _) "")))
      (cl-progv '(my-hist-sym) (list my-hist)
        (let ((sym (make-symbol "my-test-history")))
          (set sym my-hist)
          (should (equal (fzfa--sort-by-history '("alpha" "beta" "gamma") sym)
                         ;; gamma is most-recent, alpha next, beta absent → fallback
                         '("gamma" "alpha" "beta"))))))))

(ert-deftest fzfa-sort-by-history-nil-history-no-tiebreak-on-sync ()
  "Nil HISTORY-SYM on score-tied candidates falls straight through to length."
  (let* ((a (copy-sequence "zzz"))
         (b (copy-sequence "ab"))
         (c (copy-sequence "yyyy")))
    (dolist (s (list a b c))
      (put-text-property 0 1 'completion-score 50 s))
    (cl-letf (((symbol-function 'fzfa--current-query) (lambda (&rest _) "x"))
              (minibuffer-history '("zzz" "yyyy" "ab")))
      (let ((sorted (fzfa--sort-by-history (list a c b) nil)))
        ;; Score-tied → length asc: "ab" (2), "zzz" (3), "yyyy" (4).
        ;; `minibuffer-history' contents are NOT consulted — if they
        ;; were, "zzz" (most recent) would have outranked the length
        ;; tiebreak.
        (should (equal (substring-no-properties (nth 0 sorted)) "ab"))
        (should (equal (substring-no-properties (nth 1 sorted)) "zzz"))
        (should (equal (substring-no-properties (nth 2 sorted)) "yyyy"))))))

;;; fzfa--completion-metadata closure

(ert-deftest fzfa-completion-metadata-bare-fn-when-no-history ()
  "Without `:history', `display-sort-function' is the bare symbol — no closure."
  (let ((md (fzfa--completion-metadata 'fzfa-misc)))
    (should (eq (alist-get 'display-sort-function (cdr md))
                #'fzfa--sort-by-history))
    (should (eq (alist-get 'cycle-sort-function (cdr md))
                #'fzfa--sort-by-history))))

(ert-deftest fzfa-completion-metadata-closure-when-history-set ()
  "With `:history', the sort entry is a closure that captures the symbol."
  (let* ((sym (make-symbol "test-hist"))
         (_ (set sym '("aaa")))
         (md (fzfa--completion-metadata 'fzfa-misc :history sym))
         (fn (alist-get 'display-sort-function (cdr md))))
    (should (functionp fn))
    (should-not (eq fn #'fzfa--sort-by-history))
    ;; The closure should invoke sort with the captured history symbol.
    (cl-letf (((symbol-function 'fzfa--current-query) (lambda (&rest _) "")))
      (should (equal (funcall fn '("bbb" "aaa")) '("aaa" "bbb"))))))

;;; Resume

(ert-deftest fzfa-session-restore-spec-overlays-runtime-state ()
  "`:command' / `:display' / `:initial-input' on the record override the spec."
  (let* ((spec (list :name "x" :command "fd" :display 'hidden))
         (rec  (list :spec spec
                     :command "fd --no-ignore"
                     :display 'compact
                     :initial-input "lib"))
         (restored (fzfa--session-restore-spec rec)))
    (should (equal (plist-get restored :name) "x"))
    (should (equal (plist-get restored :command) "fd --no-ignore"))
    (should (eq (plist-get restored :display) 'compact))
    (should (equal (plist-get restored :initial-input) "lib"))))

(ert-deftest fzfa-session-restore-spec-leaves-nil-slots-alone ()
  "A nil capture slot keeps the original spec value (no clobber)."
  (let* ((spec (list :name "x" :command "fd" :display 'hidden))
         (rec  (list :spec spec :command nil :display nil
                     :initial-input nil))
         (restored (fzfa--session-restore-spec rec)))
    (should (equal (plist-get restored :command) "fd"))
    (should (eq (plist-get restored :display) 'hidden))
    (should-not (plist-get restored :initial-input))))

(ert-deftest fzfa-session-restore-spec-drops-empty-command ()
  "Empty `:command' capture does not overlay onto a :candidates spec.

`fzfa-make-source' defaults the runtime `command' slot to \"\" for
sources that have no `:command' in their spec.  Overlaying that
back onto the spec would trick `fzfa--read''s eager-start loop
into firing a shell handle on an empty cmd — kills the sync path."
  (let* ((spec (list :name "buffers" :candidates '("a" "b")
                     :category 'buffer))
         (rec  (list :spec spec :command "" :display 'hidden
                     :initial-input "m"))
         (restored (fzfa--session-restore-spec rec)))
    (should-not (plist-get restored :command))
    (should (equal (plist-get restored :candidates) '("a" "b")))
    (should (equal (plist-get restored :initial-input) "m"))))

(ert-deftest fzfa-sessions-push-skips-excluded-command ()
  "ENTRY-COMMAND in `fzfa-sessions-exclude-commands' is not pushed.

Replay pickers themselves capture into the ring would let the
user replay a replay (confusing — the user wants the underlying
session, not the picker that surfaced it).  Same goes for helm's
`helm-maybe-exit-minibuffer' dispatcher."
  (let ((fzfa--sessions nil)
        (fzfa-sessions-exclude-commands '(fzfa-replay-from-memory
                                          helm-maybe-exit-minibuffer))
        (sources (vector (fzfa-make-source :spec '(:name "x")))))
    (fzfa--sessions-push '((:name "x")) sources "p: " nil ""
                         'fzfa-replay-from-memory)
    (should-not fzfa--sessions)
    (fzfa--sessions-push '((:name "x")) sources "p: " nil ""
                         'helm-maybe-exit-minibuffer)
    (should-not fzfa--sessions)
    ;; A non-excluded command still pushes.
    (fzfa--sessions-push '((:name "x")) sources "p: " nil ""
                         'fzfa-fd)
    (should (= (length fzfa--sessions) 1))))

(ert-deftest fzfa-sessions-push-trims-to-max ()
  "Pushing past `fzfa-sessions-max' drops the oldest entries.

Uses distinct ENTRY-COMMANDs so dedup doesn't collapse the pushes
\(otherwise five identical (command, dir, narrow, filter) tuples
would dedup to one — see `fzfa-sessions-push-dedups-by-key')."
  (let ((fzfa--sessions nil)
        (fzfa-sessions-max 3)
        (sources (vector (fzfa-make-source :spec '(:name "x" :command "fd")))))
    (dotimes (i 5)
      (fzfa--sessions-push '((:name "x" :command "fd"))
                           sources
                           (format "p%d: " i) nil ""
                           (intern (format "cmd%d" i))))
    (should (= (length fzfa--sessions) 3))
    ;; Most-recent first — the latest push is at head.
    (should (equal (plist-get (car fzfa--sessions) :prompt) "p4: "))))

(ert-deftest fzfa-sessions-push-persists-runtime-state ()
  "Captured record carries source's current-cmd, display-state, last-query."
  (let* ((fzfa--sessions nil)
         (fzfa-sessions-max 16)
         (src (fzfa-make-source :spec '(:name "x" :command "fd"))))
    (setf (fzfa-source-command src) "fd --no-ignore"
          (fzfa-source-display-state src) 'full)
    (fzfa--sessions-push '((:name "x" :command "fd"))
                         (vector src) "p: " 0 "needle" 'fzfa-fd)
    (let* ((rec (aref (plist-get (car fzfa--sessions) :sources) 0)))
      (should (equal (plist-get rec :command) "fd --no-ignore"))
      (should (eq    (plist-get rec :display) 'full))
      (should (equal (plist-get rec :initial-input) "needle")))))

(ert-deftest fzfa-sessions-push-stamps-entry-command ()
  "ENTRY-COMMAND is stored on the session record's `:command' slot."
  (let* ((fzfa--sessions nil)
         (src (fzfa-make-source :spec '(:name "x" :command "fd"))))
    (fzfa--sessions-push '((:name "x" :command "fd"))
                         (vector src) "p: " 0 "" 'fzfa-fd)
    (should (eq (plist-get (car fzfa--sessions) :command) 'fzfa-fd))))

(ert-deftest fzfa-sessions-push-dedups-by-key ()
  "Same (command, directory, narrow-idx, filter) collapses to one record."
  (let* ((fzfa--sessions nil)
         (fzfa-sessions-max 16)
         (src (fzfa-make-source :spec '(:name "x" :command "fd")))
         (default-directory "/tmp/"))
    (dotimes (_ 5)
      (fzfa--sessions-push '((:name "x" :command "fd"))
                           (vector src) "p: " 0 "" 'fzfa-fd))
    (should (= (length fzfa--sessions) 1))))

(ert-deftest fzfa-sessions-push-distinct-filters-coexist ()
  "Different filters in the same dir / command keep separate records."
  (let* ((fzfa--sessions nil)
         (fzfa-sessions-max 16)
         (src (fzfa-make-source :spec '(:name "x" :command "fd")))
         (default-directory "/tmp/"))
    (fzfa--sessions-push '((:name "x" :command "fd"))
                         (vector src) "p: " 0 "alpha" 'fzfa-fd)
    (fzfa--sessions-push '((:name "x" :command "fd"))
                         (vector src) "p: " 0 "beta" 'fzfa-fd)
    (should (= (length fzfa--sessions) 2))))

(ert-deftest fzfa-sessions-push-initial-input-only-on-narrow-target ()
  "`:initial-input' attaches only to the narrowed source; others get nil."
  (let* ((fzfa--sessions nil)
         (fzfa-sessions-max 16)
         (a (fzfa-make-source :spec '(:name "a" :command "fd")))
         (b (fzfa-make-source :spec '(:name "b" :command "rg"))))
    (fzfa--sessions-push '((:name "a" :command "fd")
                           (:name "b" :command "rg"))
                         (vector a b) "p: " 1 "needle" 'fzfa-find-any)
    (let* ((srcs (plist-get (car fzfa--sessions) :sources)))
      (should-not (plist-get (aref srcs 0) :initial-input))
      (should (equal (plist-get (aref srcs 1) :initial-input) "needle")))))

(ert-deftest fzfa-replay-errors-on-empty-sessions ()
  "`fzfa-replay' signals a `user-error' when there's nothing to replay."
  (let ((fzfa--sessions nil))
    (should-error (fzfa-replay) :type 'user-error)))

(ert-deftest fzfa-replay-rebinds-this-command-to-entry-cmd ()
  "`fzfa-replay' lets `this-command' to the session's captured command.

Inside the inner `fzfa--read', the capture site reads
`this-command' to stamp the session's `:command' field.  Without
the rebind, a replay of an `fzfa-fd' session would push a new
session under `:command fzfa-replay' — the replay machinery's
name, not the work the user is doing.  With it, extending the
seeded filter (e.g. \"q\" → \"qe\") lands a fresh `fzfa-fd'
session in the ring."
  (let ((captured-this-command nil)
        (fzfa--sessions
         (list
          (list :command 'fzfa-fd
                :prompt "fd: "
                :sources (vector (list :spec '(:name "fzfa")
                                       :command "fd"
                                       :display 'hidden
                                       :initial-input "q"))))))
    (cl-letf (((symbol-function 'fzfa--read)
               (lambda (&rest _)
                 (setq captured-this-command this-command)
                 nil)))
      (let ((this-command 'fzfa-replay))
        (fzfa-replay)))
    (should (eq captured-this-command 'fzfa-fd))))

(ert-deftest fzfa-replay-accepts-explicit-session-arg ()
  "`fzfa-replay' accepts an optional REPLAY-SESSION argument.

Picker-route commands (`fzfa-replay-from-memory' /
`-from-file' / `-any') feed their selection through here so all
replay paths share one dispatch.  The arg overrides
`(car fzfa--sessions)' — replaying an off-head session shouldn't
require shuffling the ring."
  (let ((seen-prompt nil)
        ;; Head of the ring is a fzfa-buffer session — explicit arg
        ;; should win and replay the fzfa-fd one instead.
        (fzfa--sessions
         (list (list :command 'fzfa-buffer
                     :prompt "buffer: "
                     :sources (vector (list :spec '(:name "fzfa")
                                            :command ""
                                            :display 'hidden
                                            :initial-input nil)))))
        (target-session
         (list :command 'fzfa-fd
               :prompt "fd: "
               :sources (vector (list :spec '(:name "fzfa")
                                      :command "fd"
                                      :display 'hidden
                                      :initial-input "q")))))
    (cl-letf (((symbol-function 'fzfa--read)
               (lambda (_sources &rest args)
                 (setq seen-prompt (plist-get args :prompt))
                 nil)))
      (fzfa-replay target-session))
    (should (equal seen-prompt "fd: "))))

;;; Property recovery via snapshot lookup

(ert-deftest fzfa-snapshot-lookup-recovers-stripped-properties ()
  "Content-equal `member' lookup recovers the propertized original.

Mirrors the post-result recovery in `fzfa--read' and helm's action
lambda: when a frontend hands back a bare string (e.g.
`read-from-minibuffer' stripping under
`minibuffer-allow-text-properties' nil), `member' against the
source's snapshot returns the original propertized cell so
downstream consumers (`fzfa-location-jump' etc.) still see the
in-band metadata."
  (let* ((orig (propertize "1:hello world"
                           'fzfa-location '("test.el" . 1)))
         (snapshot (list orig
                         (propertize "2:foo"
                                     'fzfa-location '("test.el" . 2))))
         ;; Simulate a frontend strip: bare string with same content,
         ;; no properties.
         (returned (substring-no-properties orig))
         ;; The actual recovery expression used in both frontends.
         (recovered (or (car (member returned snapshot)) returned)))
    (should-not (get-text-property 0 'fzfa-location returned))
    (should (equal (get-text-property 0 'fzfa-location recovered)
                   '("test.el" . 1)))
    (should (eq recovered orig))))

(ert-deftest fzfa-snapshot-lookup-falls-back-when-absent ()
  "When the candidate isn't in the snapshot, return it unchanged."
  (let* ((snapshot '("a" "b"))
         (returned "c")
         (recovered (or (car (member returned snapshot)) returned)))
    (should (equal recovered "c"))))

;;; fzfa-replay serialization

(ert-deftest fzfa-replay-scrub-spec-substitutes-fn-candidates ()
  "Function-valued `:candidates' is replaced with the captured snapshot."
  (let* ((spec '(:name "x" :candidates (lambda () (buffer-list))
                 :category buffer))
         (snapshot '("*scratch*" "*Messages*"))
         (scrubbed (fzfa-replay--scrub-spec spec snapshot)))
    (should (equal (plist-get scrubbed :candidates) snapshot))
    (should (equal (plist-get scrubbed :name) "x"))
    (should (eq (plist-get scrubbed :category) 'buffer))))

(ert-deftest fzfa-replay-scrub-spec-passes-through-static-candidates ()
  "Static-list `:candidates' is preserved verbatim."
  (let* ((spec '(:name "x" :candidates ("a" "b") :category misc))
         (scrubbed (fzfa-replay--scrub-spec spec nil)))
    (should (equal (plist-get scrubbed :candidates) '("a" "b")))))

(ert-deftest fzfa-replay-scrub-spec-drops-other-function-slots ()
  "Lambdas under `:action' / `:annotate' / etc. are dropped (not readable)."
  (let* ((spec `(:name "x" :command "fd"
                 :action ,(lambda (c) c)
                 :annotate ,(lambda (c) "ann")
                 :group ,(lambda (c _) c)))
         (scrubbed (fzfa-replay--scrub-spec spec nil)))
    (should (equal (plist-get scrubbed :name) "x"))
    (should (equal (plist-get scrubbed :command) "fd"))
    (should-not (plist-get scrubbed :action))
    (should-not (plist-get scrubbed :annotate))
    (should-not (plist-get scrubbed :group))))

(ert-deftest fzfa-replay-scrub-session-preserves-metadata ()
  "Session-level metadata round-trips through the scrubber."
  (let* ((session
          (list :prompt "p: " :narrow-idx 1
                :timestamp 1718411234.5
                :directory "/tmp/"
                :command 'fzfa-find-any
                :sources
                (vector (list :spec '(:name "a")
                              :command "fd" :display 'hidden
                              :initial-input "x"
                              :snapshot nil))))
         (scrubbed (fzfa-replay--scrub-session session)))
    (should (equal (plist-get scrubbed :prompt) "p: "))
    (should (eq    (plist-get scrubbed :narrow-idx) 1))
    (should (=     (plist-get scrubbed :timestamp) 1718411234.5))
    (should (equal (plist-get scrubbed :directory) "/tmp/"))
    (should (eq    (plist-get scrubbed :command) 'fzfa-find-any))
    (let ((src (aref (plist-get scrubbed :sources) 0)))
      (should (equal (plist-get src :command) "fd"))
      (should (eq    (plist-get src :display) 'hidden))
      (should (equal (plist-get src :initial-input) "x")))))

(ert-deftest fzfa-replay-save-load-round-trip ()
  "`save-list' + `load-list' round-trips sessions through a temp file."
  (let* ((tmpfile (make-temp-file "fzfa-replay-test-"))
         (fzfa-replay-file tmpfile)
         (fzfa-replay-max-saved-items 10)
         (fzfa--sessions
          (list (list :prompt "p: " :narrow-idx nil
                      :timestamp 1718411234.5
                      :directory "/tmp/"
                      :sources
                      (vector (list :spec '(:name "x" :candidates ("a" "b"))
                                    :command "" :display 'hidden
                                    :initial-input "filt"
                                    :snapshot nil)))))
         (fzfa-replay--persisted-sessions nil))
    (unwind-protect
        (progn
          (fzfa-replay-save-list)
          (should (file-readable-p tmpfile))
          (fzfa-replay-load-list)
          (should (= (length fzfa-replay--persisted-sessions) 1))
          (let* ((session (car fzfa-replay--persisted-sessions))
                 (src (aref (plist-get session :sources) 0)))
            (should (equal (plist-get session :prompt) "p: "))
            (should (equal (plist-get src :initial-input) "filt"))
            (should (equal (plist-get (plist-get src :spec) :candidates)
                           '("a" "b")))))
      (delete-file tmpfile))))

;;; fzfa-replay pickers

(ert-deftest fzfa-replay-session-to-candidate-attaches-session-prop ()
  "The summary candidate carries the full session as a text property."
  (let* ((session (list :prompt "p: " :timestamp 1718411234.5
                        :directory "/tmp/" :sources []))
         (cand (fzfa-replay--session-to-candidate session 0)))
    (should (stringp cand))
    (should (eq (get-text-property 0 'fzfa-replay-session cand)
                session))))

(ert-deftest fzfa-replay-session-to-candidate-is-string-unique-per-idx ()
  "Two candidates built from sessions with identical summaries differ by tofu.

Guarantees `delete-consecutive-dups' in the picker doesn't
collapse same-minute / same-directory sessions."
  (let* ((session (list :prompt "p: " :timestamp 1718411234.5
                        :directory "/tmp/" :sources []))
         (a (fzfa-replay--session-to-candidate session 0))
         (b (fzfa-replay--session-to-candidate session 1)))
    (should-not (equal a b))
    ;; Visible portion (everything before the tofu suffix) is identical.
    (should (equal (substring a 0 (1- (length a)))
                   (substring b 0 (1- (length b)))))))

(ert-deftest fzfa-replay-annotate-includes-source-count ()
  "Annotation shows the source count.

Filter / query lives in the candidate display string itself now
\(see `fzfa-replay-session-to-candidate-bakes-in-filter') so it
surfaces under ivy / helm where annotations aren't rendered;
annotation carries the source-count afterthought for vertico."
  (let* ((session (list :narrow-idx 0
                        :sources
                        (vector (list :initial-input "phx" :snapshot nil)
                                (list :initial-input nil :snapshot nil))))
         (cand (propertize "stub" 'fzfa-replay-session session))
         (ann (fzfa-replay--annotate cand)))
    (should (string-match-p "2 src" ann))))

(ert-deftest fzfa-replay-session-to-candidate-bakes-in-filter ()
  "Captured `:initial-input' is in the candidate display string itself.

Inline columns guarantee the query surfaces under every frontend
\(ivy, helm don't render `:annotate'; vertico-buffer mode trims
the annotation column).  The candidate format is
`DATE  COMMAND  QUERY  DIR'."
  (let* ((session
          (list :timestamp 0
                :command 'fzfa-fd
                :directory "/dir/"
                :narrow-idx 0
                :sources
                (vector (list :initial-input "needle" :snapshot nil))))
         (cand (fzfa-replay--session-to-candidate session 0)))
    (should (string-match-p "needle" cand))
    (should (string-match-p "fzfa-fd" cand))))

(ert-deftest fzfa-replay-session-to-candidate-shows-dash-for-empty-query ()
  "Empty / nil `:initial-input' renders as a dash in the query column.

Empty filter would collapse the column and visually mis-align
the directory across candidates.  An en-dash placeholder keeps
the columns rectangular."
  (let* ((session
          (list :timestamp 0
                :command 'fzfa-fd
                :directory "/dir/"
                :narrow-idx 0
                :sources
                (vector (list :initial-input nil :snapshot nil))))
         (cand (fzfa-replay--session-to-candidate session 0)))
    (should (string-match-p "—" cand))))

(ert-deftest fzfa-replay-sessions-to-candidates-aligns-columns ()
  "Batch helper pads the COMMAND column to the widest entry's width.

Without batch-level alignment, a session whose `:command' is
`fzfa-fd' (7 chars) shares a list with `fzfa-replay-from-memory'
\(23 chars) and the per-row fixed-width column-seam shifts off-
grid for the short row.  Two-pass alignment fixes it by using
the widest cmd's width for every row.  Distinct query markers
\(`q1', `q2') so `string-match' isn't fooled by stray chars in
the time / command columns."
  (let* ((mk (lambda (cmd query)
               (list :timestamp 0
                     :command cmd
                     :directory "/d/"
                     :narrow-idx 0
                     :sources (vector (list :initial-input query
                                            :snapshot nil)))))
         (cands (fzfa-replay--sessions-to-candidates
                 (list (funcall mk 'fzfa-fd "q1")
                       (funcall mk 'fzfa-replay-from-memory "q2")))))
    (should (= (length cands) 2))
    (let ((p1 (string-match "q1" (nth 0 cands)))
          (p2 (string-match "q2" (nth 1 cands))))
      (should p1)
      (should p2)
      ;; Both candidates start their QUERY column at the same offset
      ;; because the COMMAND column was padded to the wider entry's
      ;; width.
      (should (= p1 p2)))))

(ert-deftest fzfa-replay-sessions-to-candidates-empty-sessions ()
  "Empty / nil SESSIONS returns nil (callers feed straight to picker)."
  (should-not (fzfa-replay--sessions-to-candidates nil))
  (should-not (fzfa-replay--sessions-to-candidates '())))

(ert-deftest fzfa-replay-session-to-candidate-underlines-query ()
  "Non-empty query string gets the `fzfa-replay-query' face for visibility."
  (let* ((session
          (list :timestamp 0
                :command 'fzfa-fd
                :directory "/dir/"
                :narrow-idx 0
                :sources
                (vector (list :initial-input "needle" :snapshot nil))))
         (cand (fzfa-replay--session-to-candidate session 0))
         (start (string-match "needle" cand)))
    (should start)
    (let ((face (get-text-property start 'face cand)))
      (should (or (eq face 'fzfa-replay-query)
                  (and (listp face) (memq 'fzfa-replay-query face)))))))

(ert-deftest fzfa-replay-group-buckets-by-recency ()
  "Sessions older than a week land in the Older bucket."
  (let* ((session (list :timestamp (- (float-time) (* 86400 30))))
         (cand (propertize "stub" 'fzfa-replay-session session)))
    (should (equal (fzfa-replay--group cand nil) "Older"))))

(ert-deftest fzfa-replay-group-passes-through-on-transform ()
  "Group with TRANSFORM non-nil returns the candidate unchanged."
  (let ((cand "any-string"))
    (should (eq (fzfa-replay--group cand t) cand))))

(ert-deftest fzfa-replay-load-async-missing-file-yields-nil ()
  "No file → callback invoked with nil immediately."
  (let* ((fzfa-replay-file "/tmp/fzfa-replay-test-nonexistent-XYZ.el")
         (called nil)
         (got 'unset))
    (fzfa-replay--load-async (lambda (s) (setq called t got s)))
    (should called)
    (should-not got)))

(ert-deftest fzfa-replay-load-async-cache-hit-fires-callback-sync ()
  "Pre-populated cache → callback runs synchronously with cached data."
  (let* ((tmpfile (make-temp-file "fzfa-replay-cache-"))
         (mtime (file-attribute-modification-time
                 (file-attributes tmpfile)))
         (fzfa-replay-file tmpfile)
         (fzfa-replay--cache-mtime mtime)
         (fzfa-replay--cache-sessions '(cached-marker))
         (called nil)
         (got 'unset))
    (unwind-protect
        (progn
          (fzfa-replay--load-async (lambda (s) (setq called t got s)))
          (should called)
          (should (equal got '(cached-marker))))
      (delete-file tmpfile))))

(ert-deftest fzfa-replay-save-skips-unreadable-sessions ()
  "Sessions with non-readable nested values (markers etc.) are dropped on save."
  (let* ((tmpfile (make-temp-file "fzfa-replay-skip-"))
         (fzfa-replay-file tmpfile)
         ;; A marker prints as `#<marker ...>' — not readable.
         (bad-cand (with-temp-buffer
                     (propertize "marker-laden"
                                 'fzfa-marker (point-marker))))
         (good-session
          (list :prompt "p: " :narrow-idx nil
                :timestamp 1.0 :directory "/tmp/"
                :sources (vector
                          (list :spec '(:name "g")
                                :command "" :display 'hidden
                                :initial-input "ok" :snapshot nil))))
         (bad-session
          (list :prompt "p: " :narrow-idx nil
                :timestamp 2.0 :directory "/tmp/"
                :sources (vector
                          ;; Function `:candidates' gets substituted with
                          ;; `:snapshot' by the scrubber — the marker
                          ;; nested in the snapshot rides into the spec
                          ;; and trips the readable-p round-trip check.
                          (list :spec `(:name "b" :candidates ,(lambda () nil))
                                :command "" :display 'hidden
                                :initial-input nil
                                :snapshot (list bad-cand)))))
         (fzfa--sessions (list bad-session good-session))
         (fzfa-replay--persisted-sessions nil))
    (unwind-protect
        (progn
          (fzfa-replay-save-list)
          (fzfa-replay-load-list)
          ;; Only the good session round-trips.
          (should (= (length fzfa-replay--persisted-sessions) 1))
          (let ((src (aref (plist-get
                            (car fzfa-replay--persisted-sessions) :sources)
                           0)))
            (should (equal (plist-get src :initial-input) "ok"))))
      (delete-file tmpfile))))

(ert-deftest fzfa-replay-save-invalidates-load-cache ()
  "`save-list' clears the mtime cache so the next load re-reads."
  (let* ((tmpfile (make-temp-file "fzfa-replay-cache-"))
         (fzfa-replay-file tmpfile)
         (fzfa--sessions nil)
         (fzfa-replay--cache-mtime '(123 456))
         (fzfa-replay--cache-sessions '(stale)))
    (unwind-protect
        (progn
          (fzfa-replay-save-list)
          (should-not fzfa-replay--cache-mtime)
          (should-not fzfa-replay--cache-sessions))
      (delete-file tmpfile))))

(ert-deftest fzfa-replay-save-respects-max-saved-items ()
  "Save truncates to `fzfa-replay-max-saved-items'."
  (let* ((tmpfile (make-temp-file "fzfa-replay-test-"))
         (fzfa-replay-file tmpfile)
         (fzfa-replay-max-saved-items 2)
         (fzfa--sessions
          (cl-loop for i below 5
                   collect (list :prompt (format "p%d: " i)
                                 :narrow-idx nil
                                 :timestamp (float-time)
                                 :directory "/tmp/"
                                 :command (intern (format "cmd-%d" i))
                                 :sources (vector
                                           (list :spec `(:name ,(format "n%d" i))
                                                 :command ""
                                                 :display 'hidden
                                                 :initial-input nil
                                                 :snapshot nil)))))
         (fzfa-replay--persisted-sessions nil))
    (unwind-protect
        (progn
          (fzfa-replay-save-list)
          (fzfa-replay-load-list)
          (should (= (length fzfa-replay--persisted-sessions) 2)))
      (delete-file tmpfile))))

(ert-deftest fzfa-replay-merge-prefers-in-memory ()
  "On dedup, IN-MEMORY entries supersede PERSISTED — newer wins.

The dedup key is (command, directory, narrow-idx, narrow target's
filter); two sessions with the same key collapse, with the
in-memory entry's timestamp preserved."
  (let* ((mk (lambda (cmd ts in-mem-p)
               (list :command cmd
                     :directory "/d/"
                     :narrow-idx nil
                     :timestamp ts
                     :prompt (if in-mem-p "in: " "on: ")
                     :sources (vector (list :spec '(:name "x")
                                            :initial-input nil)))))
         (in-mem (list (funcall mk 'foo 100 t)
                       (funcall mk 'bar 200 t)))
         (persisted (list (funcall mk 'foo 50 nil)    ; same key as in-mem[0]
                          (funcall mk 'baz 75 nil)))  ; unique
         (merged (fzfa-replay--merge-sessions in-mem persisted)))
    ;; Three distinct entries (foo/bar/baz), not four
    (should (= (length merged) 3))
    ;; Sorted by timestamp desc
    (should (equal (mapcar (lambda (s) (plist-get s :command)) merged)
                   '(bar foo baz)))
    ;; In-memory `foo' (ts=100) won over persisted `foo' (ts=50)
    (let ((foo (cl-find 'foo merged
                        :key (lambda (s) (plist-get s :command)))))
      (should (equal (plist-get foo :prompt) "in: "))
      (should (= (plist-get foo :timestamp) 100)))))

(ert-deftest fzfa-replay-save-accumulates-across-runs ()
  "Save preserves on-disk sessions whose keys aren't superseded.

Simulates two Emacs lifetimes: first run saves sessions A and B,
second run captures C and saves.  After the second save, the
file should hold A, B, AND C — not just C (the previous bug was
that fzfa--sessions overwrote persisted-sessions on save)."
  (let* ((tmpfile (make-temp-file "fzfa-replay-accum-"))
         (fzfa-replay-file tmpfile)
         (fzfa-replay-max-saved-items 16)
         (mk (lambda (cmd ts)
               (list :command cmd
                     :directory "/d/"
                     :narrow-idx nil
                     :timestamp ts
                     :prompt (format "%s: " cmd)
                     :sources (vector (list :spec `(:name ,(symbol-name cmd))
                                            :command ""
                                            :display 'hidden
                                            :initial-input nil
                                            :snapshot nil))))))
    (unwind-protect
        (progn
          ;; Run 1: capture A and B, save.
          (let ((fzfa--sessions (list (funcall mk 'a 100)
                                      (funcall mk 'b 200)))
                (fzfa-replay--persisted-sessions nil))
            (fzfa-replay-save-list))
          ;; Run 2: fresh in-memory ring (just C), load from disk into
          ;; persisted-sessions (gets A+B), save.
          (let ((fzfa-replay--persisted-sessions nil)
                (fzfa--sessions (list (funcall mk 'c 300))))
            (fzfa-replay-load-list)
            (should (= (length fzfa-replay--persisted-sessions) 2))
            (fzfa-replay-save-list)
            (should (= (length fzfa-replay--persisted-sessions) 3))
            (should (equal (mapcar (lambda (s) (plist-get s :command))
                                   fzfa-replay--persisted-sessions)
                           '(c b a)))))
      (delete-file tmpfile))))

;;; External-dispatch predicate

(ert-deftest fzfa-external-p-recognizes-common-video ()
  "Common video extensions match the external-open predicate."
  (should (fzfa--external-p "/tmp/foo.mp4"))
  (should (fzfa--external-p "/tmp/foo.MKV"))     ; case-insensitive
  (should (fzfa--external-p "/tmp/foo.webm")))

(ert-deftest fzfa-external-p-recognizes-audio ()
  "Common audio extensions match the external-open predicate."
  (should (fzfa--external-p "/tmp/foo.flac"))
  (should (fzfa--external-p "/tmp/foo.mp3"))
  (should (fzfa--external-p "/tmp/foo.opus")))

(ert-deftest fzfa-external-p-skips-text-files ()
  "Text / source files do not match — they should `find-file'."
  (should-not (fzfa--external-p "/tmp/foo.el"))
  (should-not (fzfa--external-p "/tmp/foo.txt"))
  (should-not (fzfa--external-p "/tmp/foo")))   ; no extension

(ert-deftest fzfa-smart-find-file-dispatches-to-external-on-match ()
  "Matching extension + non-nil command dispatches to the OS handler."
  (let ((called nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program _infile _dest _display &rest args)
                 (setq called (cons program args))
                 0))
              (fzfa-external-open-command "open"))
      (fzfa-smart-find-file "/tmp/movie.mp4")
      (should called)
      (should (equal (car called) "open"))
      (should (equal (cadr called) (expand-file-name "/tmp/movie.mp4"))))))

(ert-deftest fzfa-smart-find-file-falls-back-to-find-file ()
  "Non-matching extension routes through `find-file', skipping the handler."
  (let ((find-file-arg nil)
        (external-called nil))
    (cl-letf (((symbol-function 'find-file)
               (lambda (file) (setq find-file-arg file)))
              ((symbol-function 'call-process)
               (lambda (&rest _) (setq external-called t) 0))
              (fzfa-external-open-command "open"))
      (fzfa-smart-find-file "/tmp/notes.txt")
      (should (equal find-file-arg "/tmp/notes.txt"))
      (should-not external-called))))

(ert-deftest fzfa-smart-find-file-nil-command-skips-external ()
  "Nil `fzfa-external-open-command' disables external dispatch entirely."
  (let ((find-file-arg nil))
    (cl-letf (((symbol-function 'find-file)
               (lambda (file) (setq find-file-arg file)))
              (fzfa-external-open-command nil))
      (fzfa-smart-find-file "/tmp/movie.mp4")
      (should (equal find-file-arg "/tmp/movie.mp4")))))

(ert-deftest fzfa-smart-find-file-other-window-under-prefix ()
  "`C-u' prefix routes non-external files through `find-file-other-window'."
  (let ((other-arg nil)
        (find-arg nil))
    (cl-letf (((symbol-function 'find-file-other-window)
               (lambda (f) (setq other-arg f)))
              ((symbol-function 'find-file)
               (lambda (f) (setq find-arg f)))
              (current-prefix-arg '(4))
              (fzfa-external-open-command nil))
      (fzfa-smart-find-file "/tmp/notes.txt")
      (should (equal other-arg "/tmp/notes.txt"))
      (should-not find-arg))))

(ert-deftest fzfa-smart-find-file-external-wins-over-prefix ()
  "External dispatch fires even under `C-u'; prefix does not interpose."
  (let ((external-arg nil))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program _infile _dest _display &rest args)
                 (setq external-arg (cons program args)) 0))
              (current-prefix-arg '(4))
              (fzfa-external-open-command "open"))
      (fzfa-smart-find-file "/tmp/movie.mp4")
      (should (equal (car external-arg) "open")))))

;;; Action alist — slot resolution

(ert-deftest fzfa-resolve-action-slot-nil-fallback ()
  "Missing slot falls through to the `nil' slot's plist."
  (let ((fzfa-action-config
         '((fzfa-file
            (nil  :action fzfa-smart-find-file)
            ((16) :directory ignore)))))
    (let ((p (fzfa--resolve-action-slot 'fzfa-file '(4))))
      (should (eq (plist-get p :action) 'fzfa-smart-find-file))
      (should-not (plist-get p :directory)))))

(ert-deftest fzfa-resolve-action-slot-key-inheritance ()
  "Matched slot overlays the `nil' slot; unset keys inherit."
  (let ((fzfa-action-config
         '((fzfa-file
            (nil  :action fzfa-smart-find-file)
            ((16) :directory ignore)))))
    (let ((p (fzfa--resolve-action-slot 'fzfa-file '(16))))
      (should (eq (plist-get p :action) 'fzfa-smart-find-file))
      (should (eq (plist-get p :directory) 'ignore)))))

(ert-deftest fzfa-resolve-action-slot-matched-wins ()
  "Matched slot's key overrides `nil' slot's same key."
  (let ((fzfa-action-config
         '((fzfa-file
            (nil  :action fzfa-smart-find-file)
            ((4)  :action find-file-other-window)))))
    (should (eq (plist-get
                 (fzfa--resolve-action-slot 'fzfa-file '(4))
                 :action)
                'find-file-other-window))))

(ert-deftest fzfa-resolve-action-slot-unknown-category ()
  "Category not in the alist returns nil."
  (let ((fzfa-action-config '((fzfa-file (nil :action ignore)))))
    (should-not (fzfa--resolve-action-slot 'fzfa-buffer nil))))

;;; Visit dispatchers

(defvar fzfa-test--visit-seen nil
  "Scratch variable for visit-dispatcher tests.

`ert-deftest' rebinds locals in a way that breaks lexical-closure
observation, so tests use this dynvar to observe action calls.")

(ert-deftest fzfa-visit-file-dispatches-through-alist ()
  "`fzfa-visit-file' funcalls the category action, not `find-file' directly."
  (setq fzfa-test--visit-seen nil)
  (let ((fzfa-action-config
         '((fzfa-file
            (nil :action (lambda (f) (setq fzfa-test--visit-seen f)))))))
    (cl-letf (((symbol-function 'run-hooks) #'ignore))
      (fzfa-visit-file "/tmp/x")
      (should (equal fzfa-test--visit-seen "/tmp/x")))))

(ert-deftest fzfa-visit-buffer-dispatches-through-alist ()
  "`fzfa-visit-buffer' funcalls the `fzfa-buffer' action."
  (setq fzfa-test--visit-seen nil)
  (let ((fzfa-action-config
         '((fzfa-buffer
            (nil :action (lambda (b) (setq fzfa-test--visit-seen b)))))))
    (cl-letf (((symbol-function 'run-hooks) #'ignore))
      (fzfa-visit-buffer "some-buffer")
      (should (equal fzfa-test--visit-seen "some-buffer")))))

;;; Multi-candidates-fetch — async producer refresh

(ert-deftest fzfa-multi-candidates-fetch-async-refresh ()
  "Async-firing producer's callback schedules REFRESH-FN.

`fzfa-replay--file-producer'-shaped sources (callback fires from a
deferred timer, not inline) need an explicit push so the
just-arrived snapshot reaches the frontend.  Mirrors the existing
`fzfa-source--restart' contract.  Without this, the first
`fzfa-replay-any' call surfaces no file replays until the user
types something to re-tick the table arm.

Inspects `timer-idle-list' for the scheduled timer rather than
waiting for it to fire — `run-with-idle-timer 0 nil' needs idle
events to elapse, which batch mode doesn't generate."
  (let* ((refreshes 0)
         (refresh-fn (lambda () (cl-incf refreshes)))
         (async-cb nil)                 ; captured callback, fired later
         (producer
          (lambda (_input cb)
            (setq async-cb cb)))        ; defer — caller returns before
                                        ; the callback runs
         (source (fzfa-make-source :spec `(:candidates ,producer)))
         (hash (make-hash-table :test 'equal))
         (before (length timer-idle-list)))
    (fzfa--multi-candidates-fetch source 0 "q" hash nil refresh-fn)
    ;; sync-call window is closed; callback hasn't fired yet
    (should-not (fzfa-source-snapshot source))
    (should (= (length timer-idle-list) before))
    ;; Fire the deferred callback with candidates
    (funcall async-cb '("alpha" "beta"))
    (should (equal (fzfa-source-snapshot source) '("alpha" "beta")))
    ;; Scheduled an idle timer that wraps `refresh-fn'
    (should (= (length timer-idle-list) (1+ before)))
    (let ((tm (car timer-idle-list)))
      (apply (timer--function tm) (timer--args tm))
      (should (= refreshes 1))
      (cancel-timer tm))))

(ert-deftest fzfa-multi-candidates-fetch-sync-no-refresh ()
  "Sync producer's inline callback does NOT schedule REFRESH-FN.

Sync producers fire their callback during the funcall, so the
caller's pass (table arm / ivy push) already sees the new
snapshot — a refresh would be a redundant tick."
  (let* ((refresh-fn (lambda () 'sentinel))
         (producer
          (lambda (_input cb) (funcall cb '("x" "y"))))
         (source (fzfa-make-source :spec `(:candidates ,producer)))
         (hash (make-hash-table :test 'equal))
         (before (length timer-idle-list)))
    (fzfa--multi-candidates-fetch source 0 "q" hash nil refresh-fn)
    ;; Snapshot landed inline
    (should (equal (fzfa-source-snapshot source) '("x" "y")))
    ;; No scheduled refresh — the sync-call flag suppressed it
    (should (= (length timer-idle-list) before))))

(ert-deftest fzfa-multi-candidates-fetch-async-without-refresh-fn ()
  "Omitted REFRESH-FN: async callback still writes snapshot, no push."
  (let* ((async-cb nil)
         (producer (lambda (_input cb) (setq async-cb cb)))
         (source (fzfa-make-source :spec `(:candidates ,producer)))
         (hash (make-hash-table :test 'equal)))
    (fzfa--multi-candidates-fetch source 0 "q" hash)
    (funcall async-cb '("late"))
    ;; Should not signal even though refresh-fn was nil
    (should (equal (fzfa-source-snapshot source) '("late")))))

;;; `fzfa--source-fetch' — protocol-only helper shared by helm + multi

(ert-deftest fzfa-source-fetch-writes-snapshot-and-total ()
  "Producer output lands in `snapshot' and `total' after fetch."
  (let* ((producer (lambda (_in cb) (funcall cb '("a" "b" "c"))))
         (source (fzfa-make-source :spec `(:candidates ,producer))))
    (fzfa--source-fetch source "q")
    (should (equal (fzfa-source-snapshot source) '("a" "b" "c")))
    (should (= (fzfa-source-total source) 3))
    (should (equal (fzfa-source-prod-input source) "q"))))

(ert-deftest fzfa-source-fetch-skips-on-equal-query ()
  "Re-firing with the same query no-ops — producer not called again."
  (let* ((calls 0)
         (producer (lambda (_in cb) (cl-incf calls) (funcall cb '("x"))))
         (source (fzfa-make-source :spec `(:candidates ,producer))))
    (fzfa--source-fetch source "q")
    (fzfa--source-fetch source "q")
    (should (= calls 1))))

(ert-deftest fzfa-source-fetch-stale-callback-discarded ()
  "Re-firing with a new query bumps prod-token; old callback no-ops.

`prod-token' is the staleness guard — an async producer whose
callback arrives after a newer fetch has been issued must not
overwrite the fresher snapshot."
  (let* ((cbs nil)
         (producer (lambda (_in cb) (push cb cbs)))
         (source (fzfa-make-source :spec `(:candidates ,producer))))
    (fzfa--source-fetch source "q1")
    (fzfa--source-fetch source "q2")
    ;; cbs are in push order; (car cbs) is q2's, (cadr cbs) is q1's.
    (funcall (cadr cbs) '("stale"))
    (should-not (fzfa-source-snapshot source))
    (funcall (car cbs) '("fresh"))
    (should (equal (fzfa-source-snapshot source) '("fresh")))))

(ert-deftest fzfa-source-fetch-on-deliver-transforms-snapshot ()
  "ON-DELIVER's return becomes the snapshot value.

This is the tagging seam: `fzfa--multi-candidates-fetch' threads
its `fzfa--tag' mapper through here so candidates carry the
source→idx mapping by the time they hit the snapshot.  Helm
callers pass nil and take output verbatim."
  (let* ((producer (lambda (_in cb) (funcall cb '("a" "b"))))
         (source (fzfa-make-source :spec `(:candidates ,producer))))
    (fzfa--source-fetch source "q" nil
                        (lambda (cands)
                          (mapcar #'upcase cands)))
    (should (equal (fzfa-source-snapshot source) '("A" "B")))))

(ert-deftest fzfa-source-fetch-async-schedules-refresh-fn ()
  "Async producer schedules REFRESH-FN; sync skips it.

Same shape as `fzfa-multi-candidates-fetch-async-refresh' but
exercises the underlying helper directly so the contract is
locked even if the wrapper changes."
  (let* ((refreshes 0)
         (refresh-fn (lambda () (cl-incf refreshes)))
         (async-cb nil)
         (producer (lambda (_in cb) (setq async-cb cb)))
         (source (fzfa-make-source :spec `(:candidates ,producer)))
         (before (length timer-idle-list)))
    (fzfa--source-fetch source "q" refresh-fn)
    (should (= (length timer-idle-list) before))
    (funcall async-cb '("alpha"))
    (should (equal (fzfa-source-snapshot source) '("alpha")))
    (should (= (length timer-idle-list) (1+ before)))
    (let ((tm (car timer-idle-list)))
      (apply (timer--function tm) (timer--args tm))
      (should (= refreshes 1))
      (cancel-timer tm))))

(ert-deftest fzfa-source-fetch-queued-refresh-rechecks-token ()
  "Cleanup after delivery must invalidate an already queued refresh."
  (let* ((refreshes 0)
         (async-cb nil)
         (producer (lambda (_in cb) (setq async-cb cb)))
         (source (fzfa-make-source :spec `(:candidates ,producer)))
         (before (length timer-idle-list)))
    (fzfa--source-fetch source "q" (lambda () (cl-incf refreshes)))
    (funcall async-cb '("alpha"))
    (should (= (length timer-idle-list) (1+ before)))
    (let ((tm (car timer-idle-list)))
      (fzfa-source--stop source)
      (apply (timer--function tm) (timer--args tm))
      (should (= refreshes 0))
      (cancel-timer tm))))

(ert-deftest fzfa-source-fetch-callback-after-stop-is-inert ()
  "A producer callback arriving after cleanup must not mutate its source."
  (let* ((async-cb nil)
         (producer (lambda (_in cb) (setq async-cb cb)))
         (source (fzfa-make-source :spec `(:candidates ,producer)))
         (before (length timer-idle-list)))
    (fzfa--source-fetch source "q" #'ignore)
    (fzfa-source--stop source)
    (funcall async-cb '("late"))
    (should-not (fzfa-source-snapshot source))
    (should (= (length timer-idle-list) before))))

(ert-deftest fzfa-source-fetch-sync-no-refresh ()
  "Sync producer's inline callback does NOT schedule REFRESH-FN."
  (let* ((refresh-fn (lambda () 'sentinel))
         (producer (lambda (_in cb) (funcall cb '("x" "y"))))
         (source (fzfa-make-source :spec `(:candidates ,producer)))
         (before (length timer-idle-list)))
    (fzfa--source-fetch source "q" refresh-fn)
    (should (equal (fzfa-source-snapshot source) '("x" "y")))
    (should (= (length timer-idle-list) before))))

(ert-deftest fzfa-helm-producer-is-not-fired-during-construction ()
  "Helm must issue one producer call for the first real fetch, not a probe."
  (let* ((calls 0)
         (callback nil)
         (producer (lambda (_input cb)
                     (cl-incf calls)
                     (setq callback cb)))
         (captured nil)
         (test-helm-map (make-sparse-keymap))
         (fzfa-preview-key nil)
         (fzfa-display-key nil)
         (before (length timer-idle-list)))
    (cl-progv '(helm-map helm-pattern helm-alive-p)
        (list test-helm-map "" t)
      (cl-letf (((symbol-function 'fzfa-helm--ensure-loaded) #'ignore)
                ((symbol-function 'helm-make-source)
                 (lambda (&rest args) (setq captured args))))
        (fzfa-helm-make-sync-source :name "probe" :items producer)
        (should (= calls 0))
        (let ((candidates (plist-get (cddr captured) :candidates))
              (cleanup (plist-get (cddr captured) :cleanup)))
          (funcall candidates)
          (should (= calls 1))
          (funcall cleanup)
          (funcall callback '("late"))
          (should (= (length timer-idle-list) before)))))))

(ert-deftest fzfa-helm-multi-producer-construction-and-cleanup-lifecycle ()
  "A Helm multi producer starts on fetch and becomes inert after cleanup."
  (let* ((calls 0)
         (callback nil)
         (scheduled 0)
         (original-maker (symbol-function 'fzfa-make-source))
         (helm-alive-p t)
         (helm-pattern "")
         (helm-map (make-sparse-keymap))
         (helm-after-update-hook nil)
         (helm-move-selection-after-hook nil)
         (fzfa-preview-key nil)
         (fzfa-display-key nil)
         runtime-source
         token-before-cleanup
         token-after-cleanup)
    (cl-letf (((symbol-function 'fzfa-helm--ensure-loaded) #'ignore)
              ((symbol-function 'fzfa--preview-handler)
               (lambda (&rest _) nil))
              ((symbol-function 'fzfa--sessions-push) #'ignore)
              ((symbol-function 'fzfa-helm--cancel-stranded-follow-timer)
               #'ignore)
              ((symbol-function 'fzfa-make-source)
               (lambda (&rest args)
                 (setq runtime-source (apply original-maker args))))
              ((symbol-function 'helm-make-source)
               (lambda (_name _class &rest args) args))
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest _)
                 (cl-incf scheduled)
                 'fake-idle-timer))
              ((symbol-function 'cancel-timer) #'ignore)
              ((symbol-function 'helm)
               (lambda (&rest args)
                 (let* ((source-plist (car (plist-get args :sources)))
                        (fetch (plist-get source-plist :candidates))
                        (cleanup (plist-get source-plist :cleanup)))
                   (should (= calls 0))
                   (funcall fetch)
                   (should (= calls 1))
                   (setq token-before-cleanup
                         (fzfa-source-prod-token runtime-source))
                   (funcall cleanup)
                   (setq token-after-cleanup
                         (fzfa-source-prod-token runtime-source)
                         helm-alive-p nil))
                 nil)))
      (fzfa-helm--read
       `((:name "producer"
          :candidates ,(lambda (_input cb)
                         (cl-incf calls)
                         (setq callback cb))
          :action identity))
       :prompt "test: "))
    (should callback)
    (should (> token-after-cleanup token-before-cleanup))
    (funcall callback '("late"))
    (should-not (fzfa-source-snapshot runtime-source))
    (should (= scheduled 0))))

(ert-deftest fzfa-session-snapshot-restart-discards-obsolete-result ()
  "A source restart during snapshot construction revokes the old result."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "old"))
         (old-handle 'old-handle)
         (replacement-handle 'replacement-handle))
    (setf (fzfa-source-handle src) old-handle
          (fzfa-source-current-cmd src) "old")
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _) 41))
              ((symbol-function 'fzf-native-async-status)
               (lambda (&rest _)
                 '(:state complete :snapshot-generation 7
                   :stale nil :filtered 1 :total 1)))
              ((symbol-function 'fzf-native-async-snapshot)
               (lambda (&rest _)
                 (fzfa-source--restart src "replacement" #'ignore)
                 '(:state complete :snapshot-generation 7 :stale nil
                   :candidates ("obsolete") :filtered 1 :total 1)))
              ((symbol-function 'fzfa--defer-async-stop) #'ignore)
              ((symbol-function 'fzfa--spawn)
               (lambda (&rest _) replacement-handle)))
      (should (eq (fzfa--source-async-out src "o" 10) t))
      (should (eq (fzfa-source-handle src) replacement-handle))
      (should (= (fzfa-source-request-id src) 0))
      (should-not (fzfa-source-request-output src)))))

(ert-deftest fzfa-session-request-epoch-blocks-equal-id-aba-result ()
  "Reusing a handle and request ID does not restore revoked ownership."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "printf alpha"))
         (handle 'same-handle)
         (snapshot-calls 0)
         first second)
    (setf (fzfa-source-handle src) handle)
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _) 7))
              ((symbol-function 'fzf-native-async-status)
               (lambda (&rest _)
                 '(:state complete :snapshot-generation 1 :stale nil
                   :filtered 1 :total 1)))
              ((symbol-function 'fzf-native-async-snapshot)
               (lambda (&rest _)
                 (cl-incf snapshot-calls)
                 (when (= snapshot-calls 1)
                   (fzfa--source-clear-request src)
                   (fzfa--source-submit src "q" 10))
                 (list :state 'complete :snapshot-generation 1 :stale nil
                       :candidates
                       (list (if (= snapshot-calls 1) "obsolete" "fresh"))
                       :filtered 1 :total 1))))
      (setq first (fzfa--source-async-out src "q" 10)
            second (fzfa--source-async-out src "q" 10)))
    (should (eq first t))
    (should (equal second '(final ("fresh") 1 1)))))

(ert-deftest fzfa-session-failure-report-rechecks-request-owner ()
  "A source restart during failure reporting revokes the old status."
  (let* ((default-directory "/tmp/")
         (src (fzfa-make-source :command "old"))
         (old-handle 'old-handle)
         (replacement-handle 'replacement-handle)
         (messages 0)
         (snapshots 0))
    (setf (fzfa-source-handle src) old-handle
          (fzfa-source-current-cmd src) "old")
    (cl-letf (((symbol-function 'fzfa--session-api-p) (lambda () t))
              ((symbol-function 'fzf-native-async-submit)
               (lambda (&rest _) 42))
              ((symbol-function 'fzf-native-async-status)
               (lambda (&rest _)
                 '(:state complete :snapshot-generation 8 :stale nil
                   :producer-error "boom" :filtered 1 :total 1)))
              ((symbol-function 'fzf-native-async-snapshot)
               (lambda (&rest _)
                 (cl-incf snapshots)
                 (error "obsolete request was materialized")))
              ((symbol-function 'message)
               (lambda (&rest _)
                 (cl-incf messages)
                 (fzfa-source--restart src "replacement" #'ignore)))
              ((symbol-function 'fzfa--defer-async-stop) #'ignore)
              ((symbol-function 'fzfa--spawn)
               (lambda (&rest _) replacement-handle)))
      (let ((fzfa--async-failed-producers nil))
        (should (eq (fzfa--source-async-out src "o" 10) t)))
      (should (= messages 1))
      (should (= snapshots 0))
      (should (eq (fzfa-source-handle src) replacement-handle))
      (should (= (fzfa-source-request-id src) 0))
      (should-not (fzfa-source-request-output src)))))

(provide 'fzfa-test)
;;; fzfa-test.el ends here
