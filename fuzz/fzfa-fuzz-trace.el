;;; fzfa-fuzz-trace.el --- Versioned fuzz trace data  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Defines the on-disk format shared by fzfa's fuzz lanes.  Trace files are
;; plain data: generation finishes before a driver receives a trace.  Raw
;; producer bytes are stored as hexadecimal strings so invalid UTF-8 survives
;; printing, reading, locale changes, and copy/paste.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar read-eval)
(defvar read-circle)

(defconst fzfa-fuzz-trace-format-version 1
  "Current version of an executable fzfa fuzz trace.")

(defconst fzfa-fuzz-artifact-format-version 1
  "Current version of an fzfa fuzz failure artifact.")

(defconst fzfa-fuzz-trace--keys
  '(:format :target :root-seed :case-seed :environment
    :initial-state :actions)
  "Keys allowed at the top level of trace format 1.")

(defconst fzfa-fuzz-trace--artifact-keys
  '(:artifact-format :failure :trace)
  "Keys allowed at the top level of failure artifact format 1.")

(defconst fzfa-fuzz-trace--environment-keys
  '(:emacs-version :system-type :system-configuration :locale
    :fzfa-version :fzfa-revision :fzf-native-version :fzf-native-revision)
  "Keys required in the recorded format-1 environment.")

(defconst fzfa-fuzz-trace--locale-keys
  '(:language :coding :lang :lc-all)
  "Keys required in the recorded format-1 locale.")

(defconst fzfa-fuzz-trace--failure-keys
  '(:signature :oracle :seed :message :expected :observed)
  "Keys required in a format-1 structured failure record.")

(defvar fzfa-fuzz-trace--environment-cache nil
  "Environment data copied into each trace made by this process.")

(defun fzfa-fuzz-trace--proper-list-p (value)
  "Return non-nil when VALUE is a finite proper list."
  (and (listp value) (numberp (proper-list-p value))))

(defun fzfa-fuzz-trace--plist-keys (value description allowed)
  "Validate VALUE as a plist named DESCRIPTION using ALLOWED keys.

Return its keys in source order."
  (unless (fzfa-fuzz-trace--proper-list-p value)
    (error "%s is not a proper list: %S" description value))
  (let ((tail value)
        keys)
    (while tail
      (let ((key (pop tail)))
        (unless tail
          (error "%s has a key without a value: %S" description key))
        (pop tail)
        (unless (keywordp key)
          (error "%s has a non-keyword key: %S" description key))
        (unless (memq key allowed)
          (error "%s has an unknown key: %S" description key))
        (when (memq key keys)
          (error "%s repeats key %S" description key))
        (push key keys)))
    (nreverse keys)))

(defun fzfa-fuzz-trace--require-keys (keys required description)
  "Require KEYS to contain REQUIRED entries for DESCRIPTION."
  (dolist (key required)
    (unless (memq key keys)
      (error "%s is missing %S" description key))))

(defun fzfa-fuzz-trace--validate-environment (environment)
  "Validate format-1 ENVIRONMENT metadata."
  (let ((keys
         (fzfa-fuzz-trace--plist-keys
          environment "fzfa fuzz environment"
          fzfa-fuzz-trace--environment-keys)))
    (fzfa-fuzz-trace--require-keys
     keys fzfa-fuzz-trace--environment-keys "fzfa fuzz environment")
    (let* ((locale (plist-get environment :locale))
           (locale-keys
            (fzfa-fuzz-trace--plist-keys
             locale "fzfa fuzz locale" fzfa-fuzz-trace--locale-keys)))
      (fzfa-fuzz-trace--require-keys
       locale-keys fzfa-fuzz-trace--locale-keys "fzfa fuzz locale"))
    environment))

(defun fzfa-fuzz-trace-validate (trace)
  "Validate and return executable TRACE in format 1."
  (let ((keys
         (fzfa-fuzz-trace--plist-keys
          trace "fzfa fuzz trace" fzfa-fuzz-trace--keys)))
    (fzfa-fuzz-trace--require-keys
     keys fzfa-fuzz-trace--keys "fzfa fuzz trace")
    (unless (eql (plist-get trace :format)
                 fzfa-fuzz-trace-format-version)
      (error "Unsupported fzfa fuzz trace format: %S"
             (plist-get trace :format)))
    (unless (symbolp (plist-get trace :target))
      (error "Trace target is not a symbol: %S" (plist-get trace :target)))
    (dolist (key '(:root-seed :case-seed))
      (let ((seed (plist-get trace key)))
        (unless (and (integerp seed) (>= seed 0))
          (error "Trace %S is not a non-negative integer: %S" key seed))))
    (fzfa-fuzz-trace--validate-environment
     (plist-get trace :environment))
    (unless (fzfa-fuzz-trace--proper-list-p
             (plist-get trace :initial-state))
      (error "Trace initial state is not a proper list"))
    (unless (fzfa-fuzz-trace--proper-list-p (plist-get trace :actions))
      (error "Trace actions are not a proper list"))
    trace))

(defun fzfa-fuzz-trace-artifact-p (value)
  "Return non-nil when VALUE has a failure artifact marker."
  (and (listp value) (plist-member value :artifact-format)))

(defun fzfa-fuzz-trace-validate-artifact (artifact)
  "Validate and return failure ARTIFACT in format 1."
  (let ((keys
         (fzfa-fuzz-trace--plist-keys
          artifact "fzfa fuzz artifact"
          fzfa-fuzz-trace--artifact-keys)))
    (fzfa-fuzz-trace--require-keys
     keys fzfa-fuzz-trace--artifact-keys "fzfa fuzz artifact")
    (unless (eql (plist-get artifact :artifact-format)
                 fzfa-fuzz-artifact-format-version)
      (error "Unsupported fzfa fuzz artifact format: %S"
             (plist-get artifact :artifact-format)))
    (let* ((failure (plist-get artifact :failure))
           (failure-keys
            (fzfa-fuzz-trace--plist-keys
             failure "fzfa fuzz failure" fzfa-fuzz-trace--failure-keys)))
      (fzfa-fuzz-trace--require-keys
       failure-keys fzfa-fuzz-trace--failure-keys "fzfa fuzz failure")
      (unless (and (stringp (plist-get failure :signature))
                   (stringp (plist-get failure :oracle))
                   (integerp (plist-get failure :seed))
                   (>= (plist-get failure :seed) 0)
                   (stringp (plist-get failure :message)))
        (error "Artifact failure record is incomplete: %S" failure)))
    (let ((trace
           (fzfa-fuzz-trace-validate (plist-get artifact :trace))))
      (unless (= (plist-get (plist-get artifact :failure) :seed)
                 (plist-get trace :case-seed))
        (error "Artifact failure seed does not match its trace")))
    artifact))

(defun fzfa-fuzz-trace-value-trace (value)
  "Return the executable trace contained in trace or artifact VALUE."
  (if (fzfa-fuzz-trace-artifact-p value)
      (plist-get (fzfa-fuzz-trace-validate-artifact value) :trace)
    (fzfa-fuzz-trace-validate value)))

(defun fzfa-fuzz-trace-read (file)
  "Read and validate one trace or failure artifact from FILE.

Reader evaluation is disabled, and trailing forms are rejected."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((read-eval nil)
          (read-circle nil)
          value)
      (condition-case err
          (setq value (read (current-buffer)))
        (error
         (error "Cannot read fzfa fuzz trace %s: %s"
                file (error-message-string err))))
      (skip-chars-forward " \t\r\n")
      (unless (eobp)
        (error "Fzfa fuzz trace has trailing data: %s" file))
      (if (fzfa-fuzz-trace-artifact-p value)
          (fzfa-fuzz-trace-validate-artifact value)
        (fzfa-fuzz-trace-validate value)))))

(defun fzfa-fuzz-trace-write (file value)
  "Atomically write validated trace or artifact VALUE to FILE."
  (if (fzfa-fuzz-trace-artifact-p value)
      (fzfa-fuzz-trace-validate-artifact value)
    (fzfa-fuzz-trace-validate value))
  (let* ((expanded (expand-file-name file))
         (directory (file-name-directory expanded)))
    (make-directory directory t)
    (let ((temporary (make-temp-file
                      (expand-file-name ".fzfa-fuzz-write-" directory))))
      (unwind-protect
          (progn
            (let ((coding-system-for-write 'utf-8-unix)
                  (print-circle nil)
                  (print-escape-newlines t)
                  (print-length nil)
                  (print-level nil))
              (with-temp-file temporary
                (prin1 value (current-buffer))
                (terpri (current-buffer))))
            (rename-file temporary expanded t)
            expanded)
        (when (file-exists-p temporary)
          (delete-file temporary))))))

(defun fzfa-fuzz-trace-bytes-to-hex (bytes)
  "Encode exact BYTES as lowercase hexadecimal text.

An ASCII-only multibyte string is accepted because its byte representation is
unambiguous."
  (unless (stringp bytes)
    (error "Producer bytes are not a string: %S" bytes))
  (let ((bytes
         (if (multibyte-string-p bytes)
             (if (cl-every (lambda (character) (< character 128))
                           (string-to-list bytes))
                 (encode-coding-string bytes 'us-ascii t)
               (error "Producer bytes must be encoded before tracing: %S"
                      bytes))
           bytes)))
    (mapconcat (lambda (byte) (format "%02x" byte))
               (string-to-list bytes) "")))

(defun fzfa-fuzz-trace-hex-to-bytes (hex)
  "Decode validated hexadecimal string HEX into an unibyte string."
  (unless (and (stringp hex)
               (= (% (length hex) 2) 0)
               (string-match-p "\\`[[:xdigit:]]*\\'" hex))
    (error "Invalid producer byte encoding: %S" hex))
  (apply #'unibyte-string
         (cl-loop for index from 0 below (length hex) by 2
                  collect (string-to-number
                           (substring hex index (+ index 2)) 16))))

(defun fzfa-fuzz-trace-encode-string (string)
  "Encode STRING as replayable text or exact unibyte data."
  (unless (stringp string)
    (error "Trace string value is not a string: %S" string))
  (if (multibyte-string-p string)
      (list :text (substring-no-properties string))
    (list :bytes-hex
          (fzfa-fuzz-trace-bytes-to-hex
           (substring-no-properties string)))))

(defun fzfa-fuzz-trace-decode-string (encoded)
  "Decode ENCODED from `fzfa-fuzz-trace-encode-string'."
  (cond
   ((and (fzfa-fuzz-trace--proper-list-p encoded)
         (= (length encoded) 2)
         (eq (car encoded) :text)
         (stringp (cadr encoded)))
    (copy-sequence (cadr encoded)))
   ((and (fzfa-fuzz-trace--proper-list-p encoded)
         (= (length encoded) 2)
         (eq (car encoded) :bytes-hex))
    (fzfa-fuzz-trace-hex-to-bytes (cadr encoded)))
   (t (error "Invalid encoded trace string: %S" encoded))))

(defun fzfa-fuzz-trace-encode-strings (strings)
  "Encode a proper list of STRINGS for a trace."
  (unless (fzfa-fuzz-trace--proper-list-p strings)
    (error "Trace string collection is not a proper list: %S" strings))
  (mapcar #'fzfa-fuzz-trace-encode-string strings))

(defun fzfa-fuzz-trace-decode-strings (encoded)
  "Decode a proper list of ENCODED trace strings."
  (unless (fzfa-fuzz-trace--proper-list-p encoded)
    (error "Encoded trace strings are not a proper list: %S" encoded))
  (mapcar #'fzfa-fuzz-trace-decode-string encoded))

(defun fzfa-fuzz-trace--source-file (library)
  "Return readable Elisp source for LIBRARY, or nil."
  (when-let* ((located (locate-library library)))
    (let ((source
           (if (string-suffix-p ".elc" located)
               (substring located 0 -1)
             located)))
      (and (file-readable-p source) source))))

(defun fzfa-fuzz-trace--library-version (library)
  "Return LIBRARY's header version, or `unknown'."
  (if-let* ((file (fzfa-fuzz-trace--source-file library)))
      (with-temp-buffer
        (insert-file-contents file nil 0 4096)
        (if (re-search-forward
             "^;;[[:space:]]+Version:[[:space:]]*\\(.+\\)$" nil t)
            (string-trim (match-string 1))
          'unknown))
    'unknown))

(defun fzfa-fuzz-trace--git-revision (library)
  "Return the Git revision containing LIBRARY, or `unknown'."
  (if-let* ((file (fzfa-fuzz-trace--source-file library))
            (git (executable-find "git")))
      (let ((default-directory (file-name-directory file)))
        (with-temp-buffer
          (if (and (= 0 (call-process git nil t nil
                                      "rev-parse" "--verify" "HEAD"))
                   (goto-char (point-min))
                   (re-search-forward "[[:xdigit:]]\\{40\\}" nil t))
              (match-string 0)
            'unknown)))
    'unknown))

(defun fzfa-fuzz-trace-environment ()
  "Return cached version and platform data for a generated trace."
  (or fzfa-fuzz-trace--environment-cache
      (setq
       fzfa-fuzz-trace--environment-cache
       (list
        :emacs-version emacs-version
        :system-type system-type
        :system-configuration system-configuration
        :locale
        (list :language current-language-environment
              :coding locale-coding-system
              :lang (getenv "LANG")
              :lc-all (getenv "LC_ALL"))
        :fzfa-version (fzfa-fuzz-trace--library-version "fzfa")
        :fzfa-revision (fzfa-fuzz-trace--git-revision "fzfa")
        :fzf-native-version
        (fzfa-fuzz-trace--library-version "fzf-native")
        :fzf-native-revision
        (fzfa-fuzz-trace--git-revision "fzf-native")))))

(defun fzfa-fuzz-trace-create
    (target root-seed case-seed initial-state actions)
  "Create a TARGET trace for ROOT-SEED, CASE-SEED, and generated case data."
  (fzfa-fuzz-trace-validate
   (list :format fzfa-fuzz-trace-format-version
         :target target
         :root-seed root-seed
         :case-seed case-seed
         :environment (copy-tree (fzfa-fuzz-trace-environment))
         :initial-state initial-state
         :actions actions)))

(provide 'fzfa-fuzz-trace)
;;; fzfa-fuzz-trace.el ends here
