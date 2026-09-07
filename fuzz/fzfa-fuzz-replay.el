;;; fzfa-fuzz-replay.el --- Exact replay for fzfa fuzz traces  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Loads one versioned trace or failure artifact and dispatches it directly to
;; the matching driver.  Replay never invokes a generator.  When the input is
;; a failure artifact, replay succeeds only if the same stable oracle signature
;; fails again.

;;; Code:

(require 'cl-lib)
(require 'fzfa-fuzz-state)
(require 'fzfa-fuzz-producer)
(require 'fzfa-fuzz-live)

(defun fzfa-fuzz-replay--file ()
  "Return the trace path selected for exact replay."
  (or (when-let* ((file (getenv "FZFA_FUZZ_TRACE_FILE"))
                  ((not (string-empty-p file))))
        file)
      (error "Set FZFA_FUZZ_TRACE_FILE or use make replay-trace TRACE=FILE")))

(defun fzfa-fuzz-replay--execute (trace)
  "Execute validated TRACE without calling a generator."
  (pcase (plist-get trace :target)
    ((or 'completion-list 'state-producer 'state-poller 'message-owner)
     (fzfa-fuzz-state-run-trace trace))
    ('native-producer
     (unless (fzfa--command-api-p)
       (error "Native producer replay requires the fzf-native 2.7 session API"))
     (fzfa-fuzz-producer-run-trace trace))
    ('ugrep-command (fzfa-fuzz-tools-run-trace trace))
    ('live-icomplete
     (when noninteractive
       (error (concat "Live trace requires a real minibuffer; run "
                      "make replay-trace-live TRACE=FILE "
                      "LIVE_EMACS_FLAGS=-nw")))
     (fzfa-fuzz-live-run-trace trace))
    (target (error "No replay driver for trace target %S" target))))

(defun fzfa-fuzz-replay-value (value)
  "Replay trace or failure artifact VALUE.

Return `reproduced' when an artifact fails with its recorded signature and
`passed' when a plain trace completes."
  (let* ((artifact-p (fzfa-fuzz-trace-artifact-p value))
         (validated
          (if artifact-p
              (fzfa-fuzz-trace-validate-artifact value)
            (fzfa-fuzz-trace-validate value)))
         (trace (fzfa-fuzz-trace-value-trace validated))
         (expected-signature
          (and artifact-p
               (plist-get (plist-get validated :failure) :signature)))
         (fzfa-fuzz--record-failures nil)
         (fzfa-fuzz--last-failure-record nil))
    (condition-case err
        (progn
          (fzfa-fuzz-replay--execute trace)
          (when expected-signature
            (error "Trace completed instead of reproducing %s"
                   expected-signature))
          'passed)
      (fzfa-fuzz-failure
       (let ((actual
              (plist-get fzfa-fuzz--last-failure-record :signature)))
         (cond
          ((null expected-signature)
           (signal (car err) (cdr err)))
          ((equal actual expected-signature) 'reproduced)
          (t
           (error "Trace reproduced %S instead of %S"
                  actual expected-signature))))))))

(defun fzfa-fuzz-replay-file (file)
  "Replay one trace or failure artifact from FILE and print the result."
  (let* ((value (fzfa-fuzz-trace-read file))
         (trace (fzfa-fuzz-trace-value-trace value))
         (recorded-environment (plist-get trace :environment))
         (current-environment (fzfa-fuzz-trace-environment))
         (result (fzfa-fuzz-replay-value value)))
    (princ (format "recorded environment: %S\n" recorded-environment))
    (unless (equal recorded-environment current-environment)
      (princ (format "current environment:  %S\n" current-environment)))
    (princ
     (format "fzfa exact replay %s (%s, seed %d, format %d)\n"
             result (plist-get trace :target)
             (plist-get trace :case-seed) (plist-get trace :format)))
    result))

(defun fzfa-fuzz-replay-trace-batch ()
  "Replay the selected non-live trace in batch Emacs."
  (unless noninteractive
    (error "Batch trace replay was started in interactive Emacs"))
  (fzfa-fuzz-replay-file (fzfa-fuzz-replay--file)))

(defun fzfa-fuzz-replay-trace-live ()
  "Replay the selected live trace, then exit interactive Emacs."
  (if noninteractive
      (error "Live trace replay must run without --batch")
    (condition-case err
        (let* ((file (fzfa-fuzz-replay--file))
               (value (fzfa-fuzz-trace-read file))
               (trace (fzfa-fuzz-trace-value-trace value)))
          (unless (eq (plist-get trace :target) 'live-icomplete)
            (error "replay-trace-live received %S"
                   (plist-get trace :target)))
          (icomplete-vertical-mode 1)
          (fzfa-fuzz-live--assert-refcount-lifecycle)
          (fzfa-fuzz-replay-file file)
          (kill-emacs 0))
      ((error quit)
       (fzfa-fuzz-live--report
        (format "fzfa exact live replay failed: %s\n"
                (error-message-string err)))
       (kill-emacs 1)))))

(defun fzfa-fuzz-replay--expect-error (pattern function)
  "Require FUNCTION to signal an error matching PATTERN."
  (let (caught)
    (condition-case err
        (funcall function)
      (error (setq caught err)))
    (unless caught
      (error "Expected replay self-test error matching %S" pattern))
    (unless (string-match-p pattern (error-message-string caught))
      (error "Replay self-test got the wrong error: %s"
             (error-message-string caught)))))

(defun fzfa-fuzz-replay-selftest-batch ()
  "Check trace round trips, strict reading, and signature replay."
  (let* ((seed 9701)
         (fzfa-fuzz-state--mutation-source-count 1)
         (fzfa-fuzz-state--mutation-operation 'truncate)
         (trace
          (fzfa-fuzz-state--mutation-trace
           seed seed (fzfa-fuzz-rng-create :state seed)))
         (trace-file (make-temp-file "fzfa-fuzz-trace-" nil ".sexp"))
         (native-file
          (make-temp-file "fzfa-fuzz-native-trace-" nil ".sexp"))
         (artifact-file
          (make-temp-file "fzfa-fuzz-artifact-" nil ".sexp"))
         (trailing-file
          (make-temp-file "fzfa-fuzz-trailing-" nil ".sexp"))
         (eval-file
          (make-temp-file "fzfa-fuzz-read-eval-" nil ".sexp"))
         (circle-file
          (make-temp-file "fzfa-fuzz-read-circle-" nil ".sexp"))
         (raw (concat "ok" (unibyte-string #xff 0) "\n"))
         (state-producer-trace
          (fzfa-fuzz-state--producer-trace
           seed seed nil 4
           '((fetch "a") (deliver 0 ("alpha")) (run 0) (stop))))
         (poller-trace (fzfa-fuzz-state--poller-trace seed seed))
         (message-trace
          (fzfa-fuzz-state--message-trace seed seed 'owner))
         (first (fzfa-fuzz-producer--lines '("first")))
         (second (fzfa-fuzz-producer--lines '("second")))
         (native-trace
          (fzfa-fuzz-producer--trace
           seed seed
           (list :kind 'trace-selftest
                 :bytes (concat first second)
                 :exit 0 :failure nil
                 :interim-expected '("first")
                 :expected '("first" "second"))
           (list first second) 0.01)))
    (unwind-protect
        (progn
          (fzfa-fuzz-trace-write trace-file trace)
          (unless (equal-including-properties
                   trace (fzfa-fuzz-trace-read trace-file))
            (error "Versioned trace changed during write/read round trip"))
          (fzfa-fuzz-trace-write native-file native-trace)
          (unless (equal native-trace (fzfa-fuzz-trace-read native-file))
            (error "Native trace changed during write/read round trip"))
          (unless (and (eq (plist-get
                            (plist-get native-trace :initial-state)
                            :expected-present)
                           t)
                       (eq (plist-get
                            (plist-get native-trace :initial-state)
                            :interim-present)
                           t))
            (error "Native trace presence markers are not booleans"))
          ;; A driver must consume trace data, not ask its generator for more
          ;; random choices during exact replay.
          (cl-letf (((symbol-function 'fzfa-fuzz--integer)
                     (lambda (&rest _) (error "Replay requested randomness")))
                    ((symbol-function 'fzfa-fuzz--pick)
                     (lambda (&rest _) (error "Replay requested randomness"))))
            (dolist (plain-trace
                     (list trace state-producer-trace poller-trace
                           message-trace native-trace))
              (unless (eq (fzfa-fuzz-replay-value plain-trace) 'passed)
                (error "Plain %S trace did not replay"
                       (plist-get plain-trace :target)))))
          (unless (equal raw
                         (fzfa-fuzz-trace-hex-to-bytes
                          (fzfa-fuzz-trace-bytes-to-hex raw)))
            (error "Raw producer bytes changed during hex round trip"))
          (let ((process-environment (copy-sequence process-environment))
                captured)
            (setenv "FZFA_FUZZ_ARTIFACT_FILE" artifact-file)
            (let ((fzfa-fuzz-state--completion-result-mutator
                   (lambda (&rest _) nil)))
              (condition-case err
                  (fzfa-fuzz-state-run-trace trace)
                (fzfa-fuzz-failure (setq captured err))))
            (unless captured
              (error "Controlled trace failure did not signal"))
            (let* ((artifact (fzfa-fuzz-trace-read artifact-file))
                   (failure (plist-get artifact :failure)))
              (when (or (eq (plist-get failure :expected) :not-recorded)
                        (eq (plist-get failure :observed) :not-recorded))
                (error "Failure artifact omitted expected or observed state"))
              (let ((fzfa-fuzz-state--completion-result-mutator
                     (lambda (&rest _) nil)))
                (unless (eq (fzfa-fuzz-replay-value artifact) 'reproduced)
                  (error "Artifact did not reproduce its failure signature")))))
          (with-temp-file trailing-file
            (prin1 trace (current-buffer))
            (insert "\n(:second form)\n"))
          (fzfa-fuzz-replay--expect-error
           "trailing data"
           (lambda () (fzfa-fuzz-trace-read trailing-file)))
          (with-temp-file eval-file
            (insert "#.(error \"reader evaluation ran\")\n"))
          (fzfa-fuzz-replay--expect-error
           "Cannot read fzfa fuzz trace"
           (lambda () (fzfa-fuzz-trace-read eval-file)))
          (with-temp-file circle-file
            (insert "#1=(:format 1 :loop #1#)\n"))
          (fzfa-fuzz-replay--expect-error
           "Cannot read fzfa fuzz trace"
           (lambda () (fzfa-fuzz-trace-read circle-file)))
          (fzfa-fuzz-replay--expect-error
           "Unsupported fzfa fuzz trace format"
           (lambda ()
             (fzfa-fuzz-trace-validate
             (plist-put (copy-tree trace) :format 999))))
          (let ((unsafe (fzfa-fuzz-tools-trace seed seed)))
            (setq unsafe
                  (plist-put
                   unsafe :actions
                   '((write-normal "../outside") (run-ugrep))))
            (fzfa-fuzz-replay--expect-error
             "Unsafe ugrep fixture path"
             (lambda ()
               (fzfa-fuzz-tools--decode-trace
                unsafe (expand-file-name "fzfa-safe-root"
                                         temporary-file-directory)))))
          (princ
           (concat "fzfa trace self-test passed "
                   "(round trip, no RNG, strict reader, exact signature)\n")))
      (dolist (file (list trace-file native-file artifact-file
                          trailing-file eval-file circle-file))
        (when (file-exists-p file)
          (delete-file file))))))

(provide 'fzfa-fuzz-replay)
;;; fzfa-fuzz-replay.el ends here
