;;; fzfa-fuzz-reduce.el --- Reduce replayable fzfa failures  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Removes actions from a recorded failure while exact replay continues to
;; produce the same stable oracle signature.  Reduction first deletes groups
;; of actions, then tries every remaining single-action deletion until none is
;; accepted.  The result is therefore one-minimal with respect to deleting
;; actions: removing any one remaining action no longer reproduces the same
;; failure.

;;; Code:

(require 'cl-lib)
(require 'fzfa-fuzz-replay)

(defun fzfa-fuzz-reduce--output-file (input)
  "Return the configured reduction output path for INPUT."
  (or (when-let* ((file (getenv "FZFA_FUZZ_REDUCED_FILE"))
                  ((not (string-empty-p file))))
        (expand-file-name file))
      (concat (expand-file-name input) ".min.sexp")))

(defun fzfa-fuzz-reduce--without-range (actions start end)
  "Return ACTIONS without the half-open range from START to END."
  (append (cl-subseq actions 0 start) (cl-subseq actions end)))

(defun fzfa-fuzz-reduce--artifact-with-actions
    (artifact actions &optional failure)
  "Return a copy of ARTIFACT containing ACTIONS and optional FAILURE."
  (let ((trace (copy-tree (plist-get artifact :trace))))
    (setq trace (plist-put trace :actions (copy-tree actions)))
    (list :artifact-format fzfa-fuzz-artifact-format-version
          :failure (copy-tree (or failure (plist-get artifact :failure)))
          :trace trace)))

(defun fzfa-fuzz-reduce--reproducing-failure (artifact)
  "Return ARTIFACT's fresh failure when its stable signature reproduces.

Return nil when the trace passes, is invalid, or reaches a different failure."
  (let* ((artifact (fzfa-fuzz-trace-validate-artifact artifact))
         (trace (plist-get artifact :trace))
         (signature (plist-get (plist-get artifact :failure) :signature))
         (fzfa-fuzz--record-failures nil)
         (fzfa-fuzz--last-failure-record nil))
    (condition-case nil
        (progn
          (fzfa-fuzz-replay--execute trace)
          nil)
      (fzfa-fuzz-failure
       (when (equal
              (plist-get fzfa-fuzz--last-failure-record :signature)
              signature)
         (copy-tree fzfa-fuzz--last-failure-record)))
      ((error quit) nil))))

(defun fzfa-fuzz-reduce-artifact (artifact)
  "Reduce actions in failure ARTIFACT while preserving its signature.

The input must reproduce before reduction starts.  Return a plist containing
`:artifact', `:attempts', `:original-actions', and `:reduced-actions'.  The
returned artifact contains the expected and observed values captured from the
last accepted replay, not stale values copied from the larger input."
  (unless (fzfa-fuzz-trace-artifact-p artifact)
    (error "Trace reduction requires a failure artifact"))
  (fzfa-fuzz-trace-validate-artifact artifact)
  (let* ((original (copy-tree artifact))
         (original-actions
          (length (plist-get (plist-get artifact :trace) :actions)))
         (attempts 1)
         (baseline (fzfa-fuzz-reduce--reproducing-failure artifact))
         current actions)
    (unless baseline
      (error "Failure artifact does not reproduce its recorded signature"))
    (setq current
          (fzfa-fuzz-reduce--artifact-with-actions
           artifact (plist-get (plist-get artifact :trace) :actions) baseline)
          actions (plist-get (plist-get current :trace) :actions))
    (cl-labels
        ((try-actions
          (candidate-actions)
          (cl-incf attempts)
          (let* ((candidate
                  (fzfa-fuzz-reduce--artifact-with-actions
                   current candidate-actions))
                 (failure
                  (fzfa-fuzz-reduce--reproducing-failure candidate)))
            (when failure
              (fzfa-fuzz-reduce--artifact-with-actions
               candidate candidate-actions failure)))))
      ;; Delta debugging removes large contiguous groups before the more
      ;; expensive single-action fixpoint below.
      (let ((granularity 2)
            done)
        (while (and (> (length actions) 1) (not done))
          (let* ((count (length actions))
                 (chunk-size (/ (+ count granularity -1) granularity))
                 (start 0)
                 accepted)
            (while (and (< start count) (not accepted))
              (let* ((end (min count (+ start chunk-size)))
                     (candidate-actions
                      (fzfa-fuzz-reduce--without-range actions start end)))
                (setq accepted (try-actions candidate-actions))
                (setq start end)))
            (if accepted
                (setq current accepted
                      actions (plist-get (plist-get current :trace) :actions)
                      granularity (max 2 (1- granularity)))
              (if (< granularity count)
                  (setq granularity (min count (* 2 granularity)))
                (setq done t))))))
      ;; Restart from the first action after every accepted deletion.  This
      ;; reaches a fixpoint even when reproduction is not monotonic.
      (let ((keep-looking t))
        (while keep-looking
          (setq keep-looking nil)
          (let ((index 0))
            (while (< index (length actions))
              (let* ((candidate-actions
                      (fzfa-fuzz-reduce--without-range
                       actions index (1+ index)))
                     (accepted (try-actions candidate-actions)))
                (if accepted
                    (setq current accepted
                          actions
                          (plist-get (plist-get current :trace) :actions)
                          keep-looking t
                          index (length actions))
                  (cl-incf index))))))))
    (unless (equal-including-properties artifact original)
      (error "Trace reducer mutated its input artifact"))
    (list :artifact (fzfa-fuzz-trace-validate-artifact current)
          :attempts attempts
          :original-actions original-actions
          :reduced-actions (length actions))))

(defun fzfa-fuzz-reduce-file (input output)
  "Reduce failure artifact INPUT and atomically write it to OUTPUT."
  (let ((input (expand-file-name input))
        (output (expand-file-name output)))
    (when (equal input output)
      (error "Reduction output must differ from its input: %s" input))
    (let* ((value (fzfa-fuzz-trace-read input))
           (result (fzfa-fuzz-reduce-artifact value))
           (artifact (plist-get result :artifact))
           (failure (plist-get artifact :failure)))
      (fzfa-fuzz-trace-write output artifact)
      (princ
       (format
        (concat "fzfa trace reduction reproduced %s\n"
                "actions: %d -> %d; replay attempts: %d\noutput: %s\n")
        (plist-get failure :signature)
        (plist-get result :original-actions)
        (plist-get result :reduced-actions)
        (plist-get result :attempts)
        output))
      result)))

(defun fzfa-fuzz-reduce-trace-batch ()
  "Reduce the selected non-live failure artifact in batch Emacs."
  (unless noninteractive
    (error "Batch trace reduction was started in interactive Emacs"))
  (let* ((input (fzfa-fuzz-replay--file))
         (value (fzfa-fuzz-trace-read input))
         (trace (fzfa-fuzz-trace-value-trace value)))
    (when (eq (plist-get trace :target) 'live-icomplete)
      (error (concat "Live trace reduction requires a real minibuffer; run "
                     "make reduce-trace-live TRACE=FILE "
                     "LIVE_EMACS_FLAGS=-nw")))
    (fzfa-fuzz-reduce-file input (fzfa-fuzz-reduce--output-file input))))

(defun fzfa-fuzz-reduce-trace-live ()
  "Reduce the selected live failure artifact, then exit interactive Emacs."
  (if noninteractive
      (error "Live trace reduction must run without --batch")
    (condition-case err
        (let* ((input (fzfa-fuzz-replay--file))
               (value (fzfa-fuzz-trace-read input))
               (trace (fzfa-fuzz-trace-value-trace value)))
          (unless (eq (plist-get trace :target) 'live-icomplete)
            (error "reduce-trace-live received %S"
                   (plist-get trace :target)))
          (icomplete-vertical-mode 1)
          (fzfa-fuzz-live--assert-refcount-lifecycle)
          (fzfa-fuzz-reduce-file
           input (fzfa-fuzz-reduce--output-file input))
          (kill-emacs 0))
      ((error quit)
       (fzfa-fuzz-live--report
        (format "fzfa live trace reduction failed: %s\n"
                (error-message-string err)))
       (kill-emacs 1)))))

(defun fzfa-fuzz-reduce--capture-artifact (trace)
  "Run TRACE and return the failure artifact produced in memory."
  (let ((fzfa-fuzz--record-failures nil)
        (fzfa-fuzz--last-failure-record nil)
        caught)
    (condition-case err
        (fzfa-fuzz-replay--execute trace)
      (fzfa-fuzz-failure (setq caught err)))
    (unless caught
      (error "Controlled reduction trace did not fail"))
    (list :artifact-format fzfa-fuzz-artifact-format-version
          :failure (copy-tree fzfa-fuzz--last-failure-record)
          :trace (copy-tree trace))))

(defun fzfa-fuzz-reduce-selftest-batch ()
  "Check signature-preserving, one-minimal action reduction."
  (let* ((seed 9801)
         (trace
          (fzfa-fuzz-state--producer-trace
           seed seed nil 7
           '((deliver 7 ("unused"))
             (run 5)
             (fetch "a")
             (fetch "a")
             (deliver 0 ("alpha" "beta"))
             (run 0)
             (stop))))
         (input-file (make-temp-file "fzfa-fuzz-reduce-" nil ".sexp"))
         (output-file (make-temp-file "fzfa-fuzz-reduced-" nil ".sexp"))
         artifact result reduced second)
    (unwind-protect
        (let ((fzfa-fuzz-state--producer-after-delivery-hook
               (lambda (_source candidates _entry)
                 (setcar candidates "corrupt"))))
          (cl-letf (((symbol-function 'fzfa-fuzz--integer)
                     (lambda (&rest _) (error "Reduction requested randomness")))
                    ((symbol-function 'fzfa-fuzz--pick)
                     (lambda (&rest _) (error "Reduction requested randomness"))))
            (setq artifact (fzfa-fuzz-reduce--capture-artifact trace))
            (let ((before (copy-tree artifact)))
              (setq result (fzfa-fuzz-reduce-artifact artifact))
              (unless (equal-including-properties artifact before)
                (error "Reduction changed the caller's artifact")))
            (setq reduced (plist-get result :artifact))
            (unless (= (plist-get result :reduced-actions) 2)
              (error "Reduction did not find the two-action witness: %S"
                     (plist-get (plist-get reduced :trace) :actions)))
            (unless (equal
                     (mapcar #'car
                             (plist-get (plist-get reduced :trace) :actions))
                     '(fetch deliver))
              (error "Reduction kept the wrong action shape: %S"
                     (plist-get (plist-get reduced :trace) :actions)))
            (unless (fzfa-fuzz-reduce--reproducing-failure reduced)
              (error "Reduced artifact lost its stable failure signature"))
            (setq second (fzfa-fuzz-reduce-artifact reduced))
            (unless (and
                     (= (plist-get second :reduced-actions) 2)
                     (equal-including-properties
                      (plist-get (plist-get reduced :trace) :actions)
                      (plist-get
                       (plist-get (plist-get second :artifact) :trace)
                       :actions)))
              (error "A second reduction changed the one-minimal witness"))
            (fzfa-fuzz-trace-write input-file artifact)
            (fzfa-fuzz-reduce-file input-file output-file)
            (unless (equal-including-properties
                     (plist-get reduced :trace)
                     (plist-get (fzfa-fuzz-trace-read output-file) :trace))
              (error "File reduction wrote a different minimized trace"))))
      (dolist (file (list input-file output-file))
        (when (file-exists-p file)
          (delete-file file))))
    (fzfa-fuzz-replay--expect-error
     "requires a failure artifact"
     (lambda () (fzfa-fuzz-reduce-artifact trace)))
    (fzfa-fuzz-replay--expect-error
     "does not reproduce"
     (lambda () (fzfa-fuzz-reduce-artifact artifact)))
    (princ
     (format
      "fzfa trace reduction self-test passed (%d -> %d actions)\n"
      (plist-get result :original-actions)
      (plist-get result :reduced-actions)))))

(provide 'fzfa-fuzz-reduce)
;;; fzfa-fuzz-reduce.el ends here
