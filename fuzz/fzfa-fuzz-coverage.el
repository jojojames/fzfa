;;; fzfa-fuzz-coverage.el --- Semantic coverage for fzfa traces  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Classifies explicit state-producer traces without running fzfa.  The model
;; reports lifecycle states, effective operations, guarded stale work,
;; observational no-ops, state transitions, state/action/outcome buckets, and
;; adjacent transition pairs.  Required semantic events are CI gates, so a
;; generator change cannot silently stop exercising them while raw case counts
;; stay the same.

;;; Code:

(require 'cl-lib)
(require 'fzfa-fuzz-state)

(defconst fzfa-fuzz-coverage--required-actions
  '(run/none fetch/new fetch/same deliver/current-fetch
    run/current restart deliver/stale deliver/current-restart run/stale
    stop stop/pending-current)
  "Semantic action results every state campaign must reach.")

(defconst fzfa-fuzz-coverage--required-pairs
  '((fetch/new fetch/new)
    (fetch/new deliver/stale)
    (fetch/new deliver/current-fetch)
    (deliver/current-fetch run/current)
    (deliver/current-fetch stop/pending-current)
    (restart deliver/current-restart))
  "Adjacent semantic results every state campaign must reach.")

(cl-defstruct (fzfa-fuzz-coverage
               (:constructor fzfa-fuzz-coverage--create))
  (operations 0)
  (effective 0)
  (guarded 0)
  (noop 0)
  states
  actions
  first-actions
  transitions
  buckets
  pairs)

(defun fzfa-fuzz-coverage--new ()
  "Return an empty semantic coverage accumulator."
  (fzfa-fuzz-coverage--create
   :states (make-hash-table :test #'equal)
   :actions (make-hash-table :test #'equal)
   :first-actions (make-hash-table :test #'equal)
   :transitions (make-hash-table :test #'equal)
   :buckets (make-hash-table :test #'equal)
   :pairs (make-hash-table :test #'equal)))

(defun fzfa-fuzz-coverage--increment (table key)
  "Increment KEY's count in hash TABLE."
  (puthash key (1+ (gethash key table 0)) table))

(defun fzfa-fuzz-coverage--age (entries token pending-only)
  "Classify ENTRIES relative to TOKEN as none, current, stale, or mixed.

ENTRIES are conses whose car is a request token.  When PENDING-ONLY is non-nil,
ignore entries whose cdr says the task has already run."
  (let (current stale)
    (dolist (entry entries)
      (when (or (not pending-only) (not (cdr entry)))
        (if (= (car entry) token)
            (setq current t)
          (setq stale t))))
    (cond
     ((and current stale) 'mixed)
     (current 'current)
     (stale 'stale)
     (t 'none))))

(defun fzfa-fuzz-coverage--request-kind (callbacks token)
  "Return the current request kind in CALLBACKS for TOKEN."
  (if-let* ((entry
             (cl-find-if (lambda (callback) (= (car callback) token))
                         callbacks)))
      (cdr entry)
    'none))

(defun fzfa-fuzz-coverage--state
    (stopped token callbacks tasks)
  "Return one semantic lifecycle state for CALLBACKS and TASKS."
  (list :phase (if stopped 'stopped 'active)
        :request (fzfa-fuzz-coverage--request-kind callbacks token)
        :callbacks (fzfa-fuzz-coverage--age callbacks token nil)
        :refreshes (fzfa-fuzz-coverage--age tasks token t)))

(defun fzfa-fuzz-coverage--record
    (coverage before action label outcome after previous-label)
  "Record one classified ACTION transition in COVERAGE."
  (cl-incf (fzfa-fuzz-coverage-operations coverage))
  (pcase outcome
    ('effective (cl-incf (fzfa-fuzz-coverage-effective coverage)))
    ('guarded (cl-incf (fzfa-fuzz-coverage-guarded coverage)))
    ('noop (cl-incf (fzfa-fuzz-coverage-noop coverage)))
    (_ (error "Unknown semantic outcome: %S" outcome)))
  (fzfa-fuzz-coverage--increment
   (fzfa-fuzz-coverage-actions coverage) label)
  (fzfa-fuzz-coverage--increment
   (fzfa-fuzz-coverage-transitions coverage)
   (list before label after))
  (fzfa-fuzz-coverage--increment
   (fzfa-fuzz-coverage-buckets coverage)
   (list before (car action) outcome))
  (when previous-label
    (fzfa-fuzz-coverage--increment
     (fzfa-fuzz-coverage-pairs coverage)
     (list previous-label label))))

(defun fzfa-fuzz-coverage-add-trace (coverage trace)
  "Classify state-producer TRACE and add it to COVERAGE."
  (let* ((description (fzfa-fuzz-state--decode-producer-trace trace))
         (actions (plist-get description :actions))
         (token 0)
         (input :unfetched)
         callbacks tasks
         stopped
         previous-label)
    (when actions
      (fzfa-fuzz-coverage--increment
       (fzfa-fuzz-coverage-first-actions coverage)
       (car (car actions))))
    (fzfa-fuzz-coverage--increment
     (fzfa-fuzz-coverage-states coverage)
     (fzfa-fuzz-coverage--state stopped token callbacks tasks))
    (dolist (action actions)
      (when stopped
        (error "Semantic coverage received an action after stop: %S" action))
      (let ((before
             (fzfa-fuzz-coverage--state stopped token callbacks tasks))
            label outcome)
        (pcase action
          (`(fetch ,query)
           (if (equal query input)
               (setq label 'fetch/same outcome 'noop)
             (setq input query
                   label 'fetch/new
                   outcome 'effective)
             (cl-incf token)
             (setq callbacks
                   (append callbacks (list (cons token 'fetch))))))
          (`(restart ,_query)
           (setq label 'restart outcome 'effective)
           (cl-incf token)
           (setq callbacks
                 (append callbacks (list (cons token 'restart)))))
          (`(deliver ,selector ,_candidates)
           (if (null callbacks)
               (setq label 'deliver/none outcome 'noop)
             (let* ((entry (nth (% selector (length callbacks)) callbacks))
                    (entry-token (car entry))
                    (kind (cdr entry)))
               (cond
                ((/= entry-token token)
                 (setq label 'deliver/stale outcome 'guarded))
                ((eq kind 'fetch)
                 (setq label 'deliver/current-fetch outcome 'effective
                       tasks (append tasks (list (cons token nil)))))
                ((eq kind 'restart)
                 (setq label 'deliver/current-restart outcome 'effective))
                (t (error "Unknown callback kind: %S" kind))))))
          (`(run ,selector)
           (let ((pending (cl-remove-if #'cdr tasks)))
             (if (null pending)
                 (setq label 'run/none outcome 'noop)
               (let ((task (nth (% selector (length pending)) pending)))
                 (setcdr task t)
                 (if (= (car task) token)
                     (setq label 'run/current outcome 'effective)
                   (setq label 'run/stale outcome 'guarded))))))
          (`(stop)
           (let ((pending-current
                  (cl-some (lambda (task)
                             (and (not (cdr task)) (= (car task) token)))
                           tasks)))
             (setq label (if pending-current
                             'stop/pending-current
                           'stop)
                   outcome 'effective
                   stopped t)
             (cl-incf token)
             (dolist (task tasks)
               (setcdr task t))))
          (_ (error "Unknown state-producer action: %S" action)))
        (let ((after
               (fzfa-fuzz-coverage--state stopped token callbacks tasks)))
          (fzfa-fuzz-coverage--increment
           (fzfa-fuzz-coverage-states coverage) after)
          (fzfa-fuzz-coverage--record
           coverage before action label outcome after previous-label)
          (setq previous-label label))))
    coverage))

(defun fzfa-fuzz-coverage--entries (table)
  "Return TABLE as a deterministically sorted key/count alist."
  (let (entries)
    (maphash (lambda (key count) (push (cons key count) entries)) table)
    (sort entries
          (lambda (left right)
            (string-lessp (prin1-to-string (car left))
                          (prin1-to-string (car right)))))))

(defun fzfa-fuzz-coverage--state-facet-p (coverage key value)
  "Return non-nil when a reached state has KEY equal to VALUE."
  (cl-some (lambda (entry) (eq (plist-get (car entry) key) value))
           (fzfa-fuzz-coverage--entries
            (fzfa-fuzz-coverage-states coverage))))

(defun fzfa-fuzz-coverage-check (coverage)
  "Require COVERAGE to contain the campaign's semantic reachability gates."
  (dolist (action fzfa-fuzz-coverage--required-actions)
    (unless (gethash action (fzfa-fuzz-coverage-actions coverage))
      (error "Semantic coverage did not reach %S" action)))
  (dolist (pair fzfa-fuzz-coverage--required-pairs)
    (unless (gethash pair (fzfa-fuzz-coverage-pairs coverage))
      (error "Semantic coverage did not reach adjacent pair %S" pair)))
  (dolist (facet '((:phase stopped)
                   (:request restart)
                   (:callbacks mixed)
                   (:refreshes current)
                   (:refreshes stale)))
    (unless (fzfa-fuzz-coverage--state-facet-p
             coverage (car facet) (cadr facet))
      (error "Semantic coverage did not reach state facet %S" facet)))
  (unless (> (+ (fzfa-fuzz-coverage-effective coverage)
                (fzfa-fuzz-coverage-guarded coverage))
             (fzfa-fuzz-coverage-noop coverage))
    (error "Observational no-ops dominate semantic coverage: %d of %d"
           (fzfa-fuzz-coverage-noop coverage)
           (fzfa-fuzz-coverage-operations coverage)))
  coverage)

(defun fzfa-fuzz-coverage-report (coverage cases steps root-seed)
  "Print a stable semantic COVERAGE summary for a campaign."
  (princ
   (format
    (concat "fzfa semantic coverage report (%d cases x %d step budget, "
            "root seed %d)\n")
    cases steps root-seed))
  (princ
   (format "operations: %d effective, %d guarded, %d no-op (%d total)\n"
           (fzfa-fuzz-coverage-effective coverage)
           (fzfa-fuzz-coverage-guarded coverage)
           (fzfa-fuzz-coverage-noop coverage)
           (fzfa-fuzz-coverage-operations coverage)))
  (princ
   (format
    "semantic sets: %d states, %d transitions, %d buckets, %d adjacent pairs\n"
    (hash-table-count (fzfa-fuzz-coverage-states coverage))
    (hash-table-count (fzfa-fuzz-coverage-transitions coverage))
    (hash-table-count (fzfa-fuzz-coverage-buckets coverage))
    (hash-table-count (fzfa-fuzz-coverage-pairs coverage))))
  (princ
   (format "action results: %S\n"
           (fzfa-fuzz-coverage--entries
            (fzfa-fuzz-coverage-actions coverage))))
  (princ
   (format "first action kinds: %S\n"
           (fzfa-fuzz-coverage--entries
            (fzfa-fuzz-coverage-first-actions coverage))))
  (princ
   (format "states reached: %S\n"
           (fzfa-fuzz-coverage--entries
            (fzfa-fuzz-coverage-states coverage))))
  (princ
   (format
    "required adjacent pairs: %S\n"
    (mapcar
     (lambda (pair)
       (cons pair (gethash pair (fzfa-fuzz-coverage-pairs coverage) 0)))
     fzfa-fuzz-coverage--required-pairs))))

(defun fzfa-fuzz-coverage--expect-error (pattern function)
  "Require FUNCTION to signal an error matching PATTERN."
  (let (caught)
    (condition-case err
        (funcall function)
      (error (setq caught err)))
    (unless (and caught
                 (string-match-p pattern (error-message-string caught)))
      (error "Expected semantic coverage error matching %S, got %S"
             pattern caught))))

(defun fzfa-fuzz-coverage-selftest-batch ()
  "Check semantic classification, input immutability, and missing gates."
  (let* ((seed 9901)
         (first
          (fzfa-fuzz-state--producer-trace
           seed seed nil 11
           '((deliver 0 ("unused"))
             (run 0)
             (fetch "a")
             (fetch "a")
             (deliver 0 ("fresh"))
             (run 0)
             (restart "b")
             (deliver 0 ("stale"))
             (deliver 1 ("inline"))
             (run 0)
             (stop))))
         (second
          (fzfa-fuzz-state--producer-trace
           seed (1+ seed) nil 6
           '((fetch "a")
             (deliver 0 ("first"))
             (fetch "b")
             (run 0)
             (deliver 1 ("second"))
             (stop))))
         (before (list (copy-tree first) (copy-tree second)))
         (coverage (fzfa-fuzz-coverage--new)))
    (fzfa-fuzz-coverage-add-trace coverage first)
    (fzfa-fuzz-coverage-add-trace coverage second)
    (unless (equal-including-properties before (list first second))
      (error "Semantic coverage mutated its input traces"))
    (unless (equal
             (list (fzfa-fuzz-coverage-effective coverage)
                   (fzfa-fuzz-coverage-guarded coverage)
                   (fzfa-fuzz-coverage-noop coverage)
                   (fzfa-fuzz-coverage-operations coverage))
             '(11 2 4 17))
      (error "Semantic operation counts changed: %S"
             (list (fzfa-fuzz-coverage-effective coverage)
                   (fzfa-fuzz-coverage-guarded coverage)
                   (fzfa-fuzz-coverage-noop coverage)
                   (fzfa-fuzz-coverage-operations coverage))))
    (dolist (action fzfa-fuzz-coverage--required-actions)
      (unless (gethash action (fzfa-fuzz-coverage-actions coverage))
        (error "Semantic self-test omitted %S" action)))
    (let ((broken (copy-fzfa-fuzz-coverage coverage)))
      (setf (fzfa-fuzz-coverage-actions broken)
            (copy-hash-table (fzfa-fuzz-coverage-actions coverage)))
      (remhash 'deliver/stale (fzfa-fuzz-coverage-actions broken))
      (fzfa-fuzz-coverage--expect-error
       "did not reach deliver/stale"
       (lambda () (fzfa-fuzz-coverage-check broken))))
    (princ
     (format
      "fzfa semantic coverage self-test passed (%d classified actions)\n"
      (fzfa-fuzz-coverage-operations coverage)))))

(defun fzfa-fuzz-coverage-batch ()
  "Run the state campaign and gate semantic coverage of its exact traces."
  (let* ((root-seed (fzfa-fuzz--seed))
         (cases (fzfa-fuzz--env-natural "FZFA_FUZZ_CASES" 300))
         (steps (fzfa-fuzz--env-natural "FZFA_FUZZ_STEPS" 40))
         (coverage (fzfa-fuzz-coverage--new))
         (run-trace (symbol-function 'fzfa-fuzz-state-run-trace)))
    ;; Observe the explicit trace after its real driver succeeds.  This avoids
    ;; copying the campaign's RNG sequencing into the reporter.
    (cl-letf (((symbol-function 'fzfa-fuzz-state-run-trace)
               (lambda (trace)
                 (prog1 (funcall run-trace trace)
                   (when (eq (plist-get trace :target) 'state-producer)
                     (fzfa-fuzz-coverage-add-trace coverage trace))))))
      (fzfa-fuzz-state-batch))
    (fzfa-fuzz-coverage-report coverage cases steps root-seed)
    (fzfa-fuzz-coverage-check coverage)
    (princ "fzfa semantic coverage gates passed\n")))

(provide 'fzfa-fuzz-coverage)
;;; fzfa-fuzz-coverage.el ends here
