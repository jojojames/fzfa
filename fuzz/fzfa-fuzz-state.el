;;; fzfa-fuzz-state.el --- Model fuzzing for fzfa state  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Drives fzfa's real completion tables and source lifecycle functions with
;; deterministic generated traces.  Dependencies such as timers and native
;; status calls are controlled at their existing boundaries; fzfa itself is
;; neither copied nor patched by this harness.

;;; Code:

(require 'cl-lib)
(require 'fzfa-fuzz-core)

(defconst fzfa-fuzz-state--words
  '("alpha" "beta" "gamma" "delta" "same" "naive" "你好" "a b" "x:y")
  "Small candidate alphabet used by the state fuzzer.")

(defvar fzfa-fuzz-state--mutation-source-count nil
  "Test-only source-count override for completion ownership canaries.")

(defvar fzfa-fuzz-state--mutation-operation nil
  "Test-only operation override for completion ownership canaries.")

(defvar fzfa-fuzz-state--completion-result-mutator nil
  "Test-only function for corrupting a completion result before its oracle.")

(defvar fzfa-fuzz-state--producer-before-delivery-hook nil
  "Test-only function called immediately before a producer callback.")

(defvar fzfa-fuzz-state--producer-after-delivery-hook nil
  "Test-only function called immediately after a producer callback.")

(defvar fzfa-fuzz-state--producer-after-teardown-hook nil
  "Test-only function called after late teardown work has run.")

(defun fzfa-fuzz-state--candidate (rng source-index candidate-index)
  "Generate a candidate using RNG for SOURCE-INDEX and CANDIDATE-INDEX."
  (let ((value (copy-sequence (fzfa-fuzz--pick rng fzfa-fuzz-state--words))))
    (add-text-properties
     0 (length value)
     `(fzfa-fuzz-origin (,source-index . ,candidate-index)) value)
    value))

(defun fzfa-fuzz-state--candidate-list (rng source-index)
  "Generate one non-empty candidate list for SOURCE-INDEX using RNG."
  (cl-loop for index below (1+ (fzfa-fuzz--integer rng 7))
           collect (fzfa-fuzz-state--candidate rng source-index index)))

(defun fzfa-fuzz-state--mutation-candidate-list (rng source-index)
  "Generate a mutation-discriminating candidate list using RNG.

The first two values are out of order, the last two have equal text with
different origin properties, and the endpoints differ.  Thus sort, dedup,
reverse, and truncation all have an observable effect."
  (cl-loop for value in
           (list "zeta" "alpha"
                 (fzfa-fuzz--pick rng fzfa-fuzz-state--words)
                 "same" "same")
           for candidate-index from 0
           collect
           (let ((candidate (copy-sequence value)))
             (add-text-properties
              0 (length candidate)
              `(fzfa-fuzz-origin (,source-index . ,candidate-index))
             candidate)
             candidate)))

(defun fzfa-fuzz-state--expected-tag (candidate source-index multi-p)
  "Build CANDIDATE's expected source tag without calling `fzfa--tag'."
  (let ((copy (copy-sequence candidate)))
    (if (not multi-p)
        copy
      (let ((tagged (concat copy (string (+ fzfa--tofu-base source-index)))))
        (add-text-properties
         (1- (length tagged)) (length tagged)
         '(invisible t display "" fzfa-multi-action identity)
         tagged)
        tagged))))

(defun fzfa-fuzz-state--mutate-list (value operation argument)
  "Apply destructive list OPERATION to VALUE using recorded ARGUMENT."
  (pcase operation
    ('nconc (when value (nconc value (list "frontend-tail"))))
    ('truncate
     (when value (setcdr value nil) value))
    ('dot
     (when value (setcdr value argument) value))
    ('reverse (nreverse value))
    ('sort (sort value #'string-lessp))
    ('dedup (delete-dups value))))

(defun fzfa-fuzz-state--mutation-trace (root-seed case-seed rng)
  "Generate a completion trace for ROOT-SEED and CASE-SEED using RNG."
  (let* ((source-count (or fzfa-fuzz-state--mutation-source-count
                           (1+ (fzfa-fuzz--integer rng 3))))
         (operation (or fzfa-fuzz-state--mutation-operation
                        (fzfa-fuzz--pick
                         rng '(nconc truncate dot reverse sort dedup))))
         (sources
          (cl-loop for source-index below source-count
                   collect
                   (fzfa-fuzz-state--mutation-candidate-list
                    rng source-index)))
         (argument (and (eq operation 'dot)
                        (fzfa-fuzz--integer rng 20))))
    (fzfa-fuzz-trace-create
     'completion-list root-seed case-seed
     (list :sources sources)
     (list (list 'lookup "")
           (list 'frontend-mutate operation argument)
           (list 'lookup "")))))

(defun fzfa-fuzz-state--decode-mutation-trace (trace)
  "Validate TRACE and return its completion-list inputs."
  (let* ((initial (plist-get trace :initial-state))
         (keys (fzfa-fuzz-trace--plist-keys
                initial "completion-list initial state" '(:sources)))
         (sources (plist-get initial :sources))
         (actions (plist-get trace :actions))
         (mutation (nth 1 actions))
         (operation (nth 1 mutation))
         (argument (nth 2 mutation)))
    (fzfa-fuzz-trace--require-keys
     keys '(:sources) "completion-list initial state")
    (unless (and (fzfa-fuzz--proper-list-p sources)
                 sources
                 (cl-every
                  (lambda (candidates)
                    (and (fzfa-fuzz--proper-list-p candidates)
                         (cl-every #'stringp candidates)))
                  sources)
                 (= (length actions) 3)
                 (equal (nth 0 actions) '(lookup ""))
                 (eq (car-safe mutation) 'frontend-mutate)
                 (= (length mutation) 3)
                 (memq operation '(nconc truncate dot reverse sort dedup))
                 (if (eq operation 'dot)
                     (and (integerp argument) (>= argument 0))
                   (null argument))
                 (equal (nth 2 actions) '(lookup "")))
      (error "Malformed completion ownership trace: %S" trace))
    (list :sources sources :operation operation :argument argument)))

(defun fzfa-fuzz-state--mutation-case (trace)
  "Run the explicit completion-list ownership TRACE."
  (let* ((description (fzfa-fuzz-state--decode-mutation-trace trace))
         (seed (plist-get trace :case-seed))
         (source-candidates (plist-get description :sources))
         (source-count (length source-candidates))
         (multi-p (> source-count 1))
         (operation (plist-get description :operation))
         (argument (plist-get description :argument))
         (specs
          (cl-loop for candidates in source-candidates
                   for source-index from 0
                   collect
                   (list :name (format "source-%d" source-index)
                         :candidates (fzfa-fuzz--copy-strings candidates)
                         :category 'fzfa-fuzz
                         :action #'identity)))
         ;; Build the oracle before fzfa creates a source or invokes a
         ;; producer callback.  Its list spines and strings share nothing with
         ;; the values that production code will cache or return.
         (expected-snapshots
          (cl-loop
           for candidates in source-candidates
           for source-index from 0
           collect
           (mapcar
            (lambda (candidate)
              (fzfa-fuzz-state--expected-tag
               candidate source-index multi-p))
            candidates)))
         (expected-result
          (cl-mapcan #'fzfa-fuzz--copy-strings expected-snapshots))
         (scheduler (fzfa-fuzz-scheduler-create))
         (original-maker (symbol-function 'fzfa-make-source))
         made-sources)
    (fzfa-fuzz--call-with-scheduler
     scheduler
     (lambda ()
       (let ((completion-category-overrides nil)
             (minibuffer-setup-hook nil)
             (minibuffer-exit-hook nil)
             (post-command-hook nil))
         (cl-letf (((symbol-function 'fzfa-make-source)
                    (lambda (&rest args)
                      (let ((source (apply original-maker args)))
                        (setq made-sources
                              (append made-sources (list source)))
                        source)))
                   ((symbol-function 'sit-for) (lambda (&rest _) nil))
                   ((symbol-function 'fzfa--sessions-push)
                    (lambda (&rest _) nil))
                   ((symbol-function 'completing-read)
                    (lambda (_prompt table &rest _)
                      (let* ((returned (funcall table "" nil t))
                             (returned
                              (if fzfa-fuzz-state--completion-result-mutator
                                  (funcall
                                   fzfa-fuzz-state--completion-result-mutator
                                   returned made-sources)
                                returned)))
                        (unless (fzfa-fuzz--proper-list-p returned)
                          (fzfa-fuzz--fail-observation
                           seed trace expected-result returned
                           "initial result is not a proper list"))
                        (unless (equal-including-properties
                                 returned expected-result)
                          (fzfa-fuzz--fail-observation
                           seed trace expected-result returned
                           "initial result is different from expected"))
                        (let ((before
                               (fzfa-fuzz--copy-strings returned)))
                          (setq returned
                                (fzfa-fuzz-state--mutate-list
                                 returned operation argument))
                          (when (equal-including-properties returned before)
                            (fzfa-fuzz--fail-observation
                             seed trace 'changed
                             (list :before before :after returned)
                             "frontend operation did not change result: %S"
                             operation)))
                        (cl-mapc
                         (lambda (source expected)
                           (let ((actual (fzfa-source-snapshot source)))
                             (unless (and (fzfa-fuzz--proper-list-p actual)
                                          (equal-including-properties
                                           actual expected))
                               (fzfa-fuzz--fail-observation
                                seed trace expected actual
                                "frontend mutation changed snapshot"))))
                         made-sources expected-snapshots)
                        (let ((second (funcall table "" nil t)))
                          (unless (and (fzfa-fuzz--proper-list-p second)
                                       (equal-including-properties
                                        second expected-result))
                            (fzfa-fuzz--fail-observation
                             seed trace expected-result second
                             "second completion result differs")))
                        nil))))
           (fzfa--read specs :prompt "fuzz: ")))))
    t))

(cl-defstruct (fzfa-fuzz-state--callback
               (:constructor fzfa-fuzz-state--callback-create))
  token kind function refresh)

(defun fzfa-fuzz-state--producer-actions (rng steps)
  "Generate producer lifecycle actions from RNG with at most STEPS entries."
  (let ((trace (list (list 'fetch "a")))
        (remaining (max 0 (1- steps)))
        stopped)
    (while (and (> remaining 0) (not stopped))
      (let ((roll (fzfa-fuzz--integer rng 100)))
        (push
         (cond
          ((< roll 30)
           (list 'fetch (fzfa-fuzz--pick rng '("" "a" "ab" "b" "same"))))
          ((< roll 60)
           (list 'deliver (fzfa-fuzz--integer rng 32)
                 (fzfa-fuzz-state--candidate-list rng 0)))
          ((< roll 78) (list 'run (fzfa-fuzz--integer rng 32)))
          ((< roll 92)
           (list 'restart (fzfa-fuzz--pick rng '("" "a" "new" "other"))))
          (t (setq stopped t) '(stop)))
         trace)
        (cl-decf remaining)))
    (nreverse trace)))

(defun fzfa-fuzz-state--producer-trace
    (root-seed case-seed rng steps &optional actions)
  "Build a ROOT-SEED and CASE-SEED trace from ACTIONS or RNG and STEPS."
  (fzfa-fuzz-trace-create
   'state-producer root-seed case-seed
   (list :source-name "state" :step-budget steps)
   (copy-tree (or actions
                  (fzfa-fuzz-state--producer-actions rng steps)))))

(defun fzfa-fuzz-state--decode-producer-trace (trace)
  "Validate TRACE and return its producer lifecycle inputs."
  (let* ((initial (plist-get trace :initial-state))
         (keys (fzfa-fuzz-trace--plist-keys
                initial "state-producer initial state"
                '(:source-name :step-budget)))
         (source-name (plist-get initial :source-name))
         (step-budget (plist-get initial :step-budget))
         (actions (plist-get trace :actions))
         stopped)
    (fzfa-fuzz-trace--require-keys
     keys '(:source-name :step-budget) "state-producer initial state")
    (unless (and (stringp source-name)
                 (integerp step-budget) (>= step-budget 0))
      (error "Malformed state-producer initial state: %S" initial))
    (dolist (action actions)
      (when stopped
        (error "State-producer action follows stop: %S" action))
      (pcase action
        ((or `(fetch ,query) `(restart ,query))
         (unless (stringp query)
           (error "Malformed state-producer query action: %S" action)))
        (`(deliver ,selector ,candidates)
         (unless (and (integerp selector) (>= selector 0)
                      (fzfa-fuzz--proper-list-p candidates)
                      (cl-every #'stringp candidates))
           (error "Malformed state-producer delivery: %S" action)))
        (`(run ,selector)
         (unless (and (integerp selector) (>= selector 0))
           (error "Malformed state-producer run action: %S" action)))
        (`(stop) (setq stopped t))
        (_ (error "Malformed state-producer action: %S" action))))
    (list :source-name source-name :actions actions)))

(defun fzfa-fuzz-state--producer-trace-features (trace)
  "Return lifecycle witnesses reached by generated TRACE.

This is a reachability check, not a correctness oracle.  It mirrors only
token creation and queued-refresh selection so the harness can report whether
its generator ever built the short traces its state oracle is meant to judge."
  (let ((token 0)
        (input :unfetched)
        callbacks tasks features)
    (dolist (operation (plist-get trace :actions))
      (pcase operation
        (`(fetch ,query)
         (unless (equal query input)
           (setq input query)
           (cl-incf token)
           (setq callbacks
                 (append callbacks (list (cons token 'fetch))))))
        (`(restart ,_query)
         (cl-incf token)
         (setq callbacks
               (append callbacks (list (cons token 'restart))))
         (cl-pushnew 'restart features))
        (`(deliver ,selector ,_candidates)
         (when callbacks
           (let ((entry (nth (% selector (length callbacks)) callbacks)))
             (if (= (car entry) token)
                 (progn
                   (cl-pushnew 'current-delivery features)
                   (if (eq (cdr entry) 'fetch)
                       (setq tasks (append tasks (list (cons token nil))))
                     (cl-pushnew 'inline-refresh features)))
               (cl-pushnew 'stale-delivery features)))))
        (`(run ,selector)
         (let ((pending (cl-remove-if #'cdr tasks)))
           (when pending
             (let ((task (nth (% selector (length pending)) pending)))
               (setcdr task t)
               (when (= (car task) token)
                 (cl-pushnew 'scheduled-refresh features))))))
        (`(stop)
         (when (cl-some (lambda (task)
                          (and (not (cdr task)) (= (car task) token)))
                        tasks)
           (cl-pushnew 'queued-refresh-at-stop features))
         (cl-pushnew 'stop features)
         (cl-incf token))))
    features))

(defun fzfa-fuzz-state--check-producer-generator ()
  "Require the producer generator to reach its discriminating witnesses."
  (let ((required '(current-delivery stale-delivery
                    queued-refresh-at-stop restart stop))
        reached
        (seeds 2000)
        (steps 40))
    (dotimes (index seeds)
      (let* ((rng (fzfa-fuzz-rng-create :state (1+ index)))
             (trace (fzfa-fuzz-state--producer-trace
                     1 (1+ index) rng steps))
             (actions (plist-get trace :actions)))
        (when-let* ((stop-tail (member '(stop) actions)))
          (unless (null (cdr stop-tail))
            (error "Producer generator emitted operations after stop: %S"
                   actions)))
        (dolist (feature (fzfa-fuzz-state--producer-trace-features trace))
          (cl-pushnew feature reached))))
    (dolist (feature required)
      (unless (memq feature reached)
        (error "Producer generator did not reach %S in %d seeds"
               feature seeds)))
    (princ
     (format "REACHED producer witnesses %S (%d seeds x %d steps)\n"
             required seeds steps))))

(defun fzfa-fuzz-state--source-view (source)
  "Return an immutable view of producer-owned state in SOURCE."
  (list :token (fzfa-source-prod-token source)
        :input (fzfa-source-prod-input source)
        :snapshot (fzfa-fuzz--copy-strings (fzfa-source-snapshot source))
        :total (fzfa-source-total source)
        :filtered (fzfa-source-filtered source)
        :last-result
        (fzfa-fuzz--copy-strings (fzfa-source-last-result source))
        :command (fzfa-source-current-cmd source)
        :request-epoch (fzfa-source-request-epoch source)))

(defun fzfa-fuzz-state--model-view
    (token input snapshot total filtered last-result command request-epoch)
  "Return an immutable expected producer-state view."
  (list :token token :input input
        :snapshot (fzfa-fuzz--copy-strings snapshot)
        :total total :filtered filtered
        :last-result (fzfa-fuzz--copy-strings last-result)
        :command command :request-epoch request-epoch))

(defun fzfa-fuzz-state--producer-case (trace)
  "Run one explicit producer lifecycle TRACE."
  (let* ((description (fzfa-fuzz-state--decode-producer-trace trace))
         (seed (plist-get trace :case-seed))
         (source-name (plist-get description :source-name))
         (actions (plist-get description :actions))
         (scheduler (fzfa-fuzz-scheduler-create))
         callbacks model-tasks
         source current-kind current-refresh
         (model-token 0)
         (model-input :unfetched)
         model-snapshot
         (model-total 0)
         (model-filtered 0)
         model-last-result
         model-command
         (model-request-epoch 0)
         (refreshes 0)
         (model-refreshes 0)
         refresh-observations
         model-refresh-observations
         (producer
          (lambda (_input callback)
            (setq callbacks
                  (append
                   callbacks
                   (list
                    (fzfa-fuzz-state--callback-create
                     :token (fzfa-source-prod-token source)
                     :kind current-kind :function callback
                     :refresh current-refresh)))))))
    (setq source (fzfa-make-source
                  :spec (list :name source-name :candidates producer)))
    (fzfa-fuzz--call-with-scheduler
     scheduler
     (lambda ()
       (dolist (operation actions)
         (pcase operation
           (`(fetch ,query)
            (let ((changed (not (equal query model-input))))
              (setq current-kind 'fetch)
              (setq current-refresh
                    (lambda ()
                      (cl-incf refreshes)
                      (setq refresh-observations
                            (append refresh-observations
                                    (list
                                     (fzfa-fuzz-state--source-view source))))))
              (unwind-protect
                  (fzfa--source-fetch source query current-refresh)
                (setq current-kind nil
                      current-refresh nil))
              (when changed
                (setq model-input query)
                (cl-incf model-token))))
           (`(restart ,query)
            (setq current-kind 'restart)
            (setq current-refresh
                  (lambda ()
                    (cl-incf refreshes)
                    (setq refresh-observations
                          (append refresh-observations
                                  (list
                                   (fzfa-fuzz-state--source-view source))))))
            (unwind-protect
                (fzfa-source--restart
                 source query current-refresh)
              (setq current-kind nil
                    current-refresh nil))
            (cl-incf model-request-epoch)
            (cl-incf model-token)
            (setq model-command query))
           (`(deliver ,selector ,candidates)
            (when callbacks
              (let* ((delivered (fzfa-fuzz--copy-strings candidates))
                     (entry (nth (% selector (length callbacks)) callbacks))
                     (token (fzfa-fuzz-state--callback-token entry))
                     (kind (fzfa-fuzz-state--callback-kind entry))
                     (expected (fzfa-fuzz--copy-strings delivered)))
                (when fzfa-fuzz-state--producer-before-delivery-hook
                  (funcall fzfa-fuzz-state--producer-before-delivery-hook
                           source delivered entry))
                (funcall (fzfa-fuzz-state--callback-function entry) delivered)
                (when fzfa-fuzz-state--producer-after-delivery-hook
                  (funcall fzfa-fuzz-state--producer-after-delivery-hook
                           source delivered entry))
                (when (= token model-token)
                  (setq model-snapshot expected
                        model-total (length expected))
                  (if (eq kind 'fetch)
                      (setq model-tasks
                            (append model-tasks (list (cons token nil))))
                    (setq model-filtered (length expected)
                          model-last-result expected)
                    (cl-incf model-refreshes)
                    (setq model-refresh-observations
                          (append
                           model-refresh-observations
                           (list
                            (fzfa-fuzz-state--model-view
                             model-token model-input model-snapshot model-total
                             model-filtered model-last-result model-command
                             model-request-epoch)))))))))
           (`(run ,selector)
            (let ((pending-model
                   (cl-remove-if #'cdr model-tasks)))
              (when pending-model
                (let* ((index (% selector (length pending-model)))
                       (model-task (nth index pending-model)))
                  (setcdr model-task t)
                  (when (= (car model-task) model-token)
                    (cl-incf model-refreshes)
                    (setq model-refresh-observations
                          (append
                           model-refresh-observations
                           (list
                            (fzfa-fuzz-state--model-view
                             model-token model-input model-snapshot model-total
                             model-filtered model-last-result model-command
                             model-request-epoch)))))
                  (fzfa-fuzz--run-task scheduler index)))))
           (`(stop)
            (fzfa-source--stop source)
            (cl-incf model-request-epoch)
            (cl-incf model-token))
           (_ (error "Malformed state-producer action: %S" operation)))
         (unless (= (fzfa-source-prod-token source) model-token)
           (fzfa-fuzz--fail-observation
            seed trace model-token (fzfa-source-prod-token source)
            "producer token is different from expected after %S" operation))
         (unless (equal (fzfa-source-prod-input source) model-input)
           (fzfa-fuzz--fail-observation
            seed trace model-input (fzfa-source-prod-input source)
            "producer input is different from expected after %S" operation))
         (unless (= (fzfa-source-request-epoch source) model-request-epoch)
           (fzfa-fuzz--fail-observation
            seed trace model-request-epoch
            (fzfa-source-request-epoch source)
            "request epoch is different from expected after %S" operation))
         (unless (equal (fzfa-source-current-cmd source) model-command)
           (fzfa-fuzz--fail-observation
            seed trace model-command (fzfa-source-current-cmd source)
            "current command is different from expected after %S" operation))
         (unless (equal-including-properties
                  (fzfa-source-snapshot source) model-snapshot)
           (fzfa-fuzz--fail-observation
            seed trace model-snapshot (fzfa-source-snapshot source)
            "snapshot is different from expected after %S" operation))
         (unless (= (fzfa-source-total source) model-total)
           (fzfa-fuzz--fail-observation
            seed trace model-total (fzfa-source-total source)
            "total is different from expected after %S" operation))
         (unless (= (fzfa-source-filtered source) model-filtered)
           (fzfa-fuzz--fail-observation
            seed trace model-filtered (fzfa-source-filtered source)
            "filtered count is different from expected after %S" operation))
         (unless (equal-including-properties
                  (fzfa-source-last-result source) model-last-result)
           (fzfa-fuzz--fail-observation
            seed trace model-last-result (fzfa-source-last-result source)
            "last result is different from expected after %S" operation))
         (unless (equal-including-properties
                  refresh-observations model-refresh-observations)
           (fzfa-fuzz--fail-observation
            seed trace model-refresh-observations refresh-observations
            "refresh observations are different from expected after %S"
            operation))
         (unless (= refreshes model-refreshes)
           (fzfa-fuzz--fail-observation
            seed trace model-refreshes refreshes
            "refresh count is different from expected after %S" operation)))))
    ;; Teardown must make every captured callback and queued refresh inert.
    (unless (and actions (eq (caar (last actions)) 'stop))
      (fzfa-source--stop source)
      (cl-incf model-request-epoch)
      (cl-incf model-token))
    (let ((snapshot (fzfa-fuzz--copy-strings model-snapshot))
          (total model-total)
          (filtered model-filtered)
          (last-result (fzfa-fuzz--copy-strings model-last-result))
          (token model-token)
          (request-epoch model-request-epoch)
          (before-refreshes refreshes))
      (dolist (entry callbacks)
        (funcall (fzfa-fuzz-state--callback-function entry) '("late")))
      (fzfa-fuzz--run-all-tasks scheduler)
      (when fzfa-fuzz-state--producer-after-teardown-hook
        (funcall fzfa-fuzz-state--producer-after-teardown-hook source))
      (let ((expected
             (list :snapshot snapshot :total total :filtered filtered
                   :last-result last-result :token token
                   :request-epoch request-epoch
                   :refreshes before-refreshes :pending-tasks 0))
            (observed
             (list :snapshot (fzfa-fuzz--copy-strings
                              (fzfa-source-snapshot source))
                   :total (fzfa-source-total source)
                   :filtered (fzfa-source-filtered source)
                   :last-result (fzfa-fuzz--copy-strings
                                 (fzfa-source-last-result source))
                   :token (fzfa-source-prod-token source)
                   :request-epoch (fzfa-source-request-epoch source)
                   :refreshes refreshes
                   :pending-tasks
                   (length (fzfa-fuzz--pending-tasks scheduler)))))
        (unless (equal-including-properties observed expected)
          (fzfa-fuzz--fail-observation
           seed trace expected observed
           "teardown allowed stale work to publish"))))
    t))

(defun fzfa-fuzz-state--poller-trace (root-seed case-seed)
  "Return the fixed stale-poller trace for ROOT-SEED and CASE-SEED."
  (fzfa-fuzz-trace-create
   'state-poller root-seed case-seed
   '(:generations ((old . 1) (new . 0)) :initial-handle old)
   '((tick) (replace-handle new) (run-publication))))

(defun fzfa-fuzz-state--poller-replay (trace)
  "Replay publication after handle replacement from TRACE."
  (let* ((seed (plist-get trace :case-seed))
         (initial-state (plist-get trace :initial-state))
         (keys (fzfa-fuzz-trace--plist-keys
                initial-state "state-poller initial state"
                '(:generations :initial-handle)))
         (source (fzfa-make-source :command "producer"))
         (generations (plist-get initial-state :generations))
         (initial-handle (plist-get initial-state :initial-handle))
         (replacement (nth 1 (assq 'replace-handle
                                   (plist-get trace :actions))))
         scheduled
         (refreshes 0)
         (alive t))
    (fzfa-fuzz-trace--require-keys
     keys '(:generations :initial-handle) "state-poller initial state")
    (unless (and (fzfa-fuzz--proper-list-p generations)
                 (cl-every
                  (lambda (entry)
                    (and (consp entry) (symbolp (car entry))
                         (integerp (cdr entry)) (>= (cdr entry) 0)))
                  generations)
                 (assq initial-handle generations)
                 (assq replacement generations)
                 (equal (plist-get trace :actions)
                        `((tick) (replace-handle ,replacement)
                          (run-publication))))
      (error "Malformed state-poller trace: %S" trace))
    (setf (fzfa-source-handle source) initial-handle)
    (cl-letf (((symbol-function 'fzfa--poll-generation)
               (lambda (handle) (alist-get handle generations)))
              ((symbol-function 'input-pending-p) (lambda () nil))
              ((symbol-function 'float-time) (lambda (&optional _) 1.0)))
      (let ((poll
             (fzfa--make-poll-fn
              (vector source) (lambda () alive)
              (lambda () (cl-incf refreshes) t)
              (lambda () nil)
              (lambda (transaction) (setq scheduled transaction)))))
        (funcall poll)
        (unless scheduled
          (fzfa-fuzz--fail-observation
           seed trace 'scheduled nil "poller did not schedule publication"))
        (setf (fzfa-source-handle source) replacement)
        (funcall scheduled)
        (unless (= (fzfa-source-last-gen source) -1)
          (fzfa-fuzz--fail-observation
           seed trace -1 (fzfa-source-last-gen source)
           "old handle generation committed after replacement"))))
    t))

(defun fzfa-fuzz-state--message-events (context format-string args)
  "Return `fzfa--print' events for CONTEXT, FORMAT-STRING, and ARGS.

CONTEXT is `owner', `process', or `none'.  The owner and process variants
model an active fzfa minibuffer; only the current buffer differs."
  (let* ((window (selected-window))
         (original-buffer (window-buffer window))
         (owner (generate-new-buffer " *fzfa fuzz owner*"))
         (worker (generate-new-buffer " *fzfa fuzz process*"))
         (session (list 'session))
         (buffer-role
          (lambda ()
            (cond
             ((eq (current-buffer) owner) 'owner)
             ((eq (current-buffer) worker) 'process)
             (t 'other))))
         events)
    (unwind-protect
        (progn
          (set-window-buffer window owner)
          (with-current-buffer owner
            (setq-local fzfa--minibuffer-session session))
          (cl-letf (((symbol-function 'active-minibuffer-window)
                     (lambda () (unless (eq context 'none) window)))
                    ((symbol-function 'minibufferp)
                     (lambda (&optional buffer &rest _)
                       (eq (or buffer (current-buffer)) owner)))
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (list 'log
                                   (apply #'format format-string args)
                                   inhibit-message (funcall buffer-role))
                             events)))
                    ((symbol-function 'minibuffer-message)
                     (lambda (format-string &rest args)
                       (push (list 'inline
                                   (apply #'format format-string args)
                                   (funcall buffer-role))
                             events))))
            (with-current-buffer (if (eq context 'owner) owner worker)
              (apply #'fzfa--print format-string args))))
      (set-window-buffer window original-buffer)
      (kill-buffer owner)
      (kill-buffer worker))
    (nreverse events)))

(defun fzfa-fuzz-state--message-violations (context events expected-text)
  "Return ownership violations in CONTEXT and EVENTS for EXPECTED-TEXT."
  (let* ((logs (cl-remove-if-not
                (lambda (event) (eq (car event) 'log)) events))
         (inlines (cl-remove-if-not
                   (lambda (event) (eq (car event) 'inline)) events))
         (active (not (eq context 'none)))
         (expected-log-role (if (eq context 'owner) 'owner 'process))
         violations)
    (unless (= (length logs) 1)
      (push 'log-count violations))
    (when (= (length logs) 1)
      (let ((log (car logs)))
        (unless (equal (nth 1 log) expected-text)
          (push 'log-text violations))
        (unless (eq (nth 3 log) expected-log-role)
          (push 'log-buffer violations))
        (if active
            (unless (eq (nth 2 log) t)
              (push 'echo-not-inhibited violations))
          (when (nth 2 log)
            (push 'echo-inhibited-without-owner violations)))))
    (if active
        (progn
          (unless (= (length inlines) 1)
            (push 'inline-count violations))
          (when (= (length inlines) 1)
            (let ((inline (car inlines)))
              (unless (equal (nth 1 inline) expected-text)
                (push 'inline-text violations))
              (unless (eq (nth 2 inline) 'owner)
                (push 'inline-buffer violations)))))
      (when inlines
        (push 'inline-without-owner violations)))
    (unless (equal (mapcar #'car events)
                   (if active '(log inline) '(log)))
      (push 'event-order violations))
    (nreverse violations)))

(defun fzfa-fuzz-state--message-trace (root-seed case-seed context)
  "Return a ROOT-SEED and CASE-SEED message trace for CONTEXT."
  (fzfa-fuzz-trace-create
   'message-owner root-seed case-seed
   (list :context context)
   '((print "problem %d" 7))))

(defun fzfa-fuzz-state--message-case (trace)
  "Run one message ownership TRACE in its deterministic context.

Return non-nil only for the exact known worker-buffer event shape."
  (let* ((seed (plist-get trace :case-seed))
         (initial (plist-get trace :initial-state))
         (keys (fzfa-fuzz-trace--plist-keys
                initial "message-owner initial state" '(:context)))
         (context (plist-get initial :context))
         (actions (plist-get trace :actions))
         (print-action (car actions)))
    (fzfa-fuzz-trace--require-keys
     keys '(:context) "message-owner initial state")
    (unless (and (memq context '(owner process none))
                 (= (length actions) 1)
                 (eq (car-safe print-action) 'print)
                 (stringp (nth 1 print-action)))
      (error "Malformed message-owner trace: %S" trace))
    (let* ((format-string (nth 1 print-action))
           (args (nthcdr 2 print-action))
           (expected-text (apply #'format format-string args))
           (events (fzfa-fuzz-state--message-events
                    context format-string args))
           (violations (fzfa-fuzz-state--message-violations
                        context events expected-text))
           (known-events (list (list 'log expected-text nil 'process))))
    (cond
     ((and (eq context 'process)
           (equal events known-events))
      (list trace violations events))
     (violations
      (fzfa-fuzz--fail-observation
       seed trace 'no-violations
       (list :events events :violations violations)
       "message events violate ownership: %S" violations))
     (t nil)))))

(defun fzfa-fuzz-state-run-trace (trace)
  "Run one validated state-lane TRACE without random generation."
  (fzfa-fuzz-trace-validate trace)
  (fzfa-fuzz--run-trace
   trace
   (lambda ()
     (pcase (plist-get trace :target)
       ('completion-list (fzfa-fuzz-state--mutation-case trace))
       ('state-producer (fzfa-fuzz-state--producer-case trace))
       ('state-poller (fzfa-fuzz-state--poller-replay trace))
       ('message-owner (fzfa-fuzz-state--message-case trace))
       (target (error "Unsupported state trace target: %S" target))))))

(defun fzfa-fuzz-state-selftest-batch ()
  "Qualify state-fuzz generators and oracles with controlled canaries."
  (fzfa-fuzz-state--check-producer-generator)
  (fzfa-fuzz--expect-detection
   "completion-result-nil" "initial result is"
   (lambda ()
     (let ((fzfa-fuzz-state--mutation-source-count 1)
           (fzfa-fuzz-state--mutation-operation 'truncate)
           (fzfa-fuzz-state--completion-result-mutator
            (lambda (&rest _) nil)))
       (fzfa-fuzz-state-run-trace
        (fzfa-fuzz-state--mutation-trace
         9101 9101 (fzfa-fuzz-rng-create :state 9101))))))
  (fzfa-fuzz--expect-detection
   "completion-properties-stripped" "initial result is"
   (lambda ()
     (let ((fzfa-fuzz-state--mutation-source-count 1)
           (fzfa-fuzz-state--mutation-operation 'reverse)
           (fzfa-fuzz-state--completion-result-mutator
            (lambda (returned _sources)
              (mapcar #'substring-no-properties returned))))
       (fzfa-fuzz-state-run-trace
        (fzfa-fuzz-state--mutation-trace
         9102 9102 (fzfa-fuzz-rng-create :state 9102))))))
  (fzfa-fuzz--expect-detection
   "completion-result-aliases-snapshot" "frontend mutation changed snapshot"
   (lambda ()
     (let ((fzfa-fuzz-state--mutation-source-count 1)
           (fzfa-fuzz-state--mutation-operation 'truncate)
           (fzfa-fuzz-state--completion-result-mutator
            (lambda (_returned sources)
              (fzfa-source-snapshot (car sources)))))
       (fzfa-fuzz-state-run-trace
        (fzfa-fuzz-state--mutation-trace
         9103 9103 (fzfa-fuzz-rng-create :state 9103))))))
  (let* ((trace
          (fzfa-fuzz-state--producer-trace
           9201 9201 (fzfa-fuzz-rng-create :state 9201) 2
           '((fetch "a") (deliver 0 ("alpha" "beta")))))
         (before (copy-tree trace)))
    (fzfa-fuzz--expect-detection
     "producer-snapshot-alias" "snapshot is"
     (lambda ()
       (let ((fzfa-fuzz-state--producer-after-delivery-hook
              (lambda (_source candidates _entry)
                (setcar candidates "corrupt"))))
         (fzfa-fuzz-state-run-trace trace))))
    (unless (equal-including-properties trace before)
      (error "Producer driver mutated its executable trace")))
  (fzfa-fuzz--expect-detection
   "stale-producer-publication" "snapshot is"
   (lambda ()
     (let ((fzfa-fuzz-state--producer-after-delivery-hook
            (lambda (source candidates entry)
              (when (/= (fzfa-fuzz-state--callback-token entry)
                         (fzfa-source-prod-token source))
                (setf (fzfa-source-snapshot source) candidates
                      (fzfa-source-total source) (length candidates))))))
       (fzfa-fuzz-state-run-trace
        (fzfa-fuzz-state--producer-trace
         9202 9202 (fzfa-fuzz-rng-create :state 9202) 3
         '((fetch "a") (fetch "ab") (deliver 0 ("stale"))))))))
  (fzfa-fuzz--expect-detection
   "refresh-before-publication" "refresh observations are"
   (lambda ()
     (let ((fzfa-fuzz-state--producer-before-delivery-hook
            (lambda (_source _candidates entry)
              (funcall (fzfa-fuzz-state--callback-refresh entry)))))
       (fzfa-fuzz-state-run-trace
        (fzfa-fuzz-state--producer-trace
         9203 9203 (fzfa-fuzz-rng-create :state 9203) 2
         '((fetch "a") (deliver 0 ("fresh"))))))))
  (fzfa-fuzz--expect-detection
   "teardown-late-publication" "teardown allowed stale work to publish"
   (lambda ()
     (let ((fzfa-fuzz-state--producer-after-teardown-hook
            (lambda (source)
              (setf (fzfa-source-snapshot source) '("late")
                    (fzfa-source-total source) 1))))
       (fzfa-fuzz-state-run-trace
        (fzfa-fuzz-state--producer-trace
         9204 9204 (fzfa-fuzz-rng-create :state 9204) 3
         '((fetch "a") (deliver 0 ("fresh")) (stop)))))))
  (fzfa-fuzz--expect-detection
   "message-inline-from-worker" "inline-buffer"
   (lambda ()
     (cl-letf (((symbol-function 'fzfa--print)
                (lambda (format-string &rest args)
                  (let ((message-text
                         (apply #'format format-string args)))
                    (let ((inhibit-message t))
                      (message "%s" message-text))
                    (minibuffer-message "%s" message-text)))))
       (fzfa-fuzz-state-run-trace
        (fzfa-fuzz-state--message-trace 9301 9301 'process)))))
  (princ "fzfa state fuzz self-test passed (8 canaries killed)\n"))

(defun fzfa-fuzz-replay-batch ()
  "Run fixed regression seeds in batch mode."
  (let* ((seed (fzfa-fuzz--seed))
         (rng (fzfa-fuzz-rng-create :state seed)))
    (fzfa-fuzz-state-run-trace
     (fzfa-fuzz-state--mutation-trace seed seed rng))
    (fzfa-fuzz-state-run-trace
     (fzfa-fuzz-state--producer-trace seed seed rng 30))
    (fzfa-fuzz-state-run-trace
     (fzfa-fuzz-state--poller-trace seed seed))
    (let ((known
           (fzfa-fuzz-state-run-trace
            (fzfa-fuzz-state--message-trace seed seed 'process))))
      (if known
          (princ (format "KNOWN message-owner/process: %S\n" (nth 1 known)))
        (princ "RESOLVED message-owner/process\n")))
    (princ (format "fzfa fuzz replay passed (seed %d)\n" seed))))

(defun fzfa-fuzz-state-batch ()
  "Run deterministic randomized state cases in batch mode."
  (let* ((root-seed (fzfa-fuzz--seed))
         (cases (fzfa-fuzz--env-natural "FZFA_FUZZ_CASES" 300))
         (steps (fzfa-fuzz--env-natural "FZFA_FUZZ_STEPS" 40))
         (known-message-gaps 0))
    (dotimes (index cases)
      (let* ((seed (+ root-seed index))
             (rng (fzfa-fuzz-rng-create :state seed))
             (mutation-trace
              (fzfa-fuzz-state--mutation-trace root-seed seed rng))
             (producer-trace
              (fzfa-fuzz-state--producer-trace
               root-seed seed rng steps))
             (message-trace
              (fzfa-fuzz-state--message-trace
               root-seed seed (nth (% index 3) '(owner process none)))))
        (fzfa-fuzz-state-run-trace mutation-trace)
        (fzfa-fuzz-state-run-trace producer-trace)
        (when (fzfa-fuzz-state-run-trace message-trace)
          (cl-incf known-message-gaps))))
    (princ
     (format
      (concat "fzfa state fuzz passed (%d cases x %d steps, root seed %d); "
              "%d cases reached the known process-buffer message gap\n")
      cases steps root-seed known-message-gaps))))

(provide 'fzfa-fuzz-state)
;;; fzfa-fuzz-state.el ends here
