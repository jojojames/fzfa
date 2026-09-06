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

(defun fzfa-fuzz-state--mutate-list (value operation rng)
  "Apply destructive list OPERATION to VALUE using RNG."
  (pcase operation
    ('nconc (when value (nconc value (list "frontend-tail"))))
    ('truncate
     (when value (setcdr value nil) value))
    ('dot
     (when value (setcdr value (fzfa-fuzz--integer rng 20)) value))
    ('reverse (nreverse value))
    ('sort (sort value #'string-lessp))
    ('dedup (delete-dups value))))

(defun fzfa-fuzz-state--mutation-case (seed rng)
  "Run one completion-list ownership case using SEED and RNG."
  (let* ((source-count (or fzfa-fuzz-state--mutation-source-count
                           (1+ (fzfa-fuzz--integer rng 3))))
         (multi-p (> source-count 1))
         (operation (or fzfa-fuzz-state--mutation-operation
                        (fzfa-fuzz--pick
                         rng '(nconc truncate dot reverse sort dedup))))
         (specs
          (cl-loop for source-index below source-count
                   collect
                   (list :name (format "source-%d" source-index)
                         :candidates
                         (fzfa-fuzz-state--mutation-candidate-list
                          rng source-index)
                         :category 'fzfa-fuzz
                         :action #'identity)))
         ;; Build the oracle before fzfa creates a source or invokes a
         ;; producer callback.  Its list spines and strings share nothing with
         ;; the values that production code will cache or return.
         (expected-snapshots
          (cl-loop
           for spec in specs
           for source-index from 0
           collect
           (mapcar
            (lambda (candidate)
              (fzfa-fuzz-state--expected-tag
               candidate source-index multi-p))
            (plist-get spec :candidates))))
         (expected-result
          (cl-mapcan #'fzfa-fuzz--copy-strings expected-snapshots))
         (trace (list :target 'completion-list
                      :sources source-count :operation operation))
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
                          (fzfa-fuzz--fail
                           seed trace "initial result is not a proper list: %S"
                           returned))
                        (unless (equal-including-properties
                                 returned expected-result)
                          (fzfa-fuzz--fail
                           seed trace
                           "initial result is %S, expected %S"
                           returned expected-result))
                        (let ((before
                               (fzfa-fuzz--copy-strings returned)))
                          (setq returned
                                (fzfa-fuzz-state--mutate-list
                                 returned operation rng))
                          (when (equal-including-properties returned before)
                            (fzfa-fuzz--fail
                             seed trace
                             "frontend operation did not change result: %S"
                             operation)))
                        (cl-mapc
                         (lambda (source expected)
                           (let ((actual (fzfa-source-snapshot source)))
                             (unless (and (fzfa-fuzz--proper-list-p actual)
                                          (equal-including-properties
                                           actual expected))
                               (fzfa-fuzz--fail
                                seed trace
                                (concat "frontend mutation changed snapshot: "
                                        "%S, expected %S")
                                actual expected))))
                         made-sources expected-snapshots)
                        (let ((second (funcall table "" nil t)))
                          (unless (and (fzfa-fuzz--proper-list-p second)
                                       (equal-including-properties
                                        second expected-result))
                            (fzfa-fuzz--fail
                             seed trace
                             "second result is %S, expected %S"
                             second expected-result)))
                        nil))))
           (fzfa--read specs :prompt "fuzz: ")))))
    t))

(cl-defstruct (fzfa-fuzz-state--callback
               (:constructor fzfa-fuzz-state--callback-create))
  token kind function refresh)

(defun fzfa-fuzz-state--producer-trace (rng steps)
  "Generate a producer lifecycle trace from RNG with at most STEPS operations."
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

(defun fzfa-fuzz-state--producer-trace-features (trace)
  "Return lifecycle witnesses reached by generated TRACE.

This is a reachability check, not a correctness oracle.  It mirrors only
token creation and queued-refresh selection so the harness can report whether
its generator ever built the short traces its state oracle is meant to judge."
  (let ((token 0)
        (input :unfetched)
        callbacks tasks features)
    (dolist (operation trace)
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
             (trace (fzfa-fuzz-state--producer-trace rng steps)))
        (when-let* ((stop-tail (member '(stop) trace)))
          (unless (null (cdr stop-tail))
            (error "Producer generator emitted operations after stop: %S"
                   trace)))
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

(defun fzfa-fuzz-state--producer-case (seed rng steps &optional fixed-trace)
  "Run one producer lifecycle case for SEED using RNG and STEPS.

Use FIXED-TRACE instead of generating operations when it is non-nil."
  (let* ((trace (if fixed-trace
                    (copy-tree fixed-trace)
                  (fzfa-fuzz-state--producer-trace rng steps)))
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
                  :spec (list :name "state" :candidates producer)))
    (fzfa-fuzz--call-with-scheduler
     scheduler
     (lambda ()
       (dolist (operation trace)
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
              (let* ((entry (nth (% selector (length callbacks)) callbacks))
                     (token (fzfa-fuzz-state--callback-token entry))
                     (kind (fzfa-fuzz-state--callback-kind entry))
                     (expected (fzfa-fuzz--copy-strings candidates)))
                (when fzfa-fuzz-state--producer-before-delivery-hook
                  (funcall fzfa-fuzz-state--producer-before-delivery-hook
                           source candidates entry))
                (funcall (fzfa-fuzz-state--callback-function entry) candidates)
                (when fzfa-fuzz-state--producer-after-delivery-hook
                  (funcall fzfa-fuzz-state--producer-after-delivery-hook
                           source candidates entry))
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
            (cl-incf model-token)))
         (unless (= (fzfa-source-prod-token source) model-token)
           (fzfa-fuzz--fail
            seed trace "producer token is %S, expected %S after %S"
            (fzfa-source-prod-token source) model-token operation))
         (unless (equal (fzfa-source-prod-input source) model-input)
           (fzfa-fuzz--fail
            seed trace "producer input is %S, expected %S after %S"
            (fzfa-source-prod-input source) model-input operation))
         (unless (= (fzfa-source-request-epoch source) model-request-epoch)
           (fzfa-fuzz--fail
            seed trace "request epoch is %S, expected %S after %S"
            (fzfa-source-request-epoch source) model-request-epoch operation))
         (unless (equal (fzfa-source-current-cmd source) model-command)
           (fzfa-fuzz--fail
            seed trace "current command is %S, expected %S after %S"
            (fzfa-source-current-cmd source) model-command operation))
         (unless (equal-including-properties
                  (fzfa-source-snapshot source) model-snapshot)
           (fzfa-fuzz--fail
            seed trace "snapshot is %S, expected %S after %S"
            (fzfa-source-snapshot source) model-snapshot operation))
         (unless (= (fzfa-source-total source) model-total)
           (fzfa-fuzz--fail
            seed trace "total is %S, expected %S after %S"
            (fzfa-source-total source) model-total operation))
         (unless (= (fzfa-source-filtered source) model-filtered)
           (fzfa-fuzz--fail
            seed trace "filtered count is %S, expected %S after %S"
            (fzfa-source-filtered source) model-filtered operation))
         (unless (equal-including-properties
                  (fzfa-source-last-result source) model-last-result)
           (fzfa-fuzz--fail
            seed trace "last result is %S, expected %S after %S"
            (fzfa-source-last-result source) model-last-result operation))
         (unless (equal-including-properties
                  refresh-observations model-refresh-observations)
           (fzfa-fuzz--fail
            seed trace "refresh observations are %S, expected %S after %S"
            refresh-observations model-refresh-observations operation))
         (unless (= refreshes model-refreshes)
           (fzfa-fuzz--fail
            seed trace "refresh count is %S, expected %S after %S"
            refreshes model-refreshes operation)))))
    ;; Teardown must make every captured callback and queued refresh inert.
    (unless (and trace (eq (caar (last trace)) 'stop))
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
      (unless (and (equal-including-properties
                    (fzfa-source-snapshot source) snapshot)
                   (= (fzfa-source-total source) total)
                   (= (fzfa-source-filtered source) filtered)
                   (equal-including-properties
                    (fzfa-source-last-result source) last-result)
                   (= (fzfa-source-prod-token source) token)
                   (= (fzfa-source-request-epoch source) request-epoch)
                   (= refreshes before-refreshes)
                   (null (fzfa-fuzz--pending-tasks scheduler)))
        (fzfa-fuzz--fail seed trace "teardown allowed stale work to publish")))
    t))

(defun fzfa-fuzz-state--poller-replay (seed)
  "Replay publication after handle replacement for SEED."
  (let* ((trace '((generation old 1) tick (replace-handle) run))
         (source (fzfa-make-source :command "producer"))
         (generations '((old . 1) (new . 0)))
         scheduled
         (refreshes 0)
         (alive t))
    (setf (fzfa-source-handle source) 'old)
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
          (fzfa-fuzz--fail seed trace "poller did not schedule publication"))
        (setf (fzfa-source-handle source) 'new)
        (funcall scheduled)
        (unless (= (fzfa-source-last-gen source) -1)
          (fzfa-fuzz--fail
           seed trace "old handle generation committed after replacement"))))
    t))

(defun fzfa-fuzz-state--message-events (context)
  "Return `fzfa--print' events for CONTEXT.

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
              (fzfa--print "problem %d" 7))))
      (set-window-buffer window original-buffer)
      (kill-buffer owner)
      (kill-buffer worker))
    (nreverse events)))

(defconst fzfa-fuzz-state--known-process-message-events
  '((log "problem 7" nil process))
  "Exact event shape of the known worker-buffer ownership gap.")

(defun fzfa-fuzz-state--message-violations (context events)
  "Return every message ownership violation in CONTEXT and EVENTS."
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
        (unless (equal (nth 1 log) "problem 7")
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
              (unless (equal (nth 1 inline) "problem 7")
                (push 'inline-text violations))
              (unless (eq (nth 2 inline) 'owner)
                (push 'inline-buffer violations)))))
      (when inlines
        (push 'inline-without-owner violations)))
    (unless (equal (mapcar #'car events)
                   (if active '(log inline) '(log)))
      (push 'event-order violations))
    (nreverse violations)))

(defun fzfa-fuzz-state--message-case (seed context)
  "Run one message ownership case for SEED in deterministic CONTEXT.

Return non-nil only for the exact known worker-buffer event shape."
  (let* ((trace (list :target 'message-owner :context context))
         (events (fzfa-fuzz-state--message-events context))
         (violations (fzfa-fuzz-state--message-violations context events)))
    (cond
     ((and (eq context 'process)
           (equal events fzfa-fuzz-state--known-process-message-events))
      (list trace violations events))
     (violations
      (fzfa-fuzz--fail
       seed trace "message events violate ownership: %S (%S)"
       events violations))
     (t nil))))

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
       (fzfa-fuzz-state--mutation-case
        9101 (fzfa-fuzz-rng-create :state 9101)))))
  (fzfa-fuzz--expect-detection
   "completion-properties-stripped" "initial result is"
   (lambda ()
     (let ((fzfa-fuzz-state--mutation-source-count 1)
           (fzfa-fuzz-state--mutation-operation 'reverse)
           (fzfa-fuzz-state--completion-result-mutator
            (lambda (returned _sources)
              (mapcar #'substring-no-properties returned))))
       (fzfa-fuzz-state--mutation-case
        9102 (fzfa-fuzz-rng-create :state 9102)))))
  (fzfa-fuzz--expect-detection
   "completion-result-aliases-snapshot" "frontend mutation changed snapshot"
   (lambda ()
     (let ((fzfa-fuzz-state--mutation-source-count 1)
           (fzfa-fuzz-state--mutation-operation 'truncate)
           (fzfa-fuzz-state--completion-result-mutator
            (lambda (_returned sources)
              (fzfa-source-snapshot (car sources)))))
       (fzfa-fuzz-state--mutation-case
        9103 (fzfa-fuzz-rng-create :state 9103)))))
  (fzfa-fuzz--expect-detection
   "producer-snapshot-alias" "snapshot is"
   (lambda ()
     (let ((fzfa-fuzz-state--producer-after-delivery-hook
            (lambda (_source candidates _entry)
              (setcar candidates "corrupt"))))
       (fzfa-fuzz-state--producer-case
        9201 (fzfa-fuzz-rng-create :state 9201) 2
        '((fetch "a") (deliver 0 ("alpha" "beta")))))))
  (fzfa-fuzz--expect-detection
   "stale-producer-publication" "snapshot is"
   (lambda ()
     (let ((fzfa-fuzz-state--producer-after-delivery-hook
            (lambda (source candidates entry)
              (when (/= (fzfa-fuzz-state--callback-token entry)
                         (fzfa-source-prod-token source))
                (setf (fzfa-source-snapshot source) candidates
                      (fzfa-source-total source) (length candidates))))))
       (fzfa-fuzz-state--producer-case
        9202 (fzfa-fuzz-rng-create :state 9202) 3
        '((fetch "a") (fetch "ab") (deliver 0 ("stale")))))))
  (fzfa-fuzz--expect-detection
   "refresh-before-publication" "refresh observations are"
   (lambda ()
     (let ((fzfa-fuzz-state--producer-before-delivery-hook
            (lambda (_source _candidates entry)
              (funcall (fzfa-fuzz-state--callback-refresh entry)))))
       (fzfa-fuzz-state--producer-case
        9203 (fzfa-fuzz-rng-create :state 9203) 2
        '((fetch "a") (deliver 0 ("fresh")))))))
  (fzfa-fuzz--expect-detection
   "teardown-late-publication" "teardown allowed stale work to publish"
   (lambda ()
     (let ((fzfa-fuzz-state--producer-after-teardown-hook
            (lambda (source)
              (setf (fzfa-source-snapshot source) '("late")
                    (fzfa-source-total source) 1))))
       (fzfa-fuzz-state--producer-case
        9204 (fzfa-fuzz-rng-create :state 9204) 3
        '((fetch "a") (deliver 0 ("fresh")) (stop))))))
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
       (fzfa-fuzz-state--message-case 9301 'process))))
  (princ "fzfa state fuzz self-test passed (8 canaries killed)\n"))

(defun fzfa-fuzz-replay-batch ()
  "Run fixed regression seeds in batch mode."
  (let* ((seed (fzfa-fuzz--seed))
         (rng (fzfa-fuzz-rng-create :state seed)))
    (fzfa-fuzz-state--mutation-case seed rng)
    (fzfa-fuzz-state--producer-case seed rng 30)
    (fzfa-fuzz-state--poller-replay seed)
    (let ((known (fzfa-fuzz-state--message-case seed 'process)))
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
             (rng (fzfa-fuzz-rng-create :state seed)))
        (fzfa-fuzz-state--mutation-case seed rng)
        (fzfa-fuzz-state--producer-case seed rng steps)
        (when (fzfa-fuzz-state--message-case
               seed (nth (% index 3) '(owner process none)))
          (cl-incf known-message-gaps))))
    (princ
     (format
      (concat "fzfa state fuzz passed (%d cases x %d steps, root seed %d); "
              "%d cases reached the known process-buffer message gap\n")
      cases steps root-seed known-message-gaps))))

(provide 'fzfa-fuzz-state)
;;; fzfa-fuzz-state.el ends here
