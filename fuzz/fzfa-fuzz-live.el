;;; fzfa-fuzz-live.el --- Live minibuffer fuzz smoke test  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Exercises the real icomplete-vertical minibuffer.  It enters an fzfa
;; completion, narrows the list, backspaces to empty input, and observes the
;; mini-window fit call after each actual frontend render.  This file must run
;; in an interactive Emacs; batch mode cannot create a live minibuffer.

;;; Code:

(require 'cl-lib)
(require 'icomplete)
(require 'fzfa-fuzz-core)

(defvar fzfa-fuzz-live--observations nil
  "Chronological mini-window observations for the current live case.")

(defvar fzfa-fuzz-live--observation-sequence 0
  "Sequence number assigned to the next live render observation.")

(defvar fzfa-fuzz-live--driver-config nil
  "Dynamically bound configuration copied into a live minibuffer.")

(defvar fzfa-fuzz-live--observation-mutator nil
  "Test-only function for corrupting a live render observation.")

(defvar fzfa-fuzz-live--fit-call-mutator nil
  "Test-only function for replacing the mini-window fit call.")

(defvar fzfa-fuzz-live--watchdog-seconds 5
  "Seconds before a live-case handshake is aborted.")

(defvar-local fzfa-fuzz-live--phase nil)
(defvar-local fzfa-fuzz-live--expected-query nil)
(defvar-local fzfa-fuzz-live--target-query nil)
(defvar-local fzfa-fuzz-live--full-candidates nil)
(defvar-local fzfa-fuzz-live--pending-actions nil)
(defvar-local fzfa-fuzz-live--remaining-actions-cell nil)
(defvar-local fzfa-fuzz-live--event-log-cell nil)
(defvar-local fzfa-fuzz-live--initial-observation nil)
(defvar-local fzfa-fuzz-live--narrow-observation nil)
(defvar-local fzfa-fuzz-live--failure-cell nil)
(defvar-local fzfa-fuzz-live--progress-cell nil)
(defvar-local fzfa-fuzz-live--driver-timer nil)
(defvar-local fzfa-fuzz-live--watchdog-timer nil)

(defun fzfa-fuzz-live--report (text)
  "Write TEXT to standard output and the optional result file."
  (princ text)
  (when-let* ((file (getenv "FZFA_FUZZ_RESULT_FILE")))
    (with-temp-file file
      (insert text))))

(defun fzfa-fuzz-live--after-string ()
  "Return icomplete's displayed completion string, or nil."
  (when (and (bound-and-true-p icomplete-overlay)
             (overlayp icomplete-overlay))
    (overlay-get icomplete-overlay 'after-string)))

(defun fzfa-fuzz-live--cached-candidates ()
  "Return plain candidate strings from icomplete's dotted cache."
  (let ((tail completion-all-sorted-completions)
        candidates)
    (while (consp tail)
      (when (stringp (car tail))
        (push (substring-no-properties (car tail)) candidates))
      (setq tail (cdr tail)))
    (nreverse candidates)))

(defun fzfa-fuzz-live--same-candidates-p (left right)
  "Return non-nil when LEFT and RIGHT contain the same strings."
  (equal (sort (copy-sequence left) #'string-lessp)
         (sort (copy-sequence right) #'string-lessp)))

(defun fzfa-fuzz-live--replace-candidates (observation candidates)
  "Return a copy of OBSERVATION whose logical CANDIDATES are replaced."
  (let ((copy (copy-sequence observation)))
    (setq copy (plist-put copy :candidates (copy-sequence candidates)))
    (plist-put copy :candidate-count (length candidates))))

(defun fzfa-fuzz-live--fail-driver (oracle format-string &rest args)
  "Record stable ORACLE and FORMAT-STRING, then make the minibuffer quit."
  (unless (car fzfa-fuzz-live--failure-cell)
    (setcar fzfa-fuzz-live--failure-cell
            (list :oracle oracle
                  :message (apply #'format format-string args))))
  (setq unread-command-events (list 7)))

(defun fzfa-fuzz-live--deliver-event (buffer event)
  "Deliver EVENT to live minibuffer BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq fzfa-fuzz-live--driver-timer nil)
      (when (minibufferp buffer)
        (setq unread-command-events
              (append unread-command-events (list event)))))))

(defun fzfa-fuzz-live--queue-event (event expected-query)
  "Queue EVENT once and wait for EXPECTED-QUERY to render afterward."
  (unless fzfa-fuzz-live--driver-timer
    (setq fzfa-fuzz-live--expected-query expected-query
          fzfa-fuzz-live--driver-timer
          (run-with-idle-timer 0 nil #'fzfa-fuzz-live--deliver-event
                               (current-buffer) event))))

(defun fzfa-fuzz-live--queue-next-action ()
  "Queue the next input action recorded in the current trace."
  ;; An automatic initial exhibit can run before the setup kick's idle timer.
  ;; Do not consume a recorded action until the timer slot is actually free.
  (unless fzfa-fuzz-live--driver-timer
    (if (null fzfa-fuzz-live--pending-actions)
        (fzfa-fuzz-live--fail-driver
         'trace-actions-exhausted
         "live trace ran out of input actions in phase %S"
         fzfa-fuzz-live--phase)
      (let ((action (car fzfa-fuzz-live--pending-actions)))
        (if (and (eq (car-safe action) 'key)
                 (integerp (nth 1 action))
                 (stringp (nth 2 action))
                 (null (nthcdr 3 action)))
            (progn
              (setq fzfa-fuzz-live--pending-actions
                    (cdr fzfa-fuzz-live--pending-actions))
              (setcar fzfa-fuzz-live--remaining-actions-cell
                      (copy-tree fzfa-fuzz-live--pending-actions))
              (setcar fzfa-fuzz-live--event-log-cell
                      (append
                       (car fzfa-fuzz-live--event-log-cell)
                       (list
                        (list :phase fzfa-fuzz-live--phase
                              :after-sequence
                              fzfa-fuzz-live--observation-sequence
                              :action (copy-tree action)))))
              (fzfa-fuzz-live--queue-event (nth 1 action) (nth 2 action)))
          (fzfa-fuzz-live--fail-driver
           'malformed-trace-action
           "malformed live input action: %S" action))))))

(defun fzfa-fuzz-live--advance-driver (observation)
  "Advance the live input handshake after OBSERVATION."
  (let ((query (plist-get observation :query))
        (candidates (plist-get observation :candidates)))
    (pcase fzfa-fuzz-live--phase
      ('initial
       (when (and (equal query "")
                  (fzfa-fuzz-live--same-candidates-p
                   candidates fzfa-fuzz-live--full-candidates)
                  (> (or (plist-get observation :lines) 0) 1)
                  (< (plist-get observation :before)
                     (plist-get observation :target))
                  (<= (plist-get observation :target)
                      (plist-get observation :after)))
         (setq fzfa-fuzz-live--initial-observation observation
               fzfa-fuzz-live--phase 'narrowing)
         (setcar fzfa-fuzz-live--progress-cell 'initial)
         (fzfa-fuzz-live--queue-next-action)))
      ('narrowing
       (when (equal query fzfa-fuzz-live--expected-query)
         (let ((length (length query)))
           (if (< length (length fzfa-fuzz-live--target-query))
               (fzfa-fuzz-live--queue-next-action)
             (if (and (member fzfa-fuzz-live--target-query candidates)
                      (> (length candidates) 0)
                      (< (length candidates)
                         (length fzfa-fuzz-live--full-candidates)))
                 (progn
                   (setq fzfa-fuzz-live--narrow-observation observation
                         fzfa-fuzz-live--phase 'widening)
                   (setcar fzfa-fuzz-live--progress-cell 'narrow)
                   (fzfa-fuzz-live--queue-next-action))
               (fzfa-fuzz-live--fail-driver
                'narrow-candidates-not-reduced
                "narrow render did not reduce candidates: %S" observation))))))
      ('widening
       (when (equal query fzfa-fuzz-live--expected-query)
         (if (> (length query) 0)
             (fzfa-fuzz-live--queue-next-action)
           (let* ((initial fzfa-fuzz-live--initial-observation)
                  (fresh (> (plist-get observation :sequence)
                            (plist-get
                             fzfa-fuzz-live--narrow-observation :sequence)))
                  (restored
                   (fzfa-fuzz-live--same-candidates-p
                    candidates fzfa-fuzz-live--full-candidates))
                  (multiline (> (or (plist-get observation :lines) 0) 1))
                  (not-collapsed
                   (>= (plist-get observation :after)
                       (plist-get initial :after))))
             (if (and fresh restored multiline not-collapsed)
                 (progn
                   (setq fzfa-fuzz-live--phase 'exiting)
                   (setcar fzfa-fuzz-live--progress-cell 'empty-restored)
                   (fzfa-fuzz-live--queue-next-action))
               (fzfa-fuzz-live--fail-driver
                'empty-candidates-not-restored
                (concat "fresh empty render did not restore the full "
                        "non-collapsed display: %S")
                observation)))))))))

(defun fzfa-fuzz-live--fit-observer (function &rest args)
  "Call FUNCTION with ARGS and record its live minibuffer fit context."
  (let* ((window (active-minibuffer-window))
         (buffer (and (window-live-p window) (window-buffer window)))
         (before (and (window-live-p window) (window-text-height window)))
         result)
    (setq result
          (if fzfa-fuzz-live--fit-call-mutator
              (funcall fzfa-fuzz-live--fit-call-mutator function args)
            (apply function args)))
    (when (and (buffer-live-p buffer) (window-live-p window))
      (with-current-buffer buffer
        (let* ((after-string (fzfa-fuzz-live--after-string))
               (lines (and (stringp after-string)
                           (1+ (cl-count ?\n after-string))))
               (query (and (minibufferp buffer)
                           (buffer-substring-no-properties
                            (minibuffer-prompt-end) (point-max))))
               (session fzfa--minibuffer-session)
               (candidates (fzfa-fuzz-live--cached-candidates))
               (sequence (cl-incf fzfa-fuzz-live--observation-sequence))
               (observation
                (list :sequence sequence :query query
                      :candidates candidates
                      :candidate-count (length candidates)
                      :lines lines :before before
                      :target (and lines
                                   (fzfa-fuzz-live--target-height lines))
                      :after (window-text-height window)
                      :session (and session t)
                      :after-string
                      (and (stringp after-string)
                           (substring-no-properties after-string)))))
          (when session
            (when fzfa-fuzz-live--observation-mutator
              (setq observation
                    (funcall fzfa-fuzz-live--observation-mutator observation)))
            (setq fzfa-fuzz-live--observations
                  (append
                   fzfa-fuzz-live--observations
                   (list observation)))
            (fzfa-fuzz-live--advance-driver observation)))))
    result))

(defun fzfa-fuzz-live--watchdog (buffer)
  "Send quit to BUFFER if a live fuzz case has not exited."
  (when-let* (((buffer-live-p buffer))
              (window (active-minibuffer-window))
              ((eq buffer (window-buffer window))))
    (with-current-buffer buffer
      (fzfa-fuzz-live--fail-driver
       'watchdog-timeout
       "live case timed out in phase %S waiting for query %S"
       fzfa-fuzz-live--phase fzfa-fuzz-live--expected-query))))

(defun fzfa-fuzz-live--kick (buffer)
  "Request the initial icomplete render in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq fzfa-fuzz-live--driver-timer nil)
      (when (minibufferp buffer)
        (icomplete-exhibit)))))

(defun fzfa-fuzz-live--driver-cleanup ()
  "Cancel the current minibuffer driver's timers."
  (when (timerp fzfa-fuzz-live--driver-timer)
    (cancel-timer fzfa-fuzz-live--driver-timer))
  (when (timerp fzfa-fuzz-live--watchdog-timer)
    (cancel-timer fzfa-fuzz-live--watchdog-timer))
  (setq fzfa-fuzz-live--driver-timer nil
        fzfa-fuzz-live--watchdog-timer nil))

(defun fzfa-fuzz-live--driver-setup ()
  "Install the observation-driven input handshake in the minibuffer."
  (let ((config fzfa-fuzz-live--driver-config))
    (setq-local fzfa-fuzz-live--phase 'initial)
    (setq-local fzfa-fuzz-live--expected-query "")
    (setq-local fzfa-fuzz-live--target-query (plist-get config :target))
    (setq-local fzfa-fuzz-live--full-candidates
                (copy-sequence (plist-get config :candidates)))
    (setq-local fzfa-fuzz-live--pending-actions
                (copy-tree (plist-get config :actions)))
    (setq-local fzfa-fuzz-live--remaining-actions-cell
                (plist-get config :remaining-actions-cell))
    (setq-local fzfa-fuzz-live--event-log-cell
                (plist-get config :event-log-cell))
    (setq-local fzfa-fuzz-live--failure-cell
                (plist-get config :failure-cell))
    (setq-local fzfa-fuzz-live--progress-cell
                (plist-get config :progress-cell)))
  (add-hook 'minibuffer-exit-hook #'fzfa-fuzz-live--driver-cleanup nil t)
  (setq fzfa-fuzz-live--watchdog-timer
        (run-at-time fzfa-fuzz-live--watchdog-seconds nil
                     #'fzfa-fuzz-live--watchdog (current-buffer)))
  (setq fzfa-fuzz-live--driver-timer
        (run-with-idle-timer 0 nil
                             #'fzfa-fuzz-live--kick (current-buffer))))

(defun fzfa-fuzz-live--target-height (lines)
  "Return the mini-window height fzfa requests for LINES."
  (let ((max-lines
         (cond ((floatp max-mini-window-height)
                (max 1 (floor (* max-mini-window-height (frame-height)))))
               ((integerp max-mini-window-height) max-mini-window-height)
               (t 25))))
    (min lines max-lines)))

(defun fzfa-fuzz-live--assert-refcount-lifecycle ()
  "Check the icomplete exhibit advice's nested-session refcount."
  (let ((start-count fzfa--icomplete-exhibit-advice-count)
        (start-member
         (advice-member-p #'fzfa--icomplete-exhibit-fit-advice
                          'icomplete-exhibit)))
    (unwind-protect
        (progn
          (fzfa--icomplete-install-fit-advice)
          (fzfa--icomplete-install-fit-advice)
          (unless (and (= fzfa--icomplete-exhibit-advice-count
                          (+ start-count 2))
                       (advice-member-p #'fzfa--icomplete-exhibit-fit-advice
                                        'icomplete-exhibit))
            (error "Nested fit advice was not retained"))
          (fzfa--icomplete-uninstall-fit-advice)
          (unless (and (= fzfa--icomplete-exhibit-advice-count
                          (1+ start-count))
                       (advice-member-p #'fzfa--icomplete-exhibit-fit-advice
                                        'icomplete-exhibit))
            (error "Inner teardown removed the fit advice")))
      (while (> fzfa--icomplete-exhibit-advice-count start-count)
        (fzfa--icomplete-uninstall-fit-advice)))
    (unless (and (= fzfa--icomplete-exhibit-advice-count start-count)
                 (eq (and (advice-member-p
                            #'fzfa--icomplete-exhibit-fit-advice
                            'icomplete-exhibit)
                           t)
                     (and start-member t)))
      (error "Fit advice did not return to its initial state"))))

(defun fzfa-fuzz-live--target-query (rng)
  "Choose a discriminating query using RNG."
  (fzfa-fuzz--pick rng '("alpha" "quartz" "violet" "mango")))

(defun fzfa-fuzz-live--candidates (rng target)
  "Build a multi-line completion set around TARGET using RNG."
  (cons (copy-sequence target)
        (cl-loop for index below (+ 12 (fzfa-fuzz--integer rng 12))
                 collect (format "xxxxx-%02d" index))))

(defun fzfa-fuzz-live--case-inputs (seed)
  "Return (TARGET . CANDIDATES) generated from SEED."
  (let* ((rng (fzfa-fuzz-rng-create :state seed))
         (target (fzfa-fuzz-live--target-query rng)))
    (cons target (fzfa-fuzz-live--candidates rng target))))

(defun fzfa-fuzz-live--input-actions (target)
  "Return recorded key actions that type and delete TARGET, then accept."
  (append
   (cl-loop for index from 0 below (length target)
            collect (list 'key (aref target index)
                          (substring target 0 (1+ index))))
   (cl-loop for length from (1- (length target)) downto 0
            collect (list 'key 127 (substring target 0 length)))
   '((key 13 ""))))

(defun fzfa-fuzz-live--trace (root-seed case-seed)
  "Generate a live trace for ROOT-SEED and CASE-SEED."
  (let* ((inputs (fzfa-fuzz-live--case-inputs case-seed))
         (target (car inputs))
         (candidates (cdr inputs)))
    (fzfa-fuzz-trace-create
     'live-icomplete root-seed case-seed
     (list :frontend 'icomplete-vertical
           :target target
           :candidates (fzfa-fuzz--copy-strings candidates)
           :icomplete-prospects-height 10
           :max-mini-window-height 10
           :watchdog-seconds fzfa-fuzz-live--watchdog-seconds)
     (fzfa-fuzz-live--input-actions target))))

(defun fzfa-fuzz-live--decode-trace (trace)
  "Validate TRACE and return its live-minibuffer inputs."
  (let* ((initial (plist-get trace :initial-state))
         (keys
          (fzfa-fuzz-trace--plist-keys
           initial "live-icomplete initial state"
           '(:frontend :target :candidates :icomplete-prospects-height
             :max-mini-window-height :watchdog-seconds)))
         (frontend (plist-get initial :frontend))
         (target (plist-get initial :target))
         (candidates (plist-get initial :candidates))
         (prospects-height (plist-get initial :icomplete-prospects-height))
         (max-height (plist-get initial :max-mini-window-height))
         (watchdog (plist-get initial :watchdog-seconds))
         (actions (plist-get trace :actions)))
    (fzfa-fuzz-trace--require-keys
     keys
     '(:frontend :target :candidates :icomplete-prospects-height
       :max-mini-window-height :watchdog-seconds)
     "live-icomplete initial state")
    (unless (and (eq frontend 'icomplete-vertical)
                 (stringp target) (> (length target) 0)
                 (fzfa-fuzz--proper-list-p candidates)
                 (cl-every #'stringp candidates)
                 (member target candidates)
                 (integerp prospects-height) (> prospects-height 0)
                 (numberp max-height) (> max-height 0)
                 (numberp watchdog) (> watchdog 0)
                 (equal actions (fzfa-fuzz-live--input-actions target)))
      (error "Malformed live icomplete trace: %S" trace))
    (list :target target :candidates candidates
          :icomplete-prospects-height prospects-height
          :max-mini-window-height max-height
          :watchdog-seconds watchdog :actions actions)))

(defun fzfa-fuzz-live--case (trace)
  "Run one real icomplete minibuffer TRACE."
  (let* ((description (fzfa-fuzz-live--decode-trace trace))
         (seed (plist-get trace :case-seed))
         (target-query (plist-get description :target))
         (candidates
          (fzfa-fuzz--copy-strings
           (plist-get description :candidates)))
         (actions (plist-get description :actions))
         (failure-cell (list nil))
         (progress-cell (list 'not-started))
         (remaining-actions-cell (list (copy-tree actions)))
         (event-log-cell (list nil))
         (initial-count fzfa--icomplete-exhibit-advice-count)
         (initial-member
          (advice-member-p #'fzfa--icomplete-exhibit-fit-advice
                           'icomplete-exhibit))
         (fzfa-fuzz-live--observations nil)
         (fzfa-fuzz-live--observation-sequence 0)
         (icomplete-prospects-height
          (plist-get description :icomplete-prospects-height))
         (icomplete-show-matches-on-no-input t)
         (icomplete-compute-delay 0)
         (max-mini-window-height
          (plist-get description :max-mini-window-height))
         (resize-mini-windows t)
         (fzfa-fuzz-live--watchdog-seconds
          (plist-get description :watchdog-seconds))
         (fzfa-fuzz-live--driver-config
          (list :target target-query :candidates candidates
                :actions actions
                :remaining-actions-cell remaining-actions-cell
                :event-log-cell event-log-cell
                :failure-cell failure-cell :progress-cell progress-cell))
         (minibuffer-setup-hook
          (cons #'fzfa-fuzz-live--driver-setup minibuffer-setup-hook))
         result)
    (advice-add 'fzfa--icomplete-fit-mini-window :around
                #'fzfa-fuzz-live--fit-observer)
    (unwind-protect
        (progn
          (condition-case err
              (setq result
                    (fzfa-completing-read
                     :prompt "fzfa live fuzz: "
                     :candidates candidates
                     :category 'fzfa-fuzz-live
                     :require-match nil))
            (quit
             (if (car failure-cell)
                 (let ((failure (car failure-cell)))
                   (fzfa-fuzz--fail-observation-key
                    seed trace (plist-get failure :oracle)
                    'completed-handshake
                    (list :driver-failure failure
                          :queued-actions (car event-log-cell)
                          :remaining-actions (car remaining-actions-cell)
                          :observations fzfa-fuzz-live--observations)
                    "%s" (plist-get failure :message)))
               (signal (car err) (cdr err)))))
          (when (car failure-cell)
            (let ((failure (car failure-cell)))
              (fzfa-fuzz--fail-observation-key
               seed trace (plist-get failure :oracle)
               'completed-handshake
               (list :driver-failure failure
                     :queued-actions (car event-log-cell)
                     :remaining-actions (car remaining-actions-cell)
                     :observations fzfa-fuzz-live--observations)
               "%s" (plist-get failure :message))))
          (unless (eq (car progress-cell) 'empty-restored)
            (fzfa-fuzz--fail-observation
             seed trace 'empty-restored
             (list :progress (car progress-cell)
                   :queued-actions (car event-log-cell)
                   :remaining-actions (car remaining-actions-cell)
                   :observations fzfa-fuzz-live--observations)
             "live handshake exited before restoring empty input"))
          (unless (null (car remaining-actions-cell))
            (fzfa-fuzz--fail-observation
             seed trace nil (car remaining-actions-cell)
             "live handshake left recorded input actions"))
          (unless (and (= fzfa--icomplete-exhibit-advice-count initial-count)
                       (eq (and (advice-member-p
                                 #'fzfa--icomplete-exhibit-fit-advice
                                 'icomplete-exhibit)
                                t)
                           (and initial-member t)))
            (fzfa-fuzz--fail-observation
             seed trace
             (list :count initial-count :member (and initial-member t))
             (list
              :count fzfa--icomplete-exhibit-advice-count
              :member
              (and (advice-member-p
                    #'fzfa--icomplete-exhibit-fit-advice 'icomplete-exhibit)
                   t)
              :result result)
             "session leaked fit advice")))
      (setq unread-command-events nil)
      (advice-remove 'fzfa--icomplete-fit-mini-window
                     #'fzfa-fuzz-live--fit-observer))
    t))

(defun fzfa-fuzz-live-run-trace (trace)
  "Run one validated live icomplete TRACE without random generation."
  (fzfa-fuzz-trace-validate trace)
  (unless (eq (plist-get trace :target) 'live-icomplete)
    (error "Unsupported live trace target: %S" (plist-get trace :target)))
  (fzfa-fuzz--run-trace trace (lambda () (fzfa-fuzz-live--case trace))))

(defun fzfa-fuzz-live-selftest ()
  "Qualify live replay and its render oracle with controlled canaries."
  (let ((trace (fzfa-fuzz-live--trace 9500 9500)))
    (cl-letf (((symbol-function 'fzfa-fuzz--integer)
               (lambda (&rest _) (error "Replay requested randomness")))
              ((symbol-function 'fzfa-fuzz--pick)
               (lambda (&rest _) (error "Replay requested randomness"))))
      (fzfa-fuzz-live-run-trace trace)))
  (fzfa-fuzz--expect-detection
   "live-fit-disabled" "timed out in phase initial"
   (lambda ()
     (let ((fzfa-fuzz-live--watchdog-seconds 0.75)
           (fzfa-fuzz-live--fit-call-mutator
            (lambda (_function _args) nil)))
       (fzfa-fuzz-live-run-trace (fzfa-fuzz-live--trace 9501 9501)))))
  (let ((fzfa-fuzz-live--watchdog-seconds 0.75))
    (let* ((seed 9502)
           (trace (fzfa-fuzz-live--trace seed seed))
           (initial (plist-get trace :initial-state))
           (target (plist-get initial :target))
           (full (plist-get initial :candidates)))
      (fzfa-fuzz--expect-detection
       "live-filter-disabled" "narrow render did not reduce candidates"
       (lambda ()
         (let ((fzfa-fuzz-live--observation-mutator
                (lambda (observation)
                  (if (equal (plist-get observation :query) target)
                      (fzfa-fuzz-live--replace-candidates observation full)
                    observation))))
           (fzfa-fuzz-live-run-trace trace))))))
  (let ((fzfa-fuzz-live--watchdog-seconds 0.75))
    (let* ((seed 9503)
           (trace (fzfa-fuzz-live--trace seed seed))
           (target (plist-get (plist-get trace :initial-state) :target))
           narrow)
      (fzfa-fuzz--expect-detection
       "live-empty-render-stale"
       "fresh empty render did not restore the full non-collapsed display"
       (lambda ()
         (let ((fzfa-fuzz-live--observation-mutator
                (lambda (observation)
                  (let ((query (plist-get observation :query)))
                    (cond
                     ((equal query target)
                      (setq narrow (plist-get observation :candidates))
                      observation)
                     ((and (equal query "") narrow)
                      (fzfa-fuzz-live--replace-candidates observation narrow))
                     (t observation))))))
           (fzfa-fuzz-live-run-trace trace))))))
  (princ
   (concat "fzfa live fuzz self-test passed "
           "(exact replay without RNG; 3 canaries killed)\n")))

(defun fzfa-fuzz-live-run ()
  "Run live icomplete fuzz cases, then exit Emacs with their status."
  (if noninteractive
      (error "Live fuzz must run without --batch")
    (condition-case err
        (progn
          (icomplete-vertical-mode 1)
          (fzfa-fuzz-live--assert-refcount-lifecycle)
          (fzfa-fuzz-live-selftest)
          (let* ((root-seed (fzfa-fuzz--seed))
                 (cases (fzfa-fuzz--env-natural "FZFA_FUZZ_CASES" 20)))
            (dotimes (index cases)
              (let ((seed (+ root-seed index)))
                (fzfa-fuzz-live-run-trace
                 (fzfa-fuzz-live--trace root-seed seed))))
            (fzfa-fuzz-live--report
             (format "fzfa live fuzz passed (%d cases, root seed %d)\n"
                     cases root-seed)))
          (kill-emacs 0))
      ((error quit)
       (fzfa-fuzz-live--report
        (format "fzfa live fuzz failed: %s\n"
                (error-message-string err)))
       (kill-emacs 1)))))

(provide 'fzfa-fuzz-live)
;;; fzfa-fuzz-live.el ends here
