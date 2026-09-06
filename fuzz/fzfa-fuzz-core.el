;;; fzfa-fuzz-core.el --- Shared deterministic fuzz helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 James Nguyen
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Deterministic random generation, trace reporting, and a controllable timer
;; queue for the fzfa fuzz targets.  This file is test-only and is not loaded by
;; the package.

;;; Code:

(require 'cl-lib)
(require 'fzfa)

(define-error 'fzfa-fuzz-failure "Fzfa fuzz failure")

(cl-defstruct (fzfa-fuzz-rng (:constructor fzfa-fuzz-rng-create))
  state)

(defun fzfa-fuzz--env-natural (name default)
  "Read non-negative integer NAME, or return DEFAULT."
  (if-let* ((raw (getenv name))
            ((string-match-p "\\`[0-9]+\\'" raw)))
      (string-to-number raw)
    default))

(defun fzfa-fuzz--seed ()
  "Return the configured fuzz seed."
  (fzfa-fuzz--env-natural "FZFA_FUZZ_SEED" 1))

(defun fzfa-fuzz--next (rng)
  "Advance RNG and return a deterministic 31-bit integer."
  (let ((next (logand #x7fffffff
                      (+ (* 1103515245 (fzfa-fuzz-rng-state rng)) 12345))))
    (setf (fzfa-fuzz-rng-state rng) next)
    next))

(defun fzfa-fuzz--integer (rng limit)
  "Return an integer in [0, LIMIT) from RNG."
  (if (<= limit 0) 0
    (% (fzfa-fuzz--next rng) limit)))

(defun fzfa-fuzz--pick (rng values)
  "Choose one element of non-empty VALUES using RNG."
  (nth (fzfa-fuzz--integer rng (length values)) values))

(defun fzfa-fuzz--chance (rng numerator denominator)
  "Return non-nil with NUMERATOR/DENOMINATOR probability using RNG."
  (< (fzfa-fuzz--integer rng denominator) numerator))

(defun fzfa-fuzz--fail (seed trace format-string &rest args)
  "Signal a fuzz failure with SEED, TRACE, and FORMAT-STRING with ARGS."
  (signal
   'fzfa-fuzz-failure
   (list (format "Fzfa fuzz failure\nseed: %s\ntrace: %S\n%s"
                 seed trace (apply #'format format-string args)))))

(defun fzfa-fuzz--expect-detection (name message-pattern function)
  "Require FUNCTION's oracle to kill canary NAME.

Only `fzfa-fuzz-failure' counts as detection.  MESSAGE-PATTERN must match the
failure so a canary cannot pass because an unrelated assertion happened to
fire."
  (condition-case err
      (progn
        (funcall function)
        (error "Fzfa fuzz canary survived: %s" name))
    (fzfa-fuzz-failure
     (let ((message (error-message-string err)))
       (unless (string-match-p message-pattern message)
         (error "Fzfa fuzz canary %s hit the wrong oracle: %s"
                name message))
       (princ (format "KILLED canary %s\n" name))
       t))))

(defun fzfa-fuzz--proper-list-p (value)
  "Return non-nil when VALUE is a finite proper list."
  (and (listp value) (numberp (proper-list-p value))))

(defun fzfa-fuzz--copy-strings (strings)
  "Copy the list spine and every string in STRINGS."
  (mapcar #'copy-sequence strings))

(cl-defstruct (fzfa-fuzz-task (:constructor fzfa-fuzz-task-create))
  id kind function args cancelled)

(cl-defstruct (fzfa-fuzz-scheduler
               (:constructor fzfa-fuzz-scheduler-create))
  (next-id 0)
  (now 0.0)
  queue)

(defun fzfa-fuzz--schedule (scheduler kind function args)
  "Queue FUNCTION with ARGS as KIND on SCHEDULER and return its task."
  (let ((task (fzfa-fuzz-task-create
               :id (cl-incf (fzfa-fuzz-scheduler-next-id scheduler))
               :kind kind :function function :args args)))
    (setf (fzfa-fuzz-scheduler-queue scheduler)
          (append (fzfa-fuzz-scheduler-queue scheduler) (list task)))
    task))

(defun fzfa-fuzz--pending-tasks (scheduler)
  "Return SCHEDULER's non-cancelled queued tasks."
  (cl-remove-if #'fzfa-fuzz-task-cancelled
                (fzfa-fuzz-scheduler-queue scheduler)))

(defun fzfa-fuzz--run-task (scheduler index)
  "Run pending task INDEX from SCHEDULER, returning the task or nil."
  (let ((task (nth index (fzfa-fuzz--pending-tasks scheduler))))
    (when task
      (setf (fzfa-fuzz-scheduler-queue scheduler)
            (delq task (fzfa-fuzz-scheduler-queue scheduler))
            (fzfa-fuzz-scheduler-now scheduler)
            (+ 0.01 (fzfa-fuzz-scheduler-now scheduler)))
      (unless (fzfa-fuzz-task-cancelled task)
        (apply (fzfa-fuzz-task-function task) (fzfa-fuzz-task-args task)))
      task)))

(defun fzfa-fuzz--run-all-tasks (scheduler)
  "Run every pending task on SCHEDULER in queue order."
  (while (fzfa-fuzz--pending-tasks scheduler)
    (fzfa-fuzz--run-task scheduler 0)))

(defun fzfa-fuzz--call-with-scheduler (scheduler function)
  "Call FUNCTION while SCHEDULER owns timer creation and time."
  (cl-letf (((symbol-function 'run-with-timer)
             (lambda (_seconds _repeat fn &rest args)
               (fzfa-fuzz--schedule scheduler 'timer fn args)))
            ((symbol-function 'run-with-idle-timer)
             (lambda (_seconds _repeat fn &rest args)
               (fzfa-fuzz--schedule scheduler 'idle fn args)))
            ((symbol-function 'cancel-timer)
             (lambda (task)
               (when (fzfa-fuzz-task-p task)
                 (setf (fzfa-fuzz-task-cancelled task) t))))
            ((symbol-function 'float-time)
             (lambda (&optional _time)
               (fzfa-fuzz-scheduler-now scheduler))))
    (funcall function)))

(provide 'fzfa-fuzz-core)
;;; fzfa-fuzz-core.el ends here
