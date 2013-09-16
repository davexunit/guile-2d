;;; guile-2d
;;; Copyright (C) 2013 David Thompson <dthompson2@worcester.edu>
;;;
;;; Guile-2d is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-2d is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Actions are composable procedures that perform an operation over a
;; period of game time.
;;
;;; Code:

(define-module (2d actions)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (2d agenda)
  #:use-module (2d coroutine)
  #:export (<action>
            make-action
            action?
            null-action
            null-action?
            action-duration
            action-proc
            perform-action
            schedule-action
            action-cons
            action-list
            action-parallel
            action-repeat
            idle
            lerp))

;;;
;;; Action Procedures
;;;

;; Actions encapsulate a procedure that performs an action and the
;; duration of the action in game ticks.
(define-record-type <action>
  (%make-action proc duration)
  action?
  (duration action-duration)
  (proc action-proc))

(define (make-action proc duration)
  "Create a new action object that takes DURATION updates to
complete. PROC is a procedure that takes a value in the range [0, 1]
as its only argument. An error is thrown if DURATION is 0."
  (if (zero? duration)
      (throw 'action-duration-zero)
      (%make-action proc duration)))

(define (step-action action t)
  "Apply ACTION procedure to the time delta, T."
  ((action-proc action) t))

(define (perform-action action)
  "Execute ACTION. `perform-action` must be called from within a
coroutine, as it yields back to the agenda after each step."
  (let ((duration (action-duration action)))
    (define (step time)
      (if (= duration time)
          (step-action action 1)
          (begin
            (step-action action (/ time duration))
            (wait)
            (step (1+ time)))))
    (step 1)))

(define (schedule-action action)
  "Schedules a coroutine in the current agenda that will perform
ACTION on the next update."
  (agenda-schedule (colambda () (perform-action action))))

(define (action-cons a1 a2)
  "Return an action that performs A1 first, followed by A2."
  (define (real-cons)
    (let* ((duration (+ (action-duration a1) (action-duration a2)))
           (t1 (/ (action-duration a1) duration))
           (t2 (/ (action-duration a2) duration)))
      (make-action
       (lambda (t)
         (if (> t t1)
             (step-action a2 (/ (- t t1) t2))
             (step-action a1 (/ t t1))))
       duration)))
  ;; a2 can be #f, if this is the last action-cons of an action-list.
  (if a2 (real-cons) a1))

(define (action-list . actions)
  "Return an action that performs every action in the list ACTIONS."
  (if (null? actions)
      #f
      (action-cons (car actions) (apply action-list (cdr actions)))))

(define (action-parallel . actions)
  "Perform every action in the list ACTIONS in parallel."
  (let ((max-duration (reduce max 0 (map action-duration actions))))
    ;; Add idle action to each action to fill the time
    ;; difference between the action's duration and the
    ;; max action duration.
    (define (fill-action action)
      (if (= (action-duration action) max-duration)
          action
          (action-cons action (idle (- max-duration (action-duration action))))))

    (let ((filled-actions (map fill-action actions)))
      (make-action
       (lambda (t)
         (for-each (lambda (a) (step-action a t)) filled-actions))
       max-duration))))

(define (action-repeat n action)
  "Return an action that will perform ACTION N times."
  (apply action-list (make-list n action)))

;;;
;;; Simple Actions
;;;

(define (idle duration)
  "Return an action that does nothing."
  (make-action (lambda (t) #t) duration))

(define (lerp proc start end duration)
  "Linearly interpolate a number from START to END that takes DURATION
updates. Apply PROC to the linearly interpolated at each step."
  (let ((delta (- end start)))
    (make-action
     (lambda (t)
       (if (= t 1)
           (proc end)
           (proc (+ start (* delta t)))))
     duration)))
