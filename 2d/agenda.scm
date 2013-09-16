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
;; Deferred procedure scheduling.
;;
;;; Code:

(define-module (2d agenda)
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-agenda
            with-agenda
            agenda-schedule
            agenda-schedule-interval
            update-agenda
            clear-agenda))

;; This code is a modified version of the agenda implementation in
;; SICP. Thank you, SICP!

;;;
;;; Time segment
;;;

(define-record-type <time-segment>
  (%make-time-segment time queue)
  time-segment?
  (time segment-time)
  (queue segment-queue))

(define (make-time-segment time . callbacks)
  "Constructs a new time segment with given time and enqueus all
callback procedures."
  (let ((segment (%make-time-segment time (make-q))))
    ;; Enqueue all callbacks
    (for-each (lambda (c) (segment-enq segment c)) callbacks)
    segment))

(define (segment-enq segment callback)
  "Enqueues a callback procedure onto the segment queue."
  (enq! (segment-queue segment) callback))

;;;
;;; Agenda
;;;

(define-record-type <agenda>
  (%make-agenda time segments)
  agenda?
  (time agenda-time set-agenda-time!)
  (segments agenda-segments set-agenda-segments!))

(define (make-agenda)
  "Creates a new empty agenda."
  (%make-agenda 0 '()))

;; The global agenda that will be used when schedule is called outside
;; of a with-agenda form.
(define global-agenda (make-agenda))

(define *current-agenda* global-agenda)

;; emacs: (put 'with-agenda 'scheme-indent-function 1)
(define-syntax-rule (with-agenda agenda body ...)
  (begin
    (set! *current-agenda* agenda)
    body
    ...
    (set! *current-agenda* global-agenda)))

(define (agenda-empty? agenda)
  "Returns #t if agenda has no scheduled procedures."
  (null? (agenda-segments agenda)))

(define (first-segment agenda)
  "Returns the first time segment in the agenda."
  (car (agenda-segments agenda)))

(define (rest-segments agenda)
  "Returns everything but the first segment in the agenda."
  (cdr (agenda-segments agenda)))

(define (agenda-add-segment agenda time callback)
  "Adds a new time segment to the beginning of the agenda and enqueues
the given callback."
  (set-agenda-segments! agenda (cons (make-time-segment time callback)
                                     (agenda-segments agenda))))

(define (insert-segment segments time callback)
  "Inserts a new segment after the first segment in the list."
  (set-cdr! segments (cons (make-time-segment time callback) (cdr segments))))

(define (first-agenda-item agenda)
  "Returns the first time segment queue in the agenda."
  (if (agenda-empty? agenda)
      (error "Agenda is empty")
      (segment-queue (first-segment agenda))))

(define (agenda-time-delay agenda dt)
  "Returns time given a delta from the current agenda time."
  (+ (agenda-time agenda) (inexact->exact (round dt))))

(define (%agenda-schedule agenda callback dt)
  "Schedules a callback procedure in the agenda relative to the
current agenda time."
  (let ((time (agenda-time-delay agenda dt)))
    (define (belongs-before? segments)
      "Determines if the time segment belongs before the first segment
in the list"
      (or (null? segments)
          (< time (segment-time (car segments)))))

    (define (add-to-segments segments)
      "Schedules callback in the proper place."
      ;; Add to existing time segment if the times match
      (if (= (segment-time (car segments)) time)
          (segment-enq (car segments) callback)
          ;; Continue searching
          (if (belongs-before? (cdr segments))
              ;; Create new time segment and insert it where it belongs
              (insert-segment segments time callback)
              ;; Continue searching
              (add-to-segments (cdr segments)))))

    ;; Handle the case of inserting a new time segment at the
    ;; beginning of the segment list.
    (if (belongs-before? (agenda-segments agenda))
        ;; Add segment if it belongs at the beginning of the list...
        (agenda-add-segment agenda time callback)
        ;; ... Otherwise, search for the right place
        (add-to-segments (agenda-segments agenda)))))

(define (flush-queue! q)
  "Dequeues and executes every element of q."
  (unless (q-empty? q)
    ((deq! q)) ;; Execute scheduled procedure
    (flush-queue! q)))

(define (%update-agenda agenda)
  "Moves agenda forward in time and run scheduled procedures."
  (set-agenda-time! agenda (1+ (agenda-time agenda)))
  (let next-segment ()
    (unless (agenda-empty? agenda)
      (let ((segment (first-segment agenda)))
        ;; Process time segment if it is scheduled before or at the
        ;; current agenda time.
        (when (>= (agenda-time agenda) (segment-time segment))
          (flush-queue! (segment-queue segment))
          (set-agenda-segments! agenda (rest-segments agenda))
          (next-segment))))))

(define (%clear-agenda agenda)
  "Removes all scheduled procedures from the agenda."
  (set-agenda-segments! agenda '()))

(define* (agenda-schedule thunk #:optional (delay 1))
  "Schedules THUNK in the current agenda to run after DELAY
updates (1 by default)."
  (%agenda-schedule *current-agenda* thunk delay))

(define* (agenda-schedule-interval thunk #:optional (interval 1) (delay 1))
  "Schedules THUNK in the current agenda to run after DELAY updates
and run every INTERVAL updates thereafter. Both DELAY and INTERVAL
default to 1. Simply pass THUNK and nothing else to schedule THUNK to
be run upon every update."
  (%agenda-schedule *current-agenda*
                    (lambda ()
                      (thunk)
                      (agenda-schedule-interval thunk interval interval))
                    delay))

(define (update-agenda)
  "Updates the current agenda."
  (%update-agenda *current-agenda*))

(define (clear-agenda)
  "Clears the current agenda."
  (%clear-agenda *current-agenda*))
