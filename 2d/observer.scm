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
;; Event listener.
;;
;;; Code:

(define-module (2d observer)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:export (<observer>
            make-observer
            alist->observer
            observer?
            observer-events
            observer-callbacks
            observer-on
            observer-off
            observer-clear
            observer-trigger))

(define-record-type <observer>
  (%make-observer events)
  observer?
  (events observer-events))

(define (make-observer)
  "Create a new observer."
  (%make-observer (make-hash-table)))

(define (alist->observer alst)
  "Return a new observer that registers the callbacks for events in
the alist ALST. Each pair in ALST should map one event type to one
callback procedure. For multiple event handlers of the same type, use
multiple pairs."
  (let ((observer (make-observer)))
    (for-each (lambda (e) (observer-on observer (car e) (cdr e))) alst)
    observer))

(define (observer-callbacks observer event-type)
  "Return a list of callback procedures for the given EVENT-TYPE. The
null list is returned if there are no callbacks for EVENT-TYPE."
  (or (hash-ref (observer-events observer) event-type)
      '()))

(define (observer-on observer event-type proc)
  "Register PROC as a callback for the given EVENT-TYPE."
  (hash-set! (observer-events observer)
             event-type
             (cons proc (observer-callbacks observer event-type))))

(define (observer-off observer event-type proc)
  "Unregister PROC as a callabck for the given EVENT-TYPE."
  (hash-set! (observer-events observer)
             event-type
             (delete proc (observer-callbacks observer event-type))))

(define (observer-clear observer event-type)
  "Unregister all callbacks for EVENT-TYPE."
  (hash-remove! (observer-events observer) event-type))

(define (observer-trigger observer event-type . args)
  "Call all callbacks for EVENT-TYPE with the given ARGS."
  (for-each (cut apply <> args)
            (observer-callbacks observer event-type)))
