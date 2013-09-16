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
;; Miscellaneous helper procedures.
;;
;;; Code:

(define-module (2d helpers)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs arithmetic bitwise)
  #:export (any-equal?
            logand?
            rgba->gl-color))

(define (any-equal? elem . args)
  "Return #t if ELEM equals any of the elements in the list ARGS."
  (any (lambda (e) (equal? elem e)) args))

(define (logand? . args)
  "Return #t if the result of a bitwise AND of the integers in list
ARGS is non-zero."
  (not (zero? (apply logand args))))
