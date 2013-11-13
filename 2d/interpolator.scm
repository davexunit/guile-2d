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
;; Linear interpolation.
;;
;;; Code:

(define-module (2d interpolator)
  #:use-module (srfi srfi-9)
  #:export (<interpolator>
            make-interpolator
            interpolator?
            interpolator-setter
            interpolator-table
            interpolator-add
            interpolator-mul
            integrate!
            interpolate))

(define-record-type <interpolator>
  (%make-interpolator setter table add mul)
  interpolator?
  (setter interpolator-setter)
  (table interpolator-table)
  (add interpolator-add)
  (mul interpolator-mul))

(define* (make-interpolator setter #:optional (add +) (mul *))
  "Create a new interpolator that wraps the SETTER procedure. ADD and
MUL are optional procedures that can add and multiply the values
passed to SETTER. By default, they are * and +."
  (%make-interpolator setter (make-hash-table) add mul))

(define (integrate! interpolator object value)
  "Set the current VALUE of the interpolated procedure for OBJECT
within INTERPOLATOR."
  (let* ((table (interpolator-table interpolator))
         (handle (hashq-create-handle!
                  table object (cons value value))))
    ((interpolator-setter interpolator) object value)
    (set-cdr! handle (cons value (cadr handle)))))

(define (interpolate interpolator object alpha)
  "Retrieve the interpolated value of INTERPOLATOR for OBJECT with the
given blending factor ALPHA in the range [0, 1]."
  (let ((add (interpolator-add interpolator))
        (mul (interpolator-mul interpolator))
        (values (hashq-ref (interpolator-table interpolator) object)))
    (add (mul (car values) alpha)
         (mul (cdr values) (- 1 alpha)))))
