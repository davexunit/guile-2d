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
;; Vector math operations.
;;
;;; Code:

(define-module (2d vector)
  #:use-module (rnrs base)
  #:export (v+
            v*
            scale
            mag
            normalize
            vector-reduce))

(define (v+ . vectors)
  "Adds vectors."
  (apply vector-map + vectors))

(define (v* . vectors)
  "Multiplies vectors."
  (apply vector-map * vectors))

(define (scale vector scalar)
  "Multiplies a vector by a scalar."
  (vector-map (lambda (e) (* scalar e)) vector))

(define (mag vector)
  "Returns the magnitude of a vector."
  (sqrt (vector-reduce + 0 (vector-map (lambda (e) (* e e)) vector))))

(define (normalize vector)
  "Normalizes a vector."
  (let ((m (magnitude vector)))
    (vector-map (lambda (e) (/ e m)) vector)))

(define (vector-reduce proc default vector)
  "Performs a reduce operation on a vector."
  (cond ((= (vector-length vector) 0)
         default)
        ((= (vector-length vector) 1)
         (vector-ref vector 0))
        (else
         (let loop ((i 2)
                    (prev (proc (vector-ref vector 0)
                                (vector-ref vector 1))))
           (if (= (vector-length vector) i)
               prev
               (loop (1+ i) (proc prev (vector-ref vector i))))))))
