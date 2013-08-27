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
;; 2D vector math operations.
;;
;;; Code:

(define-module (2d vector2)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (<vector2>
            vector2
            vector2?
            vx
            vy
            null-vector2
            identity-vector2
            vector2-polar
            v+
            v*
            vscale
            vmag
            vnorm
            vdot
            vcross))

(define-record-type <vector2>
  (vector2 x y)
  vector2?
  (x vx)
  (y vy))

(define null-vector2 (vector2 0 0))
(define identity-vector2 (vector2 1 1))

(define (vector2-polar r theta)
  "Converts polar coordinates into a cartesian vector."
  (vector2 (* r (cos theta))
           (* r (sin theta))))

(define (v+ . vectors)
  "Adds vectors."
  (define (add-vectors x y vectors)
    (cond ((null? vectors)
           (vector2 x y))
          (else
           (add-vectors (+ x (vx (car vectors)))
                        (+ y (vy (car vectors)))
                        (cdr vectors)))))
  (add-vectors 0 0 vectors))

(define (v* . vectors)
  "Multiplies vectors."
  (define (multiply-vectors x y vectors)
    (cond ((null? vectors)
           (vector2 x y))
          (else
           (multiply-vectors (* x (vx (car vectors)))
                             (* y (vy (car vectors)))
                             (cdr vectors)))))
  (multiply-vectors 1 1 vectors))

(define (vscale v scalar)
  "Multiply vector by a scalar."
  (vector2 (* (vx v) scalar)
           (* (vy v) scalar)))

(define (vmag v)
  "Returns the magnitude of a vector"
  (sqrt (+ (expt (vx v) 2)
           (expt (vy v) 2))))

(define (vnorm v)
  "Normalizes a vector."
  (let ((m (vmag v)))
    (if (zero? m)
        null-vector2
        (vector2 (/ (vx v) m)
                 (/ (vy v) m)))))

(define (vdot v1 v2)
  "Returns the dot product of two vectors."
  (+ (* (vx v1) (vx v2))
     (* (vy v1) (vy v2))))

(define (vcross v1 v2)
  "Returns the cross product of two vectors. The cross product of a 2D
vector is not defined. This function instead returns the Z coordinate
of the cross product as if the vectors were in 3D space."
  (- (* (vx v1) (vy v2))
     (* (vy v1) (vx v2))))
