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
  #:use-module (2d coroutine)
  #:export (lerp))

(define (lerp proc start end duration)
  "Linearly interpolate a number from start to end. Calls proc with the
interpolated value every frame."
  (let ((delta (- end start)))
    (define (lerp-iter time)
      (if (< time duration)
          (begin
            (proc (+ start (* delta (/ time duration))))
            (wait)
            (lerp-iter (1+ time)))
          (proc end)))
    (lerp-iter 0)))
