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
;; Rects are axis-aligned bounding boxes that can be used for
;; performing simple collision detection.
;;
;;; Code:

(define-module (2d rect)
  #:use-module (srfi srfi-9)
  #:use-module (2d vector2)
  #:export (<rect>
            make-rect
            rect?
            rect-x
            rect-y
            rect-x2
            rect-y2
            rect-center-x
            rect-center-y
            rect-half-width
            rect-half-height
            rect-width
            rect-height
            rect-position
            rect-size
            rect-move
            rect-inflate
            rect-union
            rect-clip
            rect-within?
            rect-intersects?
            rect-contains?))

;;;
;;; Rectangles
;;;

;; The rect API is very similar to the Pygame rect API, but rects are
;; immutable.

(define-record-type <rect>
  (make-rect x y width height)
  rect?
  (x rect-x)
  (y rect-y)
  (width rect-width)
  (height rect-height))

(define (rect-x2 rect)
  (+ (rect-x rect) (rect-width rect) -1))

(define (rect-y2 rect)
  (+ (rect-y rect) (rect-height rect) -1))

(define (rect-center-x rect)
  (+ (rect-x rect) (rect-half-width rect)))

(define (rect-center-y rect)
  (+ (rect-y rect) (rect-half-height rect)))

(define (rect-half-width rect)
  (/ (rect-width rect) 2))

(define (rect-half-height rect)
  (/ (rect-height rect) 2))

(define (rect-position rect)
  "Return the top-left corner of RECT as a vector2."
  (vector2 (rect-x rect)
           (rect-y rect)))

(define (rect-size rect)
  "Return the size of RECT as a vector2."
  (vector2 (rect-width rect)
           (rect-height rect)))

(define (%rect-move rect x y)
  "Move RECT by the offset X, Y."
  (make-rect (+ (rect-x rect) x)
             (+ (rect-y rect) y)
             (rect-width rect)
             (rect-height rect)))

(define rect-move
  (case-lambda
    "Create a new rectangle by moving RECT by the given
offset. rect-move accepts a vector2 or x and y coordinates as separate
arguments."
    ((rect v)
     (%rect-move rect (vx v) (vy v)))
    ((rect x y)
     (%rect-move rect x y))))

(define (%rect-inflate rect width height)
  "Grows the rect by the given amount. The rect stays centered around
its current center."
  (make-rect (+ (rect-x rect) (/ width 2))
             (+ (rect-y rect) (/ height 2))
             (+ (rect-width rect) width)
             (+ (rect-height rect) height)))

(define rect-inflate
  (case-lambda
    "Create a new rectangle by growing RECT by the given amount
without changing the center point. rect-inflate accepts a vector2 or x
and y coordinates as separate arguments."
    ((rect v)
     (%rect-inflate rect (vx v) (vy v)))
    ((rect x y)
     (%rect-inflate rect x y))))

(define (rect-union rect1 rect2)
  "Return a rect that covers the area of RECT1 and RECT2."
  (let ((x1 (min (rect-x  rect1) (rect-x  rect2)))
        (x2 (max (rect-x2 rect1) (rect-x2 rect2)))
        (y1 (min (rect-y  rect1) (rect-y  rect2)))
        (y2 (max (rect-y2 rect1) (rect-y2 rect2))))
    (make-rect x1 y1 (- x2 x1) (- y2 y1))))

(define (rect-clip rect1 rect2)
  "Return the overlapping region of RECT1 and RECT2. If the rects do
not overlap, a rect of size 0 is returned."
  (let ((x1 (max (rect-x  rect1) (rect-x  rect2)))
        (x2 (min (rect-x2 rect1) (rect-x2 rect2)))
        (y1 (max (rect-y  rect1) (rect-y  rect2)))
        (y2 (min (rect-y2 rect1) (rect-y2 rect2))))
    (make-rect x1 y1 (max (- x2 x1) 0) (max (- y2 y1) 0))))

(define (rect-within? rect1 rect2)
  "Return #t if RECT2 is completely within RECT1."
  (and (>= (rect-x  rect2) (rect-x  rect1))
       (<= (rect-x  rect2) (rect-x2 rect1))
       (>= (rect-x2 rect2) (rect-x  rect1))
       (<= (rect-x2 rect2) (rect-x2 rect1))
       (>= (rect-y  rect2) (rect-y  rect1))
       (<= (rect-y  rect2) (rect-y2 rect1))
       (>= (rect-y2 rect2) (rect-y  rect1))
       (<= (rect-y2 rect2) (rect-y2 rect1))))

(define (rect-intersects? rect1 rect2)
  "Return #t if RECT2 overlaps RECT1."
  (and (<= (rect-x  rect1) (rect-x2 rect2))
       (>= (rect-x2 rect1) (rect-x  rect2))
       (<= (rect-y  rect1) (rect-y2 rect2))
       (>= (rect-y2 rect1) (rect-y  rect2))))

(define (%rect-contains? rect x y)
  (and (>= x (rect-x  rect))
       (<= x (rect-x2 rect))
       (>= y (rect-y  rect))
       (<= y (rect-y2 rect))))

(define rect-contains?
  (case-lambda
    "Return #t if the given point is within RECT."
    ((rect v)
     (%rect-contains? rect (vx v) (vy v)))
    ((rect x y)
     (%rect-contains? rect x y))))
