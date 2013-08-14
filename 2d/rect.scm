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
  #:export (<rect>
            make-rect
            rect?
            rect-x
            rect-y
            rect-x2
            rect-y2
            rect-width
            rect-height
            rect-move
            rect-inflate
            rect-union
            rect-clip
            rect-within?
            rect-intesects?
            rect-contains?))

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

(define (rect-move rect x y)
  "Moves a rect by the given offset."
  (make-rect (+ (rect-x rect) x)
             (+ (rect-y rect) y)
             (rect-width rect)
             (rect-height rect)))

(define (rect-inflate rect width height)
  "Grows the rect by the given amount. The rect stays centered around
its current center."
  (make-rect (+ (rect-x rect) (/ width 2))
             (+ (rect-y rect) (/ height 2))
             (+ (rect-width rect) width)
             (+ (rect-height rect) height)))

(define (rect-union rect1 rect2)
  "Returns a rect that covers the area of rect1 and rect2."
  (let ((x1 (min (rect-x  rect1) (rect-x  rect2)))
        (x2 (max (rect-x2 rect1) (rect-x2 rect2)))
        (y1 (min (rect-y  rect1) (rect-y  rect2)))
        (y2 (max (rect-y2 rect1) (rect-y2 rect2))))
    (make-rect x1 y1 (- x2 x1) (- y2 y1))))

(define (rect-clip rect1 rect2)
  "Returns the overlapping region of rect1 and rect2. If the rects do
not overlap, a rect of size 0 is returned."
  (let ((x1 (max (rect-x  rect1) (rect-x  rect2)))
        (x2 (min (rect-x2 rect1) (rect-x2 rect2)))
        (y1 (max (rect-y  rect1) (rect-y  rect2)))
        (y2 (min (rect-y2 rect1) (rect-y2 rect2))))
    (make-rect x1 y1 (max (- x2 x1) 0) (max (- y2 y1) 0))))

(define (rect-within? rect1 rect2)
  "Tests if rect2 is completely within rect1."
  (and (>= (rect-x  rect2) (rect-x  rect1))
       (<= (rect-x  rect2) (rect-x2 rect1))
       (>= (rect-x2 rect2) (rect-x  rect1))
       (<= (rect-x2 rect2) (rect-x2 rect1))
       (>= (rect-y  rect2) (rect-y  rect1))
       (<= (rect-y  rect2) (rect-y2 rect1))
       (>= (rect-y2 rect2) (rect-y  rect1))
       (<= (rect-y2 rect2) (rect-y2 rect1))))

(define (rect-intersects? rect1 rect2)
  "Tests if rect2 overlaps rect1."
  (or (and (>= (rect-x  rect2) (rect-x  rect1))
           (<= (rect-x  rect2) (rect-x2 rect1)))
      (and (>= (rect-x2 rect2) (rect-x  rect1))
           (<= (rect-x2 rect2) (rect-x2 rect1))))
  (or (and (>= (rect-y  rect2) (rect-y  rect1))
           (<= (rect-y  rect2) (rect-y2 rect1)))
      (and (>= (rect-y2 rect2) (rect-y  rect1))
           (<= (rect-y2 rect2) (rect-y2 rect1)))))

(define (rect-contains? rect x y)
  "Tests if the given point is within rect."
  (and (>= x (rect-x  rect))
       (<= x (rect-x2 rect))
       (>= y (rect-y  rect))
       (<= y (rect-y2 rect))))
