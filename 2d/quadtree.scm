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
;; A quadtree is a spatial partitioning method for performing fast 2D
;; collision detection with axis aligned bounding boxes.
;;
;;; Code:

(define-module (2d quadtree)
  #:use-module (srfi srfi-9)
  #:use-module (2d rect)
  #:use-module (2d vector2)
  #:export (<quadtree>
            make-quadtree
            quadtree?
            quadtree-leaf?
            quadtree-bounds
            quadtree-capacity
            quadtree-items
            quadtree-size
            quadtree-northwest
            quadtree-northeast
            quadtree-southwest
            quadtree-southeast
            quadtree-insert!
            quadtree-remove!
            quadtree-clear!
            quadtree-query))

(define-record-type <quadtree>
  (%make-quadtree bounds max-size max-depth level items size
                  northwest northeast southwest southeast)
  quadtree?
  (bounds quadtree-bounds)
  (max-size quadtree-max-size)
  (max-depth quadtree-max-depth)
  (level quadtree-level)
  (items quadtree-items set-quadtree-items!)
  (size quadtree-size set-quadtree-size!)
  (northwest quadtree-northwest set-quadtree-northwest!)
  (northeast quadtree-northeast set-quadtree-northeast!)
  (southwest quadtree-southwest set-quadtree-southwest!)
  (southeast quadtree-southeast set-quadtree-southeast!))

(define* (make-quadtree bounds #:optional (level 1) (max-size 4) (max-depth 6))
  (%make-quadtree bounds max-size max-depth level '() 0 #f #f #f #f))

(define (subdivide! quadtree)
  (let ((bounds (quadtree-bounds quadtree))
        (capacity (quadtree-capacity quadtree)))
    (define (set-child! child x y)
      (let ((rect (make-rect x y
                             (rect-half-width bounds)
                             (rect-half-height bounds))))
        (child quadtree (make-quadtree rect ))))

    (set-child! set-quadtree-northwest!
                (rect-x bounds)
                (rect-y bounds))
    (set-child! set-quadtree-northeast!
                (rect-center-x bounds)
                (rect-y bounds))
    (set-child! set-quadtree-southwest!
                (rect-x bounds)
                (rect-center-y bounds))
    (set-child! set-quadtree-southeast!
                (rect-center-x bounds)
                (rect-center-y bounds))))

(define (quadtree-leaf? quadtree)
  "Return #t when quadtree is a leaf node."
  (if (quadtree-northwest quadtree) #f #t))

(define (quadtree-insert! quadtree position object)
  "Insert OBJECT into QUADTREE, using the procedure POSITION to get
the location."
  (define (insert-child! child)
    (quadtree-insert! (child quadtree) position object))

  (let ((point (position object)))
    (cond ((not (rect-contains? (quadtree-bounds quadtree) point))
           #f)
          ((< (quadtree-size quadtree)
              (quadtree-capacity quadtree))
           (set-quadtree-items!
            quadtree (cons (cons point object)
                           (quadtree-items quadtree)))
           (set-quadtree-size!
            quadtree (1+ (quadtree-size quadtree)))
           #t)
          (else
           (when (quadtree-leaf? quadtree)
             (subdivide! quadtree))
           (or (insert-child! quadtree-northwest)
               (insert-child! quadtree-northeast)
               (insert-child! quadtree-southwest)
               (insert-child! quadtree-southeast))))))

(define (quadtree-remove! quadtree position object)
  "Remove OBJECT from QUADTREE, using the procedure POSITION to get
the location."
  '())

(define (quadtree-clear! quadtree)
  (set-quadtree-items! quadtree '())
  (set-quadtree-size! quadtree 0)
  (set-quadtree-northwest! quadtree #f)
  (set-quadtree-northeast! quadtree #f)
  (set-quadtree-southwest! quadtree #f)
  (set-quadtree-southeast! quadtree #f))

(define (quadtree-query quadtree rect)
  "Return a list of objects that are located within RECT."
  '())
