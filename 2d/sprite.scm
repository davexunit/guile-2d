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
;; 2D sprite procedures.
;;
;;; Code:

(define-module (2d sprite)
  #:use-module (srfi srfi-9)
  #:use-module (figl gl)
  #:use-module (2d texture)
  #:export (make-sprite
            sprite?
            sprite-texture
            sprite-x
            set-sprite-x!
            sprite-y
            set-sprite-y!
            sprite-scale-x
            set-sprite-scale-x!
            sprite-scale-y
            set-sprite-scale-y!
            sprite-rotation
            set-sprite-rotation!
            set-sprite-scale!
            load-sprite
            draw-sprite))

;; The <sprite> object represents a texture with a given position, scale,
;; rotation, and color.
(define-record-type <sprite>
  (%make-sprite texture x y scale-x scale-y rotation color)
  sprite?
  (texture sprite-texture)
  (x sprite-x set-sprite-x!)
  (y sprite-y set-sprite-y!)
  (scale-x sprite-scale-x set-sprite-scale-x!)
  (scale-y sprite-scale-y set-sprite-scale-y!)
  (rotation sprite-rotation set-sprite-rotation!)
  (color sprite-color set-sprite-color!))

(define* (make-sprite texture #:optional (x 0) (y 0) (scale-x 1) (scale-y 1)
                      (rotation 0) (color '(1 1 1)))
  (%make-sprite texture x y scale-x scale-y rotation color))

(define (set-sprite-scale! sprite scale)
  "Sets sprite scale-x and scale-y to the same value."
  (set-sprite-scale-x! sprite scale)
  (set-sprite-scale-y! sprite scale))

(define (load-sprite filename)
  "Loads a sprite from file with default position, scaling, and rotation values."
  (make-sprite (load-texture filename)))

(define (draw-sprite sprite)
  "Renders a sprite."
  (let* ((texture (sprite-texture sprite))
         (width (texture-width texture))
         (height (texture-height texture)))
    (with-gl-push-matrix
      (gl-translate (sprite-x sprite) (sprite-y sprite) 0)
      (gl-rotate (sprite-rotation sprite) 0 0 1)
      (gl-scale (sprite-scale-x sprite) (sprite-scale-y sprite) 0)
      ;; Render a textured quad center on the sprite position.
      (texture-quad texture
                    (- (/ width 2)) (- (/ height 2))
                    width height
                    (sprite-color sprite)))))
