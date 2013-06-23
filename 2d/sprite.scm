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
  #:use-module (2d vector)
  #:export (make-sprite
            sprite?
            sprite-texture
            sprite-position
            set-sprite-position!
            sprite-scale
            set-sprite-scale!
            sprite-rotation
            set-sprite-rotation!
            set-sprite-scale!
            load-sprite
            draw-sprite))

;; The <sprite> object represents a texture with a given position, scale,
;; rotation, and color.
(define-record-type <sprite>
  (%make-sprite texture position scale rotation color)
  sprite?
  (texture sprite-texture)
  (position sprite-position set-sprite-position!)
  (scale sprite-scale set-sprite-scale!)
  (rotation sprite-rotation set-sprite-rotation!)
  (color sprite-color set-sprite-color!))

(define* (make-sprite texture #:optional #:key (position #(0 0))
                      (scale #(1 1)) (rotation 0) (color '(1 1 1)))
  "Makes a new sprite object."
  (%make-sprite texture position scale rotation color))

(define* (load-sprite filename #:optional #:key (position #(0 0))
                      (scale #(1 1)) (rotation 0) (color '(1 1 1)))
  "Loads a sprite from file."
  (make-sprite (load-texture filename) #:position position #:scale scale
               #:rotation rotation #:color color))

(define (draw-sprite sprite)
  "Renders a sprite."
  (let* ((texture (sprite-texture sprite))
         (width (texture-width texture))
         (height (texture-height texture))
         (pos (sprite-position sprite))
         (scale (sprite-scale sprite)))
    (with-gl-push-matrix
      (gl-translate (vx pos) (vy pos) 0)
      (gl-rotate (sprite-rotation sprite) 0 0 1)
      (gl-scale (vx scale) (vy scale) 0)
      ;; Render a textured quad center on the sprite position.
      (texture-quad texture
                    (- (/ width 2)) (- (/ height 2))
                    width height
                    (sprite-color sprite)))))
