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
  #:use-module (figl contrib packed-struct)
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
            sprite-color
            set-sprite-color!
            sprite-anchor
            set-sprite-anchor!
            sprite-vertices
            set-sprite-vertices!
            load-sprite
            draw-sprite
            make-sprite-batch
            sprite-batch?
            sprite-batch-max-size
            sprite-batch-size
            set-sprite-batch-size!
            sprite-batch-texture
            set-sprite-batch-texture!
            sprite-batch-vertices
            sprite-batch-draw
            with-sprite-batch))

;; Used to build OpenGL vertex array for a sprite.
(define-packed-struct sprite-vertex
  (x float)
  (y float)

  (r float)
  (g float)
  (b float)
  (a float)

  (s float)
  (t float))

;; The <sprite> object represents a texture with a given position, scale,
;; rotation, and color.
(define-record-type <sprite>
  (%make-sprite texture position scale rotation color anchor vertices)
  sprite?
  (texture sprite-texture)
  (position sprite-position set-sprite-position!)
  (scale sprite-scale set-sprite-scale!)
  (rotation sprite-rotation set-sprite-rotation!)
  (color sprite-color set-sprite-color!)
  (anchor sprite-anchor set-sprite-anchor!)
  (vertices sprite-vertices set-sprite-vertices!))

(define* (make-sprite texture #:optional #:key (position #(0 0)) (scale #(1 1))
                      (rotation 0) (color '(1 1 1)) (anchor 'center))
  "Makes a new sprite object."
  (let ((vertices (make-packed-array sprite-vertex 4)))
    (%make-sprite texture position scale rotation color anchor vertices)))

(define* (load-sprite filename #:optional #:key (position #(0 0)) (scale #(1 1))
                      (rotation 0) (color '(1 1 1)) (anchor 'center))
  "Loads a sprite from file."
  (make-sprite (load-texture filename) #:position position #:scale scale
               #:rotation rotation #:color color #:anchor anchor))

(define (sprite-anchor-vector sprite)
  (let ((anchor (sprite-anchor sprite)))
    (cond
     ((eq? anchor 'center)
      (let ((texture (sprite-texture sprite)))
        (vector (/ (texture-width texture) 2)
                (/ (texture-height texture) 2))))
     (else anchor))))

(define (update-sprite-vertices sprite)
  (let* ((vertices (sprite-vertices sprite))
         (texture (sprite-texture sprite))
         (anchor (sprite-anchor-vector sprite))
         (x (- (vx anchor)))
         (y (- (vy anchor)))
         (x2 (+ x (texture-width texture)))
         (y2 (+ y (texture-width texture))))
    (pack vertices 0 sprite-vertex
          x y
          1 1 1 1
          0 0)
    (pack vertices 1 sprite-vertex
          x2 y
          1 1 1 1
          1 0)
    (pack vertices 2 sprite-vertex
          x2 y2
          1 1 1 1
          1 1)
    (pack vertices 3 sprite-vertex
          x y2
          1 1 1 1
          0 1)))

(define (draw-sprite sprite)
  "Renders a sprite."
  (update-sprite-vertices sprite)
  (let* ((texture (sprite-texture sprite))
         (width (texture-width texture))
         (height (texture-height texture))
         (pos (sprite-position sprite))
         (scale (sprite-scale sprite))
         (vertices (sprite-vertices sprite)))
    (with-gl-push-matrix
      (gl-translate (vx pos) (vy pos) 0)
      (gl-rotate (sprite-rotation sprite) 0 0 1)
      (gl-scale (vx scale) (vy scale) 0)
      ;; Draw vertex array
      (gl-enable-client-state (enable-cap vertex-array))
      (gl-enable-client-state (enable-cap color-array))
      (gl-enable-client-state (enable-cap texture-coord-array))
      (with-gl-bind-texture (texture-target texture-2d) (texture-id texture)
        (set-gl-vertex-array (vertex-pointer-type float)
                             vertices
                             2
                             #:stride (packed-struct-size sprite-vertex)
                             #:offset (packed-struct-offset sprite-vertex x))
        (set-gl-color-array (color-pointer-type float)
                            vertices
                            4
                            #:stride (packed-struct-size sprite-vertex)
                            #:offset (packed-struct-offset sprite-vertex r))
        (set-gl-texture-coordinates-array (tex-coord-pointer-type float)
                                          vertices
                                          #:stride (packed-struct-size sprite-vertex)
                                          #:offset (packed-struct-offset sprite-vertex s))
        (gl-draw-arrays (begin-mode quads) 0 (packed-array-length vertices sprite-vertex)))
      (gl-disable-client-state (enable-cap texture-coord-array))
      (gl-disable-client-state (enable-cap color-array))
      (gl-disable-client-state (enable-cap vertex-array)))))

;;;
;;; Sprite Batch
;;;

;; Sprite batches allow for efficient texture rendering. Sprites drawn
;; with the same texture are drawn with the same draw call, rather
;; than binding the texture for each individual draw call.

(define-record-type <sprite-batch>
  (%make-sprite-batch max-size size texture vertices)
  sprite-batch?
  (max-size sprite-batch-max-size)
  (size sprite-batch-size set-sprite-batch-size!)
  (texture sprite-batch-texture set-sprite-batch-texture!)
  (vertices sprite-batch-vertices))

(define* (make-sprite-batch #:optional (max-size 1000))
  "Creates a new sprite batch. The default max-size is 1000."
  (%make-sprite-batch max-size 0 #f (make-packed-array sprite-vertex (* 4 max-size))))

;; TODO add transformation logic for scaling and rotating.
;; TODO add support for colors
;; TODO add support for different blending modes.
(define* (sprite-batch-draw batch texture x y center-x center-y
                            width height scale-x scale-y rotation
                            #:optional (u 0) (v 0) (u2 1) (v2 1))
  ;; Render the batch when it's full or the texture changes.
  (cond ((= (sprite-batch-size batch) (sprite-batch-max-size batch))
         (sprite-batch-render batch))
        ((not (equal? texture (sprite-batch-texture batch)))
         (sprite-batch-switch-texture batch texture)))
  ;; Add 4 new vertices.
  (let ((base (* 4 (sprite-batch-size batch)))
        (vertices (sprite-batch-vertices batch))
        (x (- x center-x))
        (y (- y center-y))
        (x2 (+ x width))
        (y2 (+ y height)))
    (pack vertices base sprite-vertex
          x y
          1 1 1 1
          u v)
    (pack vertices (+ base 1) sprite-vertex
          x2 y
          1 1 1 1
          u2 v)
    (pack vertices (+ base 2) sprite-vertex
          x2 y2
          1 1 1 1
          u2 v2)
    (pack vertices (+ base 3) sprite-vertex
          x y2
          1 1 1 1
          u v2))
  ;; Increment batch size
  (set-sprite-batch-size! batch (1+ (sprite-batch-size batch))))

(define (sprite-batch-switch-texture batch texture)
  "Change the currently bound texture. This requires flushing the
batched texture vertices first."
  (sprite-batch-render batch)
  (set-sprite-batch-texture! batch texture))

(define (sprite-batch-render batch)
  "Renders and flushes the currently batched texture vertices."
  (unless (= (sprite-batch-size batch) 0)
    ;; Draw vertex array.
    (gl-enable-client-state (enable-cap vertex-array))
    (gl-enable-client-state (enable-cap color-array))
    (gl-enable-client-state (enable-cap texture-coord-array))
    (let* ((texture (sprite-batch-texture batch))
           (size (sprite-batch-size batch))
           (struct-size (packed-struct-size sprite-vertex))
           (vertices (sprite-batch-vertices batch))
           (vertex-count (* 4 size)))
      (with-gl-bind-texture (texture-target texture-2d) (texture-id texture)
        (set-gl-vertex-array (vertex-pointer-type float)
                             vertices
                             2
                             #:stride struct-size
                             #:offset (packed-struct-offset sprite-vertex x))
        (set-gl-color-array (color-pointer-type float)
                            vertices
                            4
                            #:stride struct-size
                            #:offset (packed-struct-offset sprite-vertex r))
        (set-gl-texture-coordinates-array (tex-coord-pointer-type float)
                                          vertices
                                          #:stride struct-size
                                          #:offset (packed-struct-offset sprite-vertex s))
        (gl-draw-arrays (begin-mode quads) 0 vertex-count)))
    (gl-disable-client-state (enable-cap texture-coord-array))
    (gl-disable-client-state (enable-cap color-array))
    (gl-disable-client-state (enable-cap vertex-array))
    ;; Reset batch size to 0.
    (set-sprite-batch-size! batch 0)))

;; emacs: (put 'with-sprite-batch 'scheme-indent-function 1)
(define-syntax-rule (with-sprite-batch batch body ...)
  (begin
    (set-sprite-batch-size! batch 0)
    (set-sprite-batch-texture! batch #f)
    body
    ...
    (sprite-batch-render batch)))
