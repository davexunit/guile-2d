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
;; Sprites are typically the most important part of a 2D game. This
;; module provides sprites as an abstraction around OpenGL textures.
;;
;;; Code:

(define-module (2d sprite)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (figl gl)
  #:use-module (figl contrib packed-struct)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (2d animation)
  #:use-module (2d helpers)
  #:use-module (2d texture)
  #:use-module (2d gl)
  #:use-module (2d vector))

;;;
;;; Sprites
;;;

;; Used to build OpenGL vertex array for a sprite.
(define-packed-struct sprite-vertex
  ;; Position
  (x float)
  (y float)
  ;; Color
  (r float)
  (g float)
  (b float)
  (a float)
  ;; Texture Coordinates
  (s float)
  (t float))

;; The <sprite> type represents a drawable object (texture,
;; texture-region, animation, etc.) with a given position, scale,
;; rotation, and color.
(define-record-type <sprite>
  (%make-sprite drawable position scale rotation color anchor vertices animation-state)
  sprite?
  (drawable sprite-drawable set-sprite-drawable!)
  (position sprite-position set-sprite-position!)
  (scale sprite-scale set-sprite-scale!)
  (rotation sprite-rotation set-sprite-rotation!)
  (color sprite-color set-sprite-color!)
  (anchor sprite-anchor set-sprite-anchor!)
  (vertices sprite-vertices set-sprite-vertices!)
  (animation-state sprite-animation-state set-sprite-animation-state!))

(define* (make-sprite drawable #:optional #:key (position #(0 0)) (scale #(1 1))
                      (rotation 0) (color #xffffffff) (anchor 'center))
  "Makes a new sprite object."
  (let ((vertices (make-packed-array sprite-vertex 4))
        (animation-state (if (animation? drawable)
                             (make-animation-state drawable)
                             #f)))
    (%make-sprite drawable position scale rotation color anchor vertices animation-state)))

(define* (load-sprite filename #:optional #:key (position #(0 0)) (scale #(1 1))
                      (rotation 0) (color #xffffffff) (anchor 'center))
  "Loads a sprite from file."
  (make-sprite (load-texture filename) #:position position #:scale scale
               #:rotation rotation #:color color #:anchor anchor))

(define (sprite-animation-frame sprite)
  (animation-state-frame (sprite-animation-state sprite)))

(define (get-texture drawable)
  (cond ((texture? drawable)
         drawable)
        ((texture-region? drawable)
         (texture-region-texture drawable))))

(define (sprite-texture sprite)
  "Returns the texture for the sprite's drawable object."
    (let ((drawable (sprite-drawable sprite)))
      (cond ((or (texture? drawable)
                 (texture-region? drawable))
             (get-texture drawable))
            ((animation? drawable)
             (get-texture (sprite-animation-frame sprite))))))

(define (drawable-texture-coords drawable)
  (cond ((texture? drawable)
         '(0 0 1 1))
        ((texture-region? drawable)
         (list (texture-region-u drawable)
               (texture-region-v drawable)
               (texture-region-u2 drawable)
               (texture-region-v2 drawable)))))

(define (sprite-texture-coords sprite)
  "Returns the texture coordinates for the drawable object."
  (let ((drawable (sprite-drawable sprite)))
    (cond ((or (texture? drawable)
               (texture-region? drawable))
           (drawable-texture-coords drawable))
          ((animation? drawable)
           (drawable-texture-coords (sprite-animation-frame sprite))))))

(define (drawable-size drawable)
  (cond ((texture? drawable)
         (vector (texture-width drawable)
                 (texture-height drawable)))
        ((texture-region? drawable)
         (vector (texture-region-width drawable)
                 (texture-region-height drawable)))))

(define (sprite-drawable-size sprite)
  "Returns the size of the sprite drawable as a vector"
  (let ((drawable (sprite-drawable sprite)))
    (cond ((or (texture? drawable)
               (texture-region? drawable))
           (drawable-size drawable))
          ((animation? drawable)
           (drawable-size (sprite-animation-frame sprite))))))

(define (sprite-anchor-vector sprite)
  "Returns a vector of the coordinates for the center point of a
sprite."
  (let ((anchor (sprite-anchor sprite)))
    (cond
     ((eq? anchor 'center)
      (let ((size (sprite-drawable-size sprite)))
        (vector (/ (vx size) 2)
                (/ (vy size) 2))))
     (else anchor))))

(define (update-sprite-vertices sprite)
  "Rebuilds the internal vertex array."
  (let* ((vertices (sprite-vertices sprite))
         (texture (sprite-texture sprite))
         (size (sprite-drawable-size sprite))
         (color (rgba->gl-color (sprite-color sprite)))
         (anchor (sprite-anchor-vector sprite))
         (tex-coords (sprite-texture-coords sprite))
         (x (- (vx anchor)))
         (y (- (vy anchor)))
         (x2 (+ x (vx size)))
         (y2 (+ y (vy size)))
         (u (first tex-coords))
         (v (second tex-coords))
         (u2 (third tex-coords))
         (v2 (fourth tex-coords))
         (r (vector-ref color 0))
         (g (vector-ref color 1))
         (b (vector-ref color 2))
         (a (vector-ref color 3)))
    (pack vertices 0 sprite-vertex
          x y
          r g b a
          u v)
    (pack vertices 1 sprite-vertex
          x2 y
          r g b a
          u2 v)
    (pack vertices 2 sprite-vertex
          x2 y2
          r g b a
          u2 v2)
    (pack vertices 3 sprite-vertex
          x y2
          r g b a
          u v2)))

(define (draw-sprite sprite)
  "Renders a sprite. A sprite batch will be used if one is currently
bound."
  (when (animation? (sprite-drawable sprite))
    (let ((state (tick-animation-state (sprite-animation-state sprite))))
      (set-sprite-animation-state! sprite state)))

  (if *sprite-batch*
      (draw-sprite-batched sprite)
      (draw-sprite-vertex-array sprite)))

(define (draw-sprite-batched sprite)
  "Adds a sprite to the batch."
  (let ((texture (sprite-texture sprite))
        (pos (sprite-position sprite))
        (size (sprite-drawable-size sprite))
        (scale (sprite-scale sprite))
        (anchor (sprite-anchor-vector sprite))
        (tex-coords (sprite-texture-coords sprite)))
    (%sprite-batch-draw *sprite-batch*
                        texture
                        (- (vx pos) (vx anchor))
                        (- (vy pos) (vy anchor))
                        (vx size)
                        (vy size)
                        #:color (sprite-color sprite)
                        #:rotation (sprite-rotation sprite)
                        #:scale-x (vx scale)
                        #:scale-y (vy scale)
                        #:u (first tex-coords)
                        #:v (second tex-coords)
                        #:u2 (third tex-coords)
                        #:v2 (fourth tex-coords))))

(define (draw-sprite-vertex-array sprite)
  "Renders a sprite using its internal vertex array."
  (update-sprite-vertices sprite)
  (let* ((texture (sprite-texture sprite))
         (pos (sprite-position sprite))
         (scale (sprite-scale sprite))
         (vertices (sprite-vertices sprite))
         (struct-size (packed-struct-size sprite-vertex))
         (x-offset (packed-struct-offset sprite-vertex x))
         (r-offset (packed-struct-offset sprite-vertex r))
         (s-offset (packed-struct-offset sprite-vertex s))
         (pointer-type (tex-coord-pointer-type float)))
    (with-gl-push-matrix
      (gl-translate (vx pos) (vy pos) 0)
      (gl-rotate (sprite-rotation sprite) 0 0 1)
      (gl-scale (vx scale) (vy scale) 0)
      ;; Draw vertex array
      (gl-enable-client-state (enable-cap vertex-array))
      (gl-enable-client-state (enable-cap color-array))
      (gl-enable-client-state (enable-cap texture-coord-array))
      (with-gl-bind-texture (texture-target texture-2d) (texture-id texture)
        (set-gl-vertex-array pointer-type
                             vertices
                             2
                             #:stride struct-size
                             #:offset x-offset)
        (set-gl-color-array pointer-type
                            vertices
                            4
                            #:stride struct-size
                            #:offset r-offset)
        (set-gl-texture-coordinates-array pointer-type
                                          vertices
                                          #:stride struct-size
                                          #:offset s-offset)
        (gl-draw-arrays (primitive-type quads)
                        0
                        (packed-array-length vertices sprite-vertex)))
      (gl-disable-client-state (enable-cap texture-coord-array))
      (gl-disable-client-state (enable-cap color-array))
      (gl-disable-client-state (enable-cap vertex-array)))))

(export make-sprite
        sprite?
        sprite-drawable
        set-sprite-drawable!
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
        draw-sprite)

;;;
;;; Sprite batches
;;;

;; Sprite batches allow for efficient texture rendering. Sprites drawn
;; with the same texture are drawn in the same draw call using a
;; vertex array, rather than re-binding the texture for each
;; individual draw call.
(define-record-type <sprite-batch>
  (%make-sprite-batch max-size size texture vertices)
  sprite-batch?
  (max-size sprite-batch-max-size)
  (size sprite-batch-size set-sprite-batch-size!)
  (texture sprite-batch-texture set-sprite-batch-texture!)
  (vertices sprite-batch-vertices))

;; Dynamic state for the current sprite batch.
(define *sprite-batch* #f)

(define* (make-sprite-batch #:optional (max-size 1000))
  "Creates a new sprite batch. The default max-size is 1000."
  (%make-sprite-batch max-size 0 #f (make-packed-array sprite-vertex (* 4 max-size))))

(define (sprite-batch-draw . args)
  "Adds a textured quad to the sprite batch."
  (apply %sprite-batch-draw *sprite-batch* args))

(define* (%sprite-batch-draw batch texture x y width height
                            #:optional #:key
                            (scale-x 1) (scale-y 1) (rotation 0)
                            (u 0) (v 0) (u2 1) (v2 1)
                            (color #xffffffff))
  "Adds a textured quad to the sprite batch."
  ;; Render the batch when it's full or the texture changes.
  (cond ((= (sprite-batch-size batch) (sprite-batch-max-size batch))
         (sprite-batch-render batch))
        ((not (equal? texture (sprite-batch-texture batch)))
         (sprite-batch-switch-texture batch texture)))
  ;; Add 4 new vertices.
  (let* ((base (* 4 (sprite-batch-size batch)))
         (vertices (sprite-batch-vertices batch))
         (color (rgba->gl-color color))
         (x2 (+ x width))
         (y2 (+ y height))
         (r (vector-ref color 0))
         (g (vector-ref color 1))
         (b (vector-ref color 2))
         (a (vector-ref color 3)))
    (pack vertices base sprite-vertex
          x y
          r g b a
          u v)
    (pack vertices (+ base 1) sprite-vertex
          x2 y
          r g b a
          u2 v)
    (pack vertices (+ base 2) sprite-vertex
          x2 y2
          r g b a
          u2 v2)
    (pack vertices (+ base 3) sprite-vertex
          x y2
          r g b a
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
        (gl-draw-arrays (primitive-type quads) 0 vertex-count)))
    (gl-disable-client-state (enable-cap texture-coord-array))
    (gl-disable-client-state (enable-cap color-array))
    (gl-disable-client-state (enable-cap vertex-array))
    ;; Reset batch size to 0.
    (set-sprite-batch-size! batch 0)))

;; emacs: (put 'with-sprite-batch 'scheme-indent-function 1)
(define-syntax-rule (with-sprite-batch batch body ...)
  (begin
    (set! *sprite-batch* batch)
    (set-sprite-batch-size! batch 0)
    (set-sprite-batch-texture! batch #f)
    body
    ...
    (sprite-batch-render batch)
    (set! *sprite-batch* #f)))

(export make-sprite-batch
        sprite-batch?
        sprite-batch-max-size
        sprite-batch-size
        set-sprite-batch-size!
        sprite-batch-texture
        set-sprite-batch-texture!
        sprite-batch-vertices
        sprite-batch-draw
        with-sprite-batch)
