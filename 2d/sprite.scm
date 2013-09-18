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
  #:use-module (2d agenda)
  #:use-module (2d animation)
  #:use-module (2d color)
  #:use-module (2d helpers)
  #:use-module (2d math)
  #:use-module (2d texture)
  #:use-module (2d vector2)
  #:use-module (2d wrappers gl))

;;;
;;; Sprite Vertices
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

(define sprite-vertex-size (packed-struct-size sprite-vertex))
(define x-offset (packed-struct-offset sprite-vertex x))
(define r-offset (packed-struct-offset sprite-vertex r))
(define s-offset (packed-struct-offset sprite-vertex s))

(define (pack-sprite-vertices vertices offset x y width height origin-x origin-y
                              scale-x scale-y rotation s1 t1 s2 t2 color)
  (define (pack-sprite x1 y1 x2 y2 x3 y3 x4 y4)
    (let ((r (color-r color))
          (g (color-g color))
          (b (color-b color))
          (a (color-a color)))
      ;; Vertices go counter clockwise, starting from the top-left
      ;; corner.
      (pack vertices offset sprite-vertex
            x1 y1
            r g b a
            s1 t1)
      (pack vertices (+ offset 1) sprite-vertex
            x2 y2
            r g b a
            s1 t2)
      (pack vertices (+ offset 2) sprite-vertex
            x3 y3
            r g b a
            s2 t2)
      (pack vertices (+ offset 3) sprite-vertex
            x4 y4
            r g b a
            s2 t1)))

  (let ((local-x1 (* (- origin-x) scale-x))
        (local-y1 (* (- origin-y) scale-y))
        (local-x2 (* (- width origin-x) scale-x))
        (local-y2 (* (- height origin-y) scale-y)))
    (if (= rotation 0)
        (begin
          (let ((x1 (+ x local-x1))
                (y1 (+ y local-y1))
                (x2 (+ x local-x2))
                (y2 (+ y local-y2)))
            (pack-sprite x1 y1 x1 y2 x2 y2 x2 y1)))
        (begin
          (let* ((sin (sin-degrees rotation))
                 (cos (cos-degrees rotation))
                 (x1 (+ x (- (* cos local-x1) (* sin local-y1))))
                 (y1 (+ y (* sin local-x1) (* cos local-y1)))
                 (x2 (+ x (- (* cos local-x1) (* sin local-y2))))
                 (y2 (+ y (* sin local-x1) (* cos local-y2)))
                 (x3 (+ x (- (* cos local-x2) (* sin local-y2))))
                 (y3 (+ y (* sin local-x2) (* cos local-y2)))
                 (x4 (+ x1 (- x3 x2)))
                 (y4 (- y3 (- y2 y1))))
            (pack-sprite x1 y1 x2 y2 x3 y3 x4 y4))))))

(define (draw-sprite-vertices texture vertices size)
  (let ((pointer-type (tex-coord-pointer-type float)))
    (gl-enable-client-state (enable-cap vertex-array))
    (gl-enable-client-state (enable-cap color-array))
    (gl-enable-client-state (enable-cap texture-coord-array))
    (with-gl-bind-texture (texture-target texture-2d) (texture-id texture)
      (set-gl-vertex-array pointer-type
                           vertices
                           2
                           #:stride sprite-vertex-size
                           #:offset x-offset)
      (set-gl-color-array pointer-type
                          vertices
                          4
                          #:stride sprite-vertex-size
                          #:offset r-offset)
      (set-gl-texture-coordinates-array pointer-type
                                        vertices
                                        #:stride sprite-vertex-size
                                        #:offset s-offset)
      (gl-draw-arrays (begin-mode quads) 0 (* size 4)))
    (gl-disable-client-state (enable-cap texture-coord-array))
    (gl-disable-client-state (enable-cap color-array))
    (gl-disable-client-state (enable-cap vertex-array))))

;;;
;;; Sprites
;;;

;; The <sprite> type represents a drawable object (texture,
;; texture-region, animation, etc.) with a given position, scale,
;; rotation, and color.
(define-record-type <sprite>
  (%make-sprite drawable position scale rotation color anchor
                vertices dirty animator)
  sprite?
  (drawable sprite-drawable set-sprite-drawable!)
  (position sprite-position %set-sprite-position!)
  (scale sprite-scale %set-sprite-scale!)
  (rotation sprite-rotation %set-sprite-rotation!)
  (color sprite-color %set-sprite-color!)
  (anchor sprite-anchor %set-sprite-anchor!)
  (vertices sprite-vertices set-sprite-vertices!)
  (dirty sprite-dirty? set-sprite-dirty!)
  (animator sprite-animator))

(define* (make-sprite drawable #:optional #:key
                      (position (vector2 0 0)) (scale (vector2 1 1))
                      (rotation 0) (color white) (anchor 'center))
  "Create a new sprite object. DRAWABLE is either a texture or
animation object. All keyword arguments are optional. POSITION is a
vector2 object with a default of (0, 0). SCALE is a vector2 object
that describes how much DRAWABLE should be strected on the x and y
axes, with a default of 1x scale. ROTATION is an angle in degrees with
a default of 0. COLOR is a color object with a default of
white. ANCHOR is either a vector2 that represents the center point of
the sprite, or 'center which will place the anchor at the center of
DRAWABLE. Sprites are centered by default."
  (let ((vertices (make-packed-array sprite-vertex 4))
        (animator (if (animation? drawable)
                      (make-animator drawable)
                      #f)))
    (%make-sprite drawable position scale rotation color anchor vertices
                  #t animator)))

(define-syntax-rule (dirty-sprite-setter setter private-setter)
  "Defines a setter that calls the private version of the given
procedure name (prefix with %) and marks the sprite as dirty. Any
operation that requires a refresh of the vertex array should use this macro."
  (define (setter sprite value)
    (private-setter sprite value)
    (set-sprite-dirty! sprite #t)))

(dirty-sprite-setter set-sprite-position! %set-sprite-position!)
(dirty-sprite-setter set-sprite-scale! %set-sprite-scale!)
(dirty-sprite-setter set-sprite-rotation! %set-sprite-rotation!)
(dirty-sprite-setter set-sprite-color! %set-sprite-color!)
(dirty-sprite-setter set-sprite-anchor! %set-sprite-anchor!)

(define* (load-sprite filename #:optional #:key
                      (position (vector2 0 0)) (scale (vector2 1 1))
                      (rotation 0) (color white) (anchor 'center))
  "Load a sprite from the file at FILENAME. See make-sprite for
optional keyword arguments."
  (make-sprite (load-texture filename) #:position position #:scale scale
               #:rotation rotation #:color color #:anchor anchor))

(define (animated-sprite? sprite)
  "Return #t if SPRITE has an animation as its drawable object."
  (animation? (sprite-drawable sprite)))

(define (sprite-animation-texture sprite)
  (animator-texture (sprite-animator sprite)))

(define (sprite-texture sprite)
  "Return the texture for the SPRITE's drawable object."
  (let ((drawable (sprite-drawable sprite)))
    (cond ((texture? drawable)
           drawable)
          ((animation? drawable)
           (sprite-animation-texture sprite)))))

(define (sprite-anchor-vector sprite)
  "Return a vector2 of the coordinates for the center point of a
sprite."
  (let ((anchor (sprite-anchor sprite))
        (texture (sprite-texture sprite)))
    (cond
     ((eq? anchor 'center)
      (vector2 (/ (texture-width texture) 2)
               (/ (texture-height texture) 2)))
     (else anchor))))

(define (update-sprite-vertices! sprite)
  "Rebuild the internal vertex array."
  (let ((pos (sprite-position sprite))
        (scale (sprite-scale sprite))
        (anchor (sprite-anchor-vector sprite))
        (texture (sprite-texture sprite)))
    (pack-sprite-vertices (sprite-vertices sprite)
                          0
                          (vx pos)
                          (vy pos)
                          (texture-width texture)
                          (texture-height texture)
                          (vx anchor)
                          (vy anchor)
                          (vx scale)
                          (vy scale)
                          (sprite-rotation sprite)
                          (texture-s1 texture)
                          (texture-t1 texture)
                          (texture-s2 texture)
                          (texture-t2 texture)
                          (sprite-color sprite))))

(define (update-sprite-animator! sprite)
  (animator-update! (sprite-animator sprite)))

(define (draw-sprite sprite)
  "Render SPRITE to the screen. A sprite batch will be used if one is
currently bound."
  (when (sprite-dirty? sprite)
    (update-sprite-vertices! sprite))
  (register-animated-sprite-maybe sprite)
  (if *sprite-batch*
      (draw-sprite-batched sprite)
      (draw-sprite-vertices (sprite-texture sprite)
                            (sprite-vertices sprite)
                            1)))

(define (draw-sprite-batched sprite)
  "Add SPRITE to the current sprite batch batch."
  (let ((texture (sprite-texture sprite))
        (pos (sprite-position sprite))
        (scale (sprite-scale sprite))
        (anchor (sprite-anchor-vector sprite)))
    (register-animated-sprite-maybe sprite)
    (%sprite-batch-draw *sprite-batch*
                        texture
                        (vx pos)
                        (vy pos)
                        (texture-width texture)
                        (texture-height texture)
                        (vx anchor)
                        (vy anchor)
                        (vx scale)
                        (vy scale)
                        (sprite-rotation sprite)
                        (texture-s1 texture)
                        (texture-t1 texture)
                        (texture-s2 texture)
                        (texture-t2 texture)
                        (sprite-color sprite))))

;; A hash table for all of the animated sprites that have been drawn
;; since the last game update. It is cleared after every game update.
(define animated-sprites (make-hash-table))

(define (register-animated-sprite-maybe sprite)
  (when (animated-sprite? sprite)
    (hash-set! animated-sprites sprite sprite)))

(define (update-animated-sprites!)
  "Update all animators for sprites that have been drawn this frame."
  (hash-for-each (lambda (key val)
                   (update-sprite-animator! val))
                 animated-sprites)
  (hash-clear! animated-sprites))

;; Update animated sprites upon every update.
(agenda-schedule-interval update-animated-sprites!)

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
        animated-sprite?
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
  (let ((vertex-array (make-packed-array sprite-vertex (* 4 max-size))))
    (%make-sprite-batch max-size 0 #f vertex-array)))

(define (sprite-batch-draw . args)
  "Add a textured quad to the current sprite batch. X, Y, WIDTH, and
HEIGHT represent the quad in pixels. ORIGIN-X and ORIGIN-Y represent
the center point of the quad which is used for rotation. SCALE-X and
SCALE-Y are the scaling factors for the x and y axis,
respectively. ROTATION is the angle in degrees to rotate the quad. U,
V, U2, and V2 represent the texture coordinate region to texture the
quad with. COLOR is a color object."
  (apply %sprite-batch-draw *sprite-batch* args))

(define* (%sprite-batch-draw batch texture x y width height origin-x origin-y
                             scale-x scale-y rotation u v u2 v2 color)
  "Add a textured quad to the sprite batch."
  ;; Render the batch when it's full or the texture changes.
  (cond ((= (sprite-batch-size batch) (sprite-batch-max-size batch))
         (sprite-batch-render batch))
        ((not (equal? texture (sprite-batch-texture batch)))
         (sprite-batch-switch-texture batch texture)))

  ;; Add 4 new vertices, taking into account scaling and rotation.
  (pack-sprite-vertices (sprite-batch-vertices batch)
                        (* 4 (sprite-batch-size batch))
                        x y width height origin-x origin-y
                        scale-x scale-y rotation u v u2 v2 color)

  ;; Increment batch size
  (set-sprite-batch-size! batch (1+ (sprite-batch-size batch))))

(define (sprite-batch-switch-texture batch texture)
  "Change the currently bound texture for BATCH to TEXTURE. This
requires flushing the batched texture vertices first."
  (sprite-batch-render batch)
  (set-sprite-batch-texture! batch texture))

(define (sprite-batch-render batch)
  "Render and flushes the currently batched texture vertices within
BATCH."
  (unless (= (sprite-batch-size batch) 0)
    (draw-sprite-vertices (sprite-batch-texture batch)
                          (sprite-batch-vertices batch)
                          (sprite-batch-size batch))
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
