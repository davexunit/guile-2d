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
  #:use-module (srfi srfi-42)
  #:use-module (figl gl)
  #:use-module (figl contrib packed-struct)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (2d vector)
  #:use-module (2d helpers)
  #:export (make-texture
            texture?
            texture-id
            texture-width
            texture-height
            surface->texture
            load-texture
            texture-quad
            make-texture-region
            texture-region?
            texture-region-texture
            texture-region-width
            texture-region-height
            texture-region-u
            texture-region-v
            texture-region-u2
            texture-region-v2
            split-texture
            make-animation
            animation?
            animation-frames
            animation-duration
            animation-loop?
            animation-frame
            animation-length
            make-sprite
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

;;;
;;; Textures
;;;

;; The <texture> object is a simple wrapper around an OpenGL texture
;; id.
(define-record-type <texture>
  (make-texture id width height)
  texture?
  (id texture-id)
  (width texture-width)
  (height texture-height))

;; Use a guardian and an after GC hook that ensures that OpenGL
;; textures are deleted when texture objects are GC'd.
(define texture-guardian (make-guardian))

(define (reap-textures)
  (let loop ((texture (texture-guardian)))
    (when texture
      ;; When attempting to reap structures upon guile exit, the
      ;; dynamic pointer to gl-delete-textures becomes invalid. So, we
      ;; ignore the error and move on.
      (catch 'misc-error
        (lambda () (gl-delete-textures (list (texture-id texture))))
        (lambda (key . args) #f))
      (loop (texture-guardian)))))

(add-hook! after-gc-hook reap-textures)

(define (surface-pixel-format surface)
  "Returns the OpenGL pixel format for a surface. RGB and RGBA formats
are supported."
  (case (SDL:surface:depth surface)
    ((24) (pixel-format rgb))
    ((32) (pixel-format rgba))
    (else (throw 'unsupported-pixel-format (SDL:surface:depth surface)))))

(define (surface->texture surface)
  "Translates an SDL surface into an OpenGL texture.
Currently only works with RGBA format surfaces."
  (let ((texture-id (gl-generate-texture))
        (pixel-format (surface-pixel-format surface)))
    (with-gl-bind-texture (texture-target texture-2d) texture-id
      (gl-texture-parameter (texture-target texture-2d)
                            (texture-parameter-name texture-min-filter)
                            (texture-min-filter linear))
      (gl-texture-parameter (texture-target texture-2d)
                            (texture-parameter-name texture-mag-filter)
                            (texture-mag-filter linear))
      (gl-texture-image-2d (texture-target texture-2d)
                           0
                           pixel-format
                           (SDL:surface:w surface)
                           (SDL:surface:h surface)
                           0
                           pixel-format
                           (color-pointer-type unsigned-byte)
                           (SDL:surface-pixels surface)))
    (let ((texture (make-texture texture-id
                                 (SDL:surface:w surface)
                                 (SDL:surface:h surface))))
      (texture-guardian texture)
      texture)))

(define (load-texture filename)
  "Loads a texture from a file."
  (surface->texture (SDL:load-image filename)))

(define* (texture-quad texture x y w h #:optional (color '(1 1 1))
                       (u 0) (v 0) (u2 1) (v2 1))
  "Renders a textured quad."
  (let ((x2 (+ x w))
        (y2 (+ y h)))
    (with-gl-bind-texture (texture-target texture-2d) (texture-id texture)
      (gl-begin (begin-mode quads)
        (apply gl-color color)
        (gl-texture-coordinates u v)
        (gl-vertex x y)
        (gl-texture-coordinates u2 v)
        (gl-vertex x2 y)
        (gl-texture-coordinates u2 v2)
        (gl-vertex x2 y2)
        (gl-texture-coordinates u v2)
        (gl-vertex x y2)))))

;;;
;;; Texture Regions
;;;

;; Texture regions represent a segment of a texture.

(define-record-type <texture-region>
  (%make-texture-region texture width height u v u2 v2)
  texture-region?
  (texture texture-region-texture)
  (width texture-region-width)
  (height texture-region-height)
  (u texture-region-u)
  (v texture-region-v)
  (u2 texture-region-u2)
  (v2 texture-region-v2))

(define (make-texture-region texture x y width height)
  "Creates a new texture region given a texture and a pixel region."
  (let* ((w (texture-width texture))
         (h (texture-height texture))
         (u (/ x w))
         (v (/ y h))
         (u2 (/ (+ x width) w))
         (v2 (/ (+ y height) h)))
    (%make-texture-region texture width height u v u2 v2)))

(define* (split-texture texture width height
                        #:optional #:key (margin 0) (spacing 0))
  "Splits a texture into a vector of texture regions of width x height
size."
  (define (build-tile tx ty)
    (let* ((x (+ (* tx (+ width spacing)) margin))
           (y (+ (* ty (+ height spacing)) margin)))
      (make-texture-region texture x y width height)))

  (let* ((tw (texture-width texture))
         (th (texture-height texture))
         (rows (/ (- tw margin) (+ width spacing)))
         (columns (/ (- tw margin) (+ height spacing))))
    (vector-ec (: y rows) (: x columns) (build-tile x y))))

;;;
;;; Animations
;;;

;; The <animation> type represents a vector of textures or texture
;; regions that are to be played in sequence and possibly looped.
(define-record-type <animation>
  (make-animation frames duration loop)
  animation?
  (frames animation-frames)
  (duration animation-duration)
  (loop animation-loop?))

(define (animation-frame animation index)
  "Returns the frame for the given index."
  (vector-ref (animation-frames animation) index))

(define (animation-length animation)
  "Returns the number of frames in the animation"
  (vector-length (animation-frames animation)))

;; The <animation-state> type encapsulates the state for playing an
;; animation.
(define-record-type <animation-state>
  (%make-animation-state animation frame-index frame-time playing)
  animation-state?
  (animation animation-state-animation)
  (frame-index animation-state-frame-index)
  (frame-time animation-state-frame-time)
  (playing animation-state-playing?))

(define (make-animation-state animation)
  (%make-animation-state animation 0 0 #t))

(define (update-animation-state state)
  "Increments the frame time for the animation state and determines
which frame to show. Returns a new animation-state object."
  (let ((frame-time (1+ (animation-state-frame-time state)))
        (frame-index (animation-state-frame-index state))
        (playing (animation-state-playing? state))
        (animation (animation-state-animation state)))

    ;; Return the same state object if the animation is not playing.
    (cond ((not playing)
           state)
          ;; Return a new state object with a reset frame-index and
          ;; frame-time if we've reached the end of the animation.
          ;; Stops playing the animation if the animation does not
          ;; loop.
          ((and playing (= frame-time (animation-duration animation)))
           (let* ((frame-index (modulo (1+ frame-index)
                                       (animation-length animation)))
                  (frame-time 0)
                  (playing (or (not (= frame-index 0))
                               (animation-loop? animation))))
             (%make-animation-state animation frame-index frame-time playing)))
          ;; Return a new state object with an incremented frame index.
          (else
           (%make-animation-state animation frame-index frame-time playing)))))

(define (animation-state-frame state)
  (animation-frame (animation-state-animation state)
                   (animation-state-frame-index state)))

;;;
;;; Sprites
;;;

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
        (color (rgba->gl-color color))
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
         (anchor (sprite-anchor-vector sprite))
         (tex-coords (sprite-texture-coords sprite))
         (x (- (vx anchor)))
         (y (- (vy anchor)))
         (x2 (+ x (vx size)))
         (y2 (+ y (vy size)))
         (u (first tex-coords))
         (v (second tex-coords))
         (u2 (third tex-coords))
         (v2 (fourth tex-coords)))
    (pack vertices 0 sprite-vertex
          x y
          1 1 1 1
          u v)
    (pack vertices 1 sprite-vertex
          x2 y
          1 1 1 1
          u2 v)
    (pack vertices 2 sprite-vertex
          x2 y2
          1 1 1 1
          u2 v2)
    (pack vertices 3 sprite-vertex
          x y2
          1 1 1 1
          u v2)))

(define (draw-sprite sprite)
  "Renders a sprite. A sprite batch will be used if one is currently
bound."
  (when (animation? (sprite-drawable sprite))
    (let ((state (update-animation-state (sprite-animation-state sprite))))
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
        (gl-draw-arrays (begin-mode quads)
                        0
                        (packed-array-length vertices sprite-vertex)))
      (gl-disable-client-state (enable-cap texture-coord-array))
      (gl-disable-client-state (enable-cap color-array))
      (gl-disable-client-state (enable-cap vertex-array)))))

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
                            (u 0) (v 0) (u2 1) (v2 1))
  "Adds a textured quad to the sprite batch."
  ;; Render the batch when it's full or the texture changes.
  (cond ((= (sprite-batch-size batch) (sprite-batch-max-size batch))
         (sprite-batch-render batch))
        ((not (equal? texture (sprite-batch-texture batch)))
         (sprite-batch-switch-texture batch texture)))
  ;; Add 4 new vertices.
  (let ((base (* 4 (sprite-batch-size batch)))
        (vertices (sprite-batch-vertices batch))
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
    (set! *sprite-batch* batch)
    (set-sprite-batch-size! batch 0)
    (set-sprite-batch-texture! batch #f)
    body
    ...
    (sprite-batch-render batch)
    (set! *sprite-batch* #f)))
