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
;; OpenGL texture wrapper.
;;
;;; Code:

(define-module (2d texture)
  #:use-module (srfi srfi-9)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (figl gl)
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
            texture-region-v2))

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
    (else (throw 'unsupported-pixel-format))))

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

(define* (texture-quad texture x y w h #:optional (color '(1 1 1)))
  "Renders a textured quad."
  (let ((x2 (+ x w))
        (y2 (+ y h)))
    (with-gl-bind-texture (texture-target texture-2d) (texture-id texture)
      (gl-begin (begin-mode quads)
        (apply gl-color color)
        (gl-texture-coordinates 0 0)
        (gl-vertex x y)
        (gl-texture-coordinates 1 0)
        (gl-vertex x2 y)
        (gl-texture-coordinates 1 1)
        (gl-vertex x2 y2)
        (gl-texture-coordinates 0 1)
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
