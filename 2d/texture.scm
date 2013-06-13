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
            texture-quad))

;; The <texture> object is a simple wrapper around an OpenGL texture
;; id.
(define-record-type <texture>
  (make-texture id width height)
  texture?
  (id texture-id)
  (width texture-width)
  (height texture-height))

(define (surface->texture surface)
  "Translates an SDL surface into an OpenGL texture.
Currently only works with RGBA format surfaces."
  (let* ((texture-id (gl-generate-texture)))
    (with-gl-bind-texture (texture-target texture-2d) texture-id
      (gl-texture-parameter (texture-target texture-2d)
                            (texture-parameter-name texture-min-filter)
                            (texture-min-filter linear))
      (gl-texture-parameter (texture-target texture-2d)
                            (texture-parameter-name texture-mag-filter)
                            (texture-mag-filter linear))
      (gl-texture-image-2d (texture-target texture-2d)
                           0
                           (pixel-format rgba)
                           (SDL:surface:w surface)
                           (SDL:surface:h surface)
                           0
                           (pixel-format rgba)
                           (color-pointer-type unsigned-byte)
                           (SDL:surface-pixels surface)))
    (make-texture texture-id
                  (SDL:surface:w surface)
                  (SDL:surface:h surface))))

(define (load-texture filename)
  "Loads a texture from a file."
  (surface->texture (SDL:load-image filename)))

(define* (texture-quad texture x y w h)
  "Renders a textured quad."
  (let ((x2 (+ x w))
        (y2 (+ y h)))
    (with-gl-bind-texture (texture-target texture-2d) (texture-id texture)
      (gl-begin (begin-mode quads)
        (gl-texture-coordinates 0 0)
        (gl-vertex x y)
        (gl-texture-coordinates 1 0)
        (gl-vertex x2 y)
        (gl-texture-coordinates 1 1)
        (gl-vertex x2 y2)
        (gl-texture-coordinates 0 1)
        (gl-vertex x y2)))))
