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
;; Textures and texture regions are high level wrappers over OpenGL
;; textures.
;;
;;; Code:

(define-module (2d texture)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-42)
  #:use-module (figl gl)
  #:use-module (2d wrappers gl)
  #:use-module (2d wrappers freeimage)
  #:use-module (2d helpers)
  #:export (make-texture
            make-texture-region
            load-texture
            texture?
            texture-region?
            texture-id
            texture-width
            texture-height
            texture-s1
            texture-t1
            texture-s2
            texture-t2
            surface->texture
            draw-texture
            split-texture))

;;;
;;; Textures
;;;

;; The <texture> object is a simple wrapper around an OpenGL texture
;; id.
(define-record-type <texture>
  (%make-texture id parent width height s1 t1 s2 t2)
  texture?
  (id texture-id)
  (parent texture-parent)
  (width texture-width)
  (height texture-height)
  (s1 texture-s1)
  (t1 texture-t1)
  (s2 texture-s2)
  (t2 texture-t2))

(define (texture-region? texture)
  (texture? (texture-parent texture)))

(define (make-texture id parent width height s1 t1 s2 t2)
  (let ((texture (%make-texture id parent width height s1 t1 s2 t2)))
    (texture-guardian texture)
    texture))

(define (make-texture-region texture x y width height)
  "Creates a new texture region given a texture and a pixel region."
  (let* ((w (texture-width texture))
         (h (texture-height texture)))
    (make-texture (texture-id texture)
                  texture
                  width
                  height
                  (/ x w)
                  (/ y h)
                  (/ (+ x width) w)
                  (/ (+ y height) h))))

;; Use a guardian and an after GC hook that ensures that OpenGL
;; textures are deleted when texture objects are GC'd.
(define texture-guardian (make-guardian))

(define (reap-textures)
  (let loop ((texture (texture-guardian)))
    (when texture
      ;; Do not reap texture regions
      (unless (texture-region? texture)
        ;; When attempting to reap structures upon guile exit, the
        ;; dynamic pointer to gl-delete-textures becomes invalid. So, we
        ;; ignore the error and move on.
        (catch 'misc-error
          (lambda () (gl-delete-texture (texture-id texture)))
          (lambda (key . args) #f)))
      (loop (texture-guardian)))))

(add-hook! after-gc-hook reap-textures)

(define (bitmap->texture bitmap)
  "Translates a freeimage bitmap into an OpenGL texture."
  (let ((texture-id (gl-generate-texture))
        (pixels (freeimage-get-bits bitmap)))
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
                           (freeimage-get-width bitmap)
                           (freeimage-get-height bitmap)
                           0
                           (version-1-2 bgra)
                           (color-pointer-type unsigned-byte)
                           pixels))
    (make-texture texture-id
                  #f
                  (freeimage-get-width bitmap)
                  (freeimage-get-height bitmap)
                  0 0 1 1)))

(define (load-bitmap filename)
  (let* ((bitmap (freeimage-load (freeimage-get-file-type filename) filename))
         (32bit-bitmap (freeimage-convert-to-32-bits bitmap)))
    (freeimage-unload bitmap)
    32bit-bitmap))

(define (load-texture filename)
  "Loads a texture from a file."
  (let* ((bitmap (load-bitmap filename))
         (texture (bitmap->texture bitmap)))
    (freeimage-unload bitmap)
    texture))

(define* (draw-texture texture x y #:optional (color #xffffffff))
  "Renders a textured quad in GL immediate mode."
  (let* ((x2 (+ x (texture-width texture)))
        (y2 (+ y (texture-height texture)))
        (color (rgba->gl-color color))
        (r (vector-ref color 0))
        (g (vector-ref color 1))
        (b (vector-ref color 2))
        (a (vector-ref color 3))
        (s1 (texture-s1 texture))
        (t1 (texture-t1 texture))
        (s2 (texture-s2 texture))
        (t2 (texture-t2 texture)))
    (with-gl-bind-texture (texture-target texture-2d) (texture-id texture)
      (gl-begin (begin-mode quads)
        (gl-color r g b a)
        (gl-texture-coordinates s1 t1)
        (gl-vertex x y)
        (gl-texture-coordinates s1 t2)
        (gl-vertex x y2)
        (gl-texture-coordinates s2 t2)
        (gl-vertex x2 y2)
        (gl-texture-coordinates s2 t1)
        (gl-vertex x2 y)))))

(define* (split-texture texture width height #:optional #:key
                        (margin 0) (spacing 0))
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
