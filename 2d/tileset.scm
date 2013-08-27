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
;; Tilesets encapsulate a group of uniformly sized texture regions
;; that come from a single texture.
;;
;;; Code:

(define-module (2d tileset)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-42)
  #:use-module (2d texture)
  #:export (<tileset>
            make-tileset
            load-tileset
            tileset?
            tileset-tiles
            tileset-width
            tileset-height
            tileset-margin
            tileset-spacing
            tileset-ref))

(define-record-type <tileset>
  (%make-tileset tiles width height margin spacing)
  tileset?
  (tiles tileset-tiles)
  (width tileset-width)
  (height tileset-height)
  (margin tileset-margin)
  (spacing tileset-spacing))

(define (split-texture texture width height margin spacing)
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

(define* (make-tileset texture width height
                       #:optional #:key (margin 0) (spacing 0))
  "Returns a new tileset that is built by splitting the given texture
into tiles."
  (let ((tiles (split-texture texture
                              width
                              height
                              margin
                              spacing)))
    (%make-tileset tiles width height margin spacing)))

(define* (load-tileset filename width height
                       #:optional #:key (margin 0) (spacing 0))
  "Returns a new tileset that is built by loading the given texture
file and splitting the texture into tiles."
  (let* ((tiles (split-texture (load-texture filename)
                               width
                               height
                               margin
                               spacing)))
    (%make-tileset tiles width height margin spacing)))

(define (tileset-ref tileset i)
  "Returns the tile at index i."
  (vector-ref (tileset-tiles tileset) i))
