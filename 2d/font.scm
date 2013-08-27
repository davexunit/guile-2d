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
;; Font rendering.
;;
;;; Code:

(define-module (2d font)
  #:use-module (figl gl)
  #:use-module (srfi srfi-9)
  #:use-module (system foreign)
  #:use-module (2d wrappers ftgl)
  #:use-module (2d color)
  #:use-module (2d vector2))

;;;
;;; Font
;;;

;; Font objects represent an FTGL texture font at a given size.
(define-record-type <font>
  (make-font ftgl-font size)
  font?
  (ftgl-font font-ftgl-font)
  (size font-size))

(define (load-font filename size)
  "Loads a font from a file with a given size in points."
  (let ((ftgl-font (ftgl-create-texture-font filename)))
    ;; Hardcoded 72 dpi for now.
    (ftgl-set-font-face-size ftgl-font size 72)
    (make-font ftgl-font size)))

(define (flip-text font)
  "Flips current GL matrix about the x-axis and translates by the
negative font ascender value. This is necessary before rendering text
because guile-2d flips the y-axis so that the origin is in the
upper-left corner rather than the bottom-left."
  (gl-scale 1 -1 1)
  (gl-translate 0 (- (ftgl-get-font-ascender (font-ftgl-font font))) 0))

(define (draw-font font text)
  "Renders the string text using the given font."
  (with-gl-push-matrix
    (flip-text font)
    (ftgl-render-font (font-ftgl-font font)
                      text
                      (ftgl-render-mode all))))

(export <font>
        make-font
        font?
        font-ftgl-font
        font-size
        load-font
        draw-font)

;;;
;;; Textbox
;;;

;; A textbox is a string of word-wrapped text
(define-record-type <textbox>
  (%make-textbox font text position color alignment line-length layout)
  textbox?
  (font textbox-font)
  (text textbox-text set-textbox-text!)
  (position textbox-position set-textbox-position!)
  (color textbox-color set-textbox-color!)
  (alignment textbox-alignment)
  (line-length textbox-line-length)
  (layout textbox-layout))

(define (make-textbox font text position color alignment line-length)
  (let ((layout (ftgl-create-layout)))
    (ftgl-set-layout-font layout (font-ftgl-font font))
    ;; (ftgl-set-layout-alignment layout (ftgl-text-alignment alignment))
    (ftgl-set-layout-line-length layout line-length)
    (%make-textbox font text position color alignment line-length layout)))

(define (draw-textbox textbox)
  (let ((pos (textbox-position textbox)))
    (with-gl-push-matrix
      (gl-translate (vx pos) (vy pos) 0)
      (flip-text (textbox-font textbox))
      (apply-color (textbox-color textbox))
      (ftgl-render-layout (textbox-layout textbox)
                          (textbox-text textbox)
                          (ftgl-render-mode all)))))

(export <textbox>
        make-textbox
        textbox?
        textbox-font
        textbox-text
        set-textbox-text!
        textbox-position
        set-textbox-position!
        textbox-color
        set-textbox-color!
        textbox-alignment
        textbox-line-length
        textbox-layout
        draw-textbox)
