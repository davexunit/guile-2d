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
;; Quick and dirty wrapper for the FTGL library.
;;
;;; Code:

(define-module (2d wrappers ftgl)
  #:use-module (system foreign)
  #:use-module (2d wrappers util))

(define libftgl (dynamic-link "libftgl"))

(define-syntax-rule (define-foreign name ret string-name args)
  (define name
    (pointer->procedure ret (dynamic-func string-name libftgl) args)))

;;;
;;; Enums
;;;

(define-enumeration ftgl-render-mode
  (front #x0001)
  (back  #x0002)
  (side  #x0004)
  (all   #xffff))

(define-enumeration ftgl-text-alignment
  (left    0)
  (center  1)
  (right   2)
  (justify 3))

(export ftgl-render-mode
        ftgl-text-alignment)

;;;
;;; Fonts
;;;

(define-wrapped-pointer-type <ftgl-font>
  ftgl-font?
  wrap-ftgl-font unwrap-ftgl-font
  (lambda (r port)
    (let ((font (unwrap-ftgl-font r)))
      (format port
              "<ftgl-font ~x>"
              (pointer-address font)))))

(define-foreign %ftgl-create-texture-font
  '* "ftglCreateTextureFont" '(*))

(define-foreign %ftgl-set-font-face-size
  void "ftglSetFontFaceSize" (list '* unsigned-int unsigned-int))

(define-foreign %ftgl-render-font
  void "ftglRenderFont" (list '* '* unsigned-int))

(define (ftgl-create-texture-font filename)
  (wrap-ftgl-font
   (%ftgl-create-texture-font (string->pointer filename))))

(define (ftgl-set-font-face-size font size res)
  (%ftgl-set-font-face-size (unwrap-ftgl-font font) size res))

(define (ftgl-render-font font text render-mode)
  (%ftgl-render-font (unwrap-ftgl-font font)
                     (string->pointer text)
                     render-mode))

(export ftgl-create-texture-font
        ftgl-set-font-face-size
        ftgl-render-font)

;;;
;;; SimpleLayout
;;;

(define-wrapped-pointer-type <ftgl-simple-layout>
  ftgl-simple-layout?
  wrap-ftgl-simple-layout unwrap-ftgl-simple-layout
  (lambda (r port)
    (let ((simple-layout (unwrap-ftgl-simple-layout r)))
      (format port
              "<ftgl-simple-layout ~x>"
              (pointer-address simple-layout)))))

(define-foreign %ftgl-create-simple-layout
  '* "ftglCreateSimpleLayout" '())

(define-foreign %ftgl-destroy-layout
  void "ftglDestroyLayout" '(*))

(define-foreign %ftgl-set-layout-font
  void "ftglSetLayoutFont" '(* *))

(define-foreign %ftgl-get-layout-font
  '* "ftglGetLayoutFont" '(*))

(define-foreign %ftgl-set-layout-line-length
  void "ftglSetLayoutLineLength" (list '* float))

(define-foreign %ftgl-get-layout-line-length
  float "ftglGetLayoutLineLength" '(*))

(define-foreign %ftgl-set-layout-alignment
  void "ftglSetLayoutAlignment" (list '* int))

(define-foreign %ftgl-get-layout-alignment
  int "ftglGetLayoutAlignement" '(*))

(define-foreign %ftgl-set-layout-line-spacing
  void "ftglSetLayoutLineSpacing" (list '* float))

;; For some reason this symbol is not found.
;; (define-foreign %ftgl-get-layout-line-spacing
;;   float "ftglGetLayoutLineSpacing" '(*))

(define-foreign %ftgl-render-layout
  void "ftglRenderLayout" (list '* '* int))

(define (ftgl-create-layout)
  (wrap-ftgl-simple-layout
   (%ftgl-create-simple-layout)))

(define (ftgl-destroy-layout layout)
  (%ftgl-destroy-layout (unwrap-ftgl-simple-layout layout)))

(define (ftgl-set-layout-font layout font)
  (%ftgl-set-layout-font (unwrap-ftgl-simple-layout layout)
                         (unwrap-ftgl-font font)))

(define (ftgl-get-layout-font layout)
  (wrap-ftgl-font
   (%ftgl-get-layout-font (unwrap-ftgl-simple-layout layout))))

(define (ftgl-set-layout-line-length layout line-length)
  (%ftgl-set-layout-line-length (unwrap-ftgl-simple-layout layout)
                                line-length))

(define (ftgl-get-layout-line-length layout)
  (%ftgl-get-layout-line-length (unwrap-ftgl-simple-layout layout)))

(define (ftgl-set-layout-alignment layout alignment)
  (%ftgl-set-layout-alignment (unwrap-ftgl-simple-layout layout)
                              alignment))

(define (ftgl-get-layout-alignment layout)
  (%ftgl-get-layout-alignment (unwrap-ftgl-simple-layout layout)))

(define (ftgl-set-layout-line-spacing layout spacing)
  (%ftgl-set-layout-line-spacing (unwrap-ftgl-simple-layout layout)
                                 spacing))

(define (ftgl-render-layout layout text mode)
  (%ftgl-render-layout (unwrap-ftgl-simple-layout layout)
                       (string->pointer text)
                       mode))

(export ftgl-create-layout
        ftgl-destroy-layout
        ftgl-set-layout-font
        ftgl-get-layout-font
        ftgl-set-layout-line-length
        ftgl-get-layout-line-length
        ftgl-set-layout-alignment
        ftgl-get-layout-alignment
        ftgl-set-layout-line-spacing
        ftgl-render-layout)
