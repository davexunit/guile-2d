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
;; Quick and dirty wrapper for some freeimage functions.
;;
;;; Code:

(define-module (2d wrappers freeimage)
  #:use-module (system foreign))

(define libfreeimage (dynamic-link "libfreeimage"))

(define-syntax-rule (define-foreign name ret string-name args)
  (define name
    (pointer->procedure ret (dynamic-func string-name libfreeimage) args)))

;; Borrowed from guile-figl
(define-syntax-rule (define-enumeration enumerator (name value) ...)
  (define-syntax enumerator
    (lambda (x)
      (syntax-case x ()
        ((_)
         #''(name ...))
        ((_ enum) (number? (syntax->datum #'enum))
         #'enum)
        ((_ enum)
         (or (assq-ref '((name . value) ...)
                       (syntax->datum #'enum))
             (syntax-violation 'enumerator "invalid enumerated value"
                               #'enum)))))))

(define (number->boolean n)
  (not (zero? n)))

;;;
;;; FreeImage file formats
;;;

(define-enumeration freeimage-format
  (unknown -1)
  (bmp      0)
  (ico      1)
  (jpeg     2)
  (jng      3)
  (koala    4)
  (lbm      5)
  (iff      5)
  (mng      6)
  (pbm      7)
  (pbmraw   8)
  (pcd      9)
  (pcx      10)
  (pgm      11)
  (pgmraw   12)
  (png      13)
  (ppm      14)
  (ppmraw   15)
  (ras      16)
  (targa    17)
  (tiff     18)
  (wbmp     19)
  (psd      20)
  (cut      21)
  (xbm      22)
  (xpm      23)
  (dds      24)
  (gif      25)
  (hdr      26)
  (faxg3    27)
  (sgi      28)
  (exr      29)
  (j2k      30)
  (jp2      31)
  (pfm      32)
  (pict     33)
  (raw      34))

(export freeimage-format)

;;;
;;; General functions
;;;

(define-foreign %freeimage-get-version '* "FreeImage_GetVersion" '())
(define-foreign %freeimage-set-output-message
  void "FreeImage_SetOutputMessage" '(*))

(define (freeimage-get-version)
  (pointer->string (%freeimage-get-version)))

(define (freeimage-set-output-message callback)
  (%freeimage-set-output-message
   (procedure->pointer void
                       (lambda (image-format message)
                         (callback image-format (pointer->string message)))
                       (list unsigned-int '*))))

;; Set a default output message callback to writes to stdout.
(freeimage-set-output-message
 (lambda (image-format message)
   (display "freeimage error: ")
   (display message)
   (newline)))

(export freeimage-get-version
        freeimage-set-output-message)


;;;
;;; Bitmap management functions
;;;

(define-wrapped-pointer-type <freeimage-bitmap>
  freeimage-bitmap?
  wrap-freeimage-bitmap unwrap-freeimage-bitmap
  (lambda (r port)
    (let ((bitmap (unwrap-freeimage-bitmap r)))
      (format port
              "<freeimage-bitmap ~x width: ~d height: ~d bpp: ~d>"
              (pointer-address bitmap)
              (%freeimage-get-width bitmap)
              (%freeimage-get-height bitmap)
              (%freeimage-get-bpp bitmap)))))

(define-foreign %freeimage-load '* "FreeImage_Load" (list unsigned-int '* unsigned-int))
(define-foreign %freeimage-unload void "FreeImage_Unload" '(*))

(define (freeimage-load image-format filename)
  (wrap-freeimage-bitmap
   (%freeimage-load image-format (string->pointer filename) 0)))

(define (freeimage-unload bitmap)
  (%freeimage-unload (unwrap-freeimage-bitmap bitmap)))

(export freeimage-load
        freeimage-unload)

;;;
;;; Bitmap information functions
;;;

(define-foreign %freeimage-get-image-type unsigned-int "FreeImage_GetImageType" '(*))
(define-foreign %freeimage-get-bpp unsigned-int "FreeImage_GetBPP" '(*))
(define-foreign %freeimage-get-width unsigned-int "FreeImage_GetWidth" '(*))
(define-foreign %freeimage-get-height unsigned-int "FreeImage_GetHeight" '(*))
(define-foreign %freeimage-get-pitch unsigned-int "FreeImage_GetPitch" '(*))
(define-foreign %freeimage-get-red-mask unsigned-int "FreeImage_GetRedMask" '(*))
(define-foreign %freeimage-get-green-mask unsigned-int "FreeImage_GetGreenMask" '(*))
(define-foreign %freeimage-get-blue-mask unsigned-int "FreeImage_GetBlueMask" '(*))
(define-foreign %freeimage-has-pixels unsigned-int "FreeImage_HasPixels" '(*))

(define (freeimage-get-image-type bitmap)
  (%freeimage-get-image-type (unwrap-freeimage-bitmap bitmap)))

(define (freeimage-get-bpp bitmap)
  (%freeimage-get-bpp (unwrap-freeimage-bitmap bitmap)))

(define (freeimage-get-width bitmap)
  (%freeimage-get-width (unwrap-freeimage-bitmap bitmap)))

(define (freeimage-get-height bitmap)
  (%freeimage-get-height (unwrap-freeimage-bitmap bitmap)))

(define (freeimage-get-pitch bitmap)
  (%freeimage-get-pitch (unwrap-freeimage-bitmap bitmap)))

(define (freeimage-get-red-mask bitmap)
  (%freeimage-get-red-mask (unwrap-freeimage-bitmap bitmap)))

(define (freeimage-get-green-mask bitmap)
  (%freeimage-get-green-mask (unwrap-freeimage-bitmap bitmap)))

(define (freeimage-get-blue-mask bitmap)
  (%freeimage-get-blue-mask (unwrap-freeimage-bitmap bitmap)))

(define (freeimage-has-pixels? bitmap)
  (number->boolean
   (%freeimage-has-pixels (unwrap-freeimage-bitmap bitmap))))

(export freeimage-get-image-type
        freeimage-get-bpp
        freeimage-get-width
        freeimage-get-height
        freeimage-get-red-mask
        freeimage-get-green-mask
        freeimage-get-blue-mask
        freeimage-has-pixels?)

;;;
;;; Filetype functions
;;;

(define-foreign %freeimage-get-file-type unsigned-int "FreeImage_GetFileType" '(*))

(define (freeimage-get-file-type filename)
  (%freeimage-get-file-type (string->pointer filename)))

(export freeimage-get-file-type)

;;;
;;; Pixel access functions
;;;

(define-foreign %freeimage-get-bits '* "FreeImage_GetBits" '(*))

(define (freeimage-get-bits bitmap)
  (pointer->bytevector
     (%freeimage-get-bits (unwrap-freeimage-bitmap bitmap))
     (* (freeimage-get-height bitmap)
        (freeimage-get-pitch bitmap))))

(export freeimage-get-bits)

;;;
;;; Conversion functions
;;;

(define-foreign %freeimage-convert-to-24-bits '* "FreeImage_ConvertTo24Bits" '(*))
(define-foreign %freeimage-convert-to-32-bits '* "FreeImage_ConvertTo32Bits" '(*))
(define-foreign %freeimage-convert-to-raw-bits
  void "FreeImage_ConvertToRawBits"
  (list '* '* int unsigned-int unsigned-int unsigned-int unsigned-int uint8))

(define (freeimage-convert-to-24-bits bitmap)
  (wrap-freeimage-bitmap
   (%freeimage-convert-to-24-bits (unwrap-freeimage-bitmap bitmap))))

(define (freeimage-convert-to-32-bits bitmap)
  (wrap-freeimage-bitmap
   (%freeimage-convert-to-32-bits (unwrap-freeimage-bitmap bitmap))))

(export freeimage-convert-to-24-bits
        freeimage-convert-to-32-bits)

;;;
;;; Rotation and flipping
;;;

(define-foreign %freeimage-flip-vertical uint8 "FreeImage_FlipVertical" '(*))

(define (freeimage-flip-vertical bitmap)
  (number->boolean
   (%freeimage-flip-vertical (unwrap-freeimage-bitmap bitmap))))

(export freeimage-flip-vertical)
