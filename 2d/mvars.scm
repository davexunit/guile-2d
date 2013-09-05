;;; Synchronized Mutable Variables

;; Copyright (C) 2013 Mark Weaver

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this library; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code:

(define-module (2d mvars)
  #:use-module (ice-9 threads)
  #:use-module (srfi srfi-8)            ; receive
  #:use-module (srfi srfi-9)            ; records
  #:use-module (srfi srfi-9 gnu)
  #:export (mvar?
            mvar-empty? new-empty-mvar new-mvar
            take-mvar put-mvar read-mvar swap-mvar
            try-take-mvar try-put-mvar
            with-mvar modify-mvar modify-mvar*))

(define-record-type <mvar>
  (make-mvar contents empty? mutex full-condition empty-condition)
  mvar?
  (contents         %mvar-contents   %set-mvar-contents!)
  (empty?           %mvar-empty?     %set-mvar-empty?!)
  (mutex            mvar-mutex)
  (full-condition   mvar-full-condition)
  (empty-condition  mvar-empty-condition))

(define (mvar-empty? mvar)
  (with-mutex (mvar-mutex mvar)
    (%mvar-empty? mvar)))

(define (new-empty-mvar)
  "Return a freshly allocated mvar that is initially empty."
  (make-mvar #f  ; contents
             #t  ; empty?
             (make-mutex)
             (make-condition-variable)
             (make-condition-variable)))

(define (new-mvar x)
  "Return a freshly allocated mvar with initial contents X."
  (make-mvar x   ; contents
             #f  ; empty?
             (make-mutex)
             (make-condition-variable)
             (make-condition-variable)))

(define (take-mvar mvar)
  "Block until MVAR is full, then atomically remove and return its contents."
  (with-mutex (mvar-mutex mvar)
    (when (%mvar-empty? mvar)
      (wait-condition-variable (mvar-full-condition mvar) (mvar-mutex mvar)))
    (let ((x (%mvar-contents mvar)))
      (%set-mvar-contents! mvar #f)
      (%set-mvar-empty?! mvar #t)
      (signal-condition-variable (mvar-empty-condition mvar))
      x)))

(define (put-mvar mvar x)
  "Block until MVAR is empty, then put X into it."
  (with-mutex (mvar-mutex mvar)
    (unless (%mvar-empty? mvar)
      (wait-condition-variable (mvar-empty-condition mvar) (mvar-mutex mvar)))
    (%set-mvar-contents! mvar x)
    (%set-mvar-empty?! mvar #f)
    (signal-condition-variable (mvar-full-condition mvar))
    *unspecified*))

(define (read-mvar mvar)
  "Take a value x from MVAR, then put it back and return x.  This
procedure is atomic only if there are no other producers for MVAR."
  (let ((x (take-mvar mvar)))
    (put-mvar mvar x)
    x))

(define (swap-mvar mvar y)
  "Take a value x from MVAR, then put Y into MVAR and return x.  This
procedure is atomic only if there are no other producers for MVAR."
  (let ((x (take-mvar mvar)))
    (put-mvar mvar y)
    x))

(define (try-take-mvar mvar)
  "If MVAR is full, return its contents and #t, else return #f and #f."
  (with-mutex (mvar-mutex mvar)
    (if (%mvar-empty? mvar)
        (values #f #f)
        (let ((x (%mvar-contents mvar)))
          (%set-mvar-contents! mvar #f)
          (%set-mvar-empty?! mvar #t)
          (signal-condition-variable (mvar-empty-condition mvar))
          (values x #t)))))

(define (try-put-mvar mvar x)
  "If MVAR is empty, put X into it and return #t, else return #f."
  (with-mutex (mvar-mutex mvar)
    (and (%mvar-empty? mvar)
         (begin
           (%set-mvar-contents! mvar x)
           (%set-mvar-empty?! mvar #f)
           (signal-condition-variable (mvar-full-condition mvar))
           #t))))

(define (with-mvar mvar proc)
  "Take a value from MVAR and apply PROC to it.  If an exception is raised,
the original value is put back into MVAR.  This procedure is atomic only if
there are no other producers for MVAR."
  (let ((x (take-mvar mvar)))
    (catch #t
      (lambda () (proc x))
      (lambda (key . args)
        (put-mvar mvar x)
        (apply throw key args)))))

(define (modify-mvar mvar f)
  "Take a value x from MVAR, and then put back (F x).  If an exception is
raised, the original value is put back into MVAR.  This procedure is
atomic only if there are no other producers for MVAR."
  (let ((old (take-mvar mvar)))
    (catch #t
      (lambda () (put-mvar mvar (f old)))
      (lambda (key . args)
        (put-mvar mvar old)
        (apply throw key args)))))

(define (modify-mvar* mvar f)
  "Take a value x from MVAR, and apply F to it.  (F x) should return one
or more values: the new value to be put back into MVAR, and zero or more
additional values to be returned from MODIFY-MVAR*.  If an exception is
raised, the original value is put back into MVAR.  This procedure is
atomic only if there are no other producers for MVAR."
  (let ((old (take-mvar mvar)))
    (catch #t
      (lambda ()
        (receive (new . results) (f old)
          (put-mvar mvar new)
          (apply values results)))
      (lambda (key . args)
        (put-mvar mvar old)
        (apply throw key args)))))

(set-record-type-printer!
 <mvar>
 (lambda (mvar port)
   (display "#<mvar " port)
   (display (number->string (object-address mvar) 16) port)
   (display " " port)
   (write (with-mutex (mvar-mutex mvar)
            (if (%mvar-empty? mvar)
                '()
                (list (%mvar-contents mvar))))
          port)
   (display ">" port)))
