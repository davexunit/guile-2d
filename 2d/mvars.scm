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
  #:use-module (srfi srfi-8)     ; receive
  #:use-module (srfi srfi-9)     ; records
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 threads)
  #:export (new-mvar mvar? mvar-empty?
                     take-mvar put-mvar read-mvar swap-mvar
                     try-take-mvar try-put-mvar try-read-mvar
                     with-mvar modify-mvar modify-mvar*))

(define-record-type <mvar>
  (make-mvar contents empty? mutex condvar)
  mvar?
  (contents mvar-contents set-mvar-contents!)
  (empty? mvar-empty? set-mvar-empty?!)
  (mutex mvar-mutex)
  (condvar mvar-condvar))

(set-record-type-printer!
 <mvar>
 (lambda (mvar port)
   (display "#<mvar " port)
   (display (number->string (object-address mvar) 16) port)
   (display " " port)
   (write (receive (x full?) (try-read-mvar mvar)
            (if full? (list x) '()))
          port)
   (display ">" port)))

(define new-mvar
  (case-lambda
    "Return a freshly allocated mvar.  The optional argument, if provided,\n\
specifies the initial contents of the mvar, otherwise it will be empty."
    (()  (make-mvar #f #t (make-mutex) (make-condition-variable)))
    ((x) (make-mvar x  #f (make-mutex) (make-condition-variable)))))

(define (take-mvar mvar)
  "Block until MVAR is full, then atomically remove and return its contents."
  (with-mutex (mvar-mutex mvar)
    (when (mvar-empty? mvar)
      (wait-condition-variable (mvar-condvar mvar) (mvar-mutex mvar))
      (when (mvar-empty? mvar)
        (error "take-mvar: expected full mvar after waiting")))
    (let ((x (mvar-contents mvar)))
      (set-mvar-contents! mvar #f)
      (set-mvar-empty?! mvar #t)
      (signal-condition-variable (mvar-condvar mvar))
      x)))

(define (put-mvar mvar x)
  "Block until MVAR is empty, then put X into it."
  (with-mutex (mvar-mutex mvar)
    (unless (mvar-empty? mvar)
      (wait-condition-variable (mvar-condvar mvar) (mvar-mutex mvar))
      (unless (mvar-empty? mvar)
        (error "put-mvar: expected empty mvar after waiting")))
    (set-mvar-contents! mvar x)
    (set-mvar-empty?! mvar #f)
    (signal-condition-variable (mvar-condvar mvar))
    *unspecified*))

(define (read-mvar mvar)
  "Block until MVAR is full, then return its contents, leaving MVAR unchanged."
  (with-mutex (mvar-mutex mvar)
    (when (mvar-empty? mvar)
      (wait-condition-variable (mvar-condvar mvar) (mvar-mutex mvar))
      (when (mvar-empty? mvar)
        (error "read-mvar: expected full mvar after waiting")))
    (mvar-contents mvar)))

(define (swap-mvar mvar new)
  "Block until MVAR is full, and then atomically swap its contents\n\
with NEW and return the previous contents."
  (with-mutex (mvar-mutex mvar)
    (when (mvar-empty? mvar)
      (wait-condition-variable (mvar-condvar mvar) (mvar-mutex mvar))
      (when (mvar-empty? mvar)
        (error "swap-mvar: expected full mvar after waiting")))
    (let ((old (mvar-contents mvar)))
      (set-mvar-contents! mvar new)
      old)))

(define (try-take-mvar mvar)
  "If MVAR is full, return its contents and #t, else return #f and #f."
  (with-mutex (mvar-mutex mvar)
    (if (mvar-empty? mvar)
        (values #f #f)
        (let ((x (mvar-contents mvar)))
          (set-mvar-contents! mvar #f)
          (set-mvar-empty?! mvar #t)
          (signal-condition-variable (mvar-condvar mvar))
          (values x #t)))))

(define (try-put-mvar mvar x)
  "If MVAR is empty, put X into it and return #t, else return #f."
  (with-mutex (mvar-mutex mvar)
    (and (mvar-empty? mvar)
         (begin
           (set-mvar-contents! mvar x)
           (set-mvar-empty?! mvar #f)
           (signal-condition-variable (mvar-condvar mvar))
           #t))))

(define (try-read-mvar mvar)
  "If MVAR is full, return its contents and #t, else return #f and #f."
  (with-mutex (mvar-mutex mvar)
    (if (mvar-empty? mvar)
        (values #f #f)
        (values (mvar-contents mvar) #t))))

(define (with-mvar mvar proc)
  "Take a value from MVAR and apply PROC to it.  If an exception is raised,\n\
the original value is put back into MVAR.  This procedure is atomic only if\n\
there are no other producers for MVAR."
  (let ((x (take-mvar mvar)))
    (catch #t
      (lambda () (proc x))
      (lambda (key . args)
        (put-mvar mvar x)
        (apply throw key args)))))

(define (modify-mvar mvar f)
  "Take a value x from MVAR, and then put back (F x).  If an exception is\n\
raised, the original value is put back into MVAR.  This procedure is\n\
atomic only if there are no other producers for MVAR."
  (let ((old (take-mvar mvar)))
    (catch #t
      (lambda () (put-mvar mvar (f old)))
      (lambda (key . args)
        (put-mvar mvar old)
        (apply throw key args)))))

(define (modify-mvar* mvar f)
  "Take a value x from MVAR, and apply F to it.  (F x) should return one\n\
or more values: the new value to be put back into MVAR, and zero or more\n\
additional values to be returned from MODIFY-MVAR*.  If an exception is\n\
raised, the original value is put back into MVAR.  This procedure is\n\
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
