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
;; Cooperative multi-tasking.
;;
;;; Code:

(define-module (2d coroutine)
  #:export (coroutine
            colambda
            codefine
            codefine*)
  #:replace (yield))

(define (coroutine thunk)
  "Calls a procedure that can yield a continuation."
  (define (handler cont callback . args)
    (define (resume . args)
      ;; Call continuation that resumes the procedure.
      (call-with-prompt 'coroutine-prompt
			(lambda () (apply cont args))
			handler))
    (when (procedure? callback)
      (apply callback resume args)))

  ;; Call procedure.
  (call-with-prompt 'coroutine-prompt thunk handler))

;; emacs: (put 'colambda 'scheme-indent-function 1)
(define-syntax-rule (colambda args body ...)
  "Syntacic sugar for a lambda that is run as a coroutine."
  (lambda args
    (coroutine
     (lambda () body ...))))

;; emacs: (put 'codefine 'scheme-indent-function 1)
(define-syntax-rule (codefine (name ...) . body)
  "Syntactic sugar for defining a procedure that is run as a
coroutine."
  (define (name ...)
    ;; Create an inner procedure with the same signature so that a
    ;; recursive procedure call does not create a new prompt.
    (define (name ...) . body)
    (coroutine
      (lambda () (name ...)))))

;; emacs: (put 'codefine* 'scheme-indent-function 1)
(define-syntax-rule (codefine* (name . formals) . body)
  "Syntactic sugar for defining a procedure that is run as a
coroutine."
  (define (name . args)
    ;; Create an inner procedure with the same signature so that a
    ;; recursive procedure call does not create a new prompt.
    (define* (name . formals) . body)
    (coroutine
     (lambda () (apply name args)))))

(define (yield callback)
  "Yield continuation to a CALLBACK procedure."
  (abort-to-prompt 'coroutine-prompt callback))
