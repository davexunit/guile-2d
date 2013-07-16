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
            define-coroutine
            wait)
  #:replace (yield)
  #:use-module (2d agenda))

(define (execute-coroutine thunk)
  "Creates a procedure that can yield a continuation."
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

;; emacs: (put 'coroutine 'scheme-indent-function 0)
(define-syntax-rule (coroutine body ...)
  (begin
    (agenda-schedule
     (lambda ()
       (execute-coroutine
        (lambda () body ...))))))

;; emacs: (put 'define-coroutine 'scheme-indent-function 1)
(define-syntax define-coroutine
  (syntax-rules ()
    ((_ (name . args) . body)
     ;; Outer define for the execute-coroutine call.
     (define (name . args)
       ;; Make a new procedure with the same signature so that a
       ;; recursive procedure can be created without starting a new
       ;; coroutine with each call to itself.
       (define (name . args) . body)
       ;; Coroutine time.
       (execute-coroutine (lambda () (name . args)))))))

(define (yield callback)
  "Yield continuation to a callback procedure."
  (abort-to-prompt 'coroutine-prompt callback))

(define* (wait #:optional (delay 1))
  "Yield coroutine and schdule the continuation to be run after delay
ticks."
  (yield (lambda (resume) (agenda-schedule resume delay))))
