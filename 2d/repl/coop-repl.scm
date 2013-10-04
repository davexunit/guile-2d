(define-module (2d repl coop-repl)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (system repl common)
  #:use-module (system repl repl)
  #:use-module (system repl error-handling)
  #:use-module (2d agenda)
  #:use-module (2d coroutine)
  #:use-module (2d mvars)
  #:export (start-repl))

(define-record-type <coop-repl>
  (%make-coop-repl repl read-mvar eval-mvar)
  coop-repl?
  (repl coop-repl-repl)
  (read-mvar coop-repl-read-mvar)
  (eval-mvar coop-repl-eval-mvar))

(define (make-coop-repl lang debug)
  (%make-coop-repl (make-repl lang debug)
                   (new-empty-mvar)
                   (new-empty-mvar)))

(define repl-wait-time 6)

(define* (start-repl #:optional (lang (current-language)) #:key debug)
  ;; ,language at the REPL will update the current-language.  Make
  ;; sure that it does so in a new dynamic scope.
  (parameterize ((current-language lang))
    (let ((coop-repl (make-coop-repl lang debug)))
      (call-with-new-thread (lambda ()
                              (reader-thread-loop coop-repl)))
      (repl-option-set! (coop-repl-repl coop-repl)
                        'read-wrapper
                        (lambda (reader)
                          (repl-read-wrapper coop-repl reader)))
      (run-repl (coop-repl-repl coop-repl)))))

(define (repl-read-wrapper coop-repl reader)
  (put-mvar (coop-repl-read-mvar coop-repl) reader)
  (yield (lambda (resume) (repl-read-callback coop-repl resume))))

(define (repl-read-callback coop-repl resume)
  (define (try-to-resume)
    (receive (exp success?)
        (try-take-mvar (coop-repl-eval-mvar coop-repl))
      (if success?
          (resume exp)
          (agenda-schedule try-to-resume repl-wait-time))))
  (agenda-schedule try-to-resume repl-wait-time))

(define (reader-thread-loop coop-repl)
  (put-mvar (coop-repl-eval-mvar coop-repl)
            ((take-mvar (coop-repl-read-mvar coop-repl))))
  (reader-thread-loop coop-repl))
