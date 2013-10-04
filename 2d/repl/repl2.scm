(define-module (2d repl repl2)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (system repl common)
  #:use-module (system repl repl)
  #:use-module (system repl error-handling)
  #:use-module (2d agenda)
  #:use-module (2d coroutine)
  #:use-module (2d mvars)
  #:export (start-repl))

(define repl-read-mvar  (new-empty-mvar))
(define repl-eval-mvar (new-empty-mvar))
(define repl-wait-time 5)
(define repl-thread #f)

(define* (start-repl #:optional (lang (current-language)) #:key debug)
  ;; ,language at the REPL will update the current-language.  Make
  ;; sure that it does so in a new dynamic scope.
  (set! repl-thread (call-with-new-thread reader-thread-loop))
  (parameterize ((current-language lang))
    (let ((repl (make-repl lang debug)))
      (repl-option-set! repl 'read-wrapper repl-read-wrapper)
      (run-repl repl))))

(define (repl-read-wrapper reader)
  (put-mvar repl-read-mvar reader)
  (yield repl-read-callback))

(define (repl-read-callback resume)
  (define (try-to-resume)
    (receive (exp success?) (try-take-mvar repl-eval-mvar)
      (if success?
          (resume exp)
          (agenda-schedule try-to-resume repl-wait-time))))
  (agenda-schedule try-to-resume repl-wait-time))

(define (reader-thread-loop)
  (put-mvar repl-eval-mvar ((take-mvar repl-read-mvar)))
  (reader-thread-loop))
