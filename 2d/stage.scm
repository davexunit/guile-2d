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
;; Stages represent the game state at the present time.
;;
;;; Code:

(define-module (2d stage)
  #:use-module (srfi srfi-9)
  #:use-module (2d agenda)
  #:use-module (2d scene)
  #:export (make-stage
            stage?
            stage-agenda
            stage-env
            stage-scene
            init-stage
            enter-stage
            exit-stage
            draw-stage
            update-stage
            stage-define
            stage-ref
            stage-set!
            current-stage
            push-stage
            pop-stage
            replace-stage))

(define-record-type <stage>
  (%make-stage agenda env scene)
  stage?
  (agenda stage-agenda)
  (env stage-env)
  (scene stage-scene))

(define (make-stage scene)
  "Create a new stage object for SCENE."
  (%make-stage (make-agenda) (make-hash-table) scene))

;;;
;;; Scene callbacks
;;;

(define (init-stage stage)
  "Call the scene init callback for STAGE."
  ((scene-init (stage-scene stage))))

(define (enter-stage stage)
  "Call the scene enter callback for STAGE."
  ((scene-enter (stage-scene stage))))

(define (exit-stage stage)
  "Call the scene exit callback for STAGE."
  ((scene-exit (stage-scene stage))))

(define (update-stage stage)
  "Call the scene update callback for STAGE."
  ((scene-update (stage-scene stage))))

(define (draw-stage stage)
  "Call the scene draw callback for STAGE."
  ((scene-draw (stage-scene stage))))

;;;
;;; Stage environment
;;;

(define (%stage-define stage key value)
  "Define a new variable on STAGE with the specified symbolic name KEY
and arbitrary VALUE. An error is thrown when there is already a value
associated with KEY."
  (let ((env (stage-env stage)))
    (if (hash-get-handle env key)
        (error 'stage-already-defined-variable key)
        (hash-set! env key value))))

(define (%stage-ref stage key)
  "Return the value stored in STAGE associated with KEY. An error is
thrown if there is no value associated with KEY."
  (let ((handle (hash-get-handle (stage-env stage) key)))
    (if handle
        (cdr handle)
        (error 'stage-unbound-variable key))))

(define (%stage-set! stage key value)
  "Associate KEY with VALUE in the STAGE environment. An error is
thrown if there is no value associated with KEY."
  (let ((handle (hash-get-handle (stage-env stage) key)))
    (if handle
        (set-cdr! handle value)
        (error 'stage-unbound-variable key))))

(define-syntax-rule (stage-define key value)
  "Define the variable KEY with VALUE in the current stage
environment."
  (%stage-define (current-stage) 'key value))

(define-syntax-rule (stage-ref key)
  "Return the value bound to KEY in the current stage environment."
  (%stage-ref (current-stage) 'key))

(define-syntax-rule (stage-set! key value)
  "Assign VALUE to the variable bound to KEY in the current stage
environment."
  (%stage-set! (current-stage) 'key value))

;;;
;;; Stage management
;;;

(define stack '())

(define (current-stage)
  "Return the top of the stage stack or #f if the stack is empty."
  (if (null? stack) #f (car stack)))

(define (push-stage stage)
  "Make STAGE active and push it to the top of the stack."
  (let ((prev-stage (current-stage)))
    (when prev-stage
      (exit-stage prev-stage))
    (set! stack (cons stage stack))
    (init-stage stage)
    (enter-stage stage)))

(define (pop-stage)
  "Replace the current stage with the next one on the stack, if
present."
  (let ((prev-stage (current-stage)))
    (when prev-stage
      (exit-stage prev-stage))
    (set! stack (cdr stack))
    (when (current-stage)
      (enter-stage (car stack)))))

(define (replace-stage stage)
  "Replace the current stage with STAGE."
  (let ((prev-stage (current-stage)))
    (when prev-stage
      (exit-stage prev-stage))
    (set! stack (cons stage (cdr stack)))
    (init-stage stage)
    (enter-stage stage)))
