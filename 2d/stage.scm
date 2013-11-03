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
            stage-observer
            stage-env
            stage-scene
            enter-stage
            exit-stage
            draw-stage
            update-stage
            stage-trigger
            make-stage-variable
            define-stage-variable
            stage-on
            stage-off
            current-stage
            push-scene
            pop-scene
            replace-scene))

(define-record-type <stage>
  (%make-stage agenda scene state)
  stage?
  (agenda stage-agenda)
  (scene stage-scene)
  (state stage-state))

(define (make-stage scene)
  "Create a new stage object for SCENE."
  (%make-stage (make-agenda) scene (init-scene scene)))

;;;
;;; Scene callbacks
;;;

(define (enter-stage stage)
  "Call the scene enter callback for STAGE."
  (with-agenda (stage-agenda stage)
    (enter-scene (stage-scene stage)
                 (stage-state stage))))

(define (exit-stage stage)
  "Call the scene exit callback for STAGE."
  (with-agenda (stage-agenda stage)
    (exit-scene (stage-scene stage)
                (stage-state stage))))

(define (update-stage stage)
  "Call the scene update callback for STAGE."
  (with-agenda (stage-agenda stage)
    (update-agenda)
    (update-scene (stage-scene stage)
                  (stage-state stage))))

(define (draw-stage stage)
  "Call the scene draw callback for STAGE."
  (with-agenda (stage-agenda stage)
    (draw-scene (stage-scene stage)
                (stage-state stage))))

(define (stage-trigger stage event . args)
  (with-agenda (stage-agenda stage)
    #f))

;;;
;;; Stage management
;;;

(define stack '())

(define (current-stage)
  "Return the top of the stage stack or #f if the stack is empty."
  (if (null? stack) #f (car stack)))

(define (push-scene scene)
  "Make STAGE active and push it to the top of the stack."
  (let ((prev-stage (current-stage))
        (stage (make-stage scene)))
    (when prev-stage
      (exit-stage prev-stage))
    (set! stack (cons stage stack))
    (enter-stage stage)))

(define (pop-scene)
  "Replace the current stage with the next one on the stack, if
present."
  (let ((prev-stage (current-stage)))
    (when prev-stage
      (exit-stage prev-stage))
    (set! stack (cdr stack))
    (when (current-stage)
      (enter-stage (car stack)))))

(define (replace-scene scene)
  "Replace the current stage with STAGE."
  (let ((prev-stage (current-stage))
        (stage (make-stage scene)))
    (when prev-stage
      (exit-stage prev-stage))
    (set! stack (cons stage (cdr stack)))
    (enter-stage stage)))
