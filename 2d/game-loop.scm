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
;; Game loop.
;;
;;; Code:

(define-module (2d game-loop)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-11)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (figl gl)
  #:use-module (2d agenda)
  #:use-module (2d coroutine)
  #:use-module (2d private game)
  #:use-module (2d repl server)
  #:use-module (2d repl repl)
  #:use-module (2d mvars)
  #:use-module (2d window)
  #:export (current-fps
            push-scene
            replace-scene
            pop-scene
            run-game
            quit-game-loop!))

;;;
;;; Constants
;;;

(define target-fps 60)
(define tick-interval (floor (/ 1000 target-fps)))

;;;
;;; Mutable state
;;;

(define game-fps 0)
(define running #f)

;;;
;;; Event Handling
;;;

(define handle-events
  (let ((e (SDL:make-event)))
    (lambda ()
      "Handles all events in the SDL event queue."
      (while (SDL:poll-event e)
        (handle-event e)))))

(define (handle-event e)
  "Calls the relevant callback for the event."
  (case (SDL:event:type e)
    ((active)
     (scene-trigger current-scene 'active))
    ((video-resize)
     (scene-trigger current-scene
                    'resize
                    (SDL:event:resize:w e)
                    (SDL:event:resize:h e)))
    ((quit)
     (scene-trigger current-scene 'quit))
    ((key-down)
     (scene-trigger current-scene
                    'key-down
                    (SDL:event:key:keysym:sym e)
                    (SDL:event:key:keysym:mod e)
                    (SDL:event:key:keysym:unicode e)))
    ((key-up)
     (scene-trigger current-scene
                    'key-up
                    (SDL:event:key:keysym:sym e)
                    (SDL:event:key:keysym:mod e)
                    (SDL:event:key:keysym:unicode e)))
    ((mouse-motion)
     (scene-trigger current-scene
                    'mouse-motion
                    (SDL:event:motion:state e)
                    (SDL:event:motion:x e)
                    (SDL:event:motion:y e)
                    (SDL:event:motion:xrel e)
                    (SDL:event:motion:yrel e)))
    ((mouse-button-down)
     (scene-trigger current-scene
                    'mouse-press
                    (SDL:event:button:button e)
                    (SDL:event:button:x e)
                    (SDL:event:button:y e)))
    ((mouse-button-up)
     (scene-trigger current-scene
                    'mouse-click
                    (SDL:event:button:button e)
                    (SDL:event:button:x e)
                    (SDL:event:button:y e)))))

;;;
;;; Frames Per Second
;;;

(define accumulate-fps!
  (let* ((elapsed-time 0)
         (fps 0))
    (lambda (dt)
      "Increments. Resets to 0 every second."
      (let ((new-time (+ elapsed-time dt))
            (new-fps (1+ fps)))
        (if (>= new-time 1000)
            (begin
              (set! game-fps new-fps)
              (set! fps 0)
              (set! elapsed-time 0))
            (begin
              (set! fps new-fps)
              (set! elapsed-time new-time)))))))

(define (current-fps)
  "Returns the current FPS value."
  game-fps)

;;;
;;; Update and Render
;;;

(define (render dt)
  "Renders a frame."
  (set-gl-matrix-mode (matrix-mode modelview))
  (gl-load-identity)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (draw-scene current-scene)
  (SDL:gl-swap-buffers)
  (accumulate-fps! dt))

(define (update accumulator)
  "Call the update callback. The update callback will be called as
many times as tick-interval can divide accumulator. The return value
is the unused accumulator time."
  (if (>= accumulator tick-interval)
      (begin
        (handle-events)
        (update-agenda)
        (update-scene current-scene)
        (update (- accumulator tick-interval)))
      accumulator))

;;;
;;; REPL
;;;

(define (run-repl-thunk thunk input output error stack)
  "Run THUNK with the given REPL STACK. I/O is redirected to the given
INPUT, OUTPUT, and ERROR ports."
  (put-mvar
   repl-output-mvar
   (with-input-from-port input
     (lambda ()
       (with-output-to-port output
         (lambda ()
           (with-error-to-port error
             (lambda ()
               (with-fluids ((*repl-stack* stack))
                 (thunk))))))))))

(define (run-repl)
  "Execute a thunk from the REPL is there is one."
  (unless (mvar-empty? repl-input-mvar)
    (and-let* ((vals (try-take-mvar repl-input-mvar)))
              (apply run-repl-thunk vals))))

;;;
;;; Scene management
;;;

(define scenes '())
(define current-scene #f)
(define next-scene #f)

(define (push-scene scene)
  "Pause the current scene and start SCENE upon next game tick."
  (set! scenes (cons scene scenes))
  (set! next-scene scene))

(define (replace-scene scene)
  (set! scenes (cons scene (cdr scenes)))
  (set! next-scene scene))

(define (pop-scene)
  "Exit the current scene and resume the previous scene. If there is
no previous scene, the game loop will terminate."
  (if (null? scenes)
      (quit-game-loop!)
      (begin
        (set! next-scene (car scenes))
        (set! scenes (cdr scenes)))))

(define (switch-scenes-maybe)
  "Switch scenes if the current scene is not the scene at the top of
the stack."
  (when next-scene
    (scene-trigger current-scene 'stop)
    (set-current-scene next-scene)
    (set! next-scene #f)))

(define (set-current-scene scene)
  (scene-trigger scene 'start)
  (set! current-scene scene))

(define (set-initial-scene scene)
  (set! scenes (list scene))
  (set-current-scene scene))

;;;
;;; Game Loop
;;;

(define (game-loop last-time accumulator)
  "Runs input, render, and update hooks."
  (when running
    (let* ((current-time (SDL:get-ticks))
           (dt (- current-time last-time))
           (remainder (update (+ accumulator dt))))
      (run-repl)
      (render dt)
      (switch-scenes-maybe)
      (game-loop current-time
                 remainder))))

(define (run-game game)
  "Open a window and start playing GAME."
  (open-window (game-title game)
               (game-resolution game)
               (game-fullscreen? game))
  (set! running #t)
  (set-initial-scene ((game-first-scene game)))
  (spawn-server)
  (game-loop (SDL:get-ticks) 0)
  (close-window))

(define (quit-game-loop!)
  "Tell the game loop to finish up the current frame and then
terminate."
  (set! running #f))
