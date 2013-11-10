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
;; Game data structure.
;;
;;; Code:

(define-module (2d game)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-9)
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (figl gl)
  #:use-module (2d agenda)
  #:use-module (2d coroutine)
  #:use-module (2d game)
  #:use-module (2d mvars)
  #:use-module (2d repl server)
  #:use-module (2d repl repl)
  #:use-module (2d scene)
  #:use-module (2d stage)
  #:use-module (2d vector2)
  #:use-module (2d window)
  #:export (<game>
            make-game
            game?
            game-title
            game-resolution
            game-fullscreen?
            game-first-scene
            current-fps
            run-game
            quit-game
            pause-game
            resume-game
            game-running?
            game-paused?))

;;;
;;; Games
;;;

(define-record-type <game>
  (%make-game title resolution fullscreen? first-scene tick-rate)
  game?
  (title game-title)
  (resolution game-resolution)
  (fullscreen? game-fullscreen?)
  (first-scene game-first-scene)
  (tick-rate game-tick-rate))

(define* (make-game #:optional #:key
                    (title "A Guile-2D Game")
                    (resolution (vector2 640 480))
                    (fullscreen? #f)
                    (first-scene #f)
                    (tick-rate 60))
  "Return a new game. All game properties have some reasonable default
value."
  (%make-game title resolution fullscreen? first-scene tick-rate))

(define tick-interval 0)
(define running? #f)
(define paused? #f)

(define (game-running?)
  running?)

(define (game-paused?)
  paused?)

(define (run-game game)
  "Open a window and start the game loop for GAME."
  (open-window (game-title game)
               (game-resolution game)
               (game-fullscreen? game))
  (set! running? #t)
  (set! tick-interval (floor (/ 1000 (game-tick-rate game))))
  (resume-game)
  (push-scene (game-first-scene game))
  (spawn-server)
  (game-loop (SDL:get-ticks) 0)
  (close-window))

;;;
;;; Game Loop
;;;

(define (update-and-render stage dt accumulator)
  (let ((remainder (update stage accumulator)))
    (run-repl)
    (render stage dt)
    remainder))

(define (tick dt accumulator)
  "Advance the game by one frame."
  (if paused?
      (begin
        (run-repl)
        (SDL:delay tick-interval)
        accumulator)
      (catch #t
        (lambda ()
          (let ((stage (current-stage)))
            (if stage
                (update-and-render stage dt accumulator)
                (quit-game))))
        (lambda (key . args)
          (pause-game)
          accumulator)
        (lambda (key . args)
          (display-backtrace (make-stack #t)
                             (current-output-port))))))

(define (game-loop last-time accumulator)
  "Update game state, and render. LAST-TIME is the time in
milliseconds of the last iteration of the loop. ACCUMULATOR is the
time in milliseconds that has passed since the last game update."
  (when running?
    (let* ((current-time (SDL:get-ticks))
           (dt (- current-time last-time))
           (accumulator (+ accumulator dt)))
      (game-loop current-time (tick dt accumulator)))))

(define (pause-game)
  "Pauses the game loop. Useful when developing."
  (set! paused? #t))

(define (resume-game)
  "Resumes the game loop."
  (set! paused? #f))

(define (quit-game)
  "Finish the current frame and terminate the game loop."
  (set! running? #f))

;;;
;;; Event Handling
;;;

;; By default, pressing the escape key will pop the current scene, and
;; closing the window will quit the game.
(default-events `((key-down . ,(lambda (state key mod unicode)
                                 (when (eq? key 'escape)
                                   (pop-scene))))
                  (quit . ,(lambda (state)
                             (quit-game)))))

(define handle-events
  (let ((e (SDL:make-event)))
    (lambda (stage)
      "Handle all events in the SDL event queue."
      (while (SDL:poll-event e)
        (handle-event stage e)))))

(define (handle-event stage e)
  "Call the relevant callbacks for the event, E."
  (case (SDL:event:type e)
    ((active)
     (stage-trigger stage 'active))
    ((video-resize)
     (stage-trigger stage
                    'resize
                    (SDL:event:resize:w e)
                    (SDL:event:resize:h e)))
    ((quit)
     (stage-trigger stage 'quit))
    ((key-down)
     (stage-trigger stage
                    'key-down
                    (SDL:event:key:keysym:sym e)
                    (SDL:event:key:keysym:mod e)
                    (SDL:event:key:keysym:unicode e)))
    ((key-up)
     (stage-trigger stage
                    'key-up
                    (SDL:event:key:keysym:sym e)
                    (SDL:event:key:keysym:mod e)
                    (SDL:event:key:keysym:unicode e)))
    ((mouse-motion)
     (stage-trigger stage
                    'mouse-motion
                    (SDL:event:motion:state e)
                    (SDL:event:motion:x e)
                    (SDL:event:motion:y e)
                    (SDL:event:motion:xrel e)
                    (SDL:event:motion:yrel e)))
    ((mouse-button-down)
     (stage-trigger stage
                    'mouse-press
                    (SDL:event:button:button e)
                    (SDL:event:button:x e)
                    (SDL:event:button:y e)))
    ((mouse-button-up)
     (stage-trigger stage
                    'mouse-click
                    (SDL:event:button:button e)
                    (SDL:event:button:x e)
                    (SDL:event:button:y e)))))

;;;
;;; Frames Per Second
;;;

(define game-fps 0)

(define accumulate-fps!
  (let* ((elapsed-time 0)
         (fps 0))
    (lambda (dt)
      "Increment the frames-per-second counter. Resets to 0 every
second."
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
  "Return the current FPS value."
  game-fps)

;;;
;;; Update and Render
;;;

(define (render stage dt)
  "Render a frame."
  (set-gl-matrix-mode (matrix-mode modelview))
  (gl-load-identity)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (draw-stage stage)
  (SDL:gl-swap-buffers)
  (accumulate-fps! dt))

(define (update stage accumulator)
  "Call the update callback. The update callback will be called as
many times as `tick-interval` can divide ACCUMULATOR. The return value
is the unused accumulator time."
  (if (>= accumulator tick-interval)
      (begin
        (handle-events stage)
        (update-agenda)
        (update-stage stage)
        (update stage (- accumulator tick-interval)))
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
