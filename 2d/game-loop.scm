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
  #:use-module ((sdl sdl) #:prefix SDL:)
  #:use-module (figl gl)
  #:use-module (2d agenda)
  #:use-module (2d coroutine)
  #:use-module (system repl server)
  #:export (on-active-hook
            on-resize-hook
            on-quit-hook
            on-render-hook
            on-update-hook
            on-key-up-hook
            on-key-down-hook
            on-mouse-motion-hook
            on-mouse-button-down-hook
            on-mouse-button-up-hook
            run-game-loop
            current-fps))

;;;
;;; Constants
;;;

(define target-fps 60)
(define tick-interval (floor (/ 1000 target-fps)))

;;;
;;; Globals
;;;

(define *fps* 0)
;; The REPL sets this flag when it needs to evaluate something.
;; Only the REPL server thread will mutate this variable.
(define *repl-waiting* #f)
(define game-loop-mutex (make-mutex 'unchecked-unlock))

;;;
;;; REPL Hooks
;;;

;; Lock game loop mutex before evaluating code from REPL server and
;; unlock it afterwards.
(add-hook! before-eval-hook
           (lambda (exp)
             (set! *repl-waiting* #t)
             (lock-mutex game-loop-mutex)))

(add-hook! after-eval-hook
           (lambda (exp)
             (set! *repl-waiting* #f)
             (when (equal? (mutex-owner game-loop-mutex) (current-thread))
               (unlock-mutex game-loop-mutex))))

;;;
;;; Hooks
;;;

(define on-active-hook (make-hook))
(define on-resize-hook (make-hook 2))
(define on-quit-hook (make-hook))
(define on-render-hook (make-hook))
(define on-update-hook (make-hook))
(define on-key-up-hook (make-hook 3))
(define on-key-down-hook (make-hook 3))
(define on-mouse-motion-hook (make-hook 5))
(define on-mouse-button-down-hook (make-hook 3))
(define on-mouse-button-up-hook (make-hook 3))

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
     (run-hook on-active-hook))
    ((video-resize)
     (run-hook on-resize-hook (SDL:event:resize:w e)
               (SDL:event:resize:h e)))
    ((quit)
     (run-hook on-quit-hook))
    ((key-down)
     (run-hook on-key-down-hook
               (SDL:event:key:keysym:sym e)
               (SDL:event:key:keysym:mod e)
               (SDL:event:key:keysym:unicode e)))
    ((key-up)
     (run-hook on-key-up-hook
               (SDL:event:key:keysym:sym e)
               (SDL:event:key:keysym:mod e)
               (SDL:event:key:keysym:unicode e)))
    ((mouse-motion)
     (run-hook on-mouse-motion-hook
               (SDL:event:motion:state e)
               (SDL:event:motion:x e)
               (SDL:event:motion:y e)
               (SDL:event:motion:xrel e)
               (SDL:event:motion:yrel e)))
    ((mouse-button-down)
     (run-hook on-mouse-button-down-hook
               (SDL:event:button:button e)
               (SDL:event:button:x e)
               (SDL:event:button:y e)))
    ((mouse-button-up)
     (run-hook on-mouse-button-up-hook
               (SDL:event:button:button e)
               (SDL:event:button:x e)
               (SDL:event:button:y e)))))

;;;
;;; Frames Per Second
;;;

(define accumulate-fps!
  (let* ((frame-time 0)
         (alpha 1/5)
         (inverse-alpha (- 1 alpha)))
    (lambda (dt)
      "Computes a weighted average FPS."
      (set! frame-time (+ (* alpha dt) (* inverse-alpha frame-time)))
      (unless (zero? frame-time)
        (set! *fps* (/ 1000 frame-time))))))

(define (current-fps)
  "Returns the current FPS value."
  *fps*)

(codefine (show-fps)
  "Display the current FPS every second."
  (wait 60)
  (pk 'FPS (floor *fps*))
  (show-fps))

;;;
;;; Update and Render
;;;

(define (render)
  "Renders a frame."
  (set-gl-matrix-mode (matrix-mode modelview))
  (gl-load-identity)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (run-hook on-render-hook)
  (SDL:gl-swap-buffers))

(define (update accumulator)
  "Call the update callback. The update callback will be called as
many times as tick-interval can divide accumulator. The return value
is the unused accumulator time."
  (if (>= accumulator tick-interval)
    (begin
      (run-hook on-update-hook)
      (update-agenda)
      (update (- accumulator tick-interval)))
    accumulator))

(define (time-left current-time next-time)
  (max (floor (- next-time current-time)) 0))

(define (frame-sleep time)
  "Sleep for time milliseconds. Unlock the mutex beforehand if the
REPL server is waiting to evaluate something."
  (if *repl-waiting*
      (begin
        (unlock-mutex game-loop-mutex)
        (SDL:delay time)
        (lock-mutex game-loop-mutex))
      (SDL:delay time)))

;;;
;;; Game Loop
;;;

(define (game-loop last-time next-time accumulator)
  "Runs input, render, and update hooks."
  (handle-events)
  (let* ((time (SDL:get-ticks))
         (dt (- time last-time))
         (accumulator (+ accumulator dt))
         (remainder (update accumulator)))
    (render)
    (accumulate-fps! dt)
    (frame-sleep (time-left (SDL:get-ticks) next-time))
    (game-loop time
               (+ next-time tick-interval)
               remainder)))

(define (run-game-loop)
  "Spawns a REPL server and starts the main game loop."
  (spawn-server)
  (lock-mutex game-loop-mutex)
  (agenda-schedule show-fps)
  (let ((time (SDL:get-ticks)))
    (game-loop time (+ time tick-interval) 0)))
