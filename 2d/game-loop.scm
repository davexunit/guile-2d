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
  #:export (set-render-callback
            set-update-callback
            set-key-up-callback
            set-key-down-callback
            run-game-loop))

(define target-fps 60)

;;;
;;; Callbacks
;;;

(define render-callback (lambda () #t))
(define update-callback (lambda () #t))
(define key-up-callback (lambda (key) #t))
(define key-down-callback (lambda (key) #t))

(define (set-render-callback callback)
  "Sets the render callback procedure."
  (set! render-callback callback))

(define (set-update-callback callback)
  "Sets the update callback procedure."
  (set! update-callback callback))

(define (set-key-up-callback callback)
  "Sets the key up callback procedure."
  (set! key-up-callback callback))

(define (set-key-down-callback callback)
  "Sets the key down callback procedure."
  (set! key-down-callback callback))

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
    ((SDL_KEYDOWN)
     (key-down-callback (SDL:event:key:keysym:sym e)))
    ((SDL_KEYUP)
     (key-up-callback (SDL:event:key:keysym:sym e)))))

;;;
;;; Update and Render
;;;

(define (render)
  "Renders a frame."
  (set-gl-matrix-mode (matrix-mode modelview))
  (gl-load-identity)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (render-callback)
  (SDL:gl-swap-buffers))

(define accumulate-fps
  (let ((last-time 0)
        (fps 0))
    (lambda ()
      "Calculates frames per second."
      (let ((time (SDL:get-ticks)))
        (set! fps (1+ fps))
        (when (>= time (+ last-time 1000))
          (pk 'FPS fps)
          (set! last-time time)
          (set! fps 0))))))

(define update-and-render
  (let ((last-update 0)
        (update-interval (/ 1000 target-fps)))
    (lambda ()
      "Calls update and draw callback when enough time has passed since
the last tick."
      (let ((time (SDL:get-ticks)))
        (when (>= time (+ last-update update-interval))
          (set! last-update time)
          (update-callback)
          (accumulate-fps)
          (render))))))

;;;
;;; Game Loop
;;;

(define (run-game-loop)
  "Runs event handling, update, and render loop."
  (while #t
    (handle-events)
    (update-and-render)))
