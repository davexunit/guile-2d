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
            run-game-loop))

(define target-fps 60)
(define frame-interval (/ 1000 target-fps))

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
;;; Update and Render
;;;

(define (render)
  "Renders a frame."
  (set-gl-matrix-mode (matrix-mode modelview))
  (gl-load-identity)
  (gl-clear (clear-buffer-mask color-buffer depth-buffer))
  (run-hook on-render-hook)
  (SDL:gl-swap-buffers))

(define (increment-fps fps fps-time)
  "Increment frames-per-second counter. Resets to 0 when the
difference between time and fps-time is greater than or equal to one
second."
  (if (>= fps-time 1000)
      (begin
        (pk 'FPS fps)
        0)
      (1+ fps)))

(define (update accumulator)
  "Call the update callback. The update callback will be called as
many times as frame-interval can divide accumulator. The return value
is the unused accumulator time."
  (if (>= accumulator frame-interval)
    (begin
      (run-hook on-update-hook)
      (update-agenda)
      (update (- accumulator frame-interval)))
    accumulator))

(define (time-left current-time next-time)
  (max (floor (- next-time current-time)) 0))

;;;
;;; Game Loop
;;;

(define (run-game-loop)
  "Runs event handling, update, and render loop."
  (define (game-loop last-time next-time fps-time accumulator fps)
    (handle-events)
    (let* ((time (SDL:get-ticks))
           (dt (- time last-time))
           (accumulator (+ accumulator dt))
           (current-fps-time (+ fps-time dt))
           (remainder (update accumulator)))
      (render)
      ;; Sleep for a bit if there's time in between frames
      (SDL:delay (time-left (SDL:get-ticks) next-time))
      (game-loop time
                 (+ next-time frame-interval)
                 (modulo current-fps-time 1000)
                 remainder
                 (increment-fps fps current-fps-time))))

  (let ((time (SDL:get-ticks)))
    (game-loop time (+ time frame-interval) 0 0 0)))
