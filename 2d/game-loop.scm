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
  #:export (set-active-callback
            set-resize-callback
            set-quit-callback
            set-render-callback
            set-update-callback
            set-key-up-callback
            set-key-down-callback
            set-mouse-motion-callback
            set-mouse-button-down-callback
            set-mouse-button-up-callback
            run-game-loop))

(define target-fps 60)
(define frame-interval (/ 1000 target-fps))

;;;
;;; Callbacks
;;;

(define active-callback (lambda () #t))
(define resize-callback (lambda (width height) #t))
(define quit-callback (lambda () #t))
(define render-callback (lambda () #t))
(define update-callback (lambda () #t))
(define key-up-callback (lambda (key mod unicode) #t))
(define key-down-callback (lambda (key mod unicode) #t))
(define mouse-motion-callback (lambda (buttons x y xrel yrel) #t))
(define mouse-button-down-callback (lambda (button x y) #t))
(define mouse-button-up-callback (lambda (button x y) #t))

(define (set-active-callback callback)
  "Sets the active callback procedure."
  (set! active-callback callback))

(define (set-resize-callback callback)
  "Sets the resize callback procedure."
  (set! resize-callback callback))

(define (set-quit-callback callback)
  "Sets the quit callback procedure."
  (set! quit-callback callback))

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

(define (set-mouse-motion-callback callback)
  "Sets the mouse motion callback procedure."
  (set! mouse-motion-callback callback))

(define (set-mouse-button-down-callback callback)
  "Sets the mouse button down callback procedure."
  (set! mouse-button-down-callback callback))

(define (set-mouse-button-up-callback callback)
  "Sets the mouse button up callback procedure."
  (set! mouse-button-up-callback callback))

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
     (active-callback))
    ((video-resize)
     (resize-callback (SDL:event:resize:w e)
                      (SDL:event:resize:h e)))
    ((quit)
     (quit-callback))
    ((key-down)
     (key-down-callback (SDL:event:key:keysym:sym e)
                        (SDL:event:key:keysym:mod e)
                        (SDL:event:key:keysym:unicode e)))
    ((key-up)
     (key-up-callback (SDL:event:key:keysym:sym e)
                      (SDL:event:key:keysym:mod e)
                      (SDL:event:key:keysym:unicode e)))
    ((mouse-motion)
     (mouse-motion-callback (SDL:event:motion:state e)
                            (SDL:event:motion:x e)
                            (SDL:event:motion:y e)
                            (SDL:event:motion:xrel e)
                            (SDL:event:motion:yrel e)))
    ((mouse-button-down)
     (mouse-button-down-callback (SDL:event:button:button e)
                                 (SDL:event:button:x e)
                                 (SDL:event:button:y e)))
    ((mouse-button-up)
     (mouse-button-up-callback (SDL:event:button:button e)
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
  (render-callback)
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
      (update-callback)
      (update-agenda)
      (update (- accumulator frame-interval)))
    accumulator))

;;;
;;; Game Loop
;;;

(define (run-game-loop)
  "Runs event handling, update, and render loop."
  (define (game-loop time last-time fps-time accumulator fps)
    (handle-events)
    (let* ((dt (- time last-time))
           (accumulator (+ accumulator dt))
           (current-fps-time (+ fps-time dt))
           (current-time (SDL:get-ticks)))
      ;; Update and render when the accumulator reaches the threshold.
      (if (>= accumulator frame-interval)
          (let ((remainder (update accumulator)))
            (render)
            (game-loop current-time
                       time
                       (modulo current-fps-time 1000)
                       remainder
                       (increment-fps fps current-fps-time)))
          (game-loop current-time
                     time
                     current-fps-time
                     accumulator
                     fps))))

  (let ((time (SDL:get-ticks)))
    (game-loop time time 0 0 0)))
