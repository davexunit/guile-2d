(use-modules (2d color)
             (2d game-loop)
             (2d window)
             (2d helpers)
             (2d font))

(define window-width 800)
(define window-height 600)
(define font (load-font "fonts/Boxy-Bold.ttf" 48))
(define text "The quick brown fox jumped over the lazy dog.")

;; Open the window.
(open-window window-width window-height)

(define (quit-demo)
  (close-window)
  (quit))

(define (key-down key mod unicode)
  (cond ((any-equal? key 'escape 'q)
         (quit-demo))))

;; Draw our sprite
(define (render)
  (let ((fps (floor (inexact->exact (current-fps)))))
    (render-font font
                 (format #f "FPS: ~d" fps)
                 0
                 0
                 white
                 #f))
  (render-font font
               "Hello, world!"
               320
               300
               green
               #f))

;; Register callbacks.
(add-hook! on-quit-hook (lambda () (quit-demo)))
(add-hook! on-render-hook (lambda () (render)))
(add-hook! on-key-down-hook (lambda (key mod unicode) (key-down key mod unicode)))

;; Start the game loop.
;; The render callback will be called through this procedure.
(run-game-loop)
