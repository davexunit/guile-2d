(use-modules (figl gl)
             (2d color)
             (2d game-loop)
             (2d window)
             (2d helpers)
             (2d font))

(define window-width 800)
(define window-height 600)
(define font (load-font "fonts/Boxy-Bold.ttf" 48))
(define text "The quick brown fox jumped over the lazy dog.")
(define textbox (make-textbox font text #(320 300) white 'left 200))

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
    (with-gl-push-matrix
      (apply-color white)
      (draw-font font (format #f "FPS: ~d" fps))))
    (draw-textbox textbox))

;; Register callbacks.
(add-hook! on-quit-hook (lambda () (quit-demo)))
(add-hook! on-render-hook (lambda () (render)))
(add-hook! on-key-down-hook (lambda (key mod unicode) (key-down key mod unicode)))

;; Start the game loop.
;; The render callback will be called through this procedure.
(run-game-loop)
