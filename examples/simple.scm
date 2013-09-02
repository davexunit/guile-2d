(use-modules (2d sprite)
             (2d game)
             (2d game-loop)
             (2d helpers)
             (2d vector2))

(define sprite
  (delay (load-sprite "images/sprite.png"
                      #:position (vector2 320 240))))

(define (key-down key mod unicode)
  (cond ((any-equal? key 'escape 'q)
         (quit-game-loop!))))

;; Draw our sprite
(define (render)
  (draw-sprite (force sprite)))

;; Register callbacks.
(add-hook! on-quit-hook quit-game-loop!)
(add-hook! on-render-hook (lambda () (render)))
(add-hook! on-key-down-hook (lambda (key mod unicode)
                              (key-down key mod unicode)))

(define-game simple
  #:title "Simple Demo")

(run-game simple)
