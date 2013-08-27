(use-modules (2d sprite)
             (2d game-loop)
             (2d window)
             (2d helpers)
             (2d agenda)
             (2d coroutine)
             (2d vector2))

(define window-width 800)
(define window-height 600)

;; Open the window.
(open-window window-width window-height)

(define sprite
  (load-sprite "images/sprite.png"
               #:position (vector2 (/ window-width 2)
                                   (/ window-height 2))))

(define (key-down key mod unicode)
  (cond ((any-equal? key 'escape 'q)
         (close-window)
         (quit))))

;; Draw our sprite
(define (render)
  (draw-sprite sprite))

;; Register callbacks.
(add-hook! on-render-hook (lambda () (render)))
(add-hook! on-key-down-hook (lambda (key mod unicode) (key-down key mod unicode)))

;; Simple script that moves the sprite to a random location every
;; second.
(agenda-schedule
 (colambda ()
   (while #t
     (set-sprite-position! sprite (vector2 (random window-width)
                                           (random window-height)))
     (wait 60))))

(run-game-loop)
