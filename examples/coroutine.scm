(use-modules (2d sprite)
             (2d game-loop)
             (2d window)
             (2d helpers)
             (2d agenda)
             (2d coroutine))

(init-2d)

(define window-width 800)
(define window-height 600)
(define sprite #f)

(define (key-down key mod unicode)
  (cond ((any-equal? key 'escape 'q)
         (close-window)
         (quit))))

;; Draw our sprite
(define (render)
  (draw-sprite sprite))

;; Register callbacks.
(set-render-callback (lambda () (render)))
(set-key-down-callback (lambda (key mod unicode) (key-down key mod unicode)))

;; Open the window.
(open-window window-width window-height)

;; Load a sprite and center it on the screen.
(set! sprite (load-sprite "images/sprite.png" #:position (vector (/ window-width 2)
                                                                 (/ window-height 2))))

;; Simple script that moves the sprite to a random location every
;; second.
(agenda-schedule
 (colambda ()
   (while #t
     (set-sprite-position! sprite (vector (random window-width)
                                          (random window-height)))
     (wait 60))))

(run-game-loop)
