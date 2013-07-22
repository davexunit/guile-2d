(use-modules ((sdl sdl) #:prefix SDL:)
             (figl gl)
             (2d sprite)
             (2d game-loop)
             (2d window)
             (2d helpers))

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
;; Must be done AFTER opening the window.
(set! sprite (load-sprite "images/sprite.png" #:position (vector (/ window-width 2)
                                                                 (/ window-height 2))))

;; Start the game loop.
;; The render callback will be called through this procedure.
(run-game-loop)
