(use-modules (2d sprite)
             (2d game-loop)
             (2d window)
             (2d helpers)
             (2d agenda)
             (2d coroutine)
             (2d actions)
             (2d vector2))

(define window-width 800)
(define window-height 600)

;; Open the window.
(open-window window-width window-height)

;; Load a sprite and center it on the screen.
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

(schedule-action
 (action-parallel
  ;; Move horizontally across the screen in 60 frames.
  (lerp (lambda (x)
          (set-sprite-position! sprite (vector2 x (/ window-height 2))))
        0 800 60)
  ;; Rotate 1080 degrees in 120 frames.
  (lerp (lambda (angle)
          (set-sprite-rotation! sprite angle))
        0 1080 60)))

(run-game-loop)
