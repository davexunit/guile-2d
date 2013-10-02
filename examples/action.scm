(use-modules (2d actions)
             (2d agenda)
             (2d coroutine)
             (2d game)
             (2d sprite)
             (2d vector2))

(define (demo-sprite)
  (load-sprite "images/ghost.png"
               #:position (vector2 320 240)))

(define (start sprite)
  (let ((size (game-resolution actions)))
    (schedule-action
     (action-parallel
      ;; Move horizontally across the screen in 60 frames.
      (lerp (lambda (x)
              (set-sprite-position!
               sprite
               (vector2 x (/ (vy size) 2))))
            0 (vx size) 120)
      ;; Rotate 1080 degrees in 120 frames.
      (lerp (lambda (angle)
              (set-sprite-rotation! sprite angle))
            0 360 120)))))

(define-scene demo
  #:title  "Demo"
  #:draw   (lambda (sprite) (draw-sprite sprite))
  #:events (append
            (default-scene-events)
            `((start . ,(lambda (sprite) (start sprite)))))
  #:state  (demo-sprite))

(define-game actions
  #:title       "actions"
  #:first-scene demo)

(run-game actions)
