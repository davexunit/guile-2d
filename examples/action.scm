(use-modules (2d actions)
             (2d agenda)
             (2d coroutine)
             (2d game)
             (2d scene)
             (2d sprite)
             (2d stage)
             (2d vector2))

(define (enter sprite)
  (let ((size (game-resolution actions-demo)))
    (schedule-action
     (action-parallel
      ;; Move horizontally across the screen in 60 frames.
      (lerp (lambda (x)
              (set-sprite-position!
               sprite
               (vector2 x (/ (vy size) 2))))
            0 (vx size) 60)
      ;; Rotate 1080 degrees in 120 frames.
      (lerp (lambda (angle)
              (set-sprite-rotation! sprite angle))
            0 360 120)))))

(define actions-scene
  (make-scene
   "Actions"
   #:init (lambda ()
            (load-sprite "images/ghost.png"
                         #:position (vector2 320 240)))
   #:enter enter
   #:draw draw-sprite))

(define actions-demo
  (make-game
   #:title "Actions"
   #:first-scene actions-scene))

(run-game actions-demo)
