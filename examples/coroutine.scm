(use-modules (2d agenda)
             (2d coroutine)
             (2d game)
             (2d game-loop)
             (2d scene)
             (2d sprite)
             (2d stage)
             (2d vector2))

(define (enter sprite)
  ;; Simple script that moves the sprite to a random location every
  ;; second.
  (agenda-schedule
   (colambda ()
     (while #t
       (set-sprite-position!
        sprite
        (vector2 (random (vx (game-resolution coroutine-demo)))
                 (random (vy (game-resolution coroutine-demo)))))
       (wait 60)))))

(define coroutine-scene
  (make-scene
   "Coroutine"
   #:init (lambda ()
            (load-sprite "images/ghost.png"
                         #:position (vector2 320 240)))
   #:enter enter
   #:draw draw-sprite))

(define coroutine-demo
  (make-game
   #:title "Coroutines"
   #:first-scene coroutine-scene))

(run-game coroutine-demo)
