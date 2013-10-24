(use-modules (2d agenda)
             (2d coroutine)
             (2d game)
             (2d game-loop)
             (2d scene)
             (2d sprite)
             (2d stage)
             (2d vector2))

(define (demo-sprite)
  (load-sprite "images/ghost.png"
               #:position (vector2 320 240)))

(define (init)
  ;; Simple script that moves the sprite to a random location every
  ;; second.
  (stage-define sprite (demo-sprite))
  (agenda-schedule
   (colambda ()
     (while #t
       (set-sprite-position!
        (stage-ref sprite)
        (vector2 (random (vx (game-resolution coroutine-demo)))
                 (random (vy (game-resolution coroutine-demo)))))
       (wait 60)))))

(define demo-scene
  (make-scene
   #:init init
   #:draw (lambda () (draw-sprite (stage-ref sprite)))))

(define coroutine-demo
  (make-game
   #:title       "Coroutines"
   #:first-scene demo-scene))

(run-game coroutine-demo)
