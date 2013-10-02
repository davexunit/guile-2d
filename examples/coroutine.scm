(use-modules (2d agenda)
             (2d coroutine)
             (2d game)
             (2d sprite)
             (2d vector2))

(define (demo-sprite)
  (load-sprite "images/ghost.png"
               #:position (vector2 320 240)))

(define (start sprite)
  ;; Simple script that moves the sprite to a random location every
  ;; second.
  (agenda-schedule
   (colambda ()
     (while #t
       (set-sprite-position!
        sprite
        (vector2 (random (vx (game-resolution coroutines)))
                 (random (vy (game-resolution coroutines)))))
       (wait 60)))))

(define-scene demo
  #:title  "Demo"
  #:draw   (lambda (sprite) (draw-sprite sprite))
  #:events (append
            (default-scene-events)
            `((start . ,(lambda (sprite) (start sprite)))))
  #:state  (demo-sprite))

(define-game coroutines
  #:title       "Coroutines"
  #:first-scene demo)

(run-game coroutines)
