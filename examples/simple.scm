(use-modules (2d game)
             (2d game-loop)
             (2d scene)
             (2d sprite)
             (2d vector2))

(define (make-demo-sprite)
  (load-sprite "images/ghost.png"
               #:position (vector2 320 240)))

(define simple-scene
  (make-scene
   "Simple"
   #:init make-demo-sprite
   #:draw draw-sprite))

(define simple-demo
  (make-game
   #:title "Simple Demo"
   #:first-scene simple-scene))

(run-game simple-demo)
