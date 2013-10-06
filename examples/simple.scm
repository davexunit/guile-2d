(use-modules (2d sprite)
             (2d game)
             (2d vector2))

(define (demo-sprite)
  (load-sprite "images/ghost.png"
               #:position (vector2 320 240)))

(define (draw sprite)
  (draw-sprite sprite))

(define-scene demo
  #:title  "Demo"
  #:draw   (lambda (sprite) (draw sprite))
  #:state  (demo-sprite))

(define-game simple
  #:title       "Simple Demo"
  #:first-scene demo)

(run-game simple)
