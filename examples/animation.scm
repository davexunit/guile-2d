(use-modules (2d animation)
             (2d game)
             (2d game-loop)
             (2d scene)
             (2d sprite)
             (2d stage)
             (2d tileset)
             (2d vector2)
             (2d window))

(define (demo-animation)
  "Load a texture, split it into 64x64 tiles, and build an animated
sprite out of it."
  (let* ((tiles (load-tileset "images/princess.png" 64 64))
         (frames (vector (tileset-ref tiles 19)
                         (tileset-ref tiles 20)
                         (tileset-ref tiles 21)
                         (tileset-ref tiles 22)
                         (tileset-ref tiles 23)
                         (tileset-ref tiles 24)
                         (tileset-ref tiles 25)
                         (tileset-ref tiles 26))))
    (make-animation frames 6 #t)))

(define-stage-variable sprite
  (make-sprite (demo-animation)
               #:position (vector2 320 240)))

(define demo-scene
  (make-scene
   #:draw (lambda ()
            (draw-sprite (sprite)))))

(define animation-demo
  (make-game
   #:title       "Animation"
   #:first-scene demo-scene))

(run-game animation-demo)
