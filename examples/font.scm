(use-modules (srfi srfi-9)
             (figl gl)
             (2d color)
             (2d font)
             (2d game)
             (2d game-loop)
             (2d scene)
             (2d stage)
             (2d vector2))

(define (demo-textbox)
  (make-textbox (load-font "fonts/Boxy-Bold.ttf" 48)
                "The quick brown fox jumped over the lazy dog."
                (vector2 240 160)
                white
                'left
                200))

(define demo-scene
  (make-scene
   #:init (lambda () (stage-define textbox (demo-textbox)))
   #:draw (lambda () (draw-textbox (stage-ref textbox)))))

(define fonts-demo
  (make-game
   #:title       "Fonts"
   #:first-scene demo-scene))

(run-game fonts-demo)
