(use-modules (srfi srfi-9)
             (figl gl)
             (2d color)
             (2d font)
             (2d game)
             (2d vector2))

(define (demo-textbox)
  (make-textbox (load-font "fonts/Boxy-Bold.ttf" 48)
                "The quick brown fox jumped over the lazy dog."
                (vector2 240 160)
                white
                'left
                200))

(define-scene demo
  #:title  "Demo"
  #:draw   (lambda (textbox) (draw-textbox textbox))
  #:state  (demo-textbox))

(define-game fonts
  #:title       "Fonts"
  #:first-scene demo)

(run-game fonts)
