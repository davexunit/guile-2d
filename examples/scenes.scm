(use-modules (2d game)
             (2d game-loop)
             (2d scene)
             (2d stage)
             (2d sprite)
             (2d vector2))

;; Press the RETURN key to toggle between the 2 scenes.

;;;
;;; Scene 1
;;;

(define (make-sprite-1)
  (load-sprite "images/ghost.png"
               #:position (vector2 320 240)))

(define (scene-1-key-down sprite key mod unicode)
  (when (eq? key 'return)
    (replace-scene scene-2)))

(define scene-1
  (make-scene
   "Scene 1"
   #:init make-sprite-1
   #:enter (lambda (sprite) (display "Enter Scene 1\n"))
   #:exit (lambda (sprite) (display "Exit Scene 1\n"))
   #:draw draw-sprite
   #:events (append
             (default-events)
             `((key-down . ,scene-1-key-down)))))

;;;
;;; Scene 2
;;;

(define (make-sprite-2)
  (load-sprite "images/stars.png"
               #:position (vector2 320 240)))

(define (scene-2-key-down sprite key mod unicode)
  (when (eq? key 'return)
    (replace-scene scene-1)))

(define scene-2
  (make-scene
   "Scene 2"
   #:init make-sprite-2
   #:enter (lambda (sprite) (display "Enter Scene 2\n"))
   #:exit (lambda (sprite) (display "Exit Scene 2\n"))
   #:draw draw-sprite
   #:events (append
             (default-events)
             `((key-down . ,scene-2-key-down)))))

(define scenes-demo
  (make-game
   #:title "Scenes"
   #:first-scene scene-1))

(run-game scenes-demo)
