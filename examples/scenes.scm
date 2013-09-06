(use-modules (2d sprite)
             (2d game)
             (2d vector2))

;; Press the RETURN key to toggle between the 2 scenes.

;;;
;;; Scene 1
;;;

(define (make-sprite-1)
  (load-sprite "images/sprite.png"
               #:position (vector2 320 240)))

(define (scene-1-key-press sprite key mod unicode)
  (when (eq? key 'return)
    (replace-scene (scene-2))))

(define-scene scene-1
  #:title  "Scene 1"
  #:draw   (lambda (sprite) (draw-sprite sprite))
  #:events (append
            (default-scene-events)
            `((start    . ,(lambda (state) (display "Start Scene 1\n")))
              (stop     . ,(lambda (state) (display "Stop Scene 1\n")))
              (key-down . ,(lambda (state key mod unicode)
                             (scene-1-key-press state key mod unicode)))))
  #:state  (make-sprite-1))

;;;
;;; Scene 2
;;;

(define (make-sprite-2)
  (load-sprite "images/stars.png"
               #:position (vector2 320 240)))

(define (scene-2-key-press sprite key mod unicode)
  (when (eq? key 'return)
    (replace-scene (scene-1))))

(define-scene scene-2
  #:title  "Scene 2"
  #:draw   (lambda (sprite) (draw-sprite sprite))
  #:events (append
            (default-scene-events)
            `((start    . ,(lambda (state) (display "Start Scene 2\n")))
              (stop     . ,(lambda (state) (display "Stop Scene 2\n")))
              (key-down . ,(lambda (state key mod unicode)
                             (scene-2-key-press state key mod unicode)))))
  #:state  (make-sprite-2))

(define-game scenes
  #:title       "Scenes"
  #:first-scene scene-1)

(run-game scenes)
