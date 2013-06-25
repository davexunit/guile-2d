;; load the SDL module and some useful srfi's
(use-modules ((sdl sdl) #:prefix SDL:)
             (figl gl)
             (srfi srfi-1)
             (srfi srfi-2)
             (srfi srfi-9)
             (ice-9 format)
             (2d texture)
             (2d sprite)
             (2d game-loop)
             (2d window)
             (2d vector))

(set! *random-state* (random-state-from-platform))

;;;
;;; Particles
;;;

(define-record-type <particle>
  (make-particle sprite position velocity)
  particle?
  (sprite particle-sprite)
  (position particle-position set-particle-position!)
  (velocity particle-velocity set-particle-velocity!))

(define (draw-particle particle)
  (let* ((texture (sprite-texture (particle-sprite particle)))
         (p (particle-position particle))
         (x (vx p))
         (y (vy p))
         (x2 (+ x (texture-width texture)))
         (y2 (+ y (texture-height texture))))
    (gl-texture-coordinates 0 0)
    (gl-vertex x y)
    (gl-texture-coordinates 1 0)
    (gl-vertex x2 y)
    (gl-texture-coordinates 1 1)
    (gl-vertex x2 y2)
    (gl-texture-coordinates 0 1)
    (gl-vertex x y2)))

(define (draw-particles particles)
  (let ((texture (sprite-texture (particle-sprite (car particles)))))
    (with-gl-bind-texture (texture-target texture-2d) (texture-id texture)
      (gl-begin (begin-mode quads)
        (gl-color 1 1 1)
        (for-each (lambda (p) (draw-particle p)) particles)))))

(define (update-particle! particle)
  (set-particle-position! particle
                          (v+ (particle-position particle)
                              (particle-velocity particle))))

;;;
;;; Demo
;;;

(define window-width 800)
(define window-height 600)

(open-window window-width window-height)

(define stars (load-sprite "stars.png" #:anchor #(0 0)))
(define particle-image (load-texture "bullet.png"))
(define num-particles 300)
(define particles
  (list-tabulate num-particles
                 (lambda (n)
                   (make-particle (make-sprite particle-image)
                                  (vector (random window-width)
                                          (random window-height))
                                  (vector (* (random:normal) 1)
                                           (* (random:normal) 1))))))

(set-render-callback (lambda () (render)))
(set-update-callback (lambda () (update)))
(set-key-down-callback (lambda (key) (key-down key)))

(define (key-down key)
  (case key
    ((SDLK_ESCAPE SDLK_q)
     (close-window)
     (quit))))

(define (render)
  (draw-sprite stars)
  (draw-particles particles))

(define (update)
  (for-each update-particle! particles))

(run-game-loop)
