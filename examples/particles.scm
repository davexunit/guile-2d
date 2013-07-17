;; load the SDL module and some useful srfi's
(use-modules ((sdl sdl) #:prefix SDL:)
             (figl gl)
             (srfi srfi-1)
             (srfi srfi-2)
             (srfi srfi-9)
             (ice-9 format)
             (2d sprite)
             (2d game-loop)
             (2d window)
             (2d vector)
             (2d input)
             (2d helpers))

(set! *random-state* (random-state-from-platform))
(init-2d)

;;;
;;; Particles
;;;

(define-record-type <particle>
  (make-particle sprite position velocity)
  particle?
  (sprite particle-sprite)
  (position particle-position set-particle-position!)
  (velocity particle-velocity set-particle-velocity!))

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

(define stars (load-sprite "images/stars.png" #:anchor #(0 0)))
(define particle-image (load-texture "images/bullet.png"))
(define particle-width (texture-width particle-image))
(define particle-height (texture-height particle-image))
(define particle-count 500)
(define particles
  (list-tabulate particle-count
                 (lambda (n)
                   (make-particle (make-sprite particle-image)
                                  (vector (random window-width)
                                          (random window-height))
                                  (vector (* (random:normal) 1)
                                          (* (random:normal) 1))))))
(define batch (make-sprite-batch (* particle-count 4)))

(define (draw-particles particles)
  (with-sprite-batch batch
    (for-each
     (lambda (p)
       (let* ((sprite (particle-sprite p)))
         (set-sprite-position! sprite (particle-position p))
         (draw-sprite sprite)))
     particles)))

(set-render-callback (lambda () (render)))
(set-update-callback (lambda () (update)))
(set-key-down-callback (lambda (key mod unicode) (key-down key mod unicode)))

(define (key-down key mod unicode)
  (cond ((any-equal? key (keycode escape) (keycode q))
         (close-window)
         (quit))))

(define (render)
  (draw-sprite stars)
  (draw-particles particles))

(define (update)
  (for-each update-particle! particles))

(run-game-loop)
