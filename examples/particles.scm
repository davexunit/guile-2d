;; load the SDL module and some useful srfi's
(use-modules (srfi srfi-1)
             (srfi srfi-9)
             (2d game)
             (2d sprite)
             (2d texture)
             (2d vector2))

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

(define (update-particle! particle)
  (set-particle-position! particle
                          (v+ (particle-position particle)
                              (particle-velocity particle))))

;;;
;;; Demo
;;;

(define-record-type <demo-state>
  (make-demo-state stars particles)
  demo-state?
  (stars demo-stars)
  (particles demo-particles))

(define (generate-particles n)
  (let ((particle-image (load-texture "images/bullet.png"))
        (game-size (game-resolution particles)))
    (list-tabulate n (lambda (n)
                       (make-particle (make-sprite particle-image)
                                      (vector2 (random (vx game-size))
                                               (random (vy game-size)))
                                      (vector2 (* (random:normal) 1)
                                               (* (random:normal) 1)))))))

(define particle-count 500)
(define batch (make-sprite-batch (* particle-count 4)))

(define (draw-particles particles)
  (with-sprite-batch batch
    (for-each
     (lambda (p)
       (let* ((sprite (particle-sprite p)))
         (set-sprite-position! sprite (particle-position p))
         (draw-sprite sprite)))
     particles)))

(define (draw state)
  (draw-sprite (demo-stars state))
  (draw-particles (demo-particles state)))

(define (update state)
  (for-each update-particle! (demo-particles state)))

(define-scene demo
  #:title  "Demo"
  #:draw   (lambda (state) (draw state))
  #:update (lambda (state) (update state))
  #:state  (make-demo-state (load-sprite "images/stars.png"
                                         #:anchor null-vector2)
                            (generate-particles particle-count)))

(define-game particles
  #:title       "Particles"
  #:first-scene demo)

(run-game particles)
