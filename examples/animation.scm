(use-modules (2d animation)
             (2d game-loop)
             (2d helpers)
             (2d sprite)
             (2d texture)
             (2d vector)
             (2d window))

(init-2d)

(define window-width 800)
(define window-height 600)
(define sprite #f)

(define (key-down key mod unicode)
  (cond ((any-equal? key 'escape 'q)
         (close-window)
         (quit))))

;; Draw our sprite
(define (render)
  (draw-sprite sprite))

;; Register callbacks.
(set-render-callback (lambda () (render)))
(set-key-down-callback (lambda (key mod unicode) (key-down key mod unicode)))

;; Open the window.
(open-window window-width window-height)

;; Load a texture, split it into 64x64 tiles, and build an animated
;; sprite out of it.
(let* ((tiles (split-texture (load-texture "images/princess.png") 64 64))
       (frames (vector (vector-ref tiles 19)
                       (vector-ref tiles 20)
                       (vector-ref tiles 21)
                       (vector-ref tiles 22)
                       (vector-ref tiles 23)
                       (vector-ref tiles 24)
                       (vector-ref tiles 25)
                       (vector-ref tiles 26)))
       (animation (make-animation frames 6 #t)))
  (set! sprite (make-sprite animation
                            #:position (vector (/ window-width 2)
                                               (/ window-height 2)))))

(run-game-loop)
