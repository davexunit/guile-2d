(use-modules (2d animation)
             (2d game-loop)
             (2d helpers)
             (2d sprite)
             (2d texture)
             (2d vector)
             (2d window))

(define window-width 800)
(define window-height 600)

;; Open the window.
(open-window window-width window-height)

(define (key-down key mod unicode)
  (cond ((any-equal? key 'escape 'q)
         (close-window)
         (quit))))

;; Draw our sprite
(define (render)
  (draw-sprite sprite))

;; Register callbacks.
(add-hook! on-render-hook (lambda () (render)))
(add-hook! on-key-down-hook (lambda (key mod unicode) (key-down key mod unicode)))

;; Load a texture, split it into 64x64 tiles, and build an animated
;; sprite out of it.
(define animation
  (let* ((tiles (split-texture (load-texture "images/princess.png") 64 64))
         (frames (vector (vector-ref tiles 19)
                         (vector-ref tiles 20)
                         (vector-ref tiles 21)
                         (vector-ref tiles 22)
                         (vector-ref tiles 23)
                         (vector-ref tiles 24)
                         (vector-ref tiles 25)
                         (vector-ref tiles 26))))
    (make-animation frames 6 #t)))

(define sprite
  (make-sprite animation
               #:position (vector (/ window-width 2)
                                  (/ window-height 2))))


(run-game-loop)
