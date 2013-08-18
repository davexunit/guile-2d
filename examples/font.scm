(use-modules (2d game-loop)
             (2d window)
             (2d helpers)
             (figl gl)
             (2d wrappers ftgl))

(define window-width 800)
(define window-height 600)
(define font (ftgl-create-texture-font "fonts/Boxy-Bold.ttf"))
(define text "The quick brown fox jumped over the lazy dog.")

(ftgl-set-font-face-size font 48 72)

;; Open the window.
(open-window window-width window-height)

(define (quit-demo)
  (close-window)
  (quit))

(define (key-down key mod unicode)
  (cond ((any-equal? key 'escape 'q)
         (quit-demo))))

;; Draw our sprite
(define (render)
  (gl-color 1 1 1)
  (ftgl-render-font font text (ftgl-render-mode all)))

;; Register callbacks.
(add-hook! on-quit-hook (lambda () (quit-demo)))
(add-hook! on-render-hook (lambda () (render)))
(add-hook! on-key-down-hook (lambda (key mod unicode) (key-down key mod unicode)))


;; Start the game loop.
;; The render callback will be called through this procedure.
(run-game-loop)
