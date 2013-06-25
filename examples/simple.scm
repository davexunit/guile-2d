(use-modules ((sdl sdl) #:prefix SDL:)
             (figl gl)
             (2d sprite)
             (2d game-loop)
             (2d window))

(define window-width 800)
(define window-height 600)
(define sprite #f)

(define (key-down key)
  (display key) (newline)
  (case key
    ;; Quit program when ESCAPE or Q is pressed.
    ;; For now we have to use the SDL keycodes, but not for long!
    ((SDLK_ESCAPE SDLK_q)
     (close-window)
     (quit))))

;; Draw our sprite
(define (render)
  (draw-sprite sprite))

;; Register callbacks.
(set-render-callback (lambda () (render)))
(set-key-down-callback (lambda (key) (key-down key)))

;; Open the window.
(open-window window-width window-height)

;; Load a sprite and center it on the screen.
;; Must be done AFTER opening the window.
(set! sprite (load-sprite "sprite.png" #:position (vector (/ window-width 2)
                                                          (/ window-height 2))))

;; Start the game loop.
;; The render callback will be called through this procedure.
(run-game-loop)
