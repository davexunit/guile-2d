(use-modules (figl gl)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-42)
             (2d sprite)
             (2d game-loop)
             (2d window)
             (2d vector)
             (2d helpers))

(init-2d)

;;;
;;; Orthogonal tile map example
;;;

;; This is a quick and dirty tile map implementation. No fancy map
;; loading. Just a hardcoded tile map that demonstrates the
;; split-texture procedure.

;; tiles is a 2d array of texture regions.
(define-record-type <map-layer>
  (make-map-layer width height tile-width tile-height tiles)
  map-layer?
  (width map-layer-width)
  (height map-layer-height)
  (tile-width map-layer-tile-width)
  (tile-height map-layer-tile-height)
  (tiles map-layer-tiles))

(define draw-map-layer
  (let ((batch (make-sprite-batch 2000)))
    (lambda (layer)
      (with-sprite-batch batch
        (do-ec (: y (map-layer-height layer))
               (: x (map-layer-width layer))
               (let ((tile (array-ref (map-layer-tiles layer) y x)))
                 (draw-sprite tile)))))))

;; A small 8x8 array of tile indices.
(define map-width 8)
(define map-height 8)
(define map-tiles
  #2u32((00 01 01 01 01 01 01 02)
        (16 17 17 17 17 17 17 18)
        (16 17 17 17 17 17 17 18)
        (16 17 17 48 49 17 17 18)
        (16 17 17 64 65 17 17 18)
        (16 17 17 17 17 17 17 18)
        (16 17 17 17 17 17 17 18)
        (32 33 33 33 33 33 33 34)))

(define tile-width 32)
(define tile-height 32)
(define map #f)

(define (random-map width height tileset)
  (let ((tiles (make-array 0 height width))
        (n (vector-length tileset)))
    (do-ec (: y height) (: x width)
           (array-set! tiles (random n) y x))
    tiles))

(define (tiles->sprites width height tile-width tile-height tileset tiles)
  (define (build-sprite x y)
    (let ((region (vector-ref tileset (array-ref tiles y x))))
      (make-sprite region
                   #:position (vector (* x tile-width)
                                      (* y tile-height))
                   #:anchor #(0 0))))

  (let ((sprites (list-ec (: y height)
                          (list-ec (: x width)
                                   (build-sprite x y)))))
    (list->array 2 sprites)))

(define (key-down key mod unicode)
  (cond ((any-equal? key 'escape 'q)
         (close-window)
         (quit))))

(define (render)
  (draw-map-layer map))

(define window-width 800)
(define window-height 600)

(set-key-down-callback (lambda (key mod unicode) (key-down key mod unicode)))
(set-render-callback (lambda () (render)))
(open-window window-width window-height)

;; Load tileset and build map layer
(let* ((texture (load-texture "images/tiles.png"))
       (tileset (split-texture texture 32 32)))
  (set! map (make-map-layer map-width map-height tile-width tile-height
                            (tiles->sprites map-width
                                            map-height
                                            tile-width
                                            tile-height
                                            tileset
                                            map-tiles))))

(run-game-loop)
