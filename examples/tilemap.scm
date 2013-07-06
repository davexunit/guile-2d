(use-modules (figl gl)
             (srfi srfi-1)
             (srfi srfi-9)
             (srfi srfi-42)
             (ice-9 format)
             (2d sprite)
             (2d game-loop)
             (2d window)
             (2d vector)
             (2d input)
             (2d helpers))

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
  (let ((batch (make-sprite-batch)))
    (lambda (layer)
      (with-sprite-batch batch
        (do-ec (: y (map-layer-height layer))
               (: x (map-layer-width layer))
               (let ((tile (array-ref (map-layer-tiles layer) y x)))
                 (sprite-batch-draw batch
                                    (texture-region-texture tile)
                                    (* x (map-layer-tile-width layer))
                                    (* y (map-layer-tile-height layer))
                                    0
                                    0
                                    (texture-region-width tile)
                                    (texture-region-height tile)
                                    1
                                    1
                                    0
                                    (texture-region-u tile)
                                    (texture-region-v tile)
                                    (texture-region-u2 tile)
                                    (texture-region-v2 tile))))))))

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

(define (tiles->texture-regions width height tileset tiles)
  (list->array
   2
   (list-ec
    (: y height)
    (list-ec
     (: x width)
     (vector-ref tileset (array-ref tiles y x))))))

(define (key-down key mod unicode)
  (cond ((any-equal? key (keycode escape) (keycode q))
         (close-window)
         (quit))))

(define (render)
  (with-gl-push-matrix
   (gl-translate 100 100 0)
   (draw-map-layer map)))

(define window-width 800)
(define window-height 600)

(set-key-down-callback (lambda (key mod unicode) (key-down key mod unicode)))
(set-render-callback (lambda () (render)))
(open-window window-width window-height)

;; Load tileset and build map layer
(let* ((texture (load-texture "images/tiles.png"))
       (tileset (split-texture texture 32 32)))
  (set! map (make-map-layer map-width map-height tile-width tile-height
                            (tiles->texture-regions map-width
                                                    map-height
                                                    tileset
                                                    map-tiles))))

(run-game-loop)
