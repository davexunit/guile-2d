;;; guile-2d
;;; Copyright (C) 2013 David Thompson <dthompson2@worcester.edu>
;;;
;;; Guile-2d is free software: you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; Guile-2d is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Game data structure.
;;
;;; Code:

(define-module (2d private game)
  #:use-module (srfi srfi-9)
  #:use-module (2d observer))

;;;
;;; Scenes
;;;

(define-record-type <scene>
  (%make-scene title observer update-proc draw-proc state)
  scene?
  (title scene-title)
  (observer scene-observer)
  (update-proc scene-update-proc)
  (draw-proc scene-draw-proc)
  (state scene-state))

(define (scene-trigger scene event-type . args)
  "Trigger an event on the scene observer."
  (apply observer-trigger
         (scene-observer scene)
         event-type
         (scene-state scene)
         args))

(define (scene-draw scene)
  "Draw scene."
  ((scene-draw-proc scene) (scene-state scene)))

(define (scene-update scene)
  "Update scene."
  ((scene-update-proc scene) (scene-state scene)))

(export <scene>
        %make-scene
        scene?
        scene-title
        scene-observer
        scene-update-proc
        scene-draw-proc
        scene-state
        scene-trigger
        scene-update
        scene-draw)

;;;
;;; Games
;;;

(define-record-type <game>
  (%make-game title resolution fullscreen first-scene)
  game?
  (title game-title)
  (resolution game-resolution)
  (fullscreen game-fullscreen?)
  (first-scene game-first-scene))

(export <game>
        %make-game
        game?
        game-title
        game-resolution
        game-fullscreen?
        game-first-scene)
