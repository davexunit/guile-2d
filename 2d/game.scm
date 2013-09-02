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

(define-module (2d game)
  #:use-module (srfi srfi-9)
  #:use-module (2d game-loop)
  #:use-module (2d window)
  #:use-module (2d vector2))

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

(define* (make-game #:optional #:key
                    (title "A Guile-2D Game")
                    (resolution (vector2 640 480))
                    (fullscreen #f)
                    (first-scene #f))
  "Return a new game. All game properties have some reasonable default
value."
  (%make-game title resolution fullscreen first-scene))

(define-syntax-rule (define-game name kwargs ...)
  "Syntactic sugar for define and make-game."
  (define name (make-game kwargs ...)))

(define (run-game game)
  "Open a window and start playing GAME."
  (open-window (game-title game)
               (game-resolution game)
               (game-fullscreen? game))
  (run-game-loop)
  (close-window))

(export <game>
        make-game
        define-game
        game?
        game-title
        game-resolution
        game-fullscreen?
        game-first-scene
        run-game)
