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
  #:use-module (2d private game)
  #:use-module (2d game-loop)
  #:use-module (2d helpers)
  #:use-module (2d observer)
  #:use-module (2d vector2))

;;;
;;; Scenes
;;;

;; When no event callbacks are specified for a scene, these
;; (hopefully) convenient defaults will be used.
(define %default-scene-events
  `((quit     . ,(lambda (state) (quit-game-loop!)))
    (key-down . ,(lambda (state key mode unicode)
                   (when (any-equal? key 'escape 'q)
                     (quit-game-loop!))))))

(define (default-scene-events)
  (copy-tree %default-scene-events))

(define* (make-scene #:optional #:key
                     (title "A Guile-2D Scene")
                     (events (default-scene-events))
                     (update (lambda (s) #f))
                     (draw (lambda (s) #f))
                     (state #f))
  "Return a new scene. TITLE is a human readable name for the
scene. EVENTS is an alist of event handlers. UPDATE is a procedure
that updates the scene. DRAW is a procedure that renders the
scene. STATE is an object that encapsulates the scene state."
  (%make-scene title (alist->observer events) update draw state))

(define-syntax-rule (define-scene name kwargs ...)
  "Syntactic sugar over define and make-scene. Return a procedure that
creates a new scene."
  (define (name) (make-scene kwargs ...)))

(re-export <scene>
           scene?
           scene-title
           scene-observer
           scene-update-proc
           scene-draw-proc
           scene-state
           scene-trigger
           scene-update
           scene-draw
           push-scene
           replace-scene
           pop-scene)

(export make-scene
        define-scene
        default-scene-events)

;;;
;;; Games
;;;

(define* (make-game #:optional #:key
                    (title "A Guile-2D Game")
                    (resolution (vector2 640 480))
                    (fullscreen #f)
                    (first-scene #f))
  "Return a new game. All game properties have some reasonable default
value."
  (%make-game title resolution fullscreen first-scene))

(define-syntax-rule (define-game name kwargs ...)
  "Syntactic sugar over define and make-game."
  (define name (make-game kwargs ...)))

(re-export <game>
           game?
           game-title
           game-resolution
           game-fullscreen?
           game-first-scene
           run-game
           current-fps)

(export make-game
        define-game)
