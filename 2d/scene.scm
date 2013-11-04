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
;; Scenes describe the behavioral aspects of a game.
;;
;;; Code:

(define-module (2d scene)
  #:use-module (srfi srfi-9)
  #:use-module (2d observer)
  #:export (<scene>
            make-scene
            scene?
            scene-name
            scene-init
            scene-enter
            scene-exit
            scene-draw
            scene-update
            scene-observer
            init-scene
            enter-scene
            exit-scene
            draw-scene
            update-scene
            scene-trigger
            default-events))

(define-record-type <scene>
  (%make-scene name init enter exit draw update observer)
  scene?
  (name scene-name)
  (init scene-init)
  (enter scene-enter)
  (exit scene-exit)
  (draw scene-draw)
  (update scene-update)
  (observer scene-observer))

(define no-op (lambda args #f))
(define default-events (make-parameter '()))

(define* (make-scene name
                     #:optional #:key
                     (init no-op)
                     (enter no-op)
                     (exit no-op)
                     (draw no-op)
                     (update no-op)
                     (events (default-events)))
  "Create a new scene object. All callbacks default to a no-op."
  (%make-scene name init enter exit draw update
               (alist->observer events)))

(define (init-scene scene)
  "Return the value returned by the state constructor thunk for
SCENE."
  ((scene-init scene)))

(define (enter-scene scene state)
  "Call enter callback for SCENE with STATE."
  ((scene-enter scene) state))

(define (exit-scene scene state)
  "Call the exit callback for SCENE with STATE."
  ((scene-exit scene) state))

(define (draw-scene scene state)
  "Call the draw callback for SCENE with STATE."
  ((scene-draw scene) state))

(define (update-scene scene state)
  "Call the update callback for SCENE with STATE."
  ((scene-update scene) state))

(define (scene-trigger scene state event . args)
  (apply observer-trigger (scene-observer scene) event state args))
