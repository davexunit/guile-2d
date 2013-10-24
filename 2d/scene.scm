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
  #:export (make-scene
            scene?
            scene-init
            scene-enter
            scene-exit
            scene-draw
            scene-update))

(define-record-type <scene>
  (%make-scene init enter exit draw update)
  scene?
  (init scene-init)
  (enter scene-enter)
  (exit scene-exit)
  (draw scene-draw)
  (update scene-update))

(define no-op (lambda args #f))

(define* (make-scene #:optional #:key
                     (init no-op)
                     (enter no-op)
                     (exit no-op)
                     (draw no-op)
                     (update no-op))
  "Create a new scene object. All callbacks default to a no-op."
  (%make-scene init enter exit draw update))
