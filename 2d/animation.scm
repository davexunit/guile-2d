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
;; Animations represent a sequence of textures and/or texture regions.
;;
;;; Code:

(define-module (2d animation)
  #:use-module (srfi srfi-9)
  #:use-module (2d texture))

;;;
;;; Animations
;;;

;; The <animation> type represents a vector of textures or texture
;; regions that are to be played in sequence and possibly looped.
(define-record-type <animation>
  (make-animation frames duration loop)
  animation?
  (frames animation-frames)
  (duration animation-duration)
  (loop animation-loop?))

(define (animation-frame animation index)
  "Returns the frame for the given index."
  (vector-ref (animation-frames animation) index))

(define (animation-length animation)
  "Returns the number of frames in the animation"
  (vector-length (animation-frames animation)))

(export make-animation
        animation?
        animation-frames
        animation-duration
        animation-loop?
        animation-frame
        animation-length)

;; The <animation-state> type encapsulates the state for playing an
;; animation.
(define-record-type <animation-state>
  (%make-animation-state animation frame-index frame-time playing)
  animation-state?
  (animation animation-state-animation)
  (frame-index animation-state-frame-index)
  (frame-time animation-state-frame-time)
  (playing animation-state-playing?))

(define (make-animation-state animation)
  "Creates a new animation state object."
  (%make-animation-state animation 0 0 #t))

(define (tick-animation-state state)
  "Increments the frame time for the animation state and determines
which frame to show. Returns a new animation state object when the
animation is playing. Otherwise the state passed in is returned."
  (let ((frame-time (1+ (animation-state-frame-time state)))
        (frame-index (animation-state-frame-index state))
        (playing (animation-state-playing? state))
        (animation (animation-state-animation state)))

    ;; Return the same state object if the animation is not playing.
    (cond ((not playing)
           state)
          ;; Return a new state object with a reset frame-index and
          ;; frame-time if we've reached the end of the animation.
          ;; Stops playing the animation if the animation does not
          ;; loop.
          ((and playing (= frame-time (animation-duration animation)))
           (let* ((frame-index (modulo (1+ frame-index)
                                       (animation-length animation)))
                  (frame-time 0)
                  (playing (or (not (= frame-index 0))
                               (animation-loop? animation))))
             (%make-animation-state animation frame-index frame-time playing)))
          ;; Return a new state object with an incremented frame index.
          (else
           (%make-animation-state animation frame-index frame-time playing)))))

(define (animation-state-frame state)
  "Returns the texture or texture region for the state's animation at
the current frame index."
  (animation-frame (animation-state-animation state)
                   (animation-state-frame-index state)))

(export make-animation-state
        animation-state?
        animation-state-animation
        animation-state-frame-index
        animation-state-frame-time
        animation-state-playing?
        animation-state-frame
        tick-animation-state)
