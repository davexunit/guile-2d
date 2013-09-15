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
  (make-animation frames frame-duration loop)
  animation?
  (frames animation-frames)
  (frame-duration animation-frame-duration)
  (loop animation-loop?))

(define (animation-frame animation index)
  "Return the texture for the given frame INDEX."
  (vector-ref (animation-frames animation) index))

(define (animation-length animation)
  "Return the number of frames in the ANIMATION."
  (vector-length (animation-frames animation)))

(define (animation-duration animation)
  "Return the total duration of ANIMATION in ticks."
  (* (animation-length animation)
     (animation-frame-duration animation)))

(export make-animation
        animation?
        animation-frames
        animation-frame-duration
        animation-loop?
        animation-frame
        animation-length
        animation-duration)

;; The <animator> type encapsulates the state for playing an
;; animation.
(define-record-type <animator>
  (%make-animator animation frame time playing)
  animator?
  (animation animator-animation)
  (frame animator-frame set-animator-frame!)
  (time animator-time set-animator-time!)
  (playing animator-playing? set-animator-playing!))

(define (make-animator animation)
  "Creates a new animation animator object."
  (%make-animator animation 0 0 #t))

(define (animator-frame-complete? animator)
  (>= (animator-time animator)
      (animation-frame-duration (animator-animation animator))))

(define (animator-next-frame animator)
  "Return the next frame index for the animation ANIMATOR. Return -1 when
the animation is complete."
  (modulo (1+ (animator-frame animator))
          (animation-length (animator-animation animator))))

(define (animator-texture animator)
  "Returns the texture for the animation at the current frame index."
  (animation-frame (animator-animation animator)
                   (animator-frame animator)))

(define (animator-next! animator)
  "Advance to the next animation frame for the given animation ANIMATOR."
  (let ((next-frame (animator-next-frame animator))
        (animation (animator-animation animator)))
    (set-animator-time! animator 0)
    (set-animator-frame! animator next-frame)
    (set-animator-playing! animator (or (not (zero? next-frame))
                                        (animation-loop? animation)))))

(define (animator-update! animator)
  "Increments the frame time for the animation ANIMATOR and advances to
the next frame in the animation if necessary."
  (when (animator-playing? animator)
    (set-animator-time! animator (1+ (animator-time animator)))
    (when (animator-frame-complete? animator)
      (animator-next! animator))))

(export make-animator
        animator?
        animator-animation
        animator-frame
        animator-time
        animator-frame-complete?
        animator-playing?
        animator-next-frame
        animator-texture
        animator-next!
        animator-update!)
