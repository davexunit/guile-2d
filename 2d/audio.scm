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
;; Wrappers over SDL mixer.
;;
;;; Code:

(define-module (2d audio)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-2)
  #:use-module ((sdl mixer) #:prefix SDL:))

;; Wrapper over SDL audio objects.
(define-record-type <sample>
  (make-sample audio)
  sample?
  (audio sample-audio))

(define (load-sample filename)
  "Load audio sample from FILENAME. Return #f on failure."
  (let ((audio (SDL:load-wave filename)))
    (if audio
        (make-sample audio)
        #f)))

(define (sample-play sample)
  "Play SAMPLE."
  (SDL:play-channel (sample-audio sample)))

(define (sample-volume)
  "Return volume that samples are played at."
  (SDL:volume))

(define (set-sample-volume volume)
  "Set the volume that samples are played at."
  (SDL:volume volume))

(export make-sample
        load-sample
        sample?
        sample-audio
        sample-play
        sample-volume
        set-sample-volume)

;; Wrapper over SDL music objects.
(define-record-type <music>
  (make-music audio)
  music?
  (audio music-audio))

(define (load-music filename)
  "Load music from FILENAME. Return #f on failure."
  (let ((audio (SDL:load-music filename)))
    (if audio
        (make-music audio)
        #f)))

(define (music-play music)
  "Play MUSIC."
  (SDL:play-music (music-audio music)))

(define (music-volume)
  "Return volume that music is played at."
  (SDL:music-volume))

(define (set-music-volume volume)
  "Set the volume that music is played at."
  (SDL:volume volume))

(export make-music
        load-music
        music?
        music-audio
        music-play
        music-volume
        set-music-volume)

(re-export (SDL:pause-music . music-pause)
           (SDL:resume-music . music-resume)
           (SDL:rewind-music . music-rewind)
           (SDL:halt-music . music-stop)
           (SDL:paused-music? . music-paused?)
           (SDL:playing-music? . music-playing?))
