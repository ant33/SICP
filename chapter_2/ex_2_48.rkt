#lang racket
;Exercise 2.48:
;A directed line segment in the plane can be
;represented as a pair of vectors—the vector running from
;the origin to the start-point of the segment, and the vector
;running from the origin to the end-point of the segment.
;Use your vector representation from Exercise 2.46 to define
;a representation for segments with a constructor makesegment
;and selectors start-segment and end-segment.

(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))

; Exercise 2.48
(define (make-segment start end)
  (make-vect start end))
(define (start-segment segment)
  (xcor-vect segment))
(define (end-segment segment)
  (ycor-vect segment))

; Example :

(define segment1 (make-segment '(1 2) '(2 3)))
segment1
(start-segment segment1)
(end-segment segment1)