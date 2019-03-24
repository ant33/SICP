#lang planet neil/sicp
;Exercise :
;A two-dimensional vector v running from the origin
;to a point can be represented as a pair consisting
;of an x-coordinate and a y-coordinate. Implement a data
;abstraction for vectors by giving a constructor make-vect
;and corresponding selectors xcor-vect and ycor-vect. In
;terms of your selectors and constructor, implement procedures
;add-vect, sub-vect, and scale-vect that perform
;the operations vector addition, vector subtraction, and multiplying
;a vector by a scalar:
;(x1, y1) + (x2, y2) = (x1 + x2 ; y1 + y2);
;(x1, y1) - (x2, y2) = (x1 - x2 ; y1 - y2);
;s*(x, y) = (s*x, s*y).

(define (make-vect x y)
  (cons x y))
(define (xcor-vect vect)
  (car vect))
(define (ycor-vect vect)
  (cdr vect))

(define (add-vect vect1 vect2)
  (cons (+ (xcor-vect vect1) (xcor-vect vect2))
        (+ (ycor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
  (cons (- (xcor-vect vect1) (xcor-vect vect2))
        (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect s vect)
  (cons (* s (xcor-vect vect))
        (* s (ycor-vect vect))))

;example
(define vec1 (make-vect 2 3))
(define vec2 (make-vect 3 4))
(add-vect vec1 vec2)
(sub-vect vec1 vec2)
(scale-vect 3 vec1)