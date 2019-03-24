#lang racket

;Exercise:
;Here are two possible constructors for frames:
;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
;(define (make-frame origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))
;For each constructor supply the appropriate selectors to
;produce an implementation for frames

;First realisation
(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame1 frame)
  (car frame))

(define (edge1-frame1 frame)
  (car (cdr frame)))

(define (edge2-frame1 frame)
  (car (cdr (cdr frame))))

;Example :
(define frame1 (make-frame1 (list 0 0) (list 1 1) (list 0.5 0.5)))
frame1
(origin-frame1 frame1)
(edge1-frame1 frame1)
(edge2-frame1 frame1)

;Second realisation
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  (cdr (cdr frame)))

;Example : 
(define frame2 (make-frame (list 0 0) (list 1 1) (list 0.5 0.5)))
frame2
(origin-frame frame2)
(edge1-frame frame2)
(edge2-frame frame2)

     

