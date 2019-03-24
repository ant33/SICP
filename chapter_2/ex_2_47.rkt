#lang planet neil/sicp

;Exercise:
;Here are two possible constructors for frames:
;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
;(define (make-frame origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))

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
(define frame1 (make-frame1 1 2 3))
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
(define frame2 (make-frame 1 4 9))
frame2
(origin-frame frame2)
(edge1-frame frame2)
(edge2-frame frame2)




