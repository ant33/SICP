#lang planet neil/sicp

;Exercise 2.45: right-split and up-split can be expressed
;as instances of a general spliing operation. Define a procedure
;split with the property that evaluating
;
;(define right-split (split beside below))
;(define up-split (split below beside))
;
;produces procedures right-split and up-split with the
;same behaviors as the ones already defined.

(define (split proc1 proc2)
  (define (pos-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (pos-split painter (- n 1))))
          (proc1 painter (proc2 smaller smaller)))))
  (lambda (pic n)
    (pos-split pic n)))

(define right-split (split beside below))
(define up-split (split below beside))

; Example :
(paint (up-split einstein 4))

