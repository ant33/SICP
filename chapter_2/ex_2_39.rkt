#lang racket

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial
                      (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse sequence)
  (fold-right (lambda (x y) (append  y (list x))) '() sequence))
(define seq1 '(1 2 3 4))
(reverse seq1)

; realisation of reverse using cons instead append
(define (reverse1 sequence)
  (fold-right (lambda (x y) (cons y x)) '() sequence))
(reverse1 seq1)

(define (reverse2 sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
(reverse2 seq1)

