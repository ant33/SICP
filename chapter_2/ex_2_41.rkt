#lang racket

; Exercise 2.41
; Write a procedure to find all ordered triples
; of distinct positive integersi, j, and k less than or equal to
; a given integer n that sum to a given integer s.

; We use functions from previous examples
(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

; From exercise 2.37
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

; filter from chapter 2.2.3
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 n)))
   (enumerate-interval 1 n)))

(define (triples max_n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (append i (list j)))
          (enumerate-interval 1 max_n)))
   (pairs max_n)))

; function for NON-ORDERED triples
(define (fun1 n k)
  (define (summed-list-equals-n? items)
    (= (accumulate + 0 items)
       k))
  (filter summed-list-equals-n?
          (triples n)))

; ordered pairs
(define (ordered-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))
;Example
(ordered-pairs 4)


(define (ordered-triples n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (append (list i) j))
          (ordered-pairs (- i 1))))
   (enumerate-interval 3 n)))
;Example     
(ordered-triples 4)

; function for ORDERED triples
(define (fun2 n k)
  (define (summed-list-equals-n? items)
    (= (accumulate + 0 items)
       k))
  (filter summed-list-equals-n?
          (ordered-triples n)))
;Example
(fun2 15 15)