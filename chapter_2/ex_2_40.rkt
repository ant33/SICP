#lang racket
(define (square x)
  (* x x))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define seq0 (enumerate-interval 2 7))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
         

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(define seq (accumulate append
                        '()
                        (map (lambda (i)
                               (map (lambda (j) (list i j))
                                    (enumerate-interval 1 (- i 1))))
                             (enumerate-interval 1 6))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (car (cdr pair)))))

(define (make-pair-sum pair)
  (list (car pair) (car (cdr pair)) (+ (car pair) (car (cdr pair)))))

(prime-sum? '(1 2))
(make-pair-sum '(1 2))


(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)
