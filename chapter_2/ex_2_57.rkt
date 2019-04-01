#lang racket

;Exercise 2.57: Extend the differentiation program to handle
;sums and products of arbitrary numbers of (two or more)
;terms. Then the last example above could be expressed as

;   (deriv '(* x y (+ x 3)) 'x)

;Try to do this by changing only the representation for sums
;and products, without changing the deriv procedure at all.
;For example, the addend of a sum would be the first term,
;and the augend would be the sum of the rest of the terms

(define (variable? e)(symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (addend e) (cadr e))
(define (augend e)
  (if (and (pair? (cddr e))
           (> (length (cddr e)) 1))
      (append (list '+) (cddr e))
      (caddr e)))

(define (sum? x)
  (and (pair? x) (eq? '+ (car x))))

(define (iter op summ sum-numbers sum-symbols sum-paris)
  (cond ((null? summ) (append (list sum-numbers) sum-symbols sum-paris))
        ((number? (car summ)) (iter op
                                    (cdr summ)
                                    (op (car summ) sum-numbers)
                                    sum-symbols
                                    sum-paris))
        ((variable? (car summ)) (iter op
                                      (cdr summ)
                                      sum-numbers
                                      (append sum-symbols (list (car summ)))
                                      sum-paris))
        ((pair? (car summ)) (iter op
                                  (cdr summ)
                                  sum-numbers
                                  sum-symbols 
                                  (append sum-paris (list (car summ)))))))

(define (make-sum a1 . a2)
  (let ((order-sum (iter + (append (list a1) a2) 0 '() (list ))))
    (cond ((= (length order-sum) 1)
           (car order-sum))
          ((and (= (length order-sum) 2)
                (= (car order-sum) 0))
           (cadr order-sum))
          ((and (> (length order-sum) 2)
                (= (car order-sum) 0))
           (append (list '+) (cdr order-sum)))
          (else (append (list '+)  order-sum))
    )
  ))

(define (make-product a1 . a2)
  (let ((order-sum (iter * (append (list a1) a2) 1 '() (list ))))
    (cond ((= (car order-sum) 0) 0)
          ((= (length order-sum) 1)
           (car order-sum))
          ((and (= (length order-sum) 2)
                (= (car order-sum) 1))
           (cadr order-sum))
          ((and (> (length order-sum) 2)
                (= (car order-sum) 1))
           (append (list '*) (cdr order-sum)))
          (else (append (list '*)  order-sum))
    )
  ))

(define (multiplier e) (cadr e))
(define (multiplicand e)
  (if (and (pair? (cddr e))
           (> (length (cddr e)) 1))
      (append (list '*) (cddr e))
      (caddr e)))

(define (product? e)
  (and (pair? e) (eq? '* (car e))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 0) 0)
        (else
         (list '** base exponent))))

(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (exponentiation? e)
  (and (pair? e) (eq? '** (car e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;examples:
;(deriv '(+ x 3) 'x)
;(deriv '(* x y) 'x)
(deriv '(* x (* (* x y)(+ x 3 y))) 'x)
(deriv (deriv (deriv '(** x 3) 'x) 'x) 'x)
;(deriv '(* x x) 'x)
