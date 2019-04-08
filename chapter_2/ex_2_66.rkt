#lang racket

;Exercise 2.66: Implement the lookup procedure for the case
;where the set of records is structured as a binary tree, ordered
;by the numerical values of the keys.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))
(define (make-record key value) (cons key value))
(define (record-key record) (car record))
(define (record-value record)
  (if (pair? (cdr record))
      (cadr record)
      (cdr record)))

(define (lookup-tree given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (record-key (entry set-of-records)))
            (record-value (entry set-of-records)))
        ((< given-key (record-key (entry set-of-records)))
         (lookup-tree given-key (left-branch  set-of-records)))
        (else
         (lookup-tree given-key (right-branch set-of-records)))))

;example
(define tree1 '((4 "four") ((2 "two") () ()) ((5 "five") () ())))
tree1

(define rec1 (make-record 4 "four"))
(record-key rec1)
(record-value rec1)
(record-key (entry tree1))
(lookup-tree 4 tree1)
(lookup-tree 2 tree1)
(lookup-tree 5 tree1)
