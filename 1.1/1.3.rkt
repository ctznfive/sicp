#lang sicp

(#%require rackunit)

(define (square a) (* a a))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (f a b c)
  (cond ((and (>= a c) (>= b c))
             (sum-of-squares a b))
        ((and (>= a b) (>= c b))
             (sum-of-squares a c))
        (else
         (sum-of-squares b c))))

(check-equal? (f 0 0 0) 0)
(check-equal? (f 1 0 1) 2)
(check-equal? (f 1 2 3) 13)
(check-equal? (f 2 3 0) 13)
(check-equal? (f 4 2 3) 25)