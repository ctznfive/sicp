#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (good-enough? guess guess0)
  (< (abs (- guess guess0))
     (* 1e-10 guess0)))

(define (improve guess x)
  (/ (+ (/ x
           (square guess))
        (* 2 guess))
     3))

(define (cube-iter guess x)
  (if (good-enough? (improve guess x) guess)
      guess
      (cube-iter (improve guess x) x)))

;; if initial approximation y >= 0:
(define (qb x)
  (if (>= x 0)
      (cube-iter 1.0 x)
      (- (cube-iter 1.0 (- x)))))

;; if x = -2*y^3 => division by zero
;; y - variable initial approximation
;(define (qb x y)  
;  (if (= x (* (- 2) y y y))
;      (- (cube-iter y (- x)))
;      (cube-iter y x)))

(check-equal? (round (* 1000 (qb 0.008))) 200.0)
(check-equal? (round (* 1000 (qb -0.008))) -200.0)
(check-equal? (round (* 1000 (qb 8.0))) 2000.0)
(check-equal? (round (* 1000 (qb -8.0))) -2000.0)
(check-equal? (round (* 1000 (qb 1000.0))) 10000.0)
(check-equal? (round (* 1000 (qb -1000.0))) -10000.0)
(check-equal? (round (* 1000 (qb 1e+9))) 1000000.0)
(check-equal? (round (* 1000 (qb -1e+9))) -1000000.0)