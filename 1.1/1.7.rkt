#lang sicp

(#%require rackunit)

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))


(check-equal? (sqrt 144) 12.0)
(check-equal? (sqrt 9) 3.0)
(check-equal? (sqrt 0.0001) 0.01)
(check-equal? (sqrt 1e-100) 1e-50)
(check-equal? (sqrt 1e+10) 100000.0)
(check-equal? (sqrt 1e+200) 1e+100)
(check-equal? (round (* 1000 (sqrt 4.0))) 2000.0)
(check-equal? (round (* 1000 (sqrt 100.0))) 10000.0)
(check-equal? (round (* 1000 (sqrt 0.04))) 200.0)
(check-equal? (round (* 1000 (sqrt 1000000.0))) 1000000.0)
(check-equal? (round (* 1000 (sqrt 10000000000.0))) 100000000.0)


;; new possible implementation

(define (new-good-enough? guess guess0)
  (< (abs (/ (- guess guess0) guess0)) 1e-10))

(define (new-sqrt-iter guess guess0 x)
  (if (new-good-enough? guess guess0)
      guess
      (new-sqrt-iter (improve guess x)
                        guess
                        x)))

(define (new-sqrt x)
  (new-sqrt-iter 1.0 0.5 x))

(check-equal? (new-sqrt 144) 12.0)
(check-equal? (new-sqrt 9) 3.0)
(check-equal? (new-sqrt 0.0001) 0.01)
(check-equal? (new-sqrt 1e-100) 1e-50)
(check-equal? (new-sqrt 1e+10) 100000.0)
(check-equal? (new-sqrt 1e+200) 1e+100)
(check-equal? (round (* 1000 (new-sqrt 4.0))) 2000.0)
(check-equal? (round (* 1000 (new-sqrt 100.0))) 10000.0)
(check-equal? (round (* 1000 (new-sqrt 0.04))) 200.0)
(check-equal? (round (* 1000 (new-sqrt 1000000.0))) 1000000.0)
(check-equal? (round (* 1000 (new-sqrt 10000000000.0))) 100000000.0)