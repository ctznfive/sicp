
;; Combinations
;; (operator operands) : prefix notation
(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 5)
(+ 2.7 10)
(+ 21 35 12 7)
(* 25 4 12)
(+ (* 3 5) (- 10 6))

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))

;; pretty printing
(+ (* 3
      (+ (* 2 4)
	 (+ 3 5)))
   (+ (- 10 7)
      6))


;; names, variables and values
(define size 2)
size
(* 5 size)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))

(define circumference (* 2 pi radius))
circumference


;; compound procedures
(define (square x)
  (* x x))
(square 25)
(square radius)
(square (+ 2 5))
(square (square 3))
(define x 0)
(define y 5)
(+ (square x) (square y))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)
(f 10)


;; case analysis
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(abs -7)
(abs 0)
(abs 15)
(abs -200)

(define (abs2 x)
  (cond ((>= x 0) x)
	((< x 0) (- x))))

(abs2 -8)
(abs2 0)
(abs2 9)

(define (abs3 x)
  (cond ((> x 0) x)
	((= x 0) 0)
	(else (- x))))

(abs3 -8)
(abs3 0)
(abs3 9)

;; (if <предикат> <следствие> <альтернатива>)
(define (abs4 x)
  (if (< x 0)
      (- x)
      x))

(abs4 -8)
(abs4 0)
(abs4 9)

(define (5<x<10 x)
  (if (and (> x 5) (< x 10)) x
      (display "out of range")))

(5<x<10 8)
(5<x<10 30)

(define (>= x y)
  (and (> x y) (= x y)))

(>= 8 10)

(define (>= x y)
  (not (< x y)))

(>= 9 9)


;; square roots by Newton’s method
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)
(sqrt 2)
(sqrt (+ 100 37))
(sqrt (+ (sqrt 2) (sqrt 3)))
(sqrt (square 1000))
(square (sqrt 1000))
