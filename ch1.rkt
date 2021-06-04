#lang sicp

;Ex 1.2
(define (ex1.2) (/ (+ 5 4 (- 2 ( - 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7))))

;Ex 1.3
(define (square x) (* x x))

(define (sum-of-squares x y)
    (+ (square x) (square y)))

(define (f a)
    (sum-of-squares (+ a 1 ) (* a 2)))

;(define (abs x)
;  (cond ((> x 0) x)
;        ((= x 0) 0)
;        ((< x 0) (- x))))

;(define (abs x)
;  (cond ((< x 0) (- x))
;        (else x)))

(define (abs x)
  (if (< x 0) (- x) x))

(define x 9)
(define y 13)

(define (max x y z)
     (cond ((and (>= x y) (>= x z)) x)
           ((and (>= y x) (>= y z)) y)
           (else z)))

(define (mid x y z)
  (cond ((or (and (<= x y) (>= x z)) (and (>= x y) (<= x z))) x)
        ((or (and (<= y x) (>= y z)) (and (>= y x) (<= y z))) y)
         (else z)))

(define (maxsquares x y z)
  (sum-of-squares (max x y z) (mid x y z)))


;Ex 1.7
(define (sqrt-iter guess x prev_diff)
  (if (good-enough? guess x prev_diff)
      guess
      (sqrt-iter (improve guess x)
                 x
                 (abs-sq-diff guess x))))

(define (improve guess x)
  (average guess ( / x guess)))

(define (average x y)
  (/ ( + x y) 2))

(define (abs-sq-diff guess x)
  (abs (- (square guess) x)))
  
(define (good-enough? guess x prev_diff)
  (< (abs-sq-diff (abs-sq-diff guess x) prev_diff) 0.000001))

(define (sqrt x)
  (sqrt-iter 1.0 x 0))

;Ex 1.8
(define (cube x)
  (* x x x))

(define (abs-cube-diff guess x)
  (abs (- (cube guess) x)))

(define (good-enough-cu? guess x prev_diff)
  (< (abs-cube-diff (abs-cube-diff guess x) prev_diff) 0.000001))

(define (improve-curt guess x)
  (/ (+ guess guess (/ x (square guess))) 3 ))

(define (curt-iter guess x prev_diff)
  (if (good-enough-cu? guess x prev_diff)
      guess
      (curt-iter (improve-curt guess x)
                 x
                 (abs-cube-diff guess x))))

(define (curt x)
  (curt-iter 1.0 x 0.0))

(define (curt2 x)
  (define (cube y)
    (* y y y))
  (define (abs-cube-diff guess y)
    (abs (- (cube guess) y)))
  (define (good-enough-cu? guess prev_diff)
    (< (abs-cube-diff (abs-cube-diff guess x) prev_diff) 0.000001))
  (define (improve-curt guess)
    (/ (+ guess guess (/ x (square guess))) 3 ))
  (define (curt-iter guess prev_diff)
    (if (good-enough-cu? guess prev_diff)
        guess
        (curt-iter (improve-curt guess)
                   (abs-cube-diff guess x))))
  (curt-iter 1.0 0.0))

; = = = = = = = 
; = = = = = = = 

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial-up n)
  (define (fact-iter prod i)
    (if (> i n)
        prod
        (fact-iter (* i prod) (+ i 1))))
  (fact-iter 1 1))

(define (fib n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
              (cc (- amount
                     (first-denomination kinds-of-coins))
                  kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;Ex 1.11
;Recursive
(define (f-rec n)
  (if (< n 3)
      n
      (+ (f-rec (- n 1)) (* 2 (f-rec (- n 2))) (* 3 (f-rec (- n 3))))))

;Iterative
(define (f-it n)
  (define (f-it-loop fn-1 fn-2 fn-3 counter)
    (define (f-core )
      (+ fn-1 (* 2 fn-2) (* 3 fn-3)))
      (cond ((< n 3) n)
          ((= counter n) (f-core ) )
          (else
           (f-it-loop (f-core ) fn-1 fn-2 (+ 1 counter)))))
  (f-it-loop 2 1 0 3))

;Ex 1.12
(define (pasc-val layer position)
  (if ( or (<= position 1)
           (>= position layer))
      1
      (+ (pasc-val (- layer 1) (- position 1))
         (pasc-val (- layer 1) position))))

;Ex 1.16
;O(log n)
(define (fast-exp b n)
 (define (even x) (= (remainder x 2) 2))
 (define (halve x) (truncate (/ x 2)))
 (define (square x) (* x x))

  (define (b^n-it a bb nn)
    (cond ((= bb 0) 0)
          ((= nn 0 ) 1)
          ((= nn 1 ) (* a bb))
          ((even nn)
           (b^n-it a (* bb bb) (halve nn)))
          (else
           (b^n-it (* a bb) (* bb bb) (halve nn)))))
  (b^n-it 1 b n))

;Ex 1.17
(define (fast-* a b)
  (define (even x) (= (remainder x 2) 0))
  (define (double x) (+ x x))
  (define (halve x) (truncate (/ x 2)))
  (define (fast-*rec a1 b1)
    (cond ((= b1 1) a1)
          ((or (= a1 0) (= b1 0)) 0)
          ((even b1)
           (double (fast-*rec a1 (halve b1))))
          (else
           (+ a1 (fast-*rec a1 (- b1 1))))))

;= = = = = =

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (square a) (* a a))
(define (cube a ) (* a a a ))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
         (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes2 a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-ints a b)
  (sum identity a inc b))

(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))


;Ex 1.29
(define (simpson-integrate f a b n)
  (define (h-fun) (/ (- b a) n))
  (define (sum-fun k)
    (define (y-fun) (f (+ a (* (h-fun) k))))
    (cond ((or (= k 0) (= k n)) (y-fun))
          ((= (remainder k 2) 0) (* 2 (y-fun)))
          (else (* 4 (y-fun)))))
  (* (/ (h-fun) 3)
     (isum sum-fun 0 inc n)))

;Ex 1.30
(define (isum term a next b)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (+ res (term a)))))
  (iter a 0))

;Ex 1.31.1
(define (iprod term a b next)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (* res (term a)))))
  (iter a 1))

(define (factorial n)
  (iprod identity 1 n inc))

(define (find-pi steps)
  (define (pi-numer n)
    (+ 2 (if (= (remainder n 2) 0)
             n
             (+ n 1))))
  (define (pi-denom n)
    (+ 3 (if (= (remainder n 2) 1)
             (- n 1)
             n)))
  (define (pi-term n)
    (/ (pi-numer n) (pi-denom n)))
  (* 4 (iprod pi-term 0 steps inc)))

;Ex 1.31.2
(define (rprod term a b next)
  (define (prod-rec a)
    (if (> a b)
        1
        (* (term a) (prod-rec (next a)))))
  (prod-rec a))

;Ex 1.32.1
(define (i-accumulate combiner nullval term a b next)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a) (combiner res (term a)))))
  (iter a nullval))

(define (asum term a b next)
  (i-accumulate + 0 term a b next))

(define (aprod term a b next)
  (i-accumulate * 1 term a b next))

(define (r-accumulate combiner nullval term a b next)
  (define (rec a)
    (if (> a b)
        nullval
        (combiner (term a) (rec (next a)))))
  (rec a))

(define (rasum term a b next)
  (r-accumulate + 0 term a b next))

(define (raprod term a b next)
  (r-accumulate * 1 term a b next))

(define (even? val)
  (= (remainder val 2) 0))

;Ex 1.33
(define (filter-accumulate combiner nullval term a b next pred)
  (define (iter a res)
    (if (> a b)
        res
        (iter (next a)
              (if (pred (term a))
                  (combiner res (term a))
                  res))))
  
  (iter a nullval))


;;1.3.2
(define (test-let x)
  (let ((x 3)
        (y (+ x 2))) ; x on this line uses the 'test-let' x binding
    (* x y))) ;x on this line is bound to the 'let' x, 

(define (search-root f neg-point pos-point)
  (define (average a b)
    (/ (+ a b) 2.0))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond
            ((positive? test-value)
             (search-root f neg-point midpoint))
            ((negative? test-value)
             (search-root f midpoint pos-point))
            (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.00001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value)
                (positive? b-value))
           (search-root f a b))
          ((and (negative? b-value)
                (positive? a-value))
           (search-root f b a))
          (else
           (display "Values not of opposite sign" a b))))) ;No error function defined

(define tolerance .00000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt-fp x)
  (define (average a b)
    (/ (+ a b) 2.0))
  (fixed-point (lambda (y)
                 (average y (/ x y))) 1.0))

;Ex 1.35
(define (phi)
  (fixed-pointD (lambda (x)
                  (+ 1 (/ 1 x))) 1.0))

;Ex 1.36
(define (fixed-pointD f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (ex1-36)
    (define (average a b)
    (/ (+ a b) 2.0))
  (fixed-pointD (lambda (x)
                  (average x (/ (log 1000) (log x))))
                2.0))

(define (ex1-36-noaverage)
    (define (average a b)
      (/ (+ a b) 2.0))
  (fixed-pointD (lambda (x)
                  (/ (log 1000) (log x)))
                2.0))

;Ex 1.37.1
(define (cont-frac-rec n d k)
  (define (cont-f-rec i)
    (if ( = i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (cont-f-rec (+ i 1))))))
  (cont-f-rec 1))

(define (find-phi-rec k)
  (cont-frac-rec (lambda (x) 1.0)
                 (lambda (x) 1.0)
                 k))

;Ex 1.37.2
(define (cont-frac-it n d k)
  (define (cont-f-it i res)
    (if (= i 1)
        res
        (cont-f-it (- i 1) ( / (n (- i 1))
                               (+ (d (- i 1)) res)))))
  (cont-f-it k (/ (n k) (d k))))

(define (find-phi-it k)
  (cont-frac-it (lambda (x) 1.0)
                 (lambda (x) 1.0)
                 k))

;Ex 1.38
(define (e-2 k)
  (cont-frac-it (lambda (x) 1.0)
                (lambda (x)
                  (if (= (remainder (+ x 1) 3) 0)
                      (* 2 (/ (+ x 1) 3))
                      1))
                k))

;Ex 1.39
(define (tan-cf x k)
  (exact->inexact
   (cont-frac-it (lambda (k) (if (= k 1) x (* x x -1)))
                 (lambda (k) (+ 1 (* 2 (- k 1))))
                 k)))


;;1.3.4
(define (average-damp f)
  (define (average a b)
    (/ (+ a b) 2))
  (lambda (x)
    (average x (f x))))

(define (sqrt-ad x)
  (fixed-point
   (average-damp (lambda (y) (/ x y)))
   1.0))

(define (curt-ad x)
  (fixed-point
   (average-damp (lambda (y) (/ x (* y y))))
   1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.000001)

;Ex 1.40
(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x) 
                 c)))

;Ex 1.41
(define (double g)
  (lambda (x) (g (g x))))

;Ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;Ex 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;Ex 1.44
(define (smooth f)
  (lambda (x) (/ (+ (f x) (f (+ dx x)) (f (- x dx))) 3.0)))
