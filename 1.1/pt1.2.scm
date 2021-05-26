;; recursive process
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(factorial 6)
(factorial 10)
(factorial 20)
(factorial 30)
(factorial 40)
(factorial 50)
(factorial 500)


;; iterative process
(define (factorial2 n)
  (define (iter product counter)
    (if (> counter n)
	product
	(iter (* product counter) (+ counter 1))))
  (iter 1 1))

(factorial2 6)


;; tree recursion
(define (fib n) ; recursive
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib (- n 1))
		 (fib (- n 2))))))

(fib 5)
(fib 30)


(define (fibb n) ; iterative
  (fibb-iter 1 0 n))

(define (fibb-iter a b count)
  (if (= count 0)
      b
      (fibb-iter (+ a b) a (- count 1))))

(fibb 30)
(fibb 50)



;; couting change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

(count-change 100) ; => 292
(count-change 10) ; => 4



;; exponentiation

;; recursive
(define (exp b n)
  (if (= n 0)
      1
      (* b (exp b (- n 1)))))

(exp 5 2)
(exp 3 3)
(exp 10 10)

;; iterative
(define (exp2 b n)
  (exp2-iter b n 1))

(define (exp2-iter b count prod)
  (if (= count 0)
      prod
      (exp2-iter b (- count 1) (* b prod))))

(exp2 5 2)
(exp2 3 3)
(exp2 10 10)

;; log
(define (exp3 b n)
  (cond ((= n 0) 1)
	((even? n) (square (exp3 b (/ n 2))))
	(else (* b (exp3 b (- n 1))))))

(define (square n)
  (* n n))

(define (even? n)
  (= (remainder n 2) 0))

(exp3 5 2)
(exp3 3 3)
(exp3 10 10)
