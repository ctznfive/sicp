#lang sicp

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define (numer rat)
  (car rat))

(define (denom rat)
  (cdr rat))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;Ex 2.1
(define (make-rat num den)
  (let ((g (gcd (abs num) (abs den)))
        (sign (/ (* num den) (abs (* num den)))))
    (cons (* sign (abs (/ num g)))
          (* (/ (abs den) g)))))

;Ex 2.2
(define (make-point x y)
  (cons x y))

(define (x-point pt)
  (car pt))

(define (y-point pt)
  (cdr pt))

(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

(define (mid-point seg)
  (let ((avg (lambda (b e)
               (/ (+ b e ) 2))))
    (make-point
     (avg (x-point (start-segment seg)) (x-point (end-segment seg)))
     (avg (y-point (start-segment seg)) (y-point (end-segment seg))))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(print-point
 (mid-point
  (make-segment (make-point 0 1) (make-point 5.0 10.0))))

;Ex 2.4.1
(define (cons1 x y) 
  (lambda (m) (m x y)))

(define (car1 z) 
  (z (lambda (p q) p)))

(car1 (cons1 1 2))
((lambda (m) ( m 1 2))
 (lambda (p q) p))

((lambda (p q)
   p) 1 2)

;Ex 2.4.2
(define (cdr1 z)
  (z (lambda (p q) q)))

(cdr1 (cons1 1 2))

(define (cons-int a b)
  (* (expt 2 a) (expt 3 b)))

(define (log2 n)
    (/ (log n) (log 2)))
(define (log3 n)
    (/ (log n) (log 3)))

(define (reduce base n)
  (if (= (remainder n base) 0)
      (reduce base (/ n base))
      n))

(define (car-int i)
  (log2 (reduce 3 i)))

(define (cdr-int i)
  (log3 (reduce 2 i)))

(car-int (cons-int 4 6))
(cdr-int (cons-int 4 6))


;;2.1.4
(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

;Ex 2.7
(define (upper-bound interval)
  (cdr interval))
(define (lower-bound interval)
  (car interval))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))
