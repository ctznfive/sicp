#lang racket

(define nil '())

(define (list-ref1 items n)
  (if (= n 0)
      (car items)
      (list-ref1 (cdr items) 
                (- n 1))))

(define (length1 items)
  (if (null? items)
      0
      (+ 1 (length1 (cdr items)))))

;Ex 2.17
(define (last-pair list1)
  (define (last-pair-iter list1 item)
    (if (null? (cdr list1))
        (cons item (car list1))
        (last-pair-iter (cdr list1) (car list1))))
  (if (null? (cdr list1))
      list1
      (last-pair-iter (cdr list1) (car list1))))

;Ex 2.18
(define (reverse1 list1)
  (if (null? list1)
      list1
      (cons (reverse1 (cdr list1)) (car list1))))

;Ex 2.19
(define us-coins 
  (list 50 25 10 5 1))

(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

;Ex 2.20
(define (same-parity . l1)
  (if (null? (car l1))
      l1
      (let ((parity (remainder (car l1) 2)))
        (define (build-list l)
          (if (null? l)
              l
              (let ((q (car l)))
                (if (= parity (remainder q 2))
                    (cons (car l) (build-list (cdr l)))
                    (build-list (cdr l))))))
        (build-list l1))))

(same-parity 2 3 4 5 6 7)

(define (map1 proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map1 proc (cdr items)))))

;Ex 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(square-list '(1 2 3 4 5))
(square-list2 '(1 2 3 4 5))

;Ex 2.25
(define ex2251 (list 1 3 (list 5 7) 9))

(cadr (caddr ex2251))
(define ex2252 (list (list 7)))

(car (car ex2252))
(define ex2253 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(cadr (cadr (cadr (cadr (cadr (cadr ex2253))))))

;2.28
(define (fringe listp)
  (cond ((null? listp) listp)
        ((not (pair? listp)) (list listp))
        (else
         (append (fringe (car listp)) (fringe (cdr listp))))))

(fringe (list (list 1 (list 5 6) 2) (list 3 4)))

;Ex 2.30.1
(define (square-tree-d in)
  (cond ((not (pair? in)) (* in in))
        ((not (pair? (cdr in)))
         (square-tree-d (car in)))
        ((not (pair? (car in)))
         (list (* (car in) (car in)) (square-tree-d (cdr in))))
        (else
         (list (square-tree-d (car in)) (square-tree-d (cdr in))))))

(square-tree-d test-mobile)

;Ex 2.30.2
(define (square-tree-m in)
  (map (lambda (sub)
         (if (pair? sub)
             (square-tree-m sub)
             (* sub sub))) in))

(square-tree-m test-mobile)

;Ex 2.31
(define (tree-map fn tree)
  (map (lambda (sub)
         (if (pair? sub)
             (tree-map fn sub)
             (fn sub)))
       tree))

(tree-map (lambda (x) (* x x)) test-mobile)

;Ex 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))


;;2.2.3
(define (filter1 predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter1 predicate 
                       (cdr sequence))))
        (else  (filter1 predicate 
                       (cdr sequence)))))

(define (accumulate1 op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate1 op 
                      initial 
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append 
               (enumerate-tree (car tree))
               (enumerate-tree (cdr tree))))))

;Ex 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate1
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0 coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

;Ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate1 op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 s)

(define (dot-product v w)
  (accumulate1 + 0 (map * v w)))

;Ex 2.37
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define test-mat 
  (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

(matrix-*-vector test-mat (list 2 2 2))

;Ex 2.37.2
(define (transpose mat)
  (accumulate-n cons nil mat))

(transpose test-mat)

;Ex 2.37.3
(define  (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(matrix-*-matrix test-mat test-mat)

;Ex 2.38
(define fold-right accumulate1)
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
