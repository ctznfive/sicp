#lang sicp

;==> Exercise 2.41 <==

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval n m)
  (if (> n m)
      '()
      (cons n
            (enumerate-interval (+ n 1) m))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j) (cons i j))
                  (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(define (right-triple? triple sum)
  (if (= (accumulate + 0 triple) sum)
      #t
      #f))

(define (func n sum)
  (filter (lambda (triple)
            (right-triple? triple sum))
          (unique-triples n)))

(func 5 6)
(func 13 10)

;==> Exercise 2.42 <==

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval n m)
  (if (> n m)
      '()
      (cons n
            (enumerate-interval (+ n 1) m))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (safe? position)
  (define (compare-row val nums)
    (if (null? nums)
        #f
        (or (= val (car nums))
             (compare-row val (cdr nums)))))
  (define (compare-diagonal rest row dir)
    (if (null? rest)
        #f
        (or (= row (car rest))
            (compare-diagonal (cdr rest) (dir row 1) dir))))
  (let ((row (car position))
        (rest (cdr position)))
    (if (or (compare-row row rest)
             (compare-diagonal rest (+ row 1) +)
             (compare-diagonal rest (- row 1) -))
        #f
        #t)))

(define (adjoin-position row rest-of-queens)
  (cons row rest-of-queens))

(length (queens 5))
(length (queens 6))
(length (queens 7))
(length (queens 8))

;==> Exercise 2.44 <==

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
            (below painter (beside smaller smaller)))))

;==> Exercise 2.45 <==

(define (split proc1 proc2)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split proc1 proc2) painter (- n 1))))
          (proc1 painter (proc2 smaller smaller))))))

;==> Exercise 2.46 <==

(define (make-vect x y)
  (cons x y))

(define (xcor-vect vec)
  (car vec))

(define (ycor-vect vec)
  (cdr vec))

(define (add-vec v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vec v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;==> Exercise 2.47 <==

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame1 frame) (car frame))

(define (edge1-frame1 frame) (cadr frame))

(define (edge2-frame1 frame) (caddr frame))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame) (car frame))

(define (edge1-frame2 frame) (cadr frame))

(define (edge2-frame2 frame) (cddr frame))

;==> Exercise 2.48 <==

(define make-segment cons)

(define start-segment car)

(define end-segment cdr)

;==> Exercise 2.49 <==

(define (bottom-left frame) 
  (origin-frame frame))

(define (top-left frame) 
  (add-vect (bottom-left frame)
            (edge1-frame frame)))

(define (top-right frame) (add-vect (top-left frame)
                                    (edge2-frame frame)))

(define (bottom-right frame) (add-vect (bottom-left frame)
                                       (edge2-frame frame)))

;a
(define (frame-painter frame)
  (let ((left (make-segment (bottom-left frame) (top-left frame)))
        (top (make-segment (top-left frame) (top-right frame)))
        (right (make-segment (top-right frame) (bottom-right frame)))
        (bottom (make-segment (bottom-right frame) (bottom-left frame)))
        )
    (segments-painter (list left top right bottom))))

;b
(define (x-painter frame)
  (let ((line1 (make-segment (bottom-left frame) (top-right frame)))
        (line2 (make-segment (top-left frame) (bottom-right frame)))
        )
    (segments-painter (list line1 line2))))

;==> Exercise 2.50 <==

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))
(define (rotate270 painter)
  (rotate90 (rotate90 (rotate90 painter))))

;==> Exercise 2.51 <==

(define (below1 painter1 painter2)
  (let ((paint-top
         (transform-painter
          painter1
          (make-vect 0.0 1.0)
          (make-vect 0.0 0.5)
          (make-vect 1.0 1.0)))
        (paint-bottom
         (transform-painter
          painter2
          (make-vect 0.0 0.0)
          (make-vect 0.0 0.5)
          (make-vect 0.0 1.0))))
    (lambda (frame)
      (paint-top frame)
      (paint-bottom frame))))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painters))))

;==> Exercise 2.54 <==

(define (equal? list1 list2)
  (cond ((and (not (pair? list1)) (not (pair? list2)))
         (eq? list1 list2))
        ((and (pair? list1) (pair? list2))
         (and (equal? (car list1) (car list2))
              (equal? (cdr list1) (cdr list2))))
        (else #f)))

(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

;==> Exercise 2.57 <==

(define (augend x)
  (if (null? (cdddr x))
      (caddr x)
      ((cons '+ (cddr x)))))

(define (multiplicand x)
  (if (null? (cdddr x))
      (caddr x)
      ((cons '* (cddr x)))))

;==> Exercise 2.58 <==

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a b)
  (cond ((=number? a 0) b)
        ((=number? b 0) a)
        ((and (number? a) (number? b)) (+ a b))
        (else (list  a '+ b ))))

(define (make-product a b)
  (cond ((or (=number? a 0) (=number? b 0)) 0)
        ((=number? a 1) b)
        ((=number? b 1) a)
        ((and (number? a) (number? b)) (* a b))
        (else (list a '* b))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        (else
         (error "unKnown expression type: DERIV" exp))))

(define (op expr)
  (cond ((memq '+ expr) '+)
        ((memq '* expr) '*)))

(define (sum? expr) (eq? '+ (op expr)))

(define (product? expr) (eq? '* (op expr)))

(define (addend expr)
  (define (iter expr result)
    (if (eq? (car expr) '+)
        result
        (iter (cdr expr) (append result (list (car expr))))))
  (let ((result (iter expr '())))
    (if (= (length result) 1)
        (car result)
        result)))

(define (augend expr)
  (let ((result (cdr (memq '+ expr))))
    (if (= (length result) 1)
        (car result)
        result)))

(define (multiplier expr)
  (define (iter expr result)
    (if (eq? (car expr) '*)
        result
        (iter (cdr expr) (append result (list (car expr))))))
  (let ((result (iter expr '())))
    (if (= (length result) 1)
        (car result)
        result)))

(define (multiplicand expr)
  (let ((result (cdr (memq '* expr))))
    (if (= (length result) 1)
        (car result)
        result)))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '(x + 3) 'x)
(deriv '(x * y * (x + 3)) 'x)

;==> Exercise 2.59 <==

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

(intersection-set (list 1 2 3 4 5) (list 1 3 5))
(union-set (list 1 2 3 4 5) (list 1 3 5))

;==> Exercise 2.60 <==

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;==> Exercise 2.61 <==

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;==> Exercise 2.62 <==

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((> x1 x2) (cons x2 (union-set (cdr set2) set1)))
                      ((< x1 x2) (cons x1 (union-set (cdr set1) set2))))))))

(union-set (list 7 8 9 10) (list 2 4 6 8))

;==> Exercise 2.63 <==

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)

(list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))

(copy-to-list tree '()))

(define tree1 (make-tree 7 (make-tree 3
                           (make-tree 1 '() '())
                           (make-tree 5 '() '()))
                           (make-tree 9 '() (make-tree 11 '() '()))))

(define tree2 (make-tree 3 (make-tree 1 '() '())
                           (make-tree 7 (make-tree 5 '() '())
                           (make-tree 9 '() (make-tree 11 '() '())))))

(define tree3 (make-tree 5 (make-tree 3 (make-tree 1 '() '())
                                      '())
                         (make-tree 9 (make-tree 7 '() '())
                                    (make-tree 11 '() '()))))
;a
(tree->list-1 tree1)
(tree->list-2 tree1)
(tree->list-1 tree2)
(tree->list-2 tree2)
(tree->list-1 tree3)
(tree->list-2 tree3)

;==> Exercise 2.65 <==

(define (union-set set1 set2) 
   (define (aux set1 set2) 
     (cond ((null? set2) set1) 
           ((null? set1) set2) 
           ((= (car set1) (car set2)) 
            (cons (car set1) (aux (cdr set1) (cdr set2)))) 
           ((< (car set1) (car set2)) (cons (car set1) (aux (cdr set1) set2))) 
           (else (cons (car set2) (aux set1 (cdr set2)))))) 
   (list->tree (aux (tree->list-2 set1) (tree->list-2 set2)))) 
  
(define (intersection-set set1 set2) 
   (define (aux set1 set2) 
         (cond ((or (null? set1) (null? set2)) '()) 
               ((= (car set1) (car set2)) 
                (cons (car set1) (aux (cdr set1) (cdr set2)))) 
               ((< (car set1) (car set2)) 
                (aux (cdr set1) set2)) 
               ((> (car set1) (car set2)) 
                (aux set1 (cdr set2))))) 
   (list->tree (aux (tree->list-2 set1) (tree->list-2 set2))))

;==> Exercise 2.66 <==

(define (look-up key set)
  (cond ((null? set) #f)
        ((= key (car set)) #t)
        ((> key (car set)) (look-up key (caddr set)))
        ((< key (car set) (look-up key (cadr set))))))

;==> Exercise 2.69 <==

(define (make-leaf symbol weight) (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
        ((= (length leaf-set) 1) (car leaf-set))
        ((= (length leaf-set) 2) (make-code-tree (car leaf-set) (cadr leaf-set)))
        (else (let ((sub-tree (make-code-tree (car leaf-set) (cadr leaf-set))))
                (let ((sub-set (adjoin-set sub-tree (cddr leaf-set))))
                  (successive-merge sub-set))))))

(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))
(generate-huffman-tree '((BOOM 1) (WAH 1) (A 2) (GET 2) (JOB 2) (SHA 3) (NA 16) (YIP 9)))

;==> Exercise 2.75 <==

(define (make-from-magang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknow op --- MAKE-FROM-MAGANG" op))))
  dispatch)

;==> Exercise 2.87 <==

(define (zero-poly? p)
  (empty-termlist? (term-list p)))

(put '=zero? 'polynomial zero-poly?)

;==> Exercise 2.89 <==

(define (first-term term-list)
  (make-term (- (length term-list) 1) (car term-list)))

(define (adjoin-term term term-list)
  (cond ((zero? term) term-list)
        ((= (order term) (length term-list)) (cons (coeff term) term-list))
        (else (adjoin-term term (cons 0 term-list)))))

;==> Exercise 2.93 <==

(define (install-rational-package)
  (define (make-rat n d)
    (if (and (integer? n) (integer? d))
        (cons (/ n (gcd n d)) (/ d (gcd n d)))
        (cons n d))))
