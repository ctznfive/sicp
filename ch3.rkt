#lang sicp

;==> Exercise 3.17 <==

(define (count-pairs x)
  (define pair-table '())
  (define (is-in-table? p table)
    (if (memq p table)
        #t
        #f))
  (define (iter cur-pair)
    (cond ((not (pair? cur-pair)) 0)
          ((is-in-table? cur-pair pair-table) 0)
          (else
           (begin (set! pair-table (cons cur-pair pair-table))
                  (+ (iter (car cur-pair))
                     (iter (cdr cur-pair))
                     1)
                  ))))
  (iter x)
  (display pair-table)
  (display "\n"))

(define a (cons 'A 'B))

(define b (cons 'C 'D))

(define list-test (cons a b))

(count-pairs list-test)

(set-cdr! a b)

(count-pairs list-test)

(set-car! a b)

(set-cdr! list-test a)

(count-pairs list-test)

;==> Exercise 3.18 <==

(define (is-cycle-list? l)
  (let ((table '()))
    (define (loop lst)
      (if (not (pair? lst))
          #f
          (if (memq lst table)
              #t
              (begin (set! table (cons lst table))
                     (or (loop (car lst))
                         (loop (cdr lst)))))))
    (loop l)))

;==> Exercise 3.21 <==

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT -> empty queue")
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! -> empty queue" queue))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

(define (print-queue queue)
  (define (print-ele p)
    (if (pair? p)
        (begin (display (car p))
         (print-ele (cdr p)))
        (display "\n")))
  (print-ele (front-ptr queue)))

(define q1 (make-queue))
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)

;==> Exercise 3.22 <==

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT -> empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?
                (set! front-ptr new-pair)
                (set! rear-ptr new-pair)))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair))))
      front-ptr)
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! -> empty queue"))
            (else
             (set! front-ptr (cdr front-ptr))))
      front-ptr)
    (define (dispatch m)
      (cond ((eq? m 'empty-queue) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue) (insert-queue!))
            ((eq? m 'delete-queue) (delete-queue!))))
    dispatch))

;==> Exercise 3.23 <==

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (make-deque) (cons '() '()))

(define (empty-deque? deque) 
  (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "GET FRONT FAILED")
      (cadr (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "GET REAR FAILED")
      (cadr (rear-ptr deque))))

(define (front-insert-deque! ele deque)
  (let ((deque-ele (cons '() (cons ele '()))))
    (cond ((empty-deque? deque)
           (set-car! deque deque-ele)
           (set-cdr! deque deque-ele)
           deque)
          (else
           (set-cdr! (cdr deque-ele) (front-ptr deque))
           (set-car! (front-ptr deque) deque-ele)
           (set-car! deque deque-ele)
           deque))))

(define (rear-insert-deque! ele deque)
  (let ((deque-ele (cons '() (cons ele '()))))
    (cond ((empty-deque? deque)
           (set-car! deque deque-ele)
           (set-cdr! deque deque-ele)
           deque)
          (else
           (set-cdr! (cdr (rear-ptr deque)) deque-ele)
           (set-car! deque-ele (rear-ptr deque))
           (set-cdr! deque (cddr (rear-ptr deque)))
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "failed to delete"))
         (else
          (set-car! deque (cddr (front-ptr deque)))
          (if (null? (front-ptr deque))
              (set-cdr! deque '())
              (set-car! (front-ptr deque) '()))        
          deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "failed to delete"))
        (else
         (set-cdr! deque (car (rear-ptr deque)))
         (if (null? (rear-ptr deque))
             (set-car! deque '())
             (set-cdr! (cdr (rear-ptr deque)) '()))         
         deque)))

(define (print-deque deque)
  (if (pair? deque)
      (begin
        (display (cadr deque))
        (print-deque (cddr deque)))
      (display "\n")))

(define q (make-deque))

(empty-deque? q)
(rear-insert-deque! 'a q)
(print-deque (front-ptr q))
(front-insert-deque! 'b q)
(print-deque (front-ptr q))
(rear-insert-deque! 'c q)
(print-deque (front-ptr q))
(front-delete-deque! q)
(print-deque (front-ptr q))
(rear-delete-deque! q)
(print-deque (front-ptr q))

;==> Exercise 3.24 <==

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: Table" m))))
  dispatch))

;==> Exercise 3.26 <==

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((< (car) (car (entry set)))
         (let ((left (left-branch set)))
           (set! left (adjoin-set x left))))
        ((> (car x) (car (entry set)))
         (let ((right (right-branch set)))
           (set! right (adjoin-set x right))))))

(define (assoc key record-tree)
  (cond ((null? record-tree) #f)
        ((eq? key (car (entry record-tree))) (entry record-tree))
        ((< key (car (entry record-tree))) (assoc key (left-branch record-tree)))
        ((> key (car (entry record-tree)) (assoc key (right-branch record-tree))))))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (look-up key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            #f)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (adjoin-set (cons key value) (cdr local-table)))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) look-up)
            ((eq? m 'insert-proc!) insert!)
            (else "wrong operation" m)))
    dispatch))

;==> Exercise 3.28 <==

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logic-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure))

(define (logic-or s1 s2)
  (if (or (= s1 1) (= s2 1))
      1
      0))

;==> Exercise 3.30 <==

(define (ripple-carry-adder a-list b-list s-list c)
  (let ((c-list (map (lambda (x) (make-wire)) (cdr a-list)))
        (c-0 (make-wire)))
    (map full-adder
         a-list
         b-list
         (append c-list (list c-0))
         s-list
         (cons c c-list))))

;==> Exercise 3.37 <==

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (c+ x (- 0 y)))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x))
  z)

(define (cv x)
  (let ((z (make-connector)))
    (constant x z))
  z)

;==> Exercise 3.47 <==

(define (make-semaphore n)
  (let ((lock (make-mutex))
        (taken 0))
    (define (semaphore command)
      (cond ((eq? command 'acquire)
             (lock 'acquire)
             (if (< taken n)
                 (begin (set! taken (+ 1 taken) (lock 'release)))
                 (begin (lock 'release) (semaphore 'acquire))))
            ((eq? command 'release)
             (lock 'acquire)
             (set! taken (- taken 1))
             (lock 'release))))
    semaphore))

;==> Exercise 3.48 <==

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (exchanger (if (< (account1 'id) (account2 'id))
                       (serializer2 (serializer1 exchange))
                       (serializer1 (serializer2 exchange)))))
    (exchanger account1 account2)))

;==> Exercise 3.50 <==

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;==> Exercise 3.54 <==

(define (mul-stream s1 s2)
  (stream-map * s1 s2))
(define factorials
  (cons-stream 1
               (mul-streams factorials integers)))

;==> Exercise 3.55 <==

(define (partial-sum s)
  (define ps (add-streams s (stream-cons 0 ps)))
  ps)

;==> Exercise 3.56 <==

(define S (cons-stream 1 (merge (merge (scale-stream S 2)
                                       (scale-stream S 3))
                                (scale-stream S 5))))

;==> Exercise 3.61 <==

(define (invert-unit-series s)
  (define series
    (stream-cons
     1
     (scale-stream (mul-series series
                               (stream-cdr s)) -1)))
  series)

;==> Exercise 3.64 <==

(define (stream-limit stream tolerance)
        (if (< (abs (- (stream-ref stream 1) (stream-ref stream 0))) tolerance)
                (stream-ref stream 1)
                (stream-limit (stream-cdr stream) tolerance)))

;==> Exercise 3.65 <==

(define (ln2-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sum (ln2-summands 1)))

;==> Exercise 3.67 <==

(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x)
                  (list (stream-car s)
                       x))
                (stream-cdr t))
    (pairs (stream-cdr s) t))))

;==> Exercise 3.69 <==

(define (triples s t u)
  (let ((pair (pairs s t)))
   (pairs (pair u))))

(define (phythagorean-numbers)
  (define (square x) (* x x))
  (define numbers (triples integers integers integers))
  (stream-filter (lambda (x)
                   (= (square (caddr x))
                      (+ (square (car x)) (square (cadr x)))))
                 numbers))

;==> Exercise 3.70 <==

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1)) (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream
                   s1car
                   (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream
                   s2car
                   (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream
                   s1car
                   (cons-stream
                    s2car
                    (merge-weighted (stream-cdr s1)
                                    (stream-cdr s2) weight)))))))))

;==> Exercise 3.71 <==

(define (Ramanujan s) 
          (define (stream-cadr s) (stream-car (stream-cdr s))) 
          (define (stream-cddr s) (stream-cdr (stream-cdr s))) 
          (let ((scar (stream-car s)) 
                    (scadr (stream-cadr s))) 
                 (if (= (sum-triple scar) (sum-triple scadr))  
                         (cons-stream (list (sum-triple scar) scar scadr) 
                                                  (Ramanujan (stream-cddr s))) 
                         (Ramanujan (stream-cdr s)))))

(define Ramanujan-numbers 
         (Ramanujan (weighted-pairs integers integers sum-triple)))
