#lang racket


(require "../util.rkt"
         "../matrix.rkt")



(define greedy-select
  (lambda (M row fresh)
    (let f ([i 0]
            [x #f])
      (set! x (Matrix-ref M row i))
      (if (vector-ref fresh x)
        (list i x)
        (f (add1 i) #f)))))


(define get-rank
  (lambda (M row x)
    (let f ([i (sub1 (Matrix.col M))])
      (if (= x (Matrix-ref M row i))
        i
        (f (sub1 i))))))


(define existing
  (lambda (M row i fresh)
    (if (= 0 i)
      #f
      (let f ([i (sub1 i)])
        (cond
          [(= i -1) #f]
          [(vector-ref fresh (Matrix-ref M row i))
           i]
          [else
           (f (sub1 i))])))))


(define pair!
  (lambda (A a fresh-A B fresh-B)
    (let* ([result (greedy-select A a fresh-B)]
           [a.i (0th result)]
           [b (1st result)]
           [b.i (get-rank B b a)])
      (let f ([b.i (existing B b b.i fresh-A)])
        (cond
          [(boolean? b.i)
           (vector-set! fresh-A a #f)
           (vector-set! fresh-B b #f)
           (list a b)]
          [else
           (let* ([a1 (Matrix-ref B b b.i)]
                  [a1.i (get-rank A a1 b)])
             (when (<= a1.i a.i)
               (set! a a1)
               (set! a.i a1.i))
             (f (existing B b b.i fresh-A)))])))))


(define a-fresh
  (lambda (sz fresh)
    (let f ([i 0])
      (cond
        [(= i sz) #f]
        [(vector-ref fresh i) i]
        [else
         (f (add1 i))]))))


(define gale-shapley
  (lambda (A B)
    (let* ([sz (Matrix.col A)]
           [fresh-A (make-vector sz #t)]
           [fresh-B (make-vector sz #t)]
           [M '()])
      (let f ([a (a-fresh sz fresh-A)])
        (when (natural? a)
          (set! M
            (cons (pair! A a fresh-A B fresh-B) M))
          (f (a-fresh sz fresh-A))))
      M)))


;; test
(define rank-group
  (lambda (size)
    (let ([M (new-Matrix size size)]
          [rank #f])
      (do ([i 0 (add1 i)]) ((= i size))
        (set! rank (random-vector 0 size))
        (do ([j 0 (add1 j)]) ((= j size))
          (Matrix-set! M i j
                       (vector-ref rank j))))
      M)))

(define size 5)

(define A (rank-group size))
(define B (rank-group size))
(define pairs (gale-shapley A B))

(do ([i 0 (add1 i)]) ((= i size))
  (do ([j 0 (add1 j)]) ((= j size))
    (Matrix-set! A i j
                 (+ (Matrix-ref A i j) size))))

(set! pairs
  (let f ([lst pairs])
    (if (null? lst)
      '()
      (let ([pair (car lst)])
        (cons (list (0th pair) (+ (1st pair) size))
              (f (cdr lst)))))))

(Matrix-print A)
(Matrix-print B)
pairs