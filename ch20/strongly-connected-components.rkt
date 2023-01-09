#lang racket


(require "../util.rkt"
         "depth-first-search.rkt"
         "topological-sort.rkt")


(define transpose
  (lambda (Graph)
    (let* ([sz (vector-length Graph)]
           [Gt (make-vector sz '())])
      (do ([i 0 (add1 i)]) ((= i sz))
        (do ([as (vector-ref Graph i) (cdr as)]) ((null? as))
          (vector-set! Gt (car as)
                       (cons i (vector-ref Gt (car as))))))
      Gt)))


(define strongly-connected
  (lambda (Graph)
    (let* ([sz (vector-length Graph)]
           [Gt (transpose Graph)]
           [order (topological-sort Graph)])
      (dfs Gt order))))


;; test
(define de-G
  '#((1) (2 4 5) (3 6) (2 7) (0 5) (6) (5 7) (7)))

(strongly-connected de-G)