#lang racket


(require "../util.rkt"
         "../ch20/topological-sort.rkt"
         "single-source-lib.rkt")


(define remove-weight
  (lambda (Graph)
    (let* ([sz (vector-length Graph)]
           [G2 (make-vector sz)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set!
         G2 i
         (let f ([edges (vector-ref Graph i)])
           (if (null? edges)
             '()
             (cons (0th (car edges))
                   (f (cdr edges)))))))
      G2)))


(define dag-shortest-paths
  (lambda (Graph s)
    (let ([order (topological-sort (remove-weight Graph))]
          [Vers (init-single-source Graph s)]
          [u-index #f]
          [v-index #f]
          [weight #f])
      (do ([order order (cdr order)]) ((null? order))
        (set! u-index (car order))
        (do ([edges (vector-ref Graph u-index)
                    (cdr edges)]) ((null? edges))
          (set! v-index (0th (car edges)))
          (set! weight (1st (car edges)))
          (relax! Vers weight u-index v-index)))
      Vers)))



;; test
(define G
  '#(((1 2) (2 6)) ((2 7) (3 4) (4 2)) ((3 -1) (4 1)) ((4 -2))
                   () ((0 5) (1 3))))

(map (lambda (source)
       (dag-shortest-paths G source))
     '(0 1 2 3 4 5))