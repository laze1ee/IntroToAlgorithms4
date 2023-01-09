#lang racket


(require "../util.rkt"
         "../lane.rkt"
         "../binary-heap.rkt"
         "single-source-lib.rkt")

(provide dijkstra)



(define dijkstra
  (lambda (Graph s)
    (let ([sz (vector-length Graph)]
          [Vers (init-single-source Graph s)]
          [heap #f]
          [lane #f])
      
      (set! lane (new-Lane sz))
      (do ([i 0 (add1 i)]) ((= i sz))
        (Lane-set! lane i i))
      
      (set! heap
        (new-BHeap (lambda (i1 i2)
                     (< (.cost (vector-ref Vers i1))
                        (.cost (vector-ref Vers i2))))
                   lane))

      (let f ([u-index (BHeap-pop! heap)])
        (let ([v-index #f]
              [weight #f]
              [index #f])
          (do ([edges (vector-ref Graph u-index)
                      (cdr edges)]) ((null? edges))
            (set! v-index (0th (car edges)))
            (set! weight (1st (car edges)))
            (relax! Vers weight u-index v-index)
            (set! index (BHeap-index heap v-index))
            (when (natural? index)
              (BHeap-inform! heap index)))
          (when (not (BHeap-null? heap))
            (f (BHeap-pop! heap)))))

      Vers)))


#|
;; test
(define G
  '#(((1 10) (4 5)) ((2 1) (4 2)) ((3 4))
                    ((0 7) (2 6)) ((1 3) (2 9) (3 2))))

(map (lambda (source)
       (dijkstra G source))
     '(0 1 2 3 4))
|#