#lang racket


(require "../util.rkt"
         "../lane.rkt"
         "../binary-heap.rkt")


;; Vertex: #(Key Parent-Index)
(define new-vertex (lambda () (vector +inf.0 -1)))
(define .key     (lambda (vertex) (ref0 vertex)))
(define .key!    (lambda (vertex key) (set0! vertex key)))
(define .parent  (lambda (vertex) (ref1 vertex)))
(define .parent! (lambda (vertex parent-index) (set1! vertex parent-index)))


(define mst-prim
  (lambda (Graph s)
    (let* ([sz (vector-length Graph)]
           [Vertex (make-vector sz)]
           [A (new-Lane sz)])
      
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! Vertex i (new-vertex))
        (Lane-set! A i i))
      
      (.key! (vector-ref Vertex s) 0)
      
      (set! A (new-BHeap
               (lambda (i1 i2)
                 (< (.key (vector-ref Vertex i1))
                    (.key (vector-ref Vertex i2))))
               A))
      
      (let f ([u-index (BHeap-pop! A)])
        (let ([u (vector-ref Vertex u-index)]
              [v-index #f]
              [v #f]
              [w #f]
              [index #f])
          (do ([vs (vector-ref Graph u-index) (cdr vs)]) ((null? vs))
            (set! v-index (0th (car vs)))
            (set! v (vector-ref Vertex v-index))
            (set! w (1st (car vs)))
            (set! index (BHeap-index A v-index))
            (when (and (natural? index)
                       (< w (.key v)))
              (.key! v w)
              (.parent! v u-index)
              (BHeap-inform! A index)))
          (when (not (BHeap-null? A))
            (f (BHeap-pop! A)))))

      Vertex)))


;; test
;; Adjacency-List: ((Vertex Weight) ...)
(define un-G
  '#(((1 4) (7 8)) ((0 4) (2 8) (7 11)) ((1 8) (3 7) (5 4) (8 2))
                   ((2 7) (4 9) (5 14)) ((3 9) (5 10))
                   ((2 4) (3 14) (4 10)) ((5 2) (7 1) (8 6))
                   ((0 8) (6 1) (8 7)) ((2 2) (6 6) (7 7))))
(mst-prim un-G 0)