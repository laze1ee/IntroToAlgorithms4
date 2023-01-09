#lang racket


(require "../util.rkt"
         "../matrix.rkt")

(provide new-vertex
         cost-ref cost-set!
         pred-ref pred-set!
         both!)



;; Vertex-Attribution: #(Cost Predecessor-Index)
(define new-vertex
  (case-lambda
    [() (vector +inf.0 -1)]
    [(cost pred) (vector cost pred)]))

(define .cost  (lambda (vertex) (ref0 vertex)))
(define .cost! (lambda (vertex c) (set0! vertex c)))
(define .pred  (lambda (vertex) (ref1 vertex)))
(define .pred! (lambda (vertex i) (set1! vertex i)))


;; Adjacency-Matrix
(define cost-ref
  (lambda (Graph row col)
    (.cost (Matrix-ref Graph row col))))

(define cost-set!
  (lambda (Graph row col c)
    (.cost! (Matrix-ref Graph row col) c)))

(define pred-ref
  (lambda (Graph row col)
    (.pred (Matrix-ref Graph row col))))

(define pred-set!
  (lambda (Graph row col i)
    (.pred! (Matrix-ref Graph row col) i)))


;; for test
(define both!
  (lambda (Graph row col cost pred-index)
    (cost-set! Graph row col cost)
    (pred-set! Graph row col pred-index)))