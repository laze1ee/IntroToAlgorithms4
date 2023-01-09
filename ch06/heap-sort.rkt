#lang racket


(require "../lane.rkt"
         "heap.rkt")


(define heapsort!
  (lambda (cmp A)
    (build-heap! cmp A)
    (let ([length (Lane.length A)])
      (do ([i (sub1 length) (sub1 i)]) ((= i 0))
        (Lane-exchange! A 0 i)
        (set! length (sub1 length))
        (heapify! cmp A 0 length)))))


(define inc-heapsort!
  (lambda (v)
    (heapsort! > v)))


(define dec-heapsort!
  (lambda (v)
    (heapsort! < v)))



;; test
(define A (new-Lane 15 0))
(Lane-random-fill! A 0 100)
A
(inc-heapsort! A)
A
(dec-heapsort! A)
A