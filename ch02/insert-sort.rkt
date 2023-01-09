#lang racket


(require "../util.rkt")


(define insert-sort!
  (lambda (cmp v)
    (let ([length (vector-length v)])
      (do ([i 1 (add1 i)]) ((= i length))
        (do ([j i (sub1 j)]
             [k (sub1 i) (sub1 k)])
          ((or (= k -1)
               (not (cmp (vector-ref v j) (vector-ref v k)))))
          (exchange! v j k))))))


;; test
(define v (vector 5 3 1 0 6 8 9 7 3 2 7 4))
(insert-sort! < v)
v