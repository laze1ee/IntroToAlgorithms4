#lang racket


(require "../ch07/quick-sort.rkt"
         "../util.rkt")

(provide random-select!)


(define random-partition!
  (lambda (v l r)
    (random-part! < v l r)))


(define random-select!
  (lambda (v i)
    (let ([len (vector-length v)])
      (cond
        [(= 0 len)
         (error 'random-select! "vector ~s is empty" v)]
        [(and (<= 0 i) (< i len))
         (select! v 0 len i)]
        [else
         (error 'random-select!
                "index ~s in invalid for vector ~s"
                i v)]))))


(define select!
  (lambda (v l r i)
    (if (= 1 (- r l))
      (vector-ref v l)
      (let ([m (random-partition! v l r)])
        (cond
          [(= m i) (vector-ref v i)]
          [(< i m)
           (select! v l m i)]
          [else
           (select! v (add1 m) r i)])))))


#|
;; test
(define v (make-vector 14 0))
(random-fill! v 0 100)
(random-select! v 6)
(random-quick-sort2! < v)
v
|#