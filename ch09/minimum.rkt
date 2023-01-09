#lang racket


(require "../util.rkt")


(define 2minimum
  (lambda (v)
    (let merge ([start 0] [bound (vector-length v)])
      (if (= 1 (- bound start))
        (list (vector-ref v start))
        (let ([mid (middle start bound)]
              [ls1 #f]
              [ls2 #f])
          (set! ls1 (merge start mid))
          (set! ls2 (merge mid bound))
          (compare ls1 ls2))))))


(define compare-iter
  (lambda (ls1 ls2 count)
    (cond
      [(= 2 count) '()]
      [(null? ls1) ls2]
      [(null? ls2) ls1]
      [(< (0th ls1) (0th ls2))
       (cons (0th ls1)
             (compare-iter (cdr ls1) ls2 (add1 count)))]
      [else
       (cons (0th ls2)
             (compare-iter ls1 (cdr ls2) (add1 count)))])))


(define compare
  (lambda (ls1 ls2)
    (compare-iter ls1 ls2 0)))



;; test
(define v (make-vector 17 0))
(random-fill! v 0 99)
v
(2minimum v)