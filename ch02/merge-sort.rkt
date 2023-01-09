#lang racket


(require "../util.rkt")


(define merge-sort
  (lambda (cmp v)
    (let ([iter #f])
      (set! iter
        (lambda (l r)
          (let ([m (middle l r)])
            (if (= (add1 l) r)
              (list (vector-ref v l))
              (merge cmp (iter l m) (iter m r))))))
      (list->vector (iter 0 (vector-length v))))))


(define merge
  (lambda (cmp l1 l2)
    (cond
      [(and (null? l1) (null? l2)) '()]
      [(null? l1) l2]
      [(null? l2) l1]
      [(cmp (car l1) (car l2))
       (cons (car l1) (merge cmp (cdr l1) l2))]
      [else
       (cons (car l2) (merge cmp l1 (cdr l2)))])))


;; test
(merge-sort > '#(3 7 0 9 1 -1 2 11 5 6 2))