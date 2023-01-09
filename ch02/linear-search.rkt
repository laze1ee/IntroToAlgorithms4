#lang racket


(define linear-search
  (lambda (v x)
    (let ([n (vector-length v)])
      (let f ([i 0])
        (cond
          [(= i n) #f]
          [(= x (vector-ref v i)) i]
          [else
           (f (+ i 1))])))))

;; test
(linear-search '#(5 0 53 2 24) 7)