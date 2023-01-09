#lang racket


(require "../util.rkt")

(provide binary-search)


(define binary-search
  (lambda (x sorted-v)
    (let f ([i 0]
            [j (vector-length sorted-v)])
      (if (= i j)
        #f
        (let ([m (middle i j)])
          (cond
            [(= x (vector-ref sorted-v m)) m]
            [(< x (vector-ref sorted-v m))
             (f i m)]
            [else
             (f (add1 m) j)]))))))


#|
;; test
(binary-search 3 '#(4 6 7 9 15 17 20 21 34))
|#