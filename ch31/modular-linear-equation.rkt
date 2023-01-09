#lang racket


(require "../util.rkt"
         "gcd.rkt")

(provide mod-le)


;; modular linear equation solver
(define mod-le
  (lambda (a b n)
    (let* ([dxy (extended-euclid a n)]
           [d (0th dxy)]
           [x (1st dxy)])
      (if (= 0 (remainder b d))
        (let ([x0 (modulo (* x (/ b d)) n)])
          (let f ([i 0])
            (if (= i d)
              '()
              (cons (modulo (+ x0 (* i (/ n d))) n)
                    (f (add1 i))))))
        #f))))


#|
;; test
(mod-le 4 2 8)
|#