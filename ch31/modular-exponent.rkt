#lang racket


(require "../util.rkt")

(provide modular-expt)



(define modular-expt
  (lambda (a b n)
    (cond
      [(= 0 b) 1]
      [(= 0 (modulo b 2))
       (let ([d (modular-expt a (/ b 2) n)])
         (modulo (* d d) n))]
      [else
       (let ([d (modular-expt a (sub1 b) n)])
         (modulo (* a d) n))])))


#|
;; test
(map (lambda (i)
       (modular-expt 3 i 7))
     (sequence 0 20))
|#