#lang racket


(require "../util.rkt")

(provide mod-expt)


(define mod-expt
  (lambda (a b n)
    (cond
      [(= 0 b) 1]
      [(even? b)
       (let ([d (mod-expt a (/ b 2) n)])
         (modulo (* d d) n))]
      [else
       (let ([d (mod-expt a (sub1 b) n)])
         (modulo (* a d) n))])))


#|
;; test
(map (lambda (i)
       (mod-expt 3 i 7))
     (sequence 0 20))
|#