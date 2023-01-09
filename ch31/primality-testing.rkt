#lang racket


(require "../util.rkt"
         "modular-exponent.rkt")


(define pseudoprime
  (lambda (n)
    (if (= 1 (modular-expt 2 (sub1 n) n))
      #t
      #f)))



(define composite
  (lambda (n)
    (let f ([t 0]
            [u n])
      (if (= 0 (remainder u 2))
        (f (add1 t) (/ u 2))
        (list t u)))))


(define witness
  (lambda (a n)
    (let* ([result (composite (sub1 n))]
           [t (0th result)]
           [u (1st result)]
           [x0 (modular-expt a u n)]
           [x1 x0])
      (call/cc
       (lambda (hop)
         (let f ([i 0])
           (when (not (= i t))
             (set! x1 (modulo (* x0 x0) n))
             (when (and (= 1 x1)
                        (not (= 1 x0))
                        (not (= (sub1 n) x0)))
               (hop #t))
             (set! x0 x1)
             (f (add1 i))))
         (if (= 1 x1)
           #f
           #t))))))


(define miller-rabin
  (lambda (n s)
    (let f ([i 0]
            [a (random-integer 2 (- n 2))])
      (cond
        [(= i s) #t]
        [(witness a n) #f]
        [else
         (f (add1 i) (random-integer 2 n))]))))


;; test
(map miller-rabin
     '(341 561 645 1999 59719 7000061)
     '(5 3 4 5 6 9))