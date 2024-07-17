#lang racket


(require "../util.rkt"
         "modular-exponent.rkt")


(define pseudoprime
  (lambda (n)
    (if (= 1 (mod-expt 2 (sub1 n) n))
      #t
      #f)))



(define t-of
  (lambda (n)
    (if (even? n)
      (+ 1 (t-of (/ n 2)))
      0)))

(define u-of
  (lambda (n)
    (if (even? n)
      (u-of (/ n 2))
      n)))

(define witness
  (lambda (a n)
    (let ([t (t-of (sub1 n))]
          [u (u-of (sub1 n))]
          [x1 #f]
          [iter #f])
      (set! iter
        (lambda (i x0)
          (let ([x1 (modulo (* x0 x0) n)])
            (cond
              [(= (add1 i) t)
               x1]
              [(and (= 1 x1) (< 1 x0) (< x0 (sub1 n)))
               #t]
              [else
               (iter (add1 i) x1)]))))
      (let ([out (iter 0 (mod-expt a u n))])
        (if (boolean? out)
          out
          (not (= 1 out)))))))

(define miller-rabin-prime?
  (lambda (n times)
    (let f ([i 0])
      (cond
        [(= i times) #t]
        [(witness (random 2 (sub1 n)) n) #f]
        [else (f (add1 i))]))))


;; test
(map miller-rabin-prime?
     '(341 561 645 1999 59719 7000061)
     '(5 3 4 5 6 9))