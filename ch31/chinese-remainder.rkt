#lang racket


(require "../util.rkt"
         "modular-linear-equation.rkt")


;; Pairs: (n1 a1) (n2 a2) ...
(define chinese-remainder
  (lambda pairs
    (let f ([result '(1 0)]
            [lst pairs])
      (if (null? lst)
        result
        (let* ([n1 (0th result)]
               [a1 (1st result)]
               [n2 (0th (car lst))]
               [a2 (1st (car lst))]
               [c1 (* a1 n2 (0th (mod-le n2 1 n1)))]
               [c2 (* n1 a2 (0th (mod-le n1 1 n2)))]
               [n (* n1 n2)])
          (f (list n (modulo (+ c1 c2) n))
             (cdr lst)))))))
          

;; test
(chinese-remainder '(5 2) '(7 3) '(8 4) '(9 5))