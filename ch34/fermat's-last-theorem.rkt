#lang racket



(define fer
  (lambda (n bound)
    (let ([total 3]
          [z 0]
          [ls '()])
      (let f ()
        (when (<= total bound)
          (do ([x 1 (add1 x)]) ((> x (- total 2)))
            (do ([y 1 (add1 y)]) ((> y (- total x 1)))
              (set! z (- total x y))
              (when (= (+ (expt x n) (expt y n)) (expt z n))
                (set! ls (append ls (list (list x y z)))))))
          (set! total (add1 total))
          (f)))
      ls)))


;; test
(fer 4 500)