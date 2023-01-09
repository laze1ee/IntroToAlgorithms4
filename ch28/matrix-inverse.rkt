#lang racket


(require "../util.rkt"
         "../matrix.rkt"
         "lup-decomposition.rkt")


(define Matrix-inverse
  (lambda (M)
    (if (= 0 (Matrix-determin M))
      (error 'Matrix-inverse
             "singular matrix is not invertible")
      (inverse (Matrix.row M) M))))


(define inverse
  (lambda (sz M)
    (let ([iM (new-Matrix sz sz 0)]
          [In (new-Matrix sz sz 0)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (Matrix-set! In i i 1))
      (do ([i 0 (add1 i)]) ((= i sz))
        (Matrix-col-set!
         iM i
         (lup-solve M (Matrix-extract-col In i))
         sz))
      iM)))


;; test
(define size 5)
(define A (new-Matrix size size))
(Matrix-random-fill! A 0 10)

(Matrix-print A)
(Matrix-print (Matrix-inverse A))