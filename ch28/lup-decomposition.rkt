#lang racket


(require "../util.rkt"
         "../matrix.rkt")


(provide lup-solve)


(define de-low
  (lambda (sz M)
    (let ([L (new-Matrix sz sz 0)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (do ([j 0 (add1 j)]) ((< i j))
          (if (= i j)
            (Matrix-set! L i j 1)
            (Matrix-set! L i j (Matrix-ref M i j)))))
      L)))


(define de-up
  (lambda (sz M)
    (let ([U (new-Matrix sz sz 0)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (do ([j i (add1 j)]) ((= j sz))
          (Matrix-set! U i j (Matrix-ref M i j))))
      U)))
          

(define lup-decomposition
  (lambda (sz M)
    (let ([Tmp (Matrix-copy M)]
          [per (list->vector (sequence 0 sz))])
      (do ([k 0 (add1 k)]) ((= k sz))
        (let ([max 0]
              [k1 (vector-ref per k)])
          (do ([i k (add1 i)]) ((= i sz))
            (let ([a (abs (Matrix-ref Tmp k i))])
              (when (< max a)
                (set! max a)
                (set! k1 i))))
          (when (= 0 max)
            (error 'lup-decomposition "singular matrix"))
          (when (not (= k1 (vector-ref per k)))
            (exchange! per k k1)
            (do ([i 0 (add1 i)]) ((= i sz))
              (Matrix-exchange! Tmp k i k1 i))))

        (do ([i (add1 k) (add1 i)]) ((= i sz))
          (Matrix-set! Tmp i k
                       (/ (Matrix-ref Tmp i k)
                          (Matrix-ref Tmp k k)))
          (do ([j (add1 k) (add1 j)]) ((= j sz))
            (Matrix-set! Tmp i j
                         (- (Matrix-ref Tmp i j)
                            (* (Matrix-ref Tmp i k)
                               (Matrix-ref Tmp k j)))))))
      (vector (de-low sz Tmp)
              (de-up sz Tmp)
              per))))


(define l-sum
  (lambda (v1 v2 bound)
    (let ([n 0])
      (do ([i 0 (add1 i)]) ((= i bound))
        (set! n (+ n (* (vector-ref v1 i)
                        (vector-ref v2 i)))))
      n)))


(define u-sum
  (lambda (sz v1 v2 bound)
    (let ([n 0])
      (do ([i (sub1 sz) (sub1 i)]) ((= i bound))
        (set! n (+ n (* (vector-ref v1 i)
                        (vector-ref v2 i)))))
      n)))


(define lup-solve
  (lambda (A b)
    (let* ([sz (Matrix.row A)]
           [lup (lup-decomposition sz A)]
           [L (ref0 lup)]
           [U (ref1 lup)]
           [per (ref2 lup)]
           [y (make-vector sz 0)]
           [x (make-vector sz 0)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! y i
                     (- (vector-ref b (vector-ref per i))
                        (l-sum (Matrix-extract-row L i) y i))))
      (do ([i (sub1 sz) (sub1 i)]) ((= i -1))
        (vector-set!
         x i
         (/ (- (vector-ref y i)
               (u-sum sz (Matrix-extract-row U i) x i))
            (Matrix-ref U i i))))
      x)))


#|
;; test
(define size 6)
(define A (new-Matrix size size))
(define b (make-vector size))
(Matrix-random-fill! A 0 20)
(random-fill! b 0 100)

(Matrix-print A)
b
(printf "~a" (lup-solve A b))
|#