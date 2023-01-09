#lang racket


(require math/base
         "../util.rkt"
         "../matrix.rkt")



(define product
  (lambda (p1 p2)
    (let* ([n (vector-length p1)]
           [sz (sub1 (* 2 n))]
           [ps (new-Matrix n sz 0)]
           [p3 (make-vector sz 0)]
           [sum #f])
      (do ([i 0 (add1 i)]) ((= i n))
        (do ([j 0 (add1 j)]) ((= j n))
          (Matrix-set! ps i (+ i j)
                       (* (vector-ref p1 i) (vector-ref p2 j)))))
      (do ([j 0 (add1 j)]) ((= j sz))
        (set! sum 0)
        (do ([i 0 (add1 i)]) ((= i n))
          (set! sum (+ sum (Matrix-ref ps i j))))
        (vector-set! p3 j sum))
      p3)))
          


(define root-unity
  (lambda (n)
    (expt euler.0 (/ (* 2 pi 0+1i) n))))


(define pick-even
  (lambda (n poly)
    (let ([even (make-vector (/ n 2))])
      (do ([i 0 (+ 2 i)]) ((>= i n))
        (vector-set! even (/ i 2) (vector-ref poly i)))
      even)))


(define pick-odd
  (lambda (n poly)
    (let ([odd (make-vector (/ n 2))])
      (do ([i 1 (+ 2 i)]) ((>= i n))
        (vector-set! odd (/ (sub1 i) 2) (vector-ref poly  i)))
      odd)))


(define fft
  (lambda (n poly)
    (if (= 1 n)
      poly
      (let ([even (pick-even n poly)]
            [odd (pick-odd n poly)]
            [result (make-vector n)]
            [unity (root-unity n)]
            [w 1])
        (set! n (/ n 2))
        (set! even (fft n even))
        (set! odd (fft n odd))
        (do ([i 0 (add1 i)]) ((= i n))
          (vector-set! result i
                       (+ (vector-ref even i)
                          (* w (vector-ref odd i))))
          (vector-set! result (+ i n)
                       (- (vector-ref even i)
                          (* w (vector-ref odd i))))
          (set! w (* w unity)))
        result))))


(define dft
  (lambda (poly)
    (let* ([sz (vector-length poly)]
           [n (* 2 sz)]
           [new (make-vector n 0)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! new i (vector-ref poly i)))
      (fft n new))))


(define fft-reverse
  (lambda (n y)
    (if (= 1 n)
      y
      (let ([even (pick-even n y)]
            [odd (pick-odd n y)]
            [result (make-vector n)]
            [unity (/ 1 (root-unity n))]
            [w 1])
        (set! n (/ n 2))
        (set! even (fft-reverse n even))
        (set! odd (fft-reverse n odd))
        (do ([i 0 (add1 i)]) ((= i n))
          (vector-set! result i
                       (+ (vector-ref even i)
                          (* w (vector-ref odd i))))
          (vector-set! result (+ i n)
                       (- (vector-ref even i)
                          (* w (vector-ref odd i))))
          (set! w (* w unity)))
        result))))


(define mul
  (lambda (p1 p2)
    (let* ([sz (vector-length p1)]
           [y (make-vector sz)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! y i
                     (* (vector-ref p1 i) (vector-ref p2 i))))
      y)))


(define div
  (lambda (y n)
    (let ([y1 (make-vector n)])
      (do ([i 0 (add1 i)]) ((= i n))
        (vector-set! y1 i
                     (/ (vector-ref y i) n)))
      y1)))


(define product+
  (lambda (p1 p2)
    (let ([n (* 2 (vector-length p1))]
          [y (mul (dft p1) (dft p2))])
      (div (fft-reverse n y) n))))



;; test
(define n 4)
(define p1 (make-vector n))
(random-fill! p1 -9 10)
(define p2 (make-vector n))
(random-fill! p2 -9 10)
p1
p2
(product p1 p2)
(product+ p1 p2)