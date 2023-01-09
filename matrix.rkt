#lang racket


(require "util.rkt")

(provide Matrix? new-Matrix Matrix.row Matrix.col
         Matrix-null? Matrix-ref
         Matrix-set! Matrix-row-set! Matrix-col-set!
         Matrix-copy Matrix-exchange! Matrix-transpose Matrix-determin
         Matrix-extract-row Matrix-extract-col
         Matrix-random-fill! Matrix-compatible? Matrix-similar?
         Matrix-* Matrix-single?
         Matrix->string Matrix-print)



;; Matrix: #('matrix Number-of-Rows Number-of-Cols Vector)
(define Matrix?
  (lambda (M)
    (and (vector? M)
         (immutable? M)
         (= 4 (vector-length M))
         (eq? 'matrix (ref0 M))
         (natural? (ref1 M))
         (natural? (ref2 M))
         (vector? (ref3 M)))))


(define new-Matrix
  (case-lambda
    [(row col init-data)
     (if (and (natural? row)
              (natural? col))
       (vector-immutable 'matrix row col
                         (make-vector (* row col) init-data))
       (error 'new-Matrix
              "row ~s or col ~s not a natural number"
              row col))]
    [(row col)
     (new-Matrix row col 0)]))

(define Matrix.row  (lambda (M) (ref1 M)))
(define Matrix.col  (lambda (M) (ref2 M)))
(define Matrix.data (lambda (M) (ref3 M)))


(define Matrix-null?
  (lambda (M)
    (or (= 0 (Matrix.row M))
        (= 0 (Matrix.col M)))))

(define Matrix-single?
  (lambda (M)
    (and (= 1 (Matrix.row M))
         (= 1 (Matrix.col M)))))


(define valid-refs?
  (lambda (M row col)
    (and (<= 0 row)
         (< row (Matrix.row M))
         (<= 0 col)
         (< col (Matrix.col M)))))


(define Matrix-ref
  (lambda (M row col)
    (if (valid-refs? M row col)
      (vector-ref (Matrix.data M)
                  (+ col (* row (Matrix.col M))))
      (error 'Matrix-ref
             "invalid refs (~s ~s) for Matrix~%~a"
             row col (Matrix->string M)))))


(define Matrix-set!
  (lambda (M row col val)
    (if (valid-refs? M row col)
      (vector-set! (Matrix.data M)
                   (+ col (* row (Matrix.col M)))
                   val)
      (error 'Matrix-set!
             "invalid refs (~s ~s) for Matrix~%~a"
             row col (Matrix->string M)))))


(define Matrix-row-set!
  (lambda (M row v sz)
    (if (valid-refs? M row 0)
      (do ([i 0 (add1 i)]) ((= i sz))
        (Matrix-set! M row i (vector-ref v i)))
      (error 'Matrix-row-set!
             "invalid row ref ~s for Matrix~%~a"
             row (Matrix->string M)))))


(define Matrix-col-set!
  (lambda (M col v sz)
    (if (valid-refs? M 0 col)
      (do ([i 0 (add1 i)]) ((= i sz))
        (Matrix-set! M i col (vector-ref v i)))
      (error 'Matrix-row-set!
             "invalid col ref ~s for Matrix~%~a"
             col (Matrix->string M)))))


(define Matrix-copy
  (lambda (M)
    (vector-immutable 'matrix
                      (Matrix.row M) (Matrix.col M)
                      (copy (Matrix.data M)))))


(define Matrix-exchange!
  (lambda (M r1 c1 r2 c2)
    (cond
      [(not (valid-refs? M r1 c1))
       (error 'Matrix-exchange!
              "invalid refs (~s ~s) for Matrix~%~a"
              r1 c1 (Matrix->string M))]
      [(not (valid-refs? M r2 c2))
       (error 'Matrix-exchange!
              "invalid refs (~s ~s) for Matrix~%~a"
              r2 c2 (Matrix->string M))]
      [else
       (exchange! (Matrix.data M)
                  (+ c1 (* r1 (Matrix.col M)))
                  (+ c2 (* r2 (Matrix.col M))))])))


(define Matrix-transpose
  (lambda (M)
    (let* ([row (Matrix.row M)]
           [col (Matrix.col M)]
           [T (new-Matrix col row)])
      (do ([i 0 (add1 i)]) ((= i row))
        (do ([j 0 (add1 j)]) ((= j col))
          (Matrix-set! T j i (Matrix-ref M i j))))
      T)))


(define Matrix-determin
  (lambda (M)
    (cond
      [(or (Matrix-null? M)
           (Matrix-single? M))
       (error 'Matrix-determin
              "the matrix has to be square")]
      [(not (= (Matrix.row M) (Matrix.col M)))
       (error 'Matrix-determin
              "row ~s and col ~s is not equal"
              (Matrix.row M) (Matrix.col M))]
      [else
       (determin (Matrix.row M) M)])))


(define select
  (lambda (sz M exclude)
    (let ([M1 (new-Matrix (sub1 sz) (sub1 sz))])
      (do ([i 1 (add1 i)]) ((= i sz))
        (do ([j 0 (add1 j)]) ((= j exclude))
          (Matrix-set! M1 (sub1 i) j
                       (Matrix-ref M i j))))
      (do ([i 1 (add1 i)]) ((= i sz))
        (do ([j (add1 exclude) (add1 j)]) ((= j sz))
          (Matrix-set! M1 (sub1 i) (sub1 j)
                       (Matrix-ref M i j))))
      M1)))


(define determin
  (let ([sign '#(1 -1)])
    (lambda (sz M)
      (if (= 2 sz)
        (- (* (Matrix-ref M 0 0) (Matrix-ref M 1 1))
           (* (Matrix-ref M 0 1) (Matrix-ref M 1 0)))
        (let ([sum 0])
          (do ([i 0 (add1 i)]) ((= i sz))
            (set! sum
              (+ sum
                 (* (vector-ref sign (remainder i 2))
                    (Matrix-ref M 0 i)
                    (determin (sub1 sz) (select sz M i))))))
          sum)))))



(define Matrix-extract-row
  (lambda (M row)
    (let* ([sz (Matrix.col M)]
           [v (make-vector sz)])
    (if (valid-refs? M row 0)
      (do ([i 0 (add1 i)])
        ((= i sz) v)
        (vector-set! v i
                     (Matrix-ref M row i)))
      (error 'Matrix-extract-row
             "invalid row ref ~s for Matrix~%~a"
             row (Matrix->string M))))))


(define Matrix-extract-col
  (lambda (M col)
    (let* ([sz (Matrix.col M)]
           [v (make-vector sz)])
    (if (valid-refs? M 0 col)
      (do ([i 0 (add1 i)])
        ((= i sz) v)
        (vector-set! v i
                     (Matrix-ref M i col)))
      (error 'Matrix-extract-col
             "invalid col ref ~s for Matrix~%~a"
             col (Matrix->string M))))))


(define Matrix-random-fill!
  (lambda (M min max)
    (let ([row (Matrix.row M)]
          [col (Matrix.col M)]
          [seed (lambda () (remainder
                            (current-milliseconds)
                            (expt 2 31)))])
      (do ([i 0 (add1 i)]) ((= i row))
        (do ([j 0 (add1 j)]) ((= j col))
          (Matrix-set! M i j
                       (random-integer min max)))))))


(define Matrix-compatible?
  (lambda (M1 M2)
    (= (Matrix.col M1)
       (Matrix.row M2))))


(define Matrix-similar?
  (lambda (M1 M2)
    (and (= (Matrix.row M1) (Matrix.row M2))
         (= (Matrix.col M1) (Matrix.col M2)))))


(define Matrix-*
  (lambda (M1 M2)
    (if (Matrix-compatible? M1 M2)
      (let ([row (Matrix.row M1)]
            [col (Matrix.col M2)]
            [com (Matrix.col M1)])
        (let ([M3 (new-Matrix row col)]
              [sum 0])
          (do ([i 0 (add1 i)]) ((= i row))
            (do ([j 0 (add1 j)]) ((= j col))
              (set! sum 0)
              (do ([k 0 (add1 k)]) ((= k com))
                (set! sum
                  (+ sum
                     (* (Matrix-ref M1 i k)
                        (Matrix-ref M2 k j)))))
              (Matrix-set! M3 i j sum)))
          M3))
      (error 'Matrix-*
             "two matrices not compatible~%~a~%and~% ~a"
             (Matrix->string M1) (Matrix->string M2)))))



(define get-max-width
  (lambda (M)
    (let ([row (Matrix.row M)]
          [col (Matrix.col M)]
          [widths '()]
          [max 0]
          [width 0])
      (do ([j 0 (add1 j)]) ((= j col))
        (set! max 0)
        (do ([i 0 (add1 i)]) ((= i row))
          (set! width
            (string-length
             (format "~s" (Matrix-ref M i j))))
          (when (< max width)
            (set! max width)))
        (set! widths (append widths (list max))))
      widths)))


              
(define Matrix->string
  (lambda (M)
    (let ([row (Matrix.row M)]
          [col (Matrix.col M)]
          [widths (get-max-width M)]
          [out (string #\()])
      (set! out (string-append out "matrix"))
      (do ([i 0 (add1 i)]) ((= i row))
        (set! out (string-append out (format "~%")))
        (do ([j 0 (add1 j)] [w widths (cdr w)]) ((= j col))
          (set! out (string-append
                     out
                     (~a (Matrix-ref M i j)
                         #:align 'right
                         #:width (add1 (car w)))))))
      (set! out (string-append out ")"))
      out)))


(define Matrix-print
  (lambda (M)
    (displayln (Matrix->string M))))


#|
;; test
(define A (new-Matrix 6 6))
(Matrix-random-fill! A -10 11)
(Matrix-print A)
(Matrix-extract-row A 0)
|#