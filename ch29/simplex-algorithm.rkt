#lang racket


(require "../util.rkt"
         "../matrix.rkt")



(define init-simplex
  (lambda (A b c)
    (let* ([row (add1 (Matrix.row A))]
           [col (+ 2 (Matrix.col A))]
           [m (vector-length b)]
           [n (vector-length c)]
           [Slack (new-Matrix row col 0)]
           [N (list->vector (sequence 0 n))]
           [B (list->vector (sequence n (+ m n)))])

      (do ([i 0 (add1 i)]) ((= i m))
        (do ([j 0 (add1 j)]) ((= j n))
          (Matrix-set! Slack i j (* -1 (Matrix-ref A i j)))))

      (do ([i 0 (add1 i)]) ((= i m))
        (Matrix-set! Slack i n (vector-ref b i)))

      (do ([j 0 (add1 j)]) ((= j n))
        (Matrix-set! Slack m j (vector-ref c j)))

      (set! n (add1 n))
      (do ([i 0 (add1 i)]) ((= i row))
        (Matrix-set! Slack i n 1))

      (vector N B Slack))))


(define c-ref
  (lambda (Slack j)
    (let ([row (sub1 (Matrix.row Slack))])
      (Matrix-ref Slack row j))))


(define choose-col
  (lambda (Slack)
    (let ([n (- (Matrix.col Slack) 2)])
      (let f ([i 0])
        (cond
          [(= i n) #f]
          [(< 0 (c-ref Slack i)) i]
          [else
           (f (add1 i))])))))


(define b-ref
  (lambda (Slack i)
    (let ([col (- (Matrix.col Slack) 2)])
      (Matrix-ref Slack i col))))


(define choose-row
  (lambda (Slack cref)
    (let* ([n (sub1 (Matrix.row Slack))]
           [v (make-vector n)]
           [a #f]
           [ref #f])
      (do ([i 0 (add1 i)]) ((= i n))
        (set! a (Matrix-ref Slack i cref))
        (if (> 0 a)
          (vector-set! v i (/ (b-ref Slack i) a))
          (vector-set! v i -inf.0)))
      (set! ref (vector-min/max > v))
      (vector ref (vector-ref v ref)))))


(define coeff!
  (lambda (Slack rref cref)
    (let ([sz (Matrix.col Slack)]
          [a (* -1 (Matrix-ref Slack rref cref))])
      (do ([j 0 (add1 j)]) ((= j sz))
        (Matrix-set! Slack rref j
                     (/ (Matrix-ref Slack rref j) a))))))


(define transform!
  (lambda (N B Slack rref cref)
    (let ([sz (Matrix.col Slack)]
          [a (vector-ref N cref)])
      (vector-set! N cref (vector-ref B rref))
      (vector-set! B rref a)
      (set! sz (sub1 sz))
      (Matrix-exchange! Slack rref cref rref sz)
      (Matrix-set! Slack rref cref
                   (* -1 (Matrix-ref Slack rref cref)))
      (Matrix-set! Slack rref sz
                   (* -1 (Matrix-ref Slack rref sz))))))


(define multiple
  (lambda (Slack rref coe)
    (let ([n (Matrix.col Slack)]
          [v (Matrix-extract-row Slack rref)])
      (do ([i 0 (add1 i)]) ((= i n))
        (vector-set! v i (* (vector-ref v i) coe)))
      v)))


(define row-operat!
  (lambda (Slack rref cref v)
    (let ([row (sub1 (Matrix.row Slack))]
          [col (sub1 (Matrix.col Slack))])
      (do ([j 0 (add1 j)]) ((= j col))
        (if (= j cref)
          (Matrix-set! Slack rref j (vector-ref v j))
          (Matrix-set! Slack rref j
                       (+ (Matrix-ref Slack rref j)
                          (vector-ref v j)))))
      (set! col (sub1 col))
      (Matrix-set! Slack row col
                   (Matrix-ref Slack row col)))))


(define pivot!
  (lambda (N B Slack rref cref)
    (coeff! Slack rref cref)
    (transform! N B Slack rref cref)
    (let ([row (Matrix.row Slack)]
          [v #f])
      (do ([i 0 (add1 i)]) ((= i row))
        (when (not (= i rref))
          (set! v (multiple Slack rref (Matrix-ref Slack i cref)))
          (row-operat! Slack i cref v))))))


(define answer
  (lambda (N B Slack)
    (let* ([m (vector-length B)]
           [n (vector-length N)]
           [v (make-vector (+ m n) 0)]
           [ref #f])
      (do ([i 0 (add1 i)]) ((= i m))
        (set! ref (vector-ref B i))
        (vector-set! v ref (b-ref Slack i)))
      (vector v (b-ref Slack m)))))


(define simplex
  (lambda (A b c)
    (let* ([result (init-simplex A b c)]
           [N (ref0 result)]
           [B (ref1 result)]
           [Slack (ref2 result)])
      (let f ([cref (choose-col Slack)])
        (when (natural? cref)
          (set! result (choose-row Slack cref))
          (if (= -inf.0 (ref1 result))
            (error 'simplex "unbounded")
            (begin
              (pivot! N B Slack (ref0 result) cref)
              (f (choose-col Slack))))))
      (answer N B Slack))))


;; test
#|
(define vars 5)
(define constraints (+ 2 vars))
(define A2 (new-Matrix constraints vars))
(Matrix-random-fill! A2 -5 20)
(define b2 (make-vector constraints))
(random-fill! b2 100 1000)
(define c2 (make-vector vars))
(random-fill! c2 -10 20)

(define result2 (init-simplex A2 b2 c2))
(Matrix-print (ref2 result2))

(simplex A2 b2 c2)
|#

(define A3 (new-Matrix 3 3))
(Matrix-row-set! A3 0 '#(1 2 1) 3)
(Matrix-row-set! A3 1 '#(2 1 3) 3)
(Matrix-row-set! A3 2 '#(2 2 1) 3)
(define b3 '#(2 4 8))
(define c3 '#(1 4 4))

(Matrix-print (ref2 (init-simplex A3 b3 c3)))

(simplex A3 b3 c3)