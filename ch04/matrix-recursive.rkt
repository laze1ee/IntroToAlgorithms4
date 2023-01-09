#lang racket


(require "../util.rkt"
         "../matrix.rkt")


(define complete
  (lambda (n)
    (let ([l (inexact->exact (floor (log n 2)))])
      (- (expt 2 (add1 l))
         n))))

(define spare
  (lambda (M)
    (let ([side (Matrix.row M)]
          [u #f])
      (set! u (complete side))
      (if (= u side)
        M
        (let* ([sz (+ side u)]
               [S (new-Matrix sz sz)])
          (set-subMatrix!
           S 0 0 side side M)
          S)))))

(define set-subMatrix!
  (lambda (S tr tc br bc M)
    (do ([tr tr (add1 tr)]
         [i 0 (add1 i)]) ((= tr br))
      (do ([tc tc (add1 tc)]
           [j 0 (add1 j)]) ((= tc bc))
        (Matrix-set! S tr tc
                     (Matrix-ref M i j))))))

(define subMatrix
  (lambda (S tr tc br bc)
    (let ([row (- br tr)]
          [col (- bc tc)])
      (if (and (= row (Matrix.row S))
               (= col (Matrix.col S)))
        S
        (let ([M (new-Matrix row col)])
          (do ([i 0 (add1 i)]
               [tr tr (add1 tr)]) ((= tr br))
            (do ([j 0 (add1 j)]
                 [tc tc (add1 tc)]) ((= tc bc))
              (Matrix-set! M i j
                           (Matrix-ref S tr tc))))
          M)))))

(define part
  (lambda (S n)
    (let ([row (Matrix.row S)]
          [col (Matrix.col S)]
          [r-mid #f]
          [c-mid #f]
          [trow #f]
          [tcol #f]
          [brow #f]
          [bcol #f])
      (set! r-mid (ceiling (/ row 2)))
      (set! c-mid (ceiling (/ col 2)))
      (cond
        [(= 1 n)
         (set! trow 0)
         (set! tcol 0)
         (set! brow r-mid)
         (set! bcol c-mid)]
        [(= 2 n)
         (set! trow 0)
         (set! tcol c-mid)
         (set! brow r-mid)
         (set! bcol col)]
        [(= 3 n)
         (set! trow r-mid)
         (set! tcol 0)
         (set! brow row)
         (set! bcol c-mid)]
        [(= 4 n)
         (set! trow r-mid)
         (set! tcol c-mid)
         (set! brow row)
         (set! bcol col)]
        [else
         (error 'part
                "part number ~s not in the set ~s"
                n '(1 2 3 4))])
      (list trow tcol brow bcol))))

(define quarter
  (lambda (S n)
    (let ([coor (part S n)])
      (subMatrix
       S
       (0th coor) (1st coor) (2nd coor) (3rd coor)))))

(define set-quarter!
  (lambda (S n M)
    (let ([coor (part S n)])
      (set-subMatrix!
       S
       (0th coor) (1st coor) (2nd coor) (3rd coor)
       M))))

(define add/sub
  (lambda (o M1 M2)
    (let ([row (Matrix.row M1)]
          [col (Matrix.col M1)]
          [M #f])
      (set! M (new-Matrix row col))
      (do ([i 0 (add1 i)]) ((= i row))
        (do ([j 0 (add1 j)]) ((= j col))
          (Matrix-set! M i j
                       (o
                        (Matrix-ref M1 i j)
                        (Matrix-ref M2 i j)))))
      M)))

(define Matrix-add
  (lambda (M . Ms)
    (let iter ([M M] [Ms Ms])
      (if (null? Ms)
        M
        (iter (add/sub + M (car Ms)) (cdr Ms))))))

(define Matrix-sub
  (lambda (M . Ms)
    (let iter ([M M] [Ms Ms])
      (if (null? Ms)
        M
        (iter (add/sub - M (car Ms)) (cdr Ms))))))



(define Matrix-*-recur
  (lambda (M1 M2)
    (if (and (Matrix-compatible? M1 M2)
             (Matrix-similar? M1 M2))
      (subMatrix
       (*-recur (spare M1) (spare M2))
       0 0 (Matrix.row M1) (Matrix.col M1))
      (error 'Matrix-*-recur
             "two matrices are not same form"))))

(define *-recur
  (lambda (M1 M2)
    (if (Matrix-single? M1)
      (new-Matrix 1 1
                  (* (Matrix-ref M1 0 0)
                     (Matrix-ref M2 0 0)))
      (let ([M (new-Matrix (Matrix.row M1)
                           (Matrix.col M2))]
            [p11 (quarter M1 1)]
            [p12 (quarter M1 2)]
            [p13 (quarter M1 3)]
            [p14 (quarter M1 4)]
            [p21 (quarter M2 1)]
            [p22 (quarter M2 2)]
            [p23 (quarter M2 3)]
            [p24 (quarter M2 4)])
        (set-quarter!
         M 1
         (Matrix-add
          (*-recur p11 p21) (*-recur p12 p23)))
        (set-quarter!
         M 2
         (Matrix-add
          (*-recur p11 p22) (*-recur p12 p24)))
        (set-quarter!
         M 3
         (Matrix-add
          (*-recur p13 p21) (*-recur p14 p23)))
        (set-quarter!
         M 4
         (Matrix-add
          (*-recur p13 p22) (*-recur p14 p24)))
        M))))



(define *-strassen
  (lambda (M1 M2)
    (if (Matrix-single? M1)
      (new-Matrix 1 1
                  (* (Matrix-ref M1 0 0)
                     (Matrix-ref M2 0 0)))
      (let ([M (new-Matrix (Matrix.row M1)
                           (Matrix.col M2))]
            [p11 (quarter M1 1)]
            [p12 (quarter M1 2)]
            [p13 (quarter M1 3)]
            [p14 (quarter M1 4)]
            [p21 (quarter M2 1)]
            [p22 (quarter M2 2)]
            [p23 (quarter M2 3)]
            [p24 (quarter M2 4)]
            [s1 #f]
            [s2 #f]
            [s3 #f]
            [s4 #f]
            [s5 #f]
            [s6 #f]
            [s7 #f]
            [s8 #f]
            [s9 #f]
            [s10 #f]
            [t1 #f]
            [t2 #f]
            [t3 #f]
            [t4 #f]
            [t5 #f]
            [t6 #f]
            [t7 #f])
        (set! s1 (Matrix-sub p22 p24))
        (set! s2 (Matrix-add p11 p12))
        (set! s3 (Matrix-add p13 p14))
        (set! s4 (Matrix-sub p23 p21))
        (set! s5 (Matrix-add p11 p14))
        (set! s6 (Matrix-add p21 p24))
        (set! s7 (Matrix-sub p12 p14))
        (set! s8 (Matrix-add p23 p24))
        (set! s9 (Matrix-sub p11 p13))
        (set! s10 (Matrix-add p21 p22))

        (set! t1 (*-strassen p11 s1))
        (set! t2 (*-strassen s2 p24))
        (set! t3 (*-strassen s3 p21))
        (set! t4 (*-strassen p14 s4))
        (set! t5 (*-strassen s5 s6))
        (set! t6 (*-strassen s7 s8))
        (set! t7 (*-strassen s9 s10))

        (set-quarter!
         M 1
         (Matrix-sub
          (Matrix-add (quarter M 1) t5 t4 t6)
          t2))
        (set-quarter!
         M 2
         (Matrix-add (quarter M 2) t1 t2))
        (set-quarter!
         M 3
         (Matrix-add (quarter M 3) t3 t4))
        (set-quarter!
         M 4
         (Matrix-sub
          (Matrix-add (quarter M 4) t5 t1)
          t3 t7))
        M))))


(define Matrix-*-strassen
  (lambda (M1 M2)
    (if (and (Matrix-compatible? M1 M2)
             (Matrix-similar? M1 M2))
      (mul M1 M2)
      (error 'Matrix-strassen
             "two matrices are not same form"))))

(define mul
  (lambda (M1 M2)
    (if (Matrix-single? M1)
      (new-Matrix 1 1
                  (* (Matrix-ref M1 0 0)
                     (Matrix-ref M2 0 0)))
      (let ([a (split (Matrix.row M1))]
            [b (Matrix.row M1)])
        (if (= a b)
          (*-strassen M1 M2)
          (let([M (new-Matrix b b)]
               [s11 (subMatrix M1 0 0 a a)]
               [s12 (subMatrix M1 0 a a b)]
               [s13 (subMatrix M1 a 0 b a)]
               [s14 (subMatrix M1 a a b b)]
               [s21 (subMatrix M2 0 0 a a)]
               [s22 (subMatrix M2 0 a a b)]
               [s23 (subMatrix M2 a 0 b a)]
               [s24 (subMatrix M2 a a b b)])
            (set-subMatrix!
             M 0 0 a a
             (Matrix-add (*-strassen s11 s21)
                         (Matrix-* s12 s23)))
            (set-subMatrix!
             M 0 a a b
             (Matrix-add (Matrix-* s11 s22)
                         (Matrix-* s12 s24)))
            (set-subMatrix!
             M a 0 b a
             (Matrix-add (Matrix-* s13 s21)
                         (Matrix-* s14 s23)))
            (set-subMatrix!
             M a a b b
             (Matrix-add (Matrix-* s13 s22)
                         (mul s14 s24)))
            M))))))

(define split
  (lambda (n)
    (expt 2
          (inexact->exact (floor (log n 2))))))


;; test
(define A (new-Matrix 9 9))
(define B (new-Matrix 9 9))
(Matrix-random-fill! A 0 10)
(Matrix-random-fill! B 0 30)
(Matrix-print (Matrix-* A B))
(Matrix-print (Matrix-*-strassen A B))