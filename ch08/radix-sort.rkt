#lang racket


(define counting-sort!
  (lambda (refs v range)
    (let ([vc (make-vector range '())])
      (counting! refs v vc)
      (locating! v vc))))


(define counting!
  (lambda (refs v vc)
    (let ([len (vector-length v)])
      (do ([i 0 (add1 i)]) ((= i len))
        (let ([j (refs (vector-ref v i))])
          (vector-set! vc j
                       (append (vector-ref vc j)
                               (list (vector-ref v i)))))))))


(define locating!
  (lambda (v vc)
    (let ([len (vector-length vc)]
          [i 0])
      (do ([j 0 (add1 j)]) ((= j len))
        (let f ([ls (vector-ref vc j)])
          (when (not (null? ls))
            (vector-set! v i (car ls))
            (set! i (add1 i))
            (f (cdr ls))))))))


(define radix-sort!
  (lambda (v d)
    (do ([i (sub1 d) (sub1 i)]) ((= i -1))
      (counting-sort! (lambda (v) (vector-ref v i))
                      v
                      10))))


;; test
(define v (vector
           (vector 3 6 8 3)
           (vector 1 4 6 4)
           (vector 0 9 7 8)
           (vector 7 2 3 5)
           (vector 1 1 7 0)
           (vector 5 0 1 9)))
(radix-sort! v 4)
v