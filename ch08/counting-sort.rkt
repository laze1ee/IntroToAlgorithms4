#lang racket


(require "../util.rkt")


(define counting-sort
  (lambda (v min bound)
    (let* ([len (vector-length v)]
           [v2 (make-vector len 0)]
           [vc (make-vector (- bound min) 0)])
      (counting! v min vc)
      (increasing! vc)
      (locating! v min vc v2)
      v2)))


(define counting!
  (lambda (v min vc)
    (let ([len (vector-length v)])
      (do ([i 0 (add1 i)]) ((= i len))
        (let ([ref (- (vector-ref v i) min)])
          (vector-set! vc ref
                       (add1 (vector-ref vc ref))))))))


(define increasing!
  (lambda (vc)
    (let ([len (vector-length vc)])
      (do ([i 1 (add1 i)]) ((= i len))
        (vector-set! vc i
                     (+ (vector-ref vc (sub1 i))
                        (vector-ref vc i)))))))


(define locating!
  (lambda (v min vc v2)
    (let ([len (vector-length v)])
      (do ([i (sub1 len) (sub1 i)]) ((= i -1))
        (let ([ref (- (vector-ref v i) min)])
          (vector-set! vc ref
                       (sub1 (vector-ref vc ref)))
          (vector-set! v2
                       (vector-ref vc ref)
                       (vector-ref v i)))))))



(define counting-sort2!
  (lambda (v min bound)
    (let* ([len (- bound min)]
           [vc (make-vector len 0)]
           [i 0])
      (counting! v min vc)
      (do ([j 0 (add1 j)]) ((= j len))
        (let f ([k (vector-ref vc j)])
          (when (< 0 k)
            (vector-set! v i (+ j min))
            (set! i (add1 i))
            (f (sub1 k))))))))



;; test
(define v (make-vector 23 0))
(random-fill! v 7 29)
v
(counting-sort2! v 7 29)
v