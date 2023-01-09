#lang racket


(require srfi/27)


(define bucket-sort
  (lambda (v)
    (let* ([len (vector-length v)]
           [v2 (make-vector len '())])
      (insert! v v2)
      (concatenate v2))))


(define insert!
  (lambda (v v2)
    (let ([n (vector-length v)])
      (do ([i 0 (add1 i)]) ((= i n))
        (let ([j (inexact->exact
                  (floor (* n (vector-ref v i))))])
          (vector-set! v2 j
                       (inc-insert (vector-ref v i)
                                   (vector-ref v2 j))))))))

(define inc-insert
  (lambda (f ls)
    (cond
      [(null? ls)
       (list f)]
      [(< f (car ls))
       (cons f ls)]
      [else
       (cons (car ls)
             (inc-insert f (cdr ls)))])))


(define concatenate
  (lambda (v)
    (let* ([len (vector-length v)]
           [v2 (make-vector len 0)]
           [i 0])
      (do ([j 0 (add1 j)]) ((= j len))
        (let f ([ls (vector-ref v j)])
          (when (not (null? ls))
            (vector-set! v2 i (car ls))
            (set! i (add1 i))
            (f (cdr ls)))))
      v2)))


;; test
(define random-real-fill!
  (lambda (v)
    (let ([len (vector-length v)])
      (do ([i 0 (add1 i)]) ((= i len))
        (vector-set! v i
                     (random-real))))))

(define v (make-vector 25))
(random-real-fill! v)
v
(bucket-sort v)