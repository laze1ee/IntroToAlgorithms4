#lang racket


(require "../util.rkt")


(define make-table
  (lambda (n init)
    (let ([table (make-vector n 0)])
      (do ([i 0 (add1 i)]) ((= i n))
        (vector-set! table i
                     (make-vector (add1 i) 0)))
      table)))


(define table-ref
  (lambda (table col row)
    (vector-ref (vector-ref table row) col)))


(define table-set!
  (lambda (table col row val)
    (vector-set! (vector-ref table row)
                 col val)))


;; minimum infomation
;; #(cost root)
(define nil?
  (lambda (info)
    (and (= 2 (vector-length info))
         (= +inf.0 (ref0 info))
         (= -1 (ref1 info)))))


(define init-cost
  (lambda (dummies)
    (let* ([len (vector-length dummies)]
           [c (make-table len 0)])
      (do ([i 0 (add1 i)]) ((= i len))
        (do ([j i (add1 j)]) ((= j len))
          (if (= i j)
            (table-set! c i j
                        (vector (vector-ref dummies i)
                                -1))
            (table-set! c i j
                        (vector +inf.0 -1)))))
      c)))


(define weight
  (lambda (keys dummies)
    (let* ([len (vector-length dummies)]
           [w (make-table len 0)])
      (do ([i 0 (add1 i)]) ((= i len))
        (table-set! w i i (vector-ref dummies i)))
      (do ([i 0 (add1 i)]) ((= i (sub1 len)))
        (do ([j (add1 i) (add1 j)]) ((= j len))
          (table-set! w i j
                      (+ (table-ref w i (sub1 j))
                         (vector-ref keys (sub1 j))
                         (vector-ref dummies j)))))
      w)))


(define optimize!
  (lambda (w c)
    (let f ([start 0]
            [bound (sub1 (vector-length w))])
      (let ([now (table-ref c start bound)])
        (if (not (nil? now))
          now
          (let ([left #f]
                [right #f]
                [tmp #f])
            (do ([i start (add1 i)]) ((= i bound))
              (set! left (f start i))
              (set! right (f (add1 i) bound))
              (set! tmp (+ (ref0 left) (ref0 right)
                           (table-ref w start bound)))
              (when (> (ref0 now) tmp)
                (set0! now tmp)
                (set1! now i)))
            now))))))


(define optimal-solution
  (lambda (c)
    (let* ([bound (sub1 (vector-length c))]
           [cost (ref0 (table-ref c 0 bound))]
           [f #f])
      (set! f
        (lambda (start bound)
          (let ([i (ref1 (table-ref c start bound))])
            (if (>= 1 (- bound start))
              i
              (list i
                    (f start i)
                    (f (add1 i) bound))))))
      (list cost (f 0 bound)))))


(define optimal-bst
  (lambda (keys dummies)
    (let* ([len (vector-length dummies)]
           [w (weight keys dummies)]
           [c (init-cost dummies)])
      (optimize! w c)
      (optimal-solution c))))


;; test
(define keys (vector 0.04 0.06 0.08 0.02 0.1 0.12 0.14))
(define dummies (vector 0.06 0.06 0.06 0.06 0.05 0.05 0.05 0.05))
(optimal-bst keys dummies)