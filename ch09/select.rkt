#lang racket


(require "random-select.rkt"
         "../util.rkt")


(define linear-select!
  (lambda (v index)
    (let ([len (vector-length v)])
      (cond
        [(= 0 len)
         (error 'random-select! "vector ~s is empty" v)]
        [(and (<= 0 index) (< index len))
         (select! v 0 len index)]
        [else
         (error 'random-select!
                "index ~s in invalid for vector ~s"
                index v)]))))


(define median-of-5!
  (lambda (v i1 i2 i3 i4 i5)
    (when (> (vector-ref v i1) (vector-ref v i2))
      (exchange! v i1 i2))
    (when (> (vector-ref v i3) (vector-ref v i4))
      (exchange! v i3 i4))
    (let ([j1 (if (< (vector-ref v i1) (vector-ref v i3))
                i3 i1)]
          [j2 (if (< (vector-ref v i2) (vector-ref v i4))
                i2 i4)])
      (when (> (vector-ref v j1) (vector-ref v j2))
        (exchange! v j1 j2))
      (cond
        [(<= (vector-ref v i5) (vector-ref v j1))
         (exchange! v j1 i3)]
        [(>= (vector-ref v i5) (vector-ref v j2))
         (exchange! v j2 i3)]
        [else
         (exchange! v i5 i3)]))))
                


(define partition!
  (lambda (v start bound x)
    (let f ([i start]
            [j (sub1 bound)])
      (cond
        [(= i j) i]
        [(= 1 (- j i))
         (if (= x (vector-ref v i))
           i
           j)]
        [(and (<= x (vector-ref v i))
              (> x (vector-ref v j)))
         (exchange! v i j)
         (f (add1 i) (sub1 j))]
        [(<= x (vector-ref v i))
         (f i (sub1 j))]
        [(> x (vector-ref v j))
         (f (add1 i) j)]
        [else
         (f (add1 i) (sub1 j))]))))


(define select!
  (lambda (v start bound index)
    (call/cc
     (lambda (hop)
       (let ([iter #f]
             [g #f]
             [x #f]
             [p #f])
         
         (set! iter
           (lambda ()
             (when (not (= 0 (modulo (- bound start) 5)))
               (do ([i (add1 start) (add1 i)]) ((= i bound))
                 (when (> (vector-ref v start) (vector-ref v i))
                   (exchange! v start i)))
               (when (= start index)
                 (hop (vector-ref v start)))
               (set! start (add1 start))
               (iter))))
         (iter)

         (set! g (/ (- bound start) 5))

         (do ([i start (add1 i)]) ((= i (+ start g)))
           (median-of-5!
            v i (+ i g) (+ i (* 2 g)) (+ i (* 3 g)) (+ i (* 4 g))))

         (let ([l (+ start (* 2 g))]
               [r (+ start (* 3 g))])
           (set! x (select! v l r
                            (middle l r))))

         (set! p (partition! v start bound x))

         (cond
           [(= index p)
            (vector-ref v p)]
           [(< index p)
            (select! v start p index)]
           [else
            (select! v (add1 p) bound index)]))))))


;; test
(define v (make-vector 19 0))
(random-fill! v 0 100)
v
(define test
  (lambda (f ls)
    (map (lambda (i) (f v i)) ls)))
(test linear-select! '(0 1 3 5 7 9 10 11 14))
(test random-select! '(0 1 3 5 7 9 10 11 14))