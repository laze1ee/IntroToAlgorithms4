#lang racket


(require "../util.rkt")

(provide random-part! random-quick-sort!
         random-part2! random-quick-sort2!)


(define part!
  (lambda (cmp v l r)
    (set! r (sub1 r))
    (let ([pivot (vector-ref v r)]
          [i l])
      (do ([j l (add1 j)]) ((= j r))
        (when (cmp (vector-ref v j) pivot)
          (exchange! v i j)
          (set! i (add1 i))))
      (exchange! v i r)
      i)))


(define quick-sort!
  (lambda (cmp v)
    (let sort ([l 0]
               [r (vector-length v)])
      (when (< 1 (- r l))
        (let ([p (part! cmp v l r)])
          (sort l p)
          (sort (add1 p) r))))))


(define random-part!
  (lambda (cmp v l r)
    (let ([p (random-integer l r)])
      (exchange! v p (sub1 r))
      (part! cmp v l r))))


(define random-quick-sort!
  (lambda (cmp v)
    (let sort ([l 0]
               [r (vector-length v)])
      (when (< 1 (- r l))
        (let ([p (random-part! cmp v l r)])
          (sort l p)
          (sort (add1 p) r))))))



(define part2!
  (lambda (cmp v l r)
    (set! r (sub1 r))
    (let ([pivot (vector-ref v r)]
          [i l]
          [j l])
      (do ([k l (add1 k)]) ((= k r))
        (cond
          [(cmp (vector-ref v k) pivot)
           (exchange! v i k)
           (when (not (= i j))
             (exchange! v j k))
           (set! j (add1 j))
           (set! i (add1 i))]
          [(= (vector-ref v k) pivot)
           (exchange! v j k)
           (set! j (add1 j))]))
      (exchange! v j r)
      (set! j (add1 j))
      (list i j))))


(define random-part2!
  (lambda (cmp v l r)
    (let ([p (random-integer l r)])
      (exchange! v p (sub1 r))
      (part2! cmp v l r))))


(define random-quick-sort2!
  (lambda (cmp v)
    (let sort ([l 0]
               [r (vector-length v)])
      (when (< 1 (- r l))
        (let ([p (random-part2! cmp v l r)])
          (sort l (0th p))
          (sort (1st p) r))))))



;; test
(define v (make-vector 33 0))
(random-fill! v 0 10)
v
(random-quick-sort2! > v)
v
