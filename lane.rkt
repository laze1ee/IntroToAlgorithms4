#lang racket


(require "util.rkt")

(provide Lane.capacity Lane.length
         Lane? new-Lane Lane Lane-ref Lane-set!
         Lane-index Lane-exchange!
         Lane-append! Lane-insert! Lane-delete!
         Lane-random-fill!)


(define const-plus 16)
(define multi-plus 1/4)
(define multi-trim 1/2)


;; Lane: #('lane Length Vector-of-Data)
(define Lane.capacity (lambda (lane) (vector-length (ref2 lane))))
(define Lane.length   (lambda (lane) (ref1 lane)))
(define Lane.length!  (lambda (lane sz) (set1! lane sz)))
(define Lane.data     (lambda (lane) (ref2 lane)))
(define Lane.data!    (lambda (lane v) (set2! lane v)))



(define Lane?
  (lambda (lane)
    (and (= 3 (vector-length lane))
         (eq? 'lane (ref0 lane))
         (natural? (ref1 lane))
         (vector? (ref2 lane)))))


(define new-Lane
  (case-lambda
    [(length init-data)
     (let ([capacity 0]
           [v #f])
       (cond
         [(not (natural? length))
          (error 'new-Lane
                 "length ~s is not a natural number" length)]
         [(< length const-plus)
          (set! capacity const-plus)]
         [(<= (floor (* length multi-plus))
              const-plus)
          (set! capacity (+ length const-plus))]
         [else
          (set! capacity
            (+ length (floor (* length multi-plus))))])
       (set! v (make-vector capacity))
       (when (not (eq? 0 init-data))
         (do ([i 0 (add1 i)]) ((= i length))
           (vector-set! v i init-data)))
       (vector 'lane length v))]
    [(length)
     (new-Lane length 0)]
    [()
     (new-Lane 0 0)]))


(define Lane
  (lambda anys
    (let* ([sz (length anys)]
           [lane (new-Lane sz)])
      (let f ([i 0]
              [ls anys])
        (if (= i sz)
          lane
          (begin
           (Lane-set! lane i (car ls))
           (f (add1 i) (cdr ls))))))))



(define valid-index?
  (lambda (lane index)
    (and (natural? index)
         (< index (Lane.length lane)))))

    
(define Lane-ref
  (lambda (lane index)
    (if (valid-index? lane index)
      (vector-ref (Lane.data lane) index)
      (error 'Lane-ref
             "invalid index ~s for lane ~s" index lane))))


(define Lane-set!
  (lambda (lane index any)
    (if (valid-index? lane index)
      (vector-set! (Lane.data lane) index any)
      (error 'Lane-set!
             "invalid index ~s for lane ~s" index lane))))



(define Lane-index
  (lambda (lane any)
    (let ([sz (Lane.length lane)])
      (let f ([i 0])
        (cond
          [(= i sz) #f]
          [(eq? any (Lane-ref lane i)) i]
          [else
           (f (add1 i))])))))


(define Lane-exchange!
  (lambda (lane i j)
    (cond
      [(not (valid-index? lane i))
       (error 'Lane-exchange!
              "invalid index ~s for lane ~s" i lane)]
      [(not (valid-index? lane j))
       (error 'Lane-exchange!
              "invalid index ~s for lane ~s" j lane)]
      [else
       (exchange! (Lane.data lane) i j)])))



(define capacity-plus
  (lambda (sz)
    (let ([plus (floor (* sz multi-plus))])
      (if (< plus 16)
        (+ sz 16)
        (+ sz plus)))))


(define capacity-trim?
  (lambda (sz capacity)
    (<= (+ sz 16) (* capacity multi-trim))))



(define Lane-append!
  (lambda (lane any)
    (let ([sz (add1 (Lane.length lane))])
      (when (> sz (Lane.capacity lane))
        (let* ([capacity (capacity-plus sz)]
               [v (make-vector capacity)])
          (vector-copy! v 0 (Lane.data lane))
          (Lane.data! lane v)))
      (Lane.length! lane sz)
      (Lane-set! lane (sub1 sz) any))))



(define Lane-insert!
  (lambda (lane index any)
    (if (and (<= 0 index)
             (<= index (Lane.length lane)))
      (insert! lane index any)
      (error 'Lane-insert!
             "invalid position ~s for lane ~s" index lane))))


(define insert!
  (lambda (lane index any)
    (let ([sz (add1 (Lane.length lane))]
          [v1 (Lane.data lane)])
      (if (> sz (Lane.capacity lane))
        (let* ([capacity (capacity-plus sz)]
               [v2 (make-vector capacity)])
          (vector-copy! v2 0 v1 0 index)
          (vector-copy! v2 (add1 index) v1 index (sub1 sz))
          (Lane.data! lane v2))
        (do ([i (sub1 sz) (sub1 i)]) ((= i index))
          (vector-set! v1 i (vector-ref v1 (sub1 i)))))
      (Lane.length! lane sz)
      (Lane-set! lane index any))))



(define Lane-delete!
  (lambda (lane index)
    (cond
      [(= 0 (Lane.length lane))
       (error 'Lane-delete! "Lane is empty")]
      [(valid-index? lane index)
       (delete! lane index)]
      [else
       (error 'Lane-delete! "invalid index ~s for lane ~s" index lane)])))


(define delete!
  (lambda (lane index)
    (let ([sz (sub1 (Lane.length lane))]
          [v1 (Lane.data lane)])
      (if (capacity-trim? sz (Lane.capacity lane))
        (let* ([capacity (+ sz const-plus)]
               [v2 (make-vector capacity)])
          (vector-copy! v2 0 v1 0 index)
          (vector-copy! v2 index v1 (add1 index) (add1 sz))
          (Lane.data! lane v2))
        (do ([i index (add1 i)])
          ((= i sz)
           (vector-set! v1 i 0))
          (vector-set! v1 i (vector-ref v1 (add1 i)))))
      (Lane.length! lane sz))))



(define Lane-random-fill!
  (lambda (lane start bound)
    (let ([sz (Lane.length lane)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (Lane-set! lane i
                   (random-integer start bound))))))


#|
;; test
(define size 4)

(define lane (Lane 7 'a 'c 5))
(Lane-index lane 5)

(do ([i (sub1 size) (sub1 i)]) ((= i -1))
  (Lane-delete! lane (random-integer 0 (add1 i))))
|#