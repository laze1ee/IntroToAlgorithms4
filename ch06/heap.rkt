#lang racket


(require "../lane.rkt")

(provide heapify! build-heap!
         max-heapify! min-heapify!
         build-max-heap! build-min-heap!
         max-heap-maximum max-heap-extract!
         max-heap-increase! max-heap-insert!)


(define parent
  (lambda (i)
    (- (ceiling (/ i 2)) 1)))


(define left
  (lambda (i)
    (+ 1 (* i 2))))


(define right
  (lambda (i)
    (+ 2 (* i 2))))


(define heapify!
  (lambda (cmp H i length)
    (let ([iter #f])
      (set! iter
        (lambda (i)
          (let ([l (left i)]
                [r (right i)]
                [largest i])
            (when (and (< l length)
                       (cmp (Lane-ref H l)
                            (Lane-ref H largest)))
              (set! largest l))
            (when (and (< r length)
                       (cmp (Lane-ref H r)
                            (Lane-ref H largest)))
              (set! largest r))
            (when (not (= largest i))
              (Lane-exchange! H i largest)
              (iter largest)))))
      (iter i))))


(define max-heapify!
  (lambda (H i)
    (heapify! > H i (Lane.length H))))


(define min-heapify!
  (lambda (H i)
    (heapify! < H i (Lane.length H))))


(define build-heap!
  (lambda (cmp H)
    (let ([length (Lane.length H)])
      (do ([i (sub1 (floor (/ length 2))) (sub1 i)]) ((= i -1))
        (heapify! cmp H i length)))))


(define build-max-heap!
  (lambda (H)
    (build-heap! > H)))


(define build-min-heap!
  (lambda (H)
    (build-heap! < H)))



(define max-heap-maximum
  (lambda (H)
    (if (= 0 (Lane.length H))
      (error 'max-heap-maximum "heap is empty")
      (Lane-ref H 0))))


(define max-heap-extract!
  (lambda (H)
    (let ([max (Lane-ref H 0)])
      (Lane-delete! H 0)
      (max-heapify! H 0)
      max)))


(define max-heap-increase!
  (lambda (H old new)
    (if (>= old new)
      (error 'max-heap-increase!
             "new key ~s is not larger than old key ~s"
             new old)
      (let ([index (Lane-index H old)])
        (if (boolean? index)
          (error 'max-heap-increase!
                 "not found old key ~s" old)
          (let f ([i index])
            (cond
              [(= i 0)
               (Lane-set! H i new)]
              [(>= (Lane-ref H (parent i))
                   new)
               (Lane-set! H i new)]
              [else
               (Lane-set! H i (Lane-ref H (parent i)))
               (f (parent i))])))))))
            

(define max-heap-insert!
  (lambda (H new)
    (Lane-append! H new)
    (let f ([i (sub1 (Lane.length H))])
      (cond
        [(= i 0)
         (Lane-set! H i new)]
        [(>= (Lane-ref H (parent i))
             new)
         (Lane-set! H i new)]
        [else
         (Lane-set! H i (Lane-ref H (parent i)))
         (f (parent i))]))))


#|
;; test
(define heap (Lane 7 56 9 36 76 66 73 39 34 16))
(build-max-heap! heap)
heap
(max-heap-increase! heap 9 59)
heap
(max-heap-insert! heap 50)
heap
|#