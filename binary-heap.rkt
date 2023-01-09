#lang racket


(require "lane.rkt"
         "util.rkt")

(provide BHeap.length
         BHeap-null? BHeap-ref BHeap-set! BHeap-index
         new-BHeap BHeap-top BHeap-pop! BHeap-append!
         BHeap-inform!)


(define parent
  (lambda (i)
    (- (ceiling (/ i 2)) 1)))


(define left
  (lambda (i)
    (+ 1 (* i 2))))


(define right
  (lambda (i)
    (+ 2 (* i 2))))


(define half
  (lambda (size)
    (sub1 (floor (/ size 2)))))


;; Binary-Heap: #(Compare-Procedure Lane)
(define .compare? (lambda (heap) (ref0 heap)))
(define .lane     (lambda (heap) (ref1 heap)))


(define BHeap.length
  (lambda (heap)
    (Lane.length (.lane heap))))


(define BHeap-null?
  (lambda (heap)
    (= 0 (Lane.length (.lane heap)))))


(define BHeap-ref
  (lambda (heap index)
    (Lane-ref (.lane heap) index)))


(define BHeap-set!
  (lambda (heap index any)
    (Lane-set! (.lane heap) index any)))


(define BHeap-index
  (lambda (heap any)
    (Lane-index (.lane heap) any)))



(define heapify-down!
  (lambda (heap i)
    (let ([compare? (.compare? heap)]
          [size (BHeap.length heap)])
      (let f ([i i]
              [l (left i)]
              [r (right i)]
              [m i])
        (when (and (< l size)
                   (compare? (BHeap-ref heap l)
                             (BHeap-ref heap m)))
          (set! m l))
        (when (and (< r size)
                   (compare? (BHeap-ref heap r)
                             (BHeap-ref heap m)))
          (set! m r))
        (when (not (= i m))
          (Lane-exchange! (.lane heap) i m)
          (f m (left m) (right m) m))))))


(define heapify-up!
  (lambda (heap i)
    (let ([compare? (.compare? heap)])
      (let f ([i i]
              [p (parent i)])
        (when (and (< 0 i)
                   (compare? (BHeap-ref heap i)
                             (BHeap-ref heap p)))
          (Lane-exchange! (.lane heap) i p)
          (f p (parent p)))))))



(define new-BHeap
  (lambda (compare? lane)
    (let ([size (Lane.length lane)]
          [heap (vector compare? lane)])
      (do ([i (half size) (sub1 i)]) ((= i -1))
        (heapify-down! heap i))
      heap)))


(define BHeap-top
  (lambda (heap)
    (if (BHeap-null? heap)
      (error 'BHeap-top "heap is empty")
      (BHeap-ref heap 0))))


(define BHeap-pop!
  (lambda (heap)
    (if (BHeap-null? heap)
      (error 'BHeap-pop! "heap is empty")
      (let ([top (BHeap-ref heap 0)])
        (Lane-delete! (.lane heap) 0)
        (heapify-down! heap 0)
        top))))


(define BHeap-append!
  (lambda (heap any)
    (let ([tail (BHeap.length heap)])
      (Lane-append! (.lane heap) any)
      (heapify-up! heap tail))))


(define BHeap-inform!
  (lambda (heap index)
    (let ([any (BHeap-ref heap index)]
          [cmp? (.compare? heap)]
          [size (BHeap.length heap)]
          [p (parent index)]
          [l #f]
          [r #f])
      (set! l (left index))
      (set! l (if (< l size) l #f))
      (set! r (right index))
      (set! r (if (< r size) r #f))

      (cond
        [(= 0 index)
         (heapify-down! heap 0)]
        [(boolean? l)
         (heapify-up! heap index)]
        [(cmp? any (BHeap-ref heap p))
         (heapify-up! heap index)]
        [(or (not (cmp? any (BHeap-ref heap l)))
             (if (boolean? r) r (not (cmp? any (BHeap-ref heap r)))))
         (heapify-down! heap index)]))))


#|
;; test
(define A (Lane 5 9 7 2 1 4 8 6 3 13 10))
(define heap (new-BHeap < A))
heap
(BHeap-set! heap 4 12)
(BHeap-inform! heap 4)
(BHeap-inform! heap 0)
heap
|#