#lang racket


(require "../util.rkt")



;; Block: #(Mark Block-Name)
;; Mark: #t
;;     : #f
(define make-cache
  (lambda (sz)
    (let ([cache (make-vector sz)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! cache i (vector #f -1)))
      cache)))


(define find
  (lambda (sz cache b)
    (let f ([i 0])
      (if (= i sz)
        #f
        (let ([block (vector-ref cache i)])
          (if (= b (ref1 block))
            block
            (f (add1 i))))))))


(define spare
  (lambda (sz cache)
    (let ([count 0])
      (do ([i 0 (add1 i)]) ((= i sz))
        (when (not (ref0 (vector-ref cache i)))
          (set! count (add1 count))))
      count)))


(define unmark-all!
  (lambda (sz cache)
    (do ([i 0 (add1 i)]) ((= i sz))
      (set0! (vector-ref cache i) #f))))


(define random-select
  (lambda (sz cache bound)
    (let f ([r (random-integer 0 bound)]
            [block (vector-ref cache 0)]
            [i 1])
      (cond
        [(and (= 0 r)
              (not (ref0 block)))
         block]
        [(not (ref0 block))
         (f (sub1 r) (vector-ref cache i) (add1 i))]
        [else
         (f r (vector-ref cache i) (add1 i))]))))


(define get-evict-count #f)
(define reset-evict-count #f)

(define caching!
  (let ([init-count 0]
        [evict-count 0])
    (set! get-evict-count
      (lambda () evict-count))
    (set! reset-evict-count
      (lambda () (set! evict-count 0)))
    
    (lambda (cache b)
      (let* ([sz (vector-length cache)]
             [block (find sz cache b)])
        (if (< init-count sz)
          (if (boolean? block)
            (begin
              (set! block (vector-ref cache init-count))
              (set1! block b)
              (set! init-count (add1 init-count)))
            (set0! block #t))
          (if (vector? block)
            (set0! block #t)
            (let ([bound (spare sz cache)])
              (when (= 0 bound)
                (unmark-all! sz cache)
                (set! bound sz))
              (set! block (random-select sz cache bound))
              (set0! block #t)
              (set1! block b)
              (set! evict-count (add1 evict-count)))))))))



;; test
(define cache-sz 3)
(define cache (make-cache cache-sz))

(define times 15)
(define bound 10)
(do ([i 0 (add1 i)]) ((= i times))
  (let ([b (random-integer 0 bound)])
    (caching! cache b)
    (printf "~s ~s~%" b cache)))
(get-evict-count)