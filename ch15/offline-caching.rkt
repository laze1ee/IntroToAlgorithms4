#lang racket


(require "../util.rkt")


(define belong?
  (lambda (b cache)
    (let f ([ls cache])
      (cond
        [(null? ls) #f]
        [(eq? b (car ls)) #t]
        [else
         (f (cdr ls))]))))


(define replace
  (lambda (lst index obj)
    (let f ([ls lst]
            [i index])
      (cond
        [(null? ls)
         (error 'replace
                "index ~s is out range in list ~s" index lst)]
        [(= i 0)
         (cons obj (cdr ls))]
        [else
         (cons (car ls) (f (cdr ls) (sub1 i)))]))))


(define new-cache (lambda (n) (make-list n 'nil)))


(define init-cache
  (lambda (n requests)
    (let ([cache (new-cache n)]
          [collect (lambda (a b c) (list a b c))])
      (let f ([i 0]
              [ls requests]
              [col collect])
        (cond
          [(or (= i n) (null? ls))
           (col cache ls '())]
          [(belong? (car ls) cache)
           (f i (cdr ls)
              (lambda (a b c)
                (col a b
                     (cons (list (car ls) #f) c))))]
          [else
           (set! cache (replace cache i (car ls)))
           (f (add1 i) (cdr ls)
              (lambda (a b c)
                (col a b
                     (cons (list (car ls) #f) c))))])))))


;; (request-block evicted-block)
;; evicted-block
;;   evicted: block
;;   existed: #f
(define miss!
  (lambda (cache requests)
    (let ([n (length cache)]
          [collect (lambda (a b) (list a b))])
      (let f ([cache cache]
              [requests requests]
              [col collect])
        (if (null? (cdr requests))
          (cond
            [(belong? (car requests) cache)
             (col 0 (list (list (car requests) #f)))]
            [else
             (col 1 (list (list (car requests) (car cache))))])
          (let ([b (car requests)])
            (cond
              [(belong? b cache)
               (f cache
                  (cdr requests)
                  (lambda (sum record)
                    (col sum
                         (cons (list b #f) record))))]
              [else
               (let ([result #f]
                     [min-sum +inf.0]
                     [min-record #f]
                     [min-i -1])
                 (do ([i 0 (add1 i)]) ((= i n))
                   (set! result
                     (f (replace cache i b)
                        (cdr requests)
                        collect))
                   (when (< (car result) min-sum)
                     (set! min-sum (0th result))
                     (set! min-record (1st result))
                     (set! min-i i)))
                 (col (add1 min-sum)
                      (cons (list b (list-ref cache min-i))
                            min-record)))])))))))


(define cache-miss
  (lambda (n requests)
    (let* ([result (init-cache n requests)]
           [cache (0th result)]
           [requests (1st result)]
           [store (2nd result)])
      (if (null? requests)
        (list 0 store)
        (begin
         (set! result (miss! cache requests))
         (list (0th result)
               (append store (1st result))))))))


;; test
(define make-request
  (lambda (n start bound)
    (let f ([i 0])
      (if (= i n)
        '()
        (cons (random-integer start bound)
              (f (add1 i)))))))

(define r (make-request 10 0 10))    
(cache-miss 4 r)