#lang racket


(require "../util.rkt")


(define decide-slot
  (lambda (slot)
    (next-prime slot)))


(define hash
  (lambda (key slot i)
    (modulo (+ i key) slot)))


(define hash-insert!
  (lambda (t key slot)
    (let ([len (vector-length t)])
      (let f ([i 0]
              [data (list key)])
        (if (= i len)
          (error 'hash-insert "hash table is full")
          (let ([p (hash key slot i)])
            (if (eq? 'null (vector-ref t p))
              (vector-set! t p
                           (cons p data))
              (f (add1 i) (cons p data)))))))))


;; test
(define slot 10)
(set! slot (decide-slot slot))
(define table (make-vector slot 'null))
(map (lambda (key)
       (hash-insert! table key slot))
     '(10 22 31 4 15 28 17 88 59))
table