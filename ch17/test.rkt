#lang racket


(require "../util.rkt"
         "node.rkt"
         "interval-tree.rkt")


(define random-interval
  (lambda (min bound)
    (let ([start (random-integer min bound)])
      (let f ([end (random-integer min bound)])
        (cond
          [(< start end)
           (vector start end)]
          [(> start end)
           (vector end start)]
          [else
           (f (random-integer min bound))])))))


(define range-start 0)
(define range-end 99)

(define new-interval
  (lambda ()
    (random-interval range-start (add1 range-end))))

(define RB (new-RB))
(define intervals '())

(do ([i 0 (add1 i)]) ((= i 7))
  (let ([i (new-interval)])
    (set! intervals (cons i intervals))
    (RB-insert! RB i)))

(let f ([ls (reverse intervals)])
  (when (not (null? ls))
    (RB-delete! RB (car ls))
    (pretty-print (.root RB))
    (newline)
    (f (cdr ls))))