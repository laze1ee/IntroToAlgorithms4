#lang racket


(require "../util.rkt")

(provide new-Seq Seq-insert!
         Seq-extract-head! Seq-extract-tail!)


;; Sequence
(struct Seq
  (compare
   [data #:mutable])
  #:transparent)


(define new-Seq
  (lambda (procedure)
    (Seq procedure '())))


(define Seq.compare Seq-compare)
(define Seq.data    Seq-data)
(define Seq.data!   set-Seq-data!)


(define Seq-insert!
  (lambda (S obj)
    (let ([f #f])
      (set! f
        (lambda (ls)
          (cond
            [(null? ls)
             (list obj)]
            [((Seq.compare S) obj (car ls))
             (cons obj ls)]
            [else
             (cons (car ls) (f (cdr ls)))])))
      (Seq.data! S (f (Seq.data S))))))


(define Seq-extract-head!
  (lambda (S)
    (let ([min (car (Seq.data S))])
      (Seq.data! S (cdr (Seq.data S)))
      min)))


(define Seq-extract-tail!
  (lambda (S)
    (let* ([last (sub1 (length (Seq.data S)))]
           [max (list-ref (Seq.data S) last)])
      (Seq.data! S (list-head (Seq.data S) last))
      max)))


#|
;; test
(define cmp
  (lambda (obj1 obj2)
    (< (ref0 obj1) (ref0 obj2))))

(define S (new-Seq cmp))
(Seq-insert! S '#(5))
(Seq-insert! S '#(9))
(Seq-insert! S '#(6))
(Seq-extract-head! S)
S
(Seq-extract-tail! S)
S
|#