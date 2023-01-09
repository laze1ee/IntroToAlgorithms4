#lang racket


;; An example of implementing light-weight threads,
;; that quoted the code of section 12.11 Multitasking with Engines
;; in the book The Scheme Programming Language.
;; html: https://scheme.com/tspl4/examples.html#./examples:h11


(require "engine.rkt")


#|
;; example 1
(define eng
  (make-engine
   (timed-lambda () 3)))

(eng 10
     list
     (lambda (x) x))
|#



;; example 2
(define fibonacci
  (timed-lambda (n)
                (if (< n 2)
                    n
                    (+ (fibonacci (- n 1))
                       (fibonacci (- n 2))))))
#|
(define eng
  (make-engine (timed-lambda () (fibonacci 10))))

(eng 50
     list
     (lambda (new-eng)
       (set! eng new-eng)
       "expired"))

(eng 50
     list
     (lambda (new-eng)
       (set! eng new-eng)
       "expired"))

(eng 50
     list
     (lambda (new-eng)
       (set! eng new-eng)
       "expired"))

(eng 50
     list
     (lambda (new-eng)
       (set! eng new-eng)
       "expired"))
|#


;; example 3
(define mileage
  (lambda (thunk)
    (let f ([eng (make-engine thunk)]
            [total-ticks 0])
      (eng 50
           (lambda (ticks value)
             (+ total-ticks (- 50 ticks)))
           (lambda (new-eng)
             (f new-eng (+ total-ticks 50)))))))

;(mileage (timed-lambda () (fibonacci 10)))



;; example 4
(define round-robin
  (lambda (engs)
    (if (null? engs)
        '()
        ((car engs) 1
                    (lambda (ticks value)
                      (cons value (round-robin (cdr engs))))
                    (lambda (eng)
                      (round-robin
                       (append (cdr engs) (list eng))))))))
#|
(round-robin
 (map (lambda (x)
        (make-engine
         (timed-lambda () (fibonacci x))))
      '(4 5 2 7 8 6 2 3)))
|#


;; example 5
(define first-true
  (lambda (engs)
    (if (null? engs)
        #f
        ((car engs) 1
                    (lambda (ticks value)
                      (if (boolean? value)
                          (first-true (cdr engs))
                          value))
                    (lambda (eng)
                      (first-true
                       (append (cdr engs) (list eng))))))))

(define-syntax por
  (syntax-rules ()
    [(_ x ...)
     (first-true
      (list (make-engine (timed-lambda () x)) ...))]))

(por 5 1)
(por ((timed-lambda (x) (x x))
      (timed-lambda (x) (x x)))
     (fibonacci 10))