#lang racket


;; #t -> 1
;; #f -> 0
;; little endian
;; (#t #f #t) -> (1 0 1)
;; assume (= (length a) (length b))
(define add-binary-integers
  (lambda (a b)
    (let ([carry #f]
          [f #f]
          [add-bit #f])
      (set! f
        (lambda (a b)
          (cond
            [(null? a)
             (if carry (list carry) '())]
            [else
             (let ([tmp #f])
               (if carry
                   (begin
                    (set! tmp (add-bit (car a) carry))
                    (when tmp (set! carry #f))
                    (set! tmp (add-bit tmp (car b))))
                   (set! tmp (add-bit (car a) (car b))))
               (cons tmp (f (cdr a) (cdr b))))])))
      (set! add-bit
        (lambda (b1 b2)
          (cond
            [(and b1 b2)
             (set! carry #t)
             #f]
            [(or b1 b2)
             #t]
            [else
             #f])))
      (f a b))))

;; test
(add-binary-integers '(#t #f #t #t) '(#t #f #f #t))