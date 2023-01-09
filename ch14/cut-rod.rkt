#lang racket


(define redundant-cut-rod
  (lambda (n)
    (if (= n 0)
      '()
      (let ([store '()])
        (do ([i 1 (add1 i)]) ((> i n))
          (set! store
            (append store
                    (list (cons i (redundant-cut-rod (- n i)))))))
        store))))


(define table
  '(()
    ((1))
    ((1 1) (2))
    ((1 1 1) (1 2) (3))))

(define cut-rod
  (lambda (n)
    (if (< n (length table))
      (list-ref table n)
      (let ([mid (floor (/ n 2))]
            [ntab '()])
        (do ([i 1 (add1 i)]) ((> i mid))
          (let f ([rods (cut-rod (- n i))])
            (when (not (null? rods))
              (when (<= i (caar rods))
                (set! ntab
                  (append ntab (list (cons i (car rods))))))
              (f (cdr rods)))))
        (set! ntab (append ntab (list (list n))))
        (set! table (append table (list ntab)))
        ntab))))


;; test
(cut-rod 10)