#lang racket


(require "../util.rkt"
         "depth-first-search.rkt")

(provide topological-sort)



(define sort!
  (lambda (Vers)
    (let* ([sz (vector-length Vers)]
           [is (make-vector sz)]
           [large? #f]
           [=? #f])
      (set! large?
        (lambda (i1 i2)
          (> (.finish (vector-ref Vers i1))
             (.finish (vector-ref Vers i2)))))
      (set! =?
        (lambda (i1 i2)
          (= (.finish (vector-ref Vers i1))
             (.finish (vector-ref Vers i2)))))
      
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! is i i))
      (quick-sort! large? =? is)
      (vector->list is))))


(define topological-sort
  (lambda (Graph)
    (sort!
     (dfs Graph
          (sequence 0 (vector-length Graph))))))

#|
;; test
(define da-G
  '#((4 5 11) (2 4 8) (5 6 9) (2 6 13) (7) (8 12) (5)
              () (7) (10 11) (13) () (9) ()))

(topological-sort da-G)
|#