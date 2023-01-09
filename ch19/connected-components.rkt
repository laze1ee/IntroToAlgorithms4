#lang racket


(require "../util.rkt")


;; Graph: #(List-of-Vertices List-of-Edges)
;; Vertice: Natrual-Number
;; Edge: (Natrual-Number Natrual-Number)
(define .verts (lambda (graph) (ref0 graph)))
(define .edges   (lambda (graph) (ref1 graph)))


(define make-set
  (lambda (v)
    (list v)))


(define find-set
  (lambda (sets v)
    (cond
      [(null? sets) #f]
      [(occur? = (car sets) v) (car sets)]
      [else
       (find-set (cdr sets) v)])))


(define union
  (lambda (s1 s2)
    (append s1 s2)))


(define connected-components
  (lambda (Graph)
    (let ([sets '()])
      (do ([vs (.verts Graph) (cdr vs)]) ((null? vs))
        (set! sets (cons (list (car vs)) sets)))
      (do ([es (.edges Graph) (cdr es)]) ((null? es))
        (let ([s1 (find-set sets (0th (car es)))]
              [s2 (find-set sets (1st (car es)))])
          (when (not (eq? s1 s2))
            (set! sets (remove eq? sets s1))
            (set! sets (remove eq? sets s2))
            (set! sets (insert-by-index sets (union s1 s2) 0)))))
      sets)))


(define same-component
  (lambda (sets u v)
    (eq? (find-set sets u)
         (find-set sets v))))



;; test
(define Graph
  (vector '(0 1 2 3 4 5 6 7 8 9 10)
          '((3 8) (5 10) (6 8) (1 6) (0 7) (8 9)
                  (3 10) (1 9) (3 5) (6 9) (0 4))))

(define connected (connected-components Graph))
(same-component connected 1 10)
