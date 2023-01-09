#lang racket


(require "../util.rkt"
         "../matrix.rkt"
         "../ch22/bellman-ford.rkt"
         "../ch22/dijkstra.rkt")



(define add-source
  (lambda (Graph)
    (let* ([sz (vector-length Graph)]
           [G1 (make-vector (add1 sz))]
           [f #f])
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! G1 i
                     (copy (vector-ref Graph i))))
      (set! f
        (lambda (i)
          (if (= i sz)
            '()
            (cons (list i 0)
                  (f (add1 i))))))
      (vector-set! G1 sz (f 0))
      G1)))


(define w1
  (lambda (u edge Vers)
    (let* ([v (0th edge)]
           [uw (ref0 (vector-ref Vers u))]
           [vw (ref0 (vector-ref Vers v))])
      (list v (- (+ (1st edge) uw) vw)))))


(define reweight!
  (lambda (Graph Vers)
    (let ([sz (vector-length Graph)]
          [f #f])
      (set! f
        (lambda (u edges)
          (if (null? edges)
            '()
            (cons (w1 u (car edges) Vers)
                  (f u (cdr edges))))))
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! Graph i (f i (vector-ref Graph i)))))))


(define johnson
  (lambda (Graph)
    (let* ([sz (vector-length Graph)]
           [G1 (add-source Graph)]
           [paths (new-Matrix sz sz)]
           [Vers #f])
      (reweight! G1 (bellman-ford G1 sz))
      (do ([u 0 (add1 u)]) ((= u sz))
        (set! Vers (dijkstra G1 u))
        (do ([v 0 (add1 v)]) ((= v sz))
          (Matrix-set! paths u v
                       (ref1 (vector-ref Vers v)))))
      paths)))


;; test
(define G
  '#(((1 3) (2 8) (4 -4)) ((3 1) (4 7)) ((1 4))
                          ((2 -5) (0 2)) ((3 6))))

(Matrix-print (johnson G))