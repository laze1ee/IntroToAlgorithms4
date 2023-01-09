#lang racket


(require "../util.rkt"
         "../matrix.rkt"
         "../queue.rkt")



;; Vertex-Attribute: #(Fresh Predecessor)
;; Fresh: #t
;;      | #f
;; Predecessor: Vertex-Index
(define .fresh        (lambda (vertex) (ref0 vertex)))
(define .nofresh!     (lambda (vertex) (set0! vertex #f)))
(define .predecessor  (lambda (vertex) (ref1 vertex)))
(define .predecessor! (lambda (vertex i) (set1! vertex i)))

(define new-vertex (lambda () (vector #t -1)))



;; Adjacency-Matrix-Graph
(define new-G  (lambda (n) (new-Matrix n n 0)))
(define .size  (lambda (Graph) (Matrix.row Graph)))
(define .flow  (lambda (Graph u v) (Matrix-ref Graph u v)))
(define .flow! (lambda (Graph u v f) (Matrix-set! Graph u v f)))
(define edge?
  (lambda (Graph u v)
    (and (< 0 (.flow Graph u v))
         (not (= +inf.0 (.flow Graph u v))))))



(define bfs
  (lambda (Graph s)
    (let* ([sz (.size Graph)]
           [Vers (make-vector sz)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! Vers i (new-vertex)))
      (.nofresh! (vector-ref Vers s))
      (visit! Graph Vers s)
      Vers)))


(define visit!
  (lambda (Graph Vers s)
    (let ([sz (.size Graph)]
          [Q (new-queue)]
          [v-vx #f])
      (enqueue! Q s)
      (let f ([u (dequeue! Q)])
        (do ([v 0 (add1 v)]) ((= v sz))
          (when (edge? Graph u v)
            (set! v-vx (vector-ref Vers v))
            (when (.fresh v-vx)
              (.nofresh! v-vx)
              (.predecessor! v-vx u)
              (enqueue! Q v))))
        (when (not (null-queue? Q))
          (f (dequeue! Q)))))))


(define out-path
  (lambda (Vers s t)
    (let f ([p t]
            [path '()])
      (cond
        [(= s p) (cons s path)]
        [(= -1 p) #f]
        [else
         (f (.predecessor (vector-ref Vers p))
            (cons p path))]))))


(define find-path
  (lambda (Graph s t)
    (let* ([Vers (bfs Graph s)]
           [path (out-path Vers s t)])
      (if (boolean? path)
        #f
        path))))


(define min-flow
  (lambda (Graph path)
    (let f ([u (0th path)]
            [path path]
            [minf +inf.0])
      (if (null? (cdr path))
        minf
        (let* ([v (1st path)]
               [flow (.flow Graph u v)])
          (when (> minf flow)
            (set! minf flow))
          (f v (cdr path) minf))))))


(define augment!
  (lambda (Gr Gf path minf)
    (let f ([u (0th path)]
            [path path])
      (when (not (null? (cdr path)))
        (let ([v (1st path)])
          
          ;; residue!
          (.flow! Gr u v (- (.flow Gr u v) minf))
          (.flow! Gr v u (+ (.flow Gr v u) minf))

          ;; enhance!
          (if (edge? Gf v u)
            (.flow! Gf v u (- (.flow Gf v u) minf))
            (.flow! Gf u v (+ (.flow Gf u v) minf)))
          
          (f v (cdr path)))))))


(define ford-fulkerson
  (lambda (Graph s t)
    (let* ([sz (.size Graph)]
           [Gr (Matrix-copy Graph)]
           [Gf (new-G sz)])
      (let f ([path (find-path Gr s t)])
        (when (list? path)
          (augment! Gr Gf path (min-flow Gr path))
          (f (find-path Gr s t))))
      Gf)))



;; test
(define G (new-G 6))

(.flow! G 0 1 16)
(.flow! G 0 2 13)
(.flow! G 1 3 12)
(.flow! G 2 1 4)
(.flow! G 2 4 14)
(.flow! G 3 2 9)
(.flow! G 3 5 20)
(.flow! G 4 3 7)
(.flow! G 4 5 4)

(Matrix-print (ford-fulkerson G 0 5))