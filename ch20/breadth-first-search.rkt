#lang racket


(require "../util.rkt")


;; Queue: #(List-of-Data)
;;      : #(())
(define new-Queue (lambda () (vector '())))


(define Queue-null?
  (lambda (Q)
    (null? (ref0 Q))))


(define Queue-en!
  (lambda (Q a)
    (set0! Q (append (ref0 Q) (list a)))))


(define Queue-de!
  (lambda (Q)
    (let ([a #f])
      (if (Queue-null? Q)
        (error 'Queue-de!
               "Queue ~s is empty" Q)
        (begin
         (set! a (car (ref0 Q)))
         (set0! Q (cdr (ref0 Q)))
         a)))))



;; Graph Attribute: #(Color Distance Predecessor)
;;                : nil
;; Color: white
;;      : grey
;;      ; black
;; Distance: Natrual-Number
;; Predecessor: Vertex-Index
;; Graph represented by adjacent list: #((adjacent-vertices) ...)
(define  @white '@W)
(define  @grey  '@G)
(define  @black '@B)
(define .color  (lambda (vertex) (ref0 vertex)))
(define .color! (lambda (vertex color) (set0! vertex color)))
(define .distance   (lambda (vertex) (ref1 vertex)))
(define .distance!  (lambda (vertex d) (set1! vertex d)))
(define .predecessor  (lambda (vertex) (ref2 vertex)))
(define .predecessor! (lambda (vertex index) (set2! vertex index)))


(define new-vertex  (lambda () (vector @white +inf.0 -1)))


(define visit!
  (lambda (Graph Verts s-index)
    (let ([Q (new-Queue)]
          [u #f]
          [v #f])
      (Queue-en! Q s-index)
      (let f ([index (Queue-de! Q)])
        (set! u (vector-ref Verts index))
        (do ([adj (vector-ref Graph index) (cdr adj)]) ((null? adj))
          (set! v (vector-ref Verts (car adj)))
          (when (eq? @white (.color v))
            (.color! v @grey)
            (.distance! v (add1 (.distance u)))
            (.predecessor! v index)
            (Queue-en! Q (car adj)))
          (.color! u @black))
        (when (not (Queue-null? Q))
          (f (Queue-de! Q)))))))
    

(define bfs
  (lambda (Graph s)
    (let* ([sz (vector-length Graph)]
           [Verts (make-vector sz)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! Verts i (new-vertex)))
      (.color! (vector-ref Verts s) @grey)
      (.distance! (vector-ref Verts s) 0)
      (visit! Graph Verts s)
      Verts)))


(define print-path
  (lambda (Verts s-index v-index)
    (let f ([v-index v-index]
            [path '()])
      (if (> 0 v-index)
        (error 'print-path
               "no path from ~s to ~s exists" s-index v-index)
        (let ([v (vector-ref Verts v-index)])
          (if(= s-index v-index)
            (cons s-index path)
            (f (.predecessor v)
               (cons v-index path))))))))



;; test
(define un-G
  '#((1 2 3) (0 4 5) (0 4 6) (0 5 6) (1 2 7 8) (1 3) (2 3 7)
             (4 6 8) (4 7)))

(print-path (bfs un-G 0) 0 8)