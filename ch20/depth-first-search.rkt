#lang racket


(require "../util.rkt")

(provide .finish
         new-vertex visit! dfs)


;; Graph Attribute: #(Discover Finish Color Predecessor)
;;                : nil
;; Discover: Natural-Number
;; Finish: Natural-Number
;; Color: white
;;      : grey
;;      ; black
;; Predecessor: Vertex-Index
;; Graph represented by adjacent list: #((adjacent-vertices) ...)
(define  @white '@W)
(define  @grey  '@G)
(define  @black '@B)
(define .discover   (lambda (vertex) (ref0 vertex)))
(define .discover!  (lambda (vertex n) (set0! vertex n)))
(define .finish     (lambda (vertex) (ref1 vertex)))
(define .finish!    (lambda (vertex n) (set1! vertex n)))
(define .color  (lambda (vertex) (ref2 vertex)))
(define .color! (lambda (vertex color) (set2! vertex color)))
(define .predecessor  (lambda (vertex) (ref3 vertex)))
(define .predecessor! (lambda (vertex index) (set3! vertex index)))

(define new-vertex  (lambda () (vector +inf.0 +inf.0 @white -1)))


(define visit!
  (lambda (Graph Verts index count)
    (let f ([index index])
      (let ([u (vector-ref Verts index)]
            [v #f])
        (set0! count (add1 (ref0 count)))
        (.discover! u (ref0 count))
        (.color! u @grey)
        (do ([is (vector-ref Graph index) (cdr is)]) ((null? is))
          (set! v (vector-ref Verts (car is)))
          (when (eq? @white (.color v))
            (.predecessor! v index)
            (f (car is))))
        (set0! count (add1 (ref0 count)))
        (.finish! u (ref0 count))
        (.color! u @black)))))


(define dfs
  (lambda (Graph order)
    (let* ([sz (vector-length Graph)]
           [Verts (make-vector sz)]
           [count (vector 0)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! Verts i (new-vertex)))
      (do ([is order (cdr is)]) ((null? is))
        (let ([u (vector-ref Verts (car is))])
          (when (eq? @white (.color u))
            (visit! Graph Verts (car is) count))))
      Verts)))


#|
;; test
(define un-G
  '#((1 2 3) (0 4 5) (0 4 6) (0 5 6) (1 2 7 8) (1 3) (2 3 7)
             (4 6 8) (4 7)))
(dfs un-G (sequence 0 (vector-length un-G)))

|#