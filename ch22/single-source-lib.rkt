#lang racket


(require "../util.rkt")

(provide .cost .cost! .parent .parent!
         init-single-source relax!)


;; #(#(Distance Parent) ...)
(define .cost    (lambda (vertex) (ref0 vertex)))
(define .cost!   (lambda (vertex n) (set0! vertex n)))
(define .parent  (lambda (vertex) (ref1 vertex)))
(define .parent! (lambda (vertex i) (set1! vertex i)))


(define new-vertex (lambda () (vector +inf.0 -1)))


(define init-single-source
  (lambda (Graph s)
    (let* ([sz (vector-length Graph)]
           [Vers (make-vector sz)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! Vers i (new-vertex)))
      (.cost! (vector-ref Vers s) 0)
      Vers)))


(define relax!
  (lambda (Vers weight u-index v-index)
    (let ([u (vector-ref Vers u-index)]
          [v (vector-ref Vers v-index)]
          [cost #f])
      (set! cost (+ (.cost u) weight))
      (when (> (.cost v) cost)
        (.cost! v cost)
        (.parent! v u-index)))))
