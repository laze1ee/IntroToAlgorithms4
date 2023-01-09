#lang racket


(require "../util.rkt"
         "single-source-lib.rkt")

(provide bellman-ford)



(define bellman-ford
  (lambda (Graph s)
    (let ([sz (vector-length Graph)]
          [Vers (init-single-source Graph s)]
          [v-index #f]
          [weight #f]
          [reachable #t])
      (do ([i 1 (add1 i)]) ((= i sz))
        (do ([u-index 0 (add1 u-index)]) ((= u-index sz))
          (do ([edges (vector-ref Graph u-index)
                      (cdr edges)]) ((null? edges))
            (set! v-index (0th (car edges)))
            (set! weight (1st (car edges)))
            (relax! Vers weight u-index v-index))))
      (do ([u-index 0 (add1 u-index)]) ((= u-index sz))
        (let f ([edges (vector-ref Graph u-index)])
          (when (not (null? edges))
            (set! v-index (0th (car edges)))
            (set! weight (1st (car edges)))
            (if (> (.cost (vector-ref Vers v-index))
                   (+ (.cost (vector-ref Vers u-index))
                      weight))
              (set! reachable #f)
              (f (cdr edges))))))
      (if reachable
        Vers
        #f))))


#|
;; test
(define G
  '#(((1 6) (4 7)) ((2 5) (3 -4) (4 8)) ((1 -2))
                   ((0 2) (2 7)) ((2 -3) (3 9))))

(map (lambda (source)
       (bellman-ford G source))
     '(0 1 2 3 4))
|#