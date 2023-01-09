#lang racket


(require "../matrix.rkt"
         "vertex-attributes.rkt")



(define floyd-warshall
  (lambda (Graph)
    (let ([sz (Matrix.row Graph)]
          [M (Matrix-copy Graph)]
          [cij #f]
          [cik #f]
          [pik #f]
          [ckj #f]
          [pkj #f])
      (do ([k 0 (add1 k)]) ((= k sz))
        (do ([i 0 (add1 i)]) ((= i sz))
          (set! cik (cost-ref M i k))
          (set! pik (pred-ref M i k))
          (do ([j 0 (add1 j)]) ((= j sz))
            (set! cij (cost-ref M i j))
            (set! ckj (cost-ref M k j))
            (set! pkj (pred-ref M k j))
            (when (> cij (+ cik ckj))
              (cost-set! M i j (+ cik ckj))
              (if (= k j)
                (pred-set! M i j pik)
                (pred-set! M i j pkj))))))
      M)))


;; test
(define n 5)
(define G (new-Matrix n n))

(do ([i 0 (add1 i)]) ((= i 5))
  (do ([j 0 (add1 j)]) ((= j 5))
    (if (= i j)
      (Matrix-set! G i j (new-vertex 0 -1))
      (Matrix-set! G i j (new-vertex)))))

(both! G 0 1 3 0)
(both! G 0 2 8 0)
(both! G 0 4 -4 0)
(both! G 1 3 1 1)
(both! G 1 4 7 1)
(both! G 2 1 4 2)
(both! G 3 0 2 3)
(both! G 3 2 -5 3)
(both! G 4 3 6 4)

(Matrix-print (floyd-warshall G))
