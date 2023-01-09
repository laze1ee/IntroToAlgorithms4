#lang racket


(require "../util.rkt"
         "../matrix.rkt"
         "../queue.rkt"
         "hungarian-help.rkt")


(define init-label
  (lambda (hsz bGraph)
    (let ([label (make-vector (* 2 hsz) 0)]
          [max -inf.0]
          [tmp #f])
      (do ([i 0 (add1 i)]) ((= i hsz))
        (do ([j 0 (add1 j)]) ((= j hsz))
          (set! tmp (Matrix-ref bGraph i j))
          (when (< max tmp)
            (set! max tmp)))
        (vector-set! label i max)
        (set! max -inf.0))
      label)))


;; to undirected half graph
(define 2uhg
  (lambda (hsz bGraph label)
    (let ([uhg (make-vector hsz '())]
          [max #f])
      (do ([u 0 (add1 u)]) ((= u hsz))
        (do ([v 0 (add1 v)]) ((= v hsz))
          (set! max (+ (vector-ref label u)
                       (vector-ref label (+ v hsz))))
          (when (= max (Matrix-ref bGraph u v))
            (vector-set! uhg u
                         (append (vector-ref uhg u)
                                 (list (+ v hsz)))))))
      uhg)))


;; to directed acyclic graph
(define 2dag
  (lambda (hsz Graph)
    (let ([dag (make-vector (* 2 hsz) '())]
          [fresh (make-vector (* 2 hsz) #t)]
          [es #f])
      (do ([u 0 (add1 u)]) ((= u hsz))
        (set! es (vector-ref Graph u))
        (let f ([lst es])
          (if (null? lst)
            (vector-set! dag u es)
            (let ([v (car lst)])
              (if (vector-ref fresh v)
                (begin
                  (vector-set! fresh v #f)
                  (vector-set! dag u (remove = es v))
                  (vector-set! dag v (list u)))
                (f (cdr lst)))))))
      dag)))


;; to mateched edges
(define init-matched
  (lambda (sz Graph)
    (let ([lt? (lambda (a b) (< (0th a) (0th b)))]
          [M '()]
          [edge #f])
      (do ([v (/ sz 2) (add1 v)]) ((= v sz))
        (set! edge (vector-ref Graph v))
        (when (not (null? edge))
          (set! M
            (insert lt? M (list (car edge) v)))))
      M)))



(define unmatched-left-verx
  (lambda (hsz M)
    (let ([cmp? (lambda (a b) (= a (0th b)))]
          [no-Ml '()])
      (do ([u 0 (add1 u)]) ((= u hsz))
        (when (not (occur? cmp? M u))
          (set! no-Ml (append no-Ml (list u)))))
      no-Ml)))


(define finder
  (lambda (hsz Graph Fl Fr Mr)
    (let ([Q (new-queue)]
          [fresh (make-vector (* 2 hsz) #t)]
          [parent (make-vector (* 2 hsz) -1)])
      (let/cc hop
        (let f ([lst (ref0 Fl)])
          (if (null? lst)
            (list)
            (begin
              (enqueue! Q (car lst))

              (let g ([u (dequeue! Q)]
                      [v #f])
                (do ([es (vector-ref Graph u) (cdr es)]) ((null? es))
                  (set! v (car es))
                  (when (vector-ref fresh v)
                    (vector-set! fresh v #f)
                    (vector-set! parent v u)
                    (cond
                      [(< v hsz)
                       (set0! Fl (union = (ref0 Fl) v))
                       (enqueue! Q v)]
                      [(and (<= hsz v)
                            (occur? = Mr v))
                       (set0! Fr (union = (ref0 Fr) v))
                       (enqueue! Q v)]
                      [(<= hsz v)
                       (hop (build-aug-path parent v))])
                    (when (not (null-queue? Q))
                      (g (dequeue! Q) #f)))))
             
              (f (cdr lst)))))))))


(define find-aug-path!
  (lambda (hsz bGraph Graph label M)
    (let ([Fl (vector (unmatched-left-verx hsz M))]
          [Fr (vector (list))]
          [Mr (map (lambda (edge) (1st edge)) M)])
      (let f ([path (finder hsz Graph Fl Fr Mr)])
        (if (not (null? path))
          path
          (let ([diff (minimum-diff hsz bGraph label
                                    (ref0 Fl) (ref0 Fr))]
                [uhg #f]
                [dag #f])
            (relabel! hsz label diff (ref0 Fl) (ref0 Fr))
            (set! uhg (2uhg hsz bGraph label))
            (set! dag (update-dag hsz uhg M))
            (set! Fl (vector (unmatched-left-verx hsz M)))
            (set! Fr (vector (list)))
            (f (finder hsz dag Fl Fr Mr))))))))


(define hungarian
  (lambda (bGraph)
    (let* ([hsz (Matrix.row bGraph)]
           [label (init-label hsz bGraph)]
           [uhg (2uhg hsz bGraph label)]
           [dag (2dag hsz uhg)]
           [M (init-matched (* 2 hsz) dag)])
      (let f ([P (find-aug-path! hsz bGraph dag label M)])
        (set! M (augmenting M P))
        (if (= hsz (length M))
          M
          (begin
            (set! uhg (2uhg hsz bGraph label))
            (set! dag (update-dag hsz uhg M))
            (f (find-aug-path! hsz bGraph dag label M))))))))



;; test
(define sz-bG 7)

(define bG (new-Matrix sz-bG sz-bG))
(Matrix-row-set! bG 0 '#(4  10 10 10 2  9 3) sz-bG)
(Matrix-row-set! bG 1 '#(6  8  5  12 9  7 2) sz-bG)
(Matrix-row-set! bG 2 '#(11 9  6  7  9  5 15) sz-bG)
(Matrix-row-set! bG 3 '#(3  9  6  7  5  6 3) sz-bG)
(Matrix-row-set! bG 4 '#(2  6  5  3  2  4 2) sz-bG)
(Matrix-row-set! bG 5 '#(10 8  11 4  11 2 11) sz-bG)
(Matrix-row-set! bG 6 '#(3  4  5  4  3  6 8) sz-bG)

;(Matrix-print bG)
;(hungarian bG)


(define sz-bG1 8)

(define bG1 (new-Matrix sz-bG1 sz-bG1))
(Matrix-row-set! bG1 0 '#( 9 11 2 10  1 11  4 10) sz-bG1)
(Matrix-row-set! bG1 1 '#(12 14 5  8  8  9  1  9) sz-bG1)
(Matrix-row-set! bG1 2 '#(13  7 9  1 13  3  2  9) sz-bG1)
(Matrix-row-set! bG1 3 '#( 7  6 2 11  5  9  5  5) sz-bG1)
(Matrix-row-set! bG1 4 '#(10 10 8 10 10  3  6  4) sz-bG1)
(Matrix-row-set! bG1 5 '#( 8 12 3 12  4  7  4  1) sz-bG1)
(Matrix-row-set! bG1 6 '#( 1 11 4  9  7  1 11  6) sz-bG1)
(Matrix-row-set! bG1 7 '#( 2  8 9  5  9 10  4  7) sz-bG1)


;(Matrix-print bG1)
;(hungarian bG1)


(define sz-bG2 9)

(define bG2 (new-Matrix sz-bG2 sz-bG2))
(Matrix-random-fill! bG2 0 20)

(Matrix-print bG2)
(hungarian bG2)