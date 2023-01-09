#lang racket


(require "../util.rkt")



(define 2dag
  (lambda (sz bound Graph M)
    (let* ([Gm (make-vector sz '())])
      (do ([i 0 (add1 i)]) ((= i bound))
        (vector-set!
         Gm i
         (rm-matched i (vector-ref Graph i) M)))
      (do ([es M (cdr es)]) ((null? es))
        (vector-set! Gm (1st (car es))
                     (list (0th (car es)))))
      Gm)))


(define rm-matched
  (lambda (s ends M)
    (cond
      [(or (null? M)
           (< s (0th (car M))))
       ends]
      [(= s (0th (car M)))
       (remove = ends (1st (car M)))]
      [else
       (rm-matched s ends (cdr M))])))


(define init-matched
  (lambda (sz bound Graph)
    (let ([fresh (make-vector sz #t)]
          [M '()]
          [v #f])
      (do ([u 0 (add1 u)]) ((= u bound))
        (let f ([es (vector-ref Graph u)])
          (when (not (null? es))
            (set! v (car es))
            (if (vector-ref fresh v)
              (begin
               (set! M (append M (list (list u v))))
               (vector-set! fresh v #f))
              (f (cdr es))))))
      M)))


(define get-srcs
  (lambda (bound L)
    (let ([srcs '()])
      (do ([s 0 (add1 s)]) ((= s bound))
        (when (not (occur? = L s))
          (set! srcs (append srcs (list s)))))
      srcs)))



(define m-aug-path?
  (lambda (L R path)
    (let f ([path path])
      (cond
        [(= 3 (length path))
         (and (occur? = R (car path))
              (f (cdr path)))]
        [(= 2 (length path))
         (and (occur? = L (car path))
              (f (cdr path)))]
        [(= 1 (length path))
         (not (occur? = R (car path)))]
        [else #f]))))


(define find-path
  (lambda (Graph L R s)
    (let ([path #f])
      (let f ([u s]
              [size 0])
        (let g ([es (vector-ref Graph u)])
          (if (null? es)
            '()
            (let ([v (car es)])
              (cond
                [(= 2 size)
                 (if (m-aug-path? L R (list v))
                   (list u v)
                   (g (cdr es)))]
                [else
                 (set! path
                   (f v (add1 size)))
                 (if (m-aug-path? L R path)
                   (cons u path)
                   (g (cdr es)))]))))))))


(define m-aug-set
  (lambda (path)
    (list (list (0th path) (1st path))
          (list (2nd path) (1st path))
          (list (2nd path) (3rd path)))))


(define find-m-aug-paths
  (lambda (sz bound Graph M)
    (let* ([dag (2dag sz bound Graph M)]
           [L (map (lambda (edge) (0th edge)) M)]
           [R (map (lambda (edge) (1st edge)) M)]
           [srcs (get-srcs bound L)]
           [path #f])
      (if (null? srcs)
        '()
        (let f ([lst srcs])
          (cond
            [(null? lst) '()]
            [else
             (set! path (find-path dag L R (car lst)))
             (if (null? path)
               (f (cdr lst))
               (m-aug-set path))]))))))


(define sub
  (lambda (M edge)
    (cond
      [(null? M) '()]
      [(equal? edge (car M))
       (cdr M)]
      [else
       (cons (car M) (sub (cdr M) edge))])))


(define add
  (lambda (M edge)
    (cond
      [(null? M) (list edge)]
      [(< (0th edge) (0th (car M)))
       (cons edge M)]
      [else
       (cons (car M) (add (cdr M) edge))])))


(define hopcroft-karp
  (lambda (bGraph)
    (let* ([sz (ref0 bGraph)]
           [bound (vector-length (ref1 bGraph))]
           [Graph (ref1 bGraph)]
           [M (init-matched sz bound Graph)])
      (let f ([path (find-m-aug-paths sz bound Graph M)])
        (if (null? path)
          M
          (begin
           (set! M (sub M (1st path)))
           (set! M (add M (0th path)))
           (set! M (add M (2nd path)))
           (f (find-m-aug-paths sz bound Graph M)))))
      M)))



;; test
;; Bipartite Graph: #(Size Directed-Graph-of-Left-Part)
(define bGraph
  '#(15
     #((8 9) (7 8) (7 9 10 11 12) (8 9 13) (10 11 12 13) (9 13) (11 14))))

(hopcroft-karp bGraph)