#lang racket


(require "../util.rkt")


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


(define eq-edge?
  (lambda (edge1 edge2)
    (if (= (0th edge1) (0th edge2))
      (= (1st edge1) (1st edge2))
      (if (= (0th edge1) (1st edge2))
        (= (1st edge1) (0th edge2))
        #f))))


(define less?
  (lambda (data1 data2)
    (< (ref0 data1) (ref0 data2))))


(define =?
  (lambda (data1 data2)
    (= (ref0 data1) (ref0 data2))))



;; Edge-with-Weight: #(Weight (u v))
(define edge-exist?
  (lambda (e edges)
    (cond
      [(null? edges) #f]
      [(eq-edge? e (ref1 (car edges))) #t]
      [else
       (edge-exist? e (cdr edges))])))


(define get-edges
  (lambda (Graph)
    (let ([sz (vector-length Graph)]
          [edges '()]
          [e #f])
      (do ([i 0 (add1 i)]) ((= i sz))
        (do ([as (vector-ref Graph i) (cdr as)]) ((null? as))
          (set! e (list i (1st (car as))))
          (when (not (edge-exist? e edges))
            (set! edges (cons (vector (0th (car as)) e) edges)))))
      (list->vector edges))))


(define mst-kruskal
  (lambda (Graph)
    (let ([sz (vector-length Graph)]
          [sets (map (lambda (a) (make-set a))
                     (sequence 0 (vector-length Graph)))]
          [edges (get-edges Graph)]
          [A '()])
      (quick-sort! less? =? edges)
      (do ([i 0 (add1 i)]) ((= i sz))
        (let* ([edge (ref1 (vector-ref edges i))]
               [u-set (find-set sets (0th edge))]
               [v-set (find-set sets (1st edge))])
          (when (not (eq? u-set v-set))
            (set! sets (remove eq? sets u-set))
            (set! sets (remove eq? sets v-set))
            (set! sets (cons (append u-set v-set) sets))
            (set! A (cons edge A)))))
      A)))


;; test
(define un-G
  '#(((4 1) (8 7)) ((4 0) (8 2) (11 7)) ((8 1) (7 3) (4 5) (2 8))
                   ((7 2) (9 4) (14 5)) ((9 3) (10 5))
                   ((4 2) (14 3) (10 4)) ((2 5) (1 7) (6 8))
                   ((8 0) (11 1) (7 8)) ((2 2) (6 6) (7 7))))

(mst-kruskal un-G)