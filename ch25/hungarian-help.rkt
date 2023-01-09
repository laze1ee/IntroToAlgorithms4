#lang racket


(require "../util.rkt"
         "../matrix.rkt")

(provide union edge build-aug-path
         augmenting update-dag minimum-diff relabel!)



(define union
  (lambda (pred S a)
    (let f ([lst S])
      (cond
        [(null? lst) (list a)]
        [(pred a (car lst)) S]
        [else
         (cons (car lst) (f (cdr lst)))]))))


(define edge
  (lambda (u v)
    (if (< u v) (list u v) (list v u))))


(define m-aug-path
  (lambda (parent v)
    (let f ([u (vector-ref parent v)]
            [path (list v)])
      (if (= -1 u)
        path
        (f (vector-ref parent u)
           (cons u path))))))


(define lt? (lambda (a b) (< (0th a) (0th b))))


(define m-aug-edge
  (lambda (path)
    (let f ([u (car path)]
            [lst (cdr path)]
            [M '()])
      (if (null? lst)
        M
        (f (car lst)
           (cdr lst)
           (insert lt? M (edge u (car lst))))))))


(define build-aug-path
  (lambda (parent v)
    (m-aug-edge
     (m-aug-path parent v))))



(define add&sub
  (lambda (M edge)
    (let f ([lst M])
      (cond
        [(null? lst) (list edge)]
        [(equal? edge (car lst)) (cdr lst)]
        [else
         (cons (car lst) (f (cdr lst)))]))))


(define edge-sort
  (lambda (E)
    (let ([E1 '()])
      (do ([lst E (cdr lst)]) ((null? lst))
        (set! E1 (insert lt? E1 (car lst))))
      E1)))


(define augmenting
  (lambda (M P)
    (do ([lst P (cdr lst)]) ((null? lst))
      (set! M (add&sub M (car lst))))
    (edge-sort M)))


(define update-dag
  (lambda (hsz Graph M)
    (let ([dag (make-vector (* 2 hsz) '())]
          [fresh (make-vector hsz #t)]
          [u #f]
          [v #f])
      (do ([lst M (cdr lst)]) ((null? lst))
        (set! u (0th (car lst)))
        (set! v (1st (car lst)))
        (vector-set! dag v (list u))
        (vector-set! dag u
                     (remove = (vector-ref Graph u) v))
        (vector-set! fresh u #f))
      (do ([u 0 (add1 u)]) ((= u hsz))
        (when (vector-ref fresh u)
          (vector-set! dag u (vector-ref Graph u))))
      dag)))



(define not-forest-right-verx
  (lambda (hsz Fr)
    (let ([sz (* 2 hsz)]
          [no-Fr '()])
      (do ([u hsz (add1 u)]) ((= u sz))
        (when (not (occur? = Fr u))
          (set! no-Fr (append no-Fr (list u)))))
      no-Fr)))


(define minimum-diff
  (lambda (hsz bGraph label Fl Fr)
    (let ([no-Fr (not-forest-right-verx hsz Fr)]
          [min +inf.0]
          [u #f]
          [v #f]
          [tmp #f])
      (do ([L Fl (cdr L)]) ((null? L))
        (set! u (car L))
        (do ([R no-Fr (cdr R)]) ((null? R))
          (set! v (car R))
          (set! tmp (- (+ (vector-ref label u) (vector-ref label v))
                       (Matrix-ref bGraph u (- v hsz))))
          (when (< tmp min)
            (set! min tmp))))
      min)))


(define relabel!
  (lambda (hsz label diff Fl Fr)
    (let ([u #f])
      (do ([L Fl (cdr L)]) ((null? L))
        (set! u (car L))
        (vector-set! label u
                     (- (vector-ref label u) diff)))
      (do ([R Fr (cdr R)]) ((null? R))
        (set! u (car R))
        (vector-set! label u
                     (+ (vector-ref label u)
                        diff))))))