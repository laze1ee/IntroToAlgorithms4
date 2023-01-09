#lang racket


(require "../util.rkt")


(define init-duration
  (lambda (intervals)
    (let* ([len (vector-length intervals)]
           [d (make-vector len -1)]
           [start (ref0 intervals)])
      (set0! d (- (1st start) (0th start)))
      d)))


(define init-successive-intervals
  (lambda (intervals)
    (let* ([len (vector-length intervals)]
           [s (make-vector len '())])
      (set0! s (list (list (ref0 intervals))))
      s)))


(define locate-previous
  (lambda (intervals i)
    (let f ([i (sub1 i)]
            [start (0th (vector-ref intervals i))])
      (cond
        [(= i -1) #f]
        [(<= (1st (vector-ref intervals i)) start) i]
        [else
         (f (sub1 i) start)]))))


(define overlap?
  (lambda (interval1 interval2)
    (> (1st interval1) (0th interval2))))


(define connect
  (lambda (a groups)
    (let f ([ls groups])
      (if (null? ls)
        '()
        (cons (cons a (car ls))
              (f (cdr ls)))))))


(define max-subset!
  (lambda (intervals d s i)
    (let ([a (vector-ref intervals i)]
          [pre-d (vector-ref d (sub1 i))]
          [pre-s (vector-ref s (sub1 i))]
          [cur #f]
          [j #f])
      (set! cur (- (1st a) (0th a)))
      (cond
        [(not (overlap? (caar pre-s) a))
         (vector-set! d i (+ pre-d cur))
         (vector-set! s i (connect a pre-s))]
        [else
         (set! j (locate-previous intervals i))
         (if (boolean? j)
           (cond
             [(< pre-d cur)
              (vector-set! d i cur)
              (vector-set! s i (list (list a)))]
             [(= pre-d cur)
              (vector-set! d i pre-d)
              (vector-set! s i (cons (list a) pre-s))]
             [else
              (vector-set! d i pre-d)
              (vector-set! s i pre-s)])
           (let ([tmp (+ cur (vector-ref d j))])
             (cond
               [(< pre-d tmp)
                (vector-set! d i tmp)
                (vector-set! s i (connect a (vector-ref s j)))]
               [(= pre-d tmp)
                (vector-set! d i pre-d)
                (vector-set! s i
                             (append (connect a (vector-ref s j))
                                     pre-s))]
               [else
                (vector-set! d i pre-d)
                (vector-set! s i pre-s)])))]))))


(define activity-selector
  (lambda (intervals)
    (let ([len (vector-length intervals)]
          [d (init-duration intervals)]
          [s (init-successive-intervals intervals)])
      (do ([i 1 (add1 i)]) ((= i len))
        (max-subset! intervals d s i))
      (list (vector-ref d (sub1 len))
            (vector-ref s (sub1 len))))))


;; test
(define intervals
  '#((1 4) (3 5) (0 6) (5 7) (3 9) (5 9) (6 10) (7 11) (8 12) (2 14) (12 16)))
(activity-selector intervals)