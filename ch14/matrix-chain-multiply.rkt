#lang racket


(require "../util.rkt")


(define make-table
  (lambda (n init)
    (let* ([table (make-vector n 0)])
      (do ([i 0 (add1 i)]) ((= i n))
        (vector-set! table i
                     (make-vector (add1 i) init)))
      table)))


(define table-ref
  (lambda (table start bound)
    (vector-ref (vector-ref table (sub1 bound)) start)))


(define table-set!
  (lambda (table start bound val)
    (vector-set! (vector-ref table (sub1 bound)) start val)))



(define sum-step (lambda (m1 m2) (* (0th m1) (1st m2) (1st m1))))
(define new-pair (lambda (m1 m2) (list (0th m1) (1st m2))))


;; Memorized part minimum infomation
;; #(steps (row col) part-index)
(define init-tail!
  (lambda (chain minimum-info)
    (let ([len (vector-length minimum-info)])
      (do ([i 0 (add1 i)]) ((= i len))
        (table-set! minimum-info i (add1 i)
                    (vector 0 (vector-ref chain i) 'nil))))))


(define optimize!
  (lambda (minimum-info)
    (let f ([start 0]
            [bound (vector-length minimum-info)])
      (let ([check (table-ref minimum-info start bound)])
        (if (not (eq? 'nil check))
          check
          (let ([min-index start]
                [min-steps +inf.0]
                [left #f]
                [right #f]
                [tmp #f])
            (do ([i (add1 start) (add1 i)]) ((= i bound))
              (set! left (f start i))
              (set! right (f i bound))
              (set! tmp (+ (ref0 left) (ref0 right)
                           (sum-step (ref1 left) (ref1 right))))
              (when (> min-steps tmp)
                (set! min-index i)
                (set! min-steps tmp)))
            (table-set! minimum-info start bound
                        (vector min-steps
                                (new-pair (ref1 left) (ref1 right))
                                min-index))
            (table-ref minimum-info start bound)))))))


(define optimal-solution
  (lambda (minimum-info)
    (let ([len (vector-length minimum-info)]
          [recur #f])
      (set! recur
        (lambda (start bound)
          (cond
            [(= 1 (- bound start))
             start]
            [(= 2 (- bound start))
             (list start (sub1 bound))]
            [else
             (let ([mid (ref2 (table-ref minimum-info start bound))])
               (list (recur start mid)
                     (recur mid bound)))])))
      (list (ref0 (table-ref minimum-info 0 len))
            (recur 0 len)))))
        


(define optimize-chain
  (lambda (chain)
    (let ([minimum-info (make-table (vector-length chain) 'nil)])
      (init-tail! chain minimum-info)
      (optimize! minimum-info)
      (optimal-solution minimum-info))))


;; ==========
;; ** test **
;; ==========
(define random-list
  (lambda (len)
    (if (= len 0)
      '()
      (cons (random-integer 5 100)
            (random-list (sub1 len))))))


(define matrix-chain
  (lambda (ls)
    (let* ([len (length ls)]
           [chain (make-vector (sub1 len) 0)])
      (do ([i 0 (add1 i)]
           [ls ls (cdr ls)]) ((null? (cdr ls)))
        (vector-set! chain i
                     (list (0th ls) (1st ls))))
      chain)))


(define order-chain
  (lambda (chain)
    (let ([len (vector-length chain)]
          [new (new-pair (vector-ref chain 0)
                         (vector-ref chain 1))]
          [sum (sum-step (vector-ref chain 0)
                         (vector-ref chain 1))])
      (do ([i 2 (add1 i)]) ((= i len))
        (set! sum
          (+ sum (sum-step new (vector-ref chain i))))
        (set! new
          (new-pair new (vector-ref chain i))))
      sum)))


(define reverse-chain
  (lambda (chain)
    (let* ([len (sub1 (vector-length chain))]
           [new (new-pair (vector-ref chain (sub1 len))
                          (vector-ref chain len))]
           [sum (sum-step (vector-ref chain (sub1 len))
                          (vector-ref chain len))])
      (do ([i (- len 2) (sub1 i)]) ((= i -1))
        (set! sum
          (+ sum (sum-step (vector-ref chain i) new)))
        (set! new
          (new-pair (vector-ref chain i) new)))
      sum)))


(define chain (matrix-chain (random-list 9)))
chain
(order-chain chain)
(reverse-chain chain)
(optimize-chain chain)