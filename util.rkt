#lang racket


(require math/base math/number-theory)

(provide random-integer random-prime prev-prime next-prime
         0th 1st 2nd 3rd 4th
         sequence
         find find* occur? insert insert-by-index
         remove remove* remove-by-index
         replace replace* replace-by-index list-head
         ref0 ref1 ref2 ref3 ref4 ref5
         set0! set1! set2! set3! set4! set5!
         exchange! random-fill! random-vector vector-min/max
         middle insert-sort! quick-sort! copy)



;; ==============
;; list utilities

(define 0th (lambda (lst) (list-ref lst 0)))
(define 1st (lambda (lst) (list-ref lst 1)))
(define 2nd (lambda (lst) (list-ref lst 2)))
(define 3rd (lambda (lst) (list-ref lst 3)))
(define 4th (lambda (lst) (list-ref lst 4)))


(define sequence
  (case-lambda
    [(start bound step)
     (let ([cmp (if (< 0 step)
                  (lambda (i) (>= i bound))
                  (lambda (i) (<= i bound)))])
       (let f ([start start])
         (if (cmp start)
           '()
           (cons start (f (+ start step))))))]
    [(start bonud)
     (let f ([start start])
       (if (>= start bonud)
         '()
         (cons start (f (add1 start)))))]))


;; return: index
;;       : #f
(define find
  (lambda (suit? lst a)
    (let f ([lst lst]
            [i 0])
      (cond
        [(null? lst) #f]
        [(suit? a (car lst)) i]
        [else
         (f (cdr lst) (add1 i))]))))


;; return: (List-of-Index)
(define find*
  (lambda (suit? lst a)
    (let f ([ls lst]
            [i 0])
      (cond
        [(null? ls) '()]
        [(suit? a (car ls))
         (cons i (f (cdr ls) (add1 i)))]
        [else
         (f (cdr ls) (add1 i))]))))


(define occur?
  (lambda (suit? lst a)
    (let ([index (find suit? lst a)])
      (if (boolean? index)
        #f
        #t))))


(define insert
  (lambda (suit? lst a)
    (let f ([lst lst])
      (cond
        [(null? lst) (list a)]
        [(suit? a (car lst))
         (cons a lst)]
        [else
         (cons (car lst) (f (cdr lst)))]))))


(define insert-by-index
  (lambda (lst a index)
    (let f ([ls lst]
            [i index])
      (cond
        [(and (null? ls) (> i 0))
         (error 'insert
                "insert before the index ~s is out of range [~s ~s]"
                index 0 (length lst))]
        [(= i 0) (cons a ls)]
        [else
         (cons (car ls) (f (cdr ls) (sub1 i)))]))))


(define remove
  (lambda (suit? lst a)
    (cond
      [(null? lst) '()]
      [(suit? a (car lst)) (cdr lst)]
      [else
       (cons (car lst) (remove suit? (cdr lst) a))])))


(define remove*
  (lambda (suit? lst a)
    (cond
      [(null? lst) '()]
      [(suit? a (car lst))
       (remove* (cdr lst) a)]
      [else
       (cons (car lst) (remove* suit? (cdr lst) a))])))


(define remove-by-index
  (lambda (lst index)
    (let f ([ls lst]
            [i index])
      (cond
        [(and (null? ls) (>= i 0))
         (error 'remove-by-index
                "index ~s is out of range for list ~s" index lst)]
        [(= i 0) (cdr ls)]
        [else
         (cons (car ls) (f (cdr ls) (sub1 i)))]))))


(define replace
  (lambda (suit? lst new old)
    (cond
      [(null? lst) '()]
      [(suit? old (car lst))
       (cons new (cdr lst))]
      [else
       (cons (car lst) (replace suit? (cdr lst) new old))])))


(define replace*
  (lambda (suit? lst new old)
    (cond
      [(null? lst) '()]
      [(suit? old (car lst))
       (cons new (replace* (cdr lst) new old))]
      [else
       (cons (car lst) (replace* suit? (cdr lst) new old))])))


(define replace-by-index
  (lambda (lst index new)
    (let f ([ls lst]
            [i index])
      (cond
        [(and (null? ls) (>= i 0))
         (error 'replace-by-index
                "index ~s is out of range for list ~s" index lst)]
        [(= i 0) (cons new (cdr ls))]
        [else
         (cons (car ls) (f (cdr ls) (sub1 i)))]))))


(define list-head
  (lambda (ls index)
    (if (and (natural? index)
             (<= index (length ls)))
      (let f ([ls ls] [i 0])
        (cond
          [(= i index) '()]
          [else
           (cons (car ls) (f (cdr ls) (add1 i)))]))
      (error 'list-head "invalid index ~s for list ~s" index ls))))
;; ==============



;; ================
;; vector utilities

(define ref0 (lambda (v) (vector-ref v 0)))
(define ref1 (lambda (v) (vector-ref v 1)))
(define ref2 (lambda (v) (vector-ref v 2)))
(define ref3 (lambda (v) (vector-ref v 3)))
(define ref4 (lambda (v) (vector-ref v 4)))
(define ref5 (lambda (v) (vector-ref v 5)))


(define set0! (lambda (v x) (vector-set! v 0 x)))
(define set1! (lambda (v x) (vector-set! v 1 x)))
(define set2! (lambda (v x) (vector-set! v 2 x)))
(define set3! (lambda (v x) (vector-set! v 3 x)))
(define set4! (lambda (v x) (vector-set! v 4 x)))
(define set5! (lambda (v x) (vector-set! v 5 x)))


(define exchange!
  (lambda (v i j)
    (when (not (= i j))
      (let ([tmp (vector-ref v i)])
        (vector-set! v i (vector-ref v j))
        (vector-set! v j tmp)))))


(define random-fill!
  (lambda (v start bound)
    (let ([len (vector-length v)])
      (do ([i 0 (add1 i)]) ((= i len))
        (vector-set! v i
                     (random-integer start bound))))))


(define diff-number
  (lambda (n start bound)
    (cond
      [(or (< n start)
           (<= bound n))
       (error 'diff-number
              "number ~s not in range [~s ~s)" n start bound)]
      [(> 2 (- bound start))
       (error 'diff-number
              "range must greater than 1~% given: [~s ~s)"
              start bound)]
      [else
       (let f ([m (random-integer start bound)])
         (if (= n m)
           (f (random-integer start bound))
           m))])))


(define random-vector
  (lambda (start bound)
    (let* ([sz (- bound start)]
           [v (make-vector sz 0)])
      (do ([i 0 (add1 i)]) ((= i sz))
        (vector-set! v i (+ i start)))
      (do ([i 0 (add1 i)]) ((= i sz))
        (exchange! v i (diff-number i 0 sz)))
      v)))


(define vector-min/max
  (lambda (cmp v)
    (let ([sz (vector-length v)])
      (let f ([r 0]
              [i 1])
        (cond
          [(= i sz) r]
          [(cmp (vector-ref v i) (vector-ref v r))
           (f i (add1 i))]
          [else
           (f r (add1 i))])))))
;; ================



;; ===============
;; other utilities

(define middle
  (lambda (i j)
    (floor (/ (+ i j) 2))))


(define insert-sort!
  (lambda (suit? v)
    (let ([sz (vector-length v)])
      (do ([i 1 (add1 i)]) ((= i sz))
        (let f ([j i])
          (when (and (< 0 j)
                     (suit? (vector-ref v j)
                            (vector-ref v (sub1 j))))
            (exchange! v j (sub1 j))
            (f (sub1 j))))))))


(define part!
  (lambda (order? =? v l r)
    (exchange! v (random-integer l r) (sub1 r))
    (set! r (sub1 r))
    (let ([pivot (vector-ref v r)]
          [i l]
          [j l])
      (do ([k l (add1 k)]) ((= k r))
        (cond
          [(order? (vector-ref v k) pivot)
           (exchange! v i k)
           (when (< i j)
             (exchange! v j k))
           (set! i (add1 i))
           (set! j (add1 j))]
          [(=? (vector-ref v k) pivot)
           (exchange! v j k)
           (set! j (add1 j))]))
      (exchange! v j r)
      (list i j))))


(define quick-sort!
  (lambda (order? =? v)
    (let sort! ([l 0]
                [r (vector-length v)])
      (when (< 1 (- r l))
        (let ([p (part! order? =? v l r)])
          (sort! l (0th p))
          (sort! (add1 (1st p)) r))))))


(define lst-copy
  (lambda (lst)
    (cond
      [(null? lst) '()]
      [(list? (car lst))
       (cons (lst-copy (car lst))
             (lst-copy (cdr lst)))]
      [(vector? (car lst))
       (cons (vct-copy (car lst))
             (lst-copy (cdr lst)))]
      [else
       (cons (car lst) (lst-copy (cdr lst)))])))


(define vct-copy
  (lambda (vct)
    (let* ([sz (vector-length vct)]
           [new (make-vector sz)]
           [any #f])
      (do ([i 0 (add1 i)]) ((= i sz))
        (set! any (vector-ref vct i))
        (cond
          [(list? any)
           (vector-set! new i
                        (lst-copy any))]
          [(vector? any)
           (vector-set! new i
                        (vct-copy any))]
          [else
           (vector-set! new i any)]))
      new)))


(define copy
  (lambda (any)
    (cond
      [(list? any)
       (lst-copy any)]
      [(vector? any)
       (vct-copy any)]
      [else
       (error 'copy
              "only copy list or vector")])))
;; ===============


#|
;; test
(define data (make-vector 10))
(random-fill! data 0 100)
data
(vector-min/max > data)
|#