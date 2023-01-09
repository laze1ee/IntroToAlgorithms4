#lang racket


(require "../matrix.rkt")



(define naive-string-matcher
  (lambda (text pattern)
    (let ([n (string-length text)]
          [m (string-length pattern)]
          [matcher #f])
      (set! matcher
        (lambda (i shift)
          (cond
            [(= i (- n m))
             (reverse shift)]
            [(string=? (substring text i (+ i m))
                       pattern)
             (matcher (add1 i) (cons i shift))]
            [else
             (matcher (add1 i) shift)])))
      (matcher 0 '()))))


(define rabin-karp-matcher
  (lambda (text pattern d q)
    (let ([n (string-length text)]
          [m (string-length pattern)]
          [len 0]
          [h 0]
          [p 0]
          [t0 0]
          [char 0]
          [matcher (lambda () (void))])
      (set! len (- n m))
      (set! h (expt d (sub1 m)))
      (do ([i 0 (add1 i)]) ((= i m))
        (set! char (char->integer (string-ref pattern i)))
        (set! p    (modulo (+ (* p d) char) q))
        (set! char (char->integer (string-ref text i)))
        (set! t0   (modulo (+ (* t0 d) char) q)))
      (set! matcher
        (lambda (i x shift)
          (cond
            [(> i len)
             (reverse shift)]
            [else
             (when (< i len)
               (set! n    (char->integer (string-ref text i)))
               (set! n    (- x (* n h)))
               (set! char (char->integer (string-ref text (+ i m))))
               (set! t0   (modulo (+ (* n d) char) q)))
             (if (and (= x p)
                      (string=? (substring text i (+ i m)) pattern))
               (matcher (add1 i) t0 (cons i shift))
               (matcher (add1 i) t0 shift))])))
      (matcher 0 t0 '()))))


(define suffix?
  (lambda (x y)
    (let ([len-x (string-length x)]
          [len-y (string-length y)]
          [i 0])
      (cond
        [(= 0 len-x) #t]
        [(> len-x len-y) #f]
        [else
         (set! i (- len-y len-x))
         (string=? x (substring y i))]))))


(define transit
  (lambda (pattern charset)
    (let ([m (string-length pattern)]
          [n (string-length charset)]
          [x (new-Matrix 0 0)]
          [k 0]
          [w ""])
      
      (set! x (new-Matrix (add1 m) n 0))
      
      (do ([q 0 (add1 q)]) ((> q m))
        (do ([i 0 (add1 i)]) ((= i n))
          (set! k (min m (add1 q)))
          (set! w (string-append (substring pattern 0 q)
                                 (string (string-ref charset i))))
          (let f ()
            (cond
              [(suffix? (substring pattern 0 k) w)
               (Matrix-set! x q i k)]
              [else
               (set! k (sub1 k))
               (f)]))))
      x)))


(define transit-ascii
  (lambda (pattern)
    (let ([m (string-length pattern)]
          [n 128]
          [x (new-Matrix 0 0)]
          [k 0]
          [w ""])
      
      (set! x (new-Matrix (add1 m) n 0))
      
      (do ([q 0 (add1 q)]) ((> q m))
        (do ([i 0 (add1 i)]) ((= i n))
          (set! k (min m (add1 q)))
          (set! w (string-append (substring pattern 0 q)
                                 (string (integer->char i))))
          (let f ()
            (cond
              [(suffix? (substring pattern 0 k) w)
               (Matrix-set! x q i k)]
              [else
               (set! k (sub1 k))
               (f)]))))
      x)))


(define finite-automaton-matcher
  (lambda (text ascii-transition)
    (let ([m (sub1 (Matrix.row ascii-transition))]
          [n (string-length text)]
          [char #\u0]
          [q 0])
      (let f ([i 0] [shift '()])
        (cond
          [(= i n)
           (reverse shift)]
          [else
           (set! char (string-ref text i))
           (set! q (Matrix-ref ascii-transition q (char->integer char)))
           (if (= q m)
             (f (add1 i) (cons (add1 (- i m)) shift))
             (f (add1 i) shift))])))))


(define compute-prefix
  (lambda (pattern)
    (let ([m (string-length pattern)]
          [prefix (make-vector 0)]
          [i -1]
          [jchar #\u0])
      (set! prefix (make-vector m -1))
      (do ([j 1 (add1 j)]) ((= j m))
        (set! jchar (string-ref pattern j))
        (let f ()
          (when (and (> i -1)
                     (not (eq? jchar (string-ref pattern (add1 i)))))
            (set! i (vector-ref prefix i))
            (f)))
        (when (eq? jchar (string-ref pattern (add1 i)))
          (set! i (add1 i))
          (vector-set! prefix j i)))
      prefix)))


(define kmp-matcher
  (lambda (text pattern)
    (let ([n (string-length text)]
          [m (string-length pattern)]
          [prefix (compute-prefix pattern)]
          [i -1]
          [jchar #\u0]
          [shift '()])
      (do ([j 0 (add1 j)]) ((= j n))
        (set! jchar (string-ref text j))
        (let f ()
          (when (and (> i -1)
                     (not (eq? jchar (string-ref pattern (add1 i)))))
            (set! i (vector-ref prefix i))
            (f)))
        (when (eq? jchar (string-ref pattern (add1 i)))
          (set! i (add1 i)))
        (when (= (add1 i) m)
          (set! i (vector-ref prefix i))
          (set! shift (cons (add1 (- j m)) shift))))
      (reverse shift))))



;; test
(define text
  "The correct attitude and scientific method can achieve any purpose.")
(define pattern "ie")

(naive-string-matcher text pattern)
(rabin-karp-matcher text pattern 128 97)

(define x (transit-ascii pattern))
(finite-automaton-matcher text x)

(kmp-matcher text pattern)