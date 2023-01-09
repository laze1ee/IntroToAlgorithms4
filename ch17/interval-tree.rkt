#lang racket


(require "../util.rkt"
         "node.rkt")

(provide new-RB RB-insert! RB-delete!
         RB-minimum RB-maximum
         RB-exact-search
         RB-walk)


(define new-RB
  (lambda intervals
    (let ([RB (vector (nil-node))])
      (let f ([ls intervals])
        (when (not (null? ls))
          (RB-insert! RB (car ls))
          (f (cdr ls))))
      RB)))


(define insert!
  (lambda (root interval)
    (let f ([node root]
            [path '()])
      (cond
        [(nil-node? node)
         (.interval! node interval)
         (.max! node (ref1 interval))
         (.size! node 1)
         (.color! node @red)
         (.left! node (nil-node))
         (.right! node (nil-node))
         (cons node path)]
        [else
         (when (> (ref1 interval) (.max node))
           (.max! node (ref1 interval)))
         (.size! node (add1 (.size node)))
         (if (< (ref0 interval) (ref0 (.interval node)))
           (f (.left node) (cons node path))
           (f (.right node) (cons node path)))]))))


(define insert-fixup!
  (lambda (RB path)
    (let f ([path path])
      (when (and (< 2 (length path))
                 (eq? @red (.color (1st path))))
        (let ([p (1st path)]
              [u #f]
              [g (2nd path)])
          (cond
            [(left-way? p g)
             (set! u (.right g))
             (cond
               [(eq? @red (.color u))
                (.color! p @black)
                (.color! u @black)
                (.color! g @red)
                (f (cddr path))]
               [else
                (when (right-way? (0th path) p)
                  (left-rotate! (cdr path))
                  (set! p (0th path)))
                (.color! p @black)
                (.color! g @red)
                (when (right-rotate! (cddr path))
                  (.root! RB p))])]
            [else
             (set! u (.left g))
             (cond
               [(eq? @red (.color u))
                (.color! p @black)
                (.color! u @black)
                (.color! g @red)
                (f (cddr path))]
               [else
                (when (left-way? (0th path) p)
                  (right-rotate! (cdr path))
                  (set! p (0th path)))
                (.color! p @black)
                (.color! g @red)
                (when (left-rotate! (cddr path))
                  (.root! RB p))])]))))
    (.color! (.root RB) @black)))


(define RB-insert!
  (lambda (RB interval)
    (insert-fixup! RB
                   (insert! (.root RB) interval))))


(define delete!
  (lambda (path)
    (let* ([z (0th path)]
           [z-max (.max z)]
           [z-color (.color z)]
           [x (.left z)]
           [y (.right z)])
      (cond
        [(nil-node? x)
         (.interval! z (.interval y))
         (.color! z (.color y))
         (.left! z (.left y))
         (.right! z (.right y))
         (max-fixup! path z-max)
         (size-fixup! path)
         (list z-color path)]
        [(nil-node? y)
         (.interval! z (.interval x))
         (.color! z (.color x))
         (.left! z (.left x))
         (.right! z (.right x))
         (max-fixup! path z-max)
         (size-fixup! path)
         (list z-color path)]
        [else
         (let* ([min-path (minimum y)]
                [a (car min-path)])
           (.interval! z (.interval a))
           (max-fixup! path z-max)
           (delete! (append min-path path)))]))))


(define max-fixup!
  (lambda (path z-max)
    (if (null? path)
      (void)
      (let ([z (car path)])
        (when (= z-max (.max z))
          (if (eq? 'nil (.interval z))
            (.max! z -inf.0)
            (.max! z (max (ref1 (.interval z))
                          (.max (.left z))
                          (.max (.right z)))))
          (max-fixup! (cdr path) z-max))))))


(define size-fixup!
  (lambda (path)
    (if (null? path)
      (void)
      (let ([z (car path)])
        (.size! z (sub1 (.size z)))
        (size-fixup! (cdr path))))))


(define delete-fixup!
  (lambda (RB path)
    (let f ()
      (when (and (< 1 (length path))
                 (eq? @black (.color (car path))))
        (let ([x (0th path)]
              [y #f]
              [z (1st path)])
          (cond
            [(left-way? x z)
             (set! y (.right z))
             ;;case 1
             (when (eq? @red (.color y))
               (.color! z @red)
               (.color! y @black)
               (when (left-rotate! (cdr path))
                 (.root! RB y))
               (set! path (cddr path))
               (set! path (cons x (cons z (cons y path))))
               (set! y (.right z)))
             (cond
               ;; case 2
               [(and (eq? @black (.color (.left y)))
                     (eq? @black (.color (.right y))))
                (.color! y @red)
                (set! path (cdr path))
                (f)]
               [else
                ;; case 3
                (when (eq? @black (.color (.right y)))
                  (.color! (.left y) @black)
                  (.color! y @red)
                  (right-rotate! (cons y (cdr path)))
                  (set! y (.right z)))
                ;; case 4
                (.color! y (.color z))
                (.color! z @black)
                (.color! (.right y) @black)
                (when (left-rotate! (cdr path))
                  (.root! RB y))])]
            [else
             (set! y (.left z))
             ;;case 1
             (when (eq? @red (.color y))
               (.color! z @red)
               (.color! y @black)
               (when (right-rotate! (cdr path))
                 (.root! RB y))
               (set! path (cddr path))
               (set! path (cons x (cons z (cons y path))))
               (set! y (.left z)))
             (cond
               ;; case 2
               [(and (eq? @black (.color (.left y)))
                     (eq? @black (.color (.right y))))
                (.color! y @red)
                (set! path (cdr path))
                (f)]
               [else
                ;; case 3
                (when (eq? @black (.color (.left y)))
                  (.color! (.right y) @black)
                  (.color! y @red)
                  (left-rotate! (cons y (cdr path)))
                  (set! y (.left z)))
                ;; case 4
                (.color! y (.color z))
                (.color! z @black)
                (.color! (.left y) @black)
                (when (right-rotate! (cdr path))
                  (.root! RB y))])]))))
    (.color! (car path) @black)))


(define RB-delete!
  (lambda (RB interval)
    (let ([result (exact-search (.root RB) interval)])
      (cond
        [(boolean? result)
         (error 'RB-delete!
                "not found interval ~s in trees" interval)]
        [else
         (set! result (delete! result))
         (when (eq? @black (0th result))
           (delete-fixup! RB (1st result)))]))))


(define RB-minimum
  (lambda (RB)
    (.interval (car (minimum (.root RB))))))


(define RB-maximum
  (lambda (RB)
    (.interval (car (maximum (.root RB))))))


(define RB-exact-search
  (lambda (RB interval)
    (let ([result (exact-search (.root RB) interval)])
      (if (boolean? result)
        #f
        (car result)))))


(define RB-walk
  (lambda (RB)
    (let ([store '()])
      (let f ([node (.root RB)])
        (when (not (nil-node? node))
          (f (.right node))
          (set! store (cons (.interval node) store))
          (f (.left node))))
      store)))


#|
;; test
(define A (new-RB '#(30 34) '#(21 83) '#(51 86)
                  '#(1 58) '#(25 85)))
(.color! (car (exact-search (.root A) '#(21 83))) @red)
(.color! (car (exact-search (.root A) '#(1 58))) @black)
(.color! (car (exact-search (.root A) '#(25 85))) @black)
(.root A)
(RB-delete! A '#(30 34))
;(define path (1st (delete! (list (.root A)))))
|#