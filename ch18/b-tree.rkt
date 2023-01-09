#lang racket


(require "../util.rkt"
         "node.rkt")


(define new-BT
  (case-lambda
    [(degree keys)
     (let ([BT (vector degree (nil-node))])
       (let f ([keys keys])
         (when (not (null? keys))
           (BT-insert! BT (car keys))
           (f (cdr keys))))
       BT)]
    [(degree)
     (vector degree (nil-node))]))


(define BT-insert!
  (lambda (BT key)
    (let ([d (sub1 (* 2 (.degree BT)))])
      (let f ([node (.root BT)]
              [path '()])
        (cond
          [(leaf-node? node)
           (.keys! node (insert-by-order (.keys node) key))
           (.size! node (add1 (.size node)))
           (when (= d (.size node))
             (rise! BT node path))]
          [else
           (let ([i (locate (.keys node) key)])
             (f (list-ref (.refs node) i)
                (cons i (cons node path))))])))))


(define delete!
  (lambda (BT path)
    (let ([i (0th path)]
          [target (1st path)])
      (cond
        [(leaf-node? target)
         (.keys! target (remove-by-index (.keys target) i))
         (.size! target (sub1 (.size target)))
         (fix-up! BT path)]
        [else
         (let ([lnode (list-ref (.refs target) i)]
               [rnode (list-ref (.refs target) (add1 i))])
           (if (> (.size lnode) (.size rnode))
             (set! path
               (append (maximum lnode) path))
             (begin
              (set! path (cons (add1 i) (cdr path)))
              (set! path
                (append (minimum rnode) path))))
           (.keys! target
                   (replace-by-index
                    (.keys target)
                    i
                    (list-ref (.keys (1st path)) (0th path))))
           (delete! BT path))]))))


(define BT-delete!
  (lambda (BT key)
    (let ([path (search (.root BT) key)])
      (if (boolean? path)
        (error 'BT-delete!
               "not found key ~s in B-Tree ~s" key BT)
        (delete! BT path)))))


(define BT-search
  (lambda (BT key)
    (let ([result (search (.root BT) key)])
      (if (boolean? result)
        #f
        (list-head result 2)))))


(define BT-minimum
  (lambda (BT)
    (let ([path (minimum (.root BT))])
      (list-ref (.keys (1st path)) (0th path)))))


(define BT-maximum
  (lambda (BT)
    (let ([path (maximum (.root BT))])
      (list-ref (.keys (1st path)) (0th path)))))



;; test
(define A (new-BT 3))

(define keys '())

(do ([i 0 (add1 i)]) ((= i 40))
  (let ([n (random-integer 0 100)])
    (set! keys (cons n keys))
    (BT-insert! A n)))

(let f ([ls keys])
  (when (not (null? ls))
    (BT-delete! A (car ls))
    (f (cdr ls))))

A