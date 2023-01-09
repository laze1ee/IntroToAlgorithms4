#lang racket


(require "../util.rkt")

(provide .degree .root .root!
         .size .size! .keys .keys! .refs .refs!
         nil-node leaf-node leaf-node?
         locate insert-by-order insert-before split rise!
         search minimum maximum
         left-shift! right-shift! merge! fix-up!)


;; B-Tree: '#(Minimum-Degree Root-Node)
;; Node: '#(Size List-of-Keys List-of-Nodes)
;;     : '#(Size List-of-Keys 'leaf)
(define .degree (lambda (BT) (ref0 BT)))
(define .root   (lambda (BT) (ref1 BT)))
(define .root!  (lambda (BT node) (set1! BT node)))

(define .size   (lambda (node) (ref0 node)))
(define .size!  (lambda (node n) (set0! node n)))
(define .keys   (lambda (node) (ref1 node)))
(define .keys!  (lambda (node keys) (set1! node keys)))
(define .refs   (lambda (node) (ref2 node)))
(define .refs!  (lambda (node subs) (set2! node subs)))


(define nil-node (lambda () (vector 0 '() 'leaf)))

(define leaf-node
  (lambda (size keys)
    (vector size keys 'leaf)))

(define leaf-node?
  (lambda (node)
    (eq? 'leaf (ref2 node))))


(define locate
  (lambda (ls key)
    (let f ([ls ls]
            [i 0])
      (cond
        [(null? ls) i]
        [(<= key (car ls)) i]
        [else
         (f (cdr ls) (add1 i))]))))


(define insert-by-order
  (lambda (ls key)
    (cond
      [(null? ls) (list key)]
      [(< key (car ls))
       (cons key ls)]
      [else
       (cons (car ls)
             (insert-by-order (cdr ls) key))])))


(define insert-before
  (lambda (lists index key)
    (let f ([ls lists]
            [i index])
      (cond
        [(and (null? ls) (> i 0))
         (error 'insert-before
                "index ~s is out of range for lists ~s" index lists)]
        [(= i 0) (cons key ls)]
        [else
         (cons (car ls) (f (cdr ls) (sub1 i)))]))))


(define split
  (lambda (node)
    (let* ([i (floor (/ (.size node) 2))]
           [mid (list-ref (.keys node) i)])
      (if (leaf-node? node)
        (list mid
              (leaf-node i
                         (list-head (.keys node) i))
              (leaf-node i
                         (list-tail (.keys node) (add1 i))))
        (list mid
              (vector i
                      (list-head (.keys node) i)
                      (list-head (.refs node) (add1 i)))
              (vector i
                      (list-tail (.keys node) (add1 i))
                      (list-tail (.refs node) (add1 i))))))))


(define rise!
  (lambda (BT node path)
    (let ([d (.size node)]
          [part (split node)])
      (if (null? path)
        (let ([root (nil-node)])
          (.keys! root (list-head part 1))
          (.refs! root (list-tail part 1))
          (.size! root 1)
          (.root! BT root))
        (let* ([i (0th path)]
               [parent (1st path)])
          (.keys! parent
                  (insert-before (.keys parent) i (0th part)))
          (.refs! parent
                  (replace-by-index (.refs parent) i (1st part)))
          (.refs! parent
                  (insert-before (.refs parent) (add1 i) (2nd part)))
          (.size! parent (add1 (.size parent)))
          (when (= d (.size parent))
            (rise! BT parent (cddr path))))))))


(define search
  (lambda (node key)
    (let f ([node node]
            [size (.size node)]
            [i (locate (.keys node) key)]
            [path '()])
      (cond
        [(and (< i size)
              (= key (list-ref (.keys node) i)))
         (cons i (cons node path))]
        [(leaf-node? node) #f]
        [else
         (let ([next (list-ref (.refs node) i)])
           (f next
              (.size next)
              (locate (.keys next) key)
              (cons i (cons node path))))]))))


(define minimum
  (lambda (node)
    (let f ([node node]
            [path '()])
      (if (leaf-node? node)
        (cons 0 (cons node path))
        (f (list-ref (.refs node) 0) (cons 0 (cons node path)))))))


(define maximum
  (lambda (node)
    (let f ([node node]
            [path '()])
      (if (leaf-node? node)
        (cons (sub1 (.size node)) (cons node path))
        (let ([tail (.size node)])
          (f (list-ref (.refs node) tail)
             (cons tail (cons node path))))))))


(define left-shift!
  (lambda (i node)
    (let ([left (list-ref (.refs node) i)]
          [right (list-ref (.refs node) (add1 i))])
      (.keys! left
              (append (.keys left) (list (list-ref (.keys node) i))))
      (.size! left (add1 (.size left)))
      (.keys! node
              (replace-by-index (.keys node) i (car (.keys right))))
      (.keys! right
              (remove-by-index (.keys right) 0))
      (.size! right (sub1 (.size right)))
      (when (not (leaf-node? left))
        (.refs! left
                (append (.refs left) (list (car (.refs right)))))
        (.refs! right
                (remove-by-index (.refs right) 0))))))


(define right-shift!
  (lambda (i node)
    (let* ([left (list-ref (.refs node) i)]
           [tail (sub1 (.size left))]
           [right (list-ref (.refs node) (add1 i))])
      (.keys! right
              (cons (list-ref (.keys node) i) (.keys right)))
      (.size! right (add1 (.size right)))
      (.keys! node
              (replace-by-index (.keys node) i (list-ref (.keys left) tail)))
      (.keys! left
              (remove-by-index (.keys left) tail))
      (.size! left (sub1 (.size left)))
      (when (not (leaf-node? left))
        (.refs! left
                (remove-by-index (.refs left) tail))
        (.refs! right
                (cons (list-ref (.refs left) tail) (.refs right)))))))


(define merge!
  (lambda (BT i path)
    (let* ([node (1st path)]
           [left (list-ref (.refs node) i)]
           [right (list-ref (.refs node) (add1 i))])
      (.keys! left
              (append (.keys left) (list (list-ref (.keys node) i))))
      (.keys! left
              (append (.keys left) (.keys right)))
      (when (not (leaf-node? left))
        (.refs! left
                (append (.refs left) (.refs right))))
      (.size! left (+ 1 (.size left) (.size right)))
      (.keys! node
              (remove-by-index (.keys node) i))
      (.refs! node
              (remove-by-index (.refs node) (add1 i)))
      (.size! node (sub1 (.size node)))

      (if (= 0 (.size node))
        (.root! BT left)
        (fix-up! BT path)))))


(define fix-up!
  (lambda (BT path)
    (when (and (<= 4 (length path))
               (> (sub1 (.degree BT)) (.size (1st path))))
      (let* ([dmin (sub1 (.degree BT))]
             [dmax (* 2 dmin)]
             [child (1st path)]
             [i (2nd path)]
             [parent (3rd path)]
             [tail (.size parent)]
             [left (if (= i 0)
                     #f
                     (list-ref (.refs parent) (sub1 i)))]
             [right (if (= i tail)
                      #f
                      (list-ref (.refs parent) (add1 i)))])
        (cond
          [(boolean? left)
           (if (= dmax (+ 1 (.size child) (.size right)))
             (merge! BT i (cddr path))
             (left-shift! i parent))]
          [(boolean? right)
           (if (= dmax (+ 1 (.size left) (.size child)))
             (merge! BT (sub1 i) (cddr path))
             (right-shift! (sub1 i) parent))]
          [(= dmax (+ 1 (.size left) (.size child)))
           (merge! BT (sub1 i) (cddr path))]
          [(= dmax (+ 1 (.size child) (.size right)))
           (merge! BT i (cddr path))]
          [(< (.size left) (.size right))
           (left-shift! i parent)]
          [else
           (right-shift! (sub1 i) parent)])))))