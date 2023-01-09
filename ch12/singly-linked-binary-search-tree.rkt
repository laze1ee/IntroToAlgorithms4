#lang racket


(require "../util.rkt")

(provide new-BS BS.insert! BS.delete!
         BS.search BS.minimum BS.maximum
         BS.predecessor BS.successor)


(define node?
  (lambda (node)
    (and (= 3 (vector-length node))
         (or (number? (ref0 node)) (eq? 'nil (ref0 node)))
         (or (vector? (ref1 node)) (eq? 'nil (ref1 node)))
         (or (vector? (ref2 node)) (eq? 'nil (ref2 node))))))


(define nil-node (lambda () (vector 'nil 'nil 'nil)))
(define nil? (lambda (node) (equal? node (nil-node))))


;; Singly-Linked-Binary-Search-Tree
;; node: #(object left-node right-node)
(define new-BS
  (lambda objs
    (let ([root (nil-node)])
      (let f ([objs objs])
        (when (not (null? objs))
          (BS.insert! root (car objs))
          (f (cdr objs))))
      root)))


(define object  (lambda (node) (ref0 node)))
(define object! (lambda (node x) (set0! node x)))
(define left    (lambda (node) (ref1 node)))
(define left!   (lambda (node1 node2) (set1! node1 node2)))
(define right   (lambda (node) (ref2 node)))
(define right!  (lambda (node1 node2) (set2! node1 node2)))


(define BS.insert!
  (lambda (node obj)
    (cond
      [(nil? node)
       (object! node obj)
       (left! node (nil-node))
       (right! node (nil-node))]
      [(< obj (object node))
       (BS.insert! (left node) obj)]
      [else
       (BS.insert! (right node) obj)])))


(define BS.delete!
  (lambda (root obj)
    (let ([path (BS.search root obj)])
      (if (boolean? path)
        (error 'BS.delete!
               "not found object ~s in BS tree ~s" obj root)
        (delete! (car path))))))


(define delete!
  (lambda (node)
    (let ([x #f])
      (cond
        [(or (nil? (left node)) (nil? (right node)))
         (if (nil? (left node))
           (set! x (right node))
           (set! x (left node)))
         (object! node (object x))
         (left! node (left x))
         (right! node (right x))]
        [else
         (set! x (minimum (right node)))
         (object! node (object x))
         (delete! x)]))))


(define BS.search
  (lambda (root obj)
    (let f ([node root]
            [path '()])
      (cond
        [(nil? node) #f]
        [(= obj (object node))
         (cons node path)]
        [(< obj (object node))
         (f (left node) (cons node path))]
        [else
         (f (right node) (cons node path))]))))


(define minimum
  (lambda (node)
    (if (nil? (left node))
      node
      (minimum (left node)))))


(define maximum
  (lambda (node)
    (if (nil? (right node))
      node
      (maximum (right node)))))


(define BS.minimum (lambda (node) (object (minimum node))))
(define BS.maximum (lambda (node) (object (maximum node))))


(define BS.predecessor
  (lambda (root obj)
    (let ([path (BS.search root obj)])
      (if (boolean? path)
        (error 'BS.predecessor
               "not found object ~s in BS tree ~s" obj root)
        (predecessor path obj)))))


(define predecessor
  (lambda (path obj)
    (cond
      [(nil? (left (car path)))
       (let f ([way #f]
               [path path])
         (cond
           [way
            (object (0th path))]
           [(null? (cdr path))
            (error 'BS.predecessor
                   "minimum object ~s no predecessor" obj)]
           [else
            (f (eq? (0th path) (right (1st path)))
               (cdr path))]))]
      [else
       (BS.maximum (left (car path)))])))


(define BS.successor
  (lambda (root obj)
    (let ([path (BS.search root obj)])
      (if (boolean? path)
        (error 'BS.successor
               "not found object ~s in BS tree ~s" obj root)
        (successor path obj)))))


(define successor
  (lambda (path obj)
    (cond
      [(nil? (right (car path)))
       (let f ([way #f]
               [path path])
         (cond
           [way
            (object (0th path))]
           [(null? (cdr path))
            (error 'BS.successor
                   "maximum object ~s no successor" obj)]
           [else
            (f (eq? (0th path) (left (1st path)))
               (cdr path))]))]
      [else
       (BS.minimum (right (car path)))])))


(define BS.walk
  (lambda (root)
    (let ([store '()])
      (let f ([node root])
        (when (and (node? node)
                   (not (nil? node)))
          (f (right node))
          (set! store (cons (object node) store))
          (f (left node))))
      store)))


;; test
(define root (new-BS))
(do ([i 0 (add1 i)]) ((= i 10))
  (BS.insert! root (random-integer 0 1000)))
(define sorted (BS.walk root))
sorted
(map (lambda (key)
       (BS.predecessor root key))
     (list-tail sorted 1))
(map (lambda (key)
       (BS.successor root key))
     (list-head sorted 9))