#lang racket


(require "../util.rkt")

(provide node? interval? nil-node nil-node?
         .root .root!
         .interval .interval! .max .max! .size .size!
         @red @black .color .color! .left .left! .right .right!
         left-way? right-way?
         left-rotate! right-rotate! minimum maximum
         exact-search)

         
;; Interval-Tree: #(Root-Node)
;; Node: #(Interval Max Size Color Left-Node Right-Node)
;;     : nil
;; Interval: #(Start End)
;;         : nil
;; Color: @R
;;      : @B
(define node?
  (lambda (node)
    (or (and (= 6 (vector-length node))
             (interval? (ref0 node))
             (number? (ref1 node))
             (natural? (ref2 node))
             (color? (ref3 node))
             (or (node? (ref4 node))
                 (nil-node? (ref4 node)))
             (or (node? (ref5 node))
                 (nil-node? (ref5 node))))
        (nil-node? node))))


(define interval?
  (lambda (interval)
    (or (and (= 2 (vector-length interval))
             (natural? (ref0 interval))
             (natural? (ref1 interval)))
        (eq? 'nil interval))))


(define color?
  (lambda (color)
    (or (eq? '@R color)
        (eq? '@B color))))


(define nil-node  (lambda () (vector 'nil -inf.0 0 '@B 'nil 'nil)))
(define nil-node? (lambda (node) (equal? node (nil-node))))

(define .root   (lambda (RB) (ref0 RB)))
(define .root!  (lambda (RB node) (set0! RB node)))

(define .interval  (lambda (node) (ref0 node)))
(define .interval! (lambda (node interval) (set0! node interval)))
(define .max    (lambda (node) (ref1 node)))
(define .max!   (lambda (node n) (set1! node n)))
(define .size   (lambda (node) (ref2 node)))
(define .size!  (lambda (node n) (set2! node n)))
(define  @red   '@R)
(define  @black '@B)
(define .color  (lambda (node) (ref3 node)))
(define .color! (lambda (node color) (set3! node color)))
(define .left   (lambda (node) (ref4 node)))
(define .left!  (lambda (node1 node2) (set4! node1 node2)))
(define .right  (lambda (node) (ref5 node)))
(define .right! (lambda (node1 node2) (set5! node1 node2)))


(define left-way?
  (lambda (node1 node2)
    (eq? node1 (.left node2))))


(define right-way?
  (lambda (node1 node2)
    (eq? node1 (.right node2))))


(define right-rotate!
  (lambda (path)
    (let* ([z (car path)]
           [x (.left z)]
           [y (.right z)]
           [b (.right x)])
      (.right! x z)
      (.left! z b)
      (.size! x (.size z))
      (.size! z (+ 1 (.size b) (.size y)))
      (.max! x (.max z))
      (.max! z (max (ref1 (.interval z)) (.max b) (.max y)))
      (if (null? (cdr path))
        #t
        (begin
         (if (left-way? z (1st path))
           (.left! (1st path) x)
           (.right! (1st path) x))
         #f)))))


(define left-rotate!
  (lambda (path)
    (let* ([z (car path)]
           [x (.left z)]
           [y (.right z)]
           [a (.left y)])
      (.left! y z)
      (.right! z a)
      (.size! y (.size z))
      (.size! z (+ 1 (.size x) (.size a)))
      (.max! y (.max z))
      (.max! z (max (ref1 (.interval z)) (.max x) (.max a)))
      (if (null? (cdr path))
        #t
        (begin
         (if (left-way? z (1st path))
           (.left! (1st path) y)
           (.right! (1st path) y))
         #f)))))


(define minimum
  (lambda (node)
    (let f ([node node]
            [path '()])
      (if (nil-node? (.left node))
        (cons node path)
        (f (.left node) (cons node path))))))


(define maximum
  (lambda (node)
    (let f ([node node]
            [path '()])
      (if (nil-node? (.right node))
        (cons node path)
        (f (.right node) (cons node path))))))


(define overlap?
  (lambda (i1 i2)
    (let ([i1.s (ref0 i1)]
          [i1.e (ref1 i1)]
          [i2.s (ref0 i2)]
          [i2.e (ref1 i2)])
      (if (< i1.s i2.s)
        (<= i2.s i1.e)
        (<= i1.s i2.e)))))


(define exact-search
  (lambda (node interval)
    (let f ([node node]
            [path '()])
      (cond
        [(nil-node? node) #f]
        [(equal? interval (.interval node))
         (cons node path)]
        [else
         (let ([range (vector (ref0 (.interval (car (minimum node))))
                              (.max node))])
           (if (overlap? range interval)
             (if (< (ref0 interval) (ref0 (.interval node)))
               (f (.left node) (cons node path))
               (f (.right node) (cons node path)))
             #f))]))))