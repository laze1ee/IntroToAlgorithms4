#lang racket


(require "../util.rkt")

(provide new-RB RB-insert! RB-delete!
         RB-minimum RB-maximum RB-walk)


(define node?
  (lambda (node)
    (and
     (= 4 (vector-length node))
     (or (number? (ref0 node)) (eq? 'nil (ref0 node)))
     (boolean? (ref1 node))
     (or (vector? (ref2 node)) (eq? 'nil (ref0 node)))
     (or (vector? (ref3 node)) (eq? 'nil (ref0 node))))))


(define nil-node  (lambda () (vector 'nil #f 'nil 'nil)))
(define nil? (lambda (node) (equal? node (nil-node))))


;; Singly-Linked-Red-Black-Tree: #(root-node)
;; node: #(object color left-node right-node)
(define new-RB
  (lambda objs
    (let ([rb (vector (nil-node))])
      (let f ([objs objs])
        (when (not (null? objs))
          (RB-insert! rb (car objs))
          (f (cdr objs))))
      rb)))


(define root (lambda (rb) (ref0 rb)))
(define object  (lambda (node) (ref0 node)))
(define object! (lambda (node obj) (set0! node obj)))
(define $red #t)
(define $black #f)
(define color  (lambda (node) (ref1 node)))
(define color! (lambda (node c) (set1! node c)))
(define left   (lambda (node) (ref2 node)))
(define left!  (lambda (node1 node2) (set2! node1 node2)))
(define right  (lambda (node) (ref3 node)))
(define right! (lambda (node1 node2) (set3! node1 node2)))


;; Path: ((left-or-right-in-parent node) ...)
;; left:  #t
;; right: #f
(define self        (lambda (path) (0th (0th path))))
(define self-way    (lambda (path) (1st (0th path))))
(define parent      (lambda (path) (0th (1st path)))) ;; parent
(define parent-way  (lambda (path) (1st (1st path))))
(define pparent     (lambda (path) (0th (2nd path)))) ;; parent's parent
(define pparent-way (lambda (path) (1st (2nd path))))
(define left-way?   (lambda (way) way))
(define right-way?  (lambda (way) (not way)))


(define right-rotate!
  (lambda (path)
    (let* ([x (self path)]
           [y (left x)]
           [b (right y)])
      (right! y x)
      (left! x b)
      (if (null? (cdr path))
        #t
        (begin
         (if (left-way? (self-way path))
           (left! (parent path) y)
           (right! (parent path) y))
         #f)))))


(define left-rotate!
  (lambda (path)
    (let* ([x (self path)]
           [y (right x)]
           [b (left y)])
      (left! y x)
      (right! x b)
      (if (null? (cdr path))
        #t
        (begin
         (if (left-way? (self-way path))
           (left! (parent path) y)
           (right! (parent path) y))
         #f)))))


(define insert!
  (lambda (root obj)
    (let f ([node root]
            [way 'root]
            [path  '()])
      (cond
        [(nil? node)
         (object! node obj)
         (color!  node $red)
         (left!   node (nil-node))
         (right!  node (nil-node))
         (cons (list node way) path)]
        [(< obj (object node))
         (f (left node) #t (cons (list node way) path))]
        [else
         (f (right node) #f (cons (list node way) path))]))))


(define insert-fixup!
  (lambda (rb path)
    (let f ([path path])
      (when (and (< 2 (length path))
                 (eq? $red (color (parent path))))
        (if (left-way? (parent-way path))
          (let ([p  (parent path)]
                [u (right (pparent path))]
                [pp (pparent path)])
            (cond
              [(eq? $red (color u))
               (color! p  $black)
               (color! u  $black)
               (color! pp $red)
               (f (cddr path))]
              [else
               (when (right-way? (self-way path))
                 (left-rotate! (cdr path))
                 (set! p (self path)))
               (color! p  $black)
               (color! pp $red)
               (when (right-rotate! (cddr path))
                 (set0! rb p))]))
          (let ([p  (parent path)]
                [u  (left (pparent path))]
                [pp (pparent path)])
            (cond
              [(eq? $red (color u))
               (color! p  $black)
               (color! u  $black)
               (color! pp $red)
               (f (cddr path))]
              [else
               (when (left-way? (self-way path))
                 (right-rotate! (cdr path))
                 (set! p (self path)))
               (color! p  $black)
               (color! pp $red)
               (when (left-rotate! (cddr path))
                 (set0! rb p))])))))
    (color! (root rb) $black)))


(define RB-insert!
  (lambda (rb obj)
    (insert-fixup! rb
                   (insert! (root rb) obj))))


(define RB-delete!
  (lambda (rb obj)
    (let f ([node (root rb)]
            [way  'root]
            [path '()])
      (cond
        [(nil? node)
         (error 'RB.delete!
                "not found object ~s in RB tree ~s" obj root)]
        [(= obj (object node))
         (delete! rb (cons (list node way) path))]
        [(< obj (object node))
         (f (left node) #t (cons (list node way) path))]
        [else
         (f (right node) #f (cons (list node way) path))]))))


(define delete!
  (lambda (rb path)
    (let ([target (self path)]
          [target-way (self-way path)]
          [original-color (color (self path))]
          [x #f])
      (cond
        [(nil? (left target))
         (set! x (right target))
         (if (< 1 (length path))
           (if (left-way? target-way)
             (left! (parent path) x)
             (right! (parent path) x))
           (set0! rb x))
         (set! path (cons (list x target-way) (cdr path)))]
        [(nil? (right target))
         (set! x (left target))
         (if (< 1 (length path))
           (if (left-way? target-way)
             (left! (parent path) x)
             (right! (parent path) x))
           (set0! rb x))
         (set! path (cons (list x target-way) (cdr path)))]
        [else
         (let* ([min-path (minimum (right target))]
                [y (self min-path)])
           (set! original-color (color y))
           (set! x (right y))
           (object! target (object y))
           (cond
             [(= 1 (length min-path))
              (right! target x)
              (set! path (cons (list x #f) path))]
             [else
              (left! (parent min-path) x)
              (set! min-path (cdr path))
              (set! min-path (cons (list x #t) min-path))
              (set! path (append min-path path))]))])
      (when (eq? $black original-color)
        (delete-fixup! rb path)))))


(define minimum
  (lambda (node)
    (let f ([node (left node)]
            [path (list (list node #f))])
      (if (nil? node)
        path
        (f (left node)
           (cons (list node #t) path))))))


(define delete-fixup!
  (lambda (rb path)
    (let f ()
      (when (and (< 1 (length path))
                 (eq? $black (color (self path))))
        (let ([x (self path)]
              [way (self-way path)]
              [y #f]
              [p (parent path)])
          (cond
            [(left-way? way)
             (set! y (right p))
             ;; case 1
             (when (eq? $red (color y))
               (color! y $black)
               (color! p $red)
               (set! way (parent-way path))
               (cond
                 [(left-rotate! (cdr path))
                  (set0! rb y)
                  (set! path (cons (list y 'root) '()))]
                 [else
                  (set! path (cddr path))
                  (if (left-way? way)
                    (set! path (cons (list y #t) path))
                    (set! path (cons (list y #f) path)))])
               (set! path (cons (list p #t) path))
               (set! path (cons (list x #t) path))
               (set! y (right p)))

             (cond
               ;; case 2
               [(and (eq? $black (color (left y)))
                     (eq? $black (color (right y))))
                (color! y $red)
                (set! path (cdr path))
                (f)]
               ;; case 3
               [else
                (when (eq? $black (color (right y)))
                  (color! (left y) $black)
                  (color! y $red)
                  (right-rotate! (cons (list y #f) (cdr path)))
                  (set! y (right p)))
                ;; case 4
                (color! y (color p))
                (color! p $black)
                (color! (right y) $black)
                (when (left-rotate! (cdr path))
                  (set0! rb y))])]
            
            [else
             (set! y (left p))
             ;; case 1
             (when (eq? $red (color y))
               (color! y $black)
               (color! p $red)
               (set! way (parent-way path))
               (cond
                 [(right-rotate! (cdr path))
                  (set0! rb y)
                  (set! path (cons (list y 'root) '()))]
                 [else
                  (set! path (cddr path))
                  (if (left-way? way)
                    (set! path (cons (list y #t) path))
                    (set! path (cons (list y #f) path)))])
               (set! path (cons (list p #f) path))
               (set! path (cons (list x #f) path))
               (set! y (left p)))

             (cond
               ;; case 2
               [(and (eq? $black (color (left y)))
                     (eq? $black (color (right y))))
                (color! y $red)
                (set! path (cdr path))
                (f)]
               ;; case 3
               [else
                (when (eq? $black (color (left y)))
                  (color! (right y) $black)
                  (color! y $red)
                  (left-rotate! (cons (list y #t) (cdr path)))
                  (set! y (left p)))
                ;; case 4
                (color! y (color p))
                (color! p $black)
                (color! (left y) $black)
                (when (right-rotate! (cdr path))
                  (set0! rb y))])]))))
    (color! (self path) $black)))


(define RB-minimum
  (lambda (node)
    (if (nil? (left node))
      (object node)
      (RB-minimum (left node)))))


(define RB-maximum
  (lambda (node)
    (if (nil? (right node))
      (object node)
      (RB-maximum (right node)))))


(define RB-walk
  (lambda (rb)
    (let ([store '()])
      (let f ([node (root rb)])
        (when (and (node? node)
                   (not (nil? node)))
          (f (right node))
          (set! store (cons (object node) store))
          (f (left node))))
      store)))


;; test
(define rb (new-RB 5 7 6 9 3 0 1 8 2 4))
(RB-walk rb)
(map (lambda (key)
       (RB-delete! rb key)
       (RB-walk rb))
     '(3 7 1 2 8 5 6 0 4 9))