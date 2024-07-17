#lang racket

;; ==== mate procedure ====
(define get0 (lambda (ls) (list-ref ls 0)))
(define get1 (lambda (ls) (list-ref ls 1)))
(define get2 (lambda (ls) (list-ref ls 2)))
(define get3 (lambda (ls) (list-ref ls 3)))
(define ref0 (lambda (vec) (vector-ref vec 0)))
(define ref1 (lambda (vec) (vector-ref vec 1)))
(define ref2 (lambda (vec) (vector-ref vec 2)))
(define ref3 (lambda (vec) (vector-ref vec 3)))
(define ref4 (lambda (vec) (vector-ref vec 4)))
(define ref5 (lambda (vec) (vector-ref vec 5)))
(define set0! (lambda (vec any) (vector-set! vec 0 any)))
(define set1! (lambda (vec any) (vector-set! vec 1 any)))
(define set2! (lambda (vec any) (vector-set! vec 2 any)))
(define set3! (lambda (vec any) (vector-set! vec 3 any)))
(define set4! (lambda (vec any) (vector-set! vec 4 any)))
(define set5! (lambda (vec any) (vector-set! vec 5 any)))
;; ==== mate procedure ====

;; red-black-tree node:
;; #(key value color left-node right-node)
(define node
  (lambda ()
    (vector 'nil 'nil 'black 'nil 'nil)))

(define node.key   (lambda (nd) (ref0 nd)))
(define node.value (lambda (nd) (ref1 nd)))
(define node.color (lambda (nd) (ref2 nd)))
(define node.left  (lambda (nd) (ref3 nd)))
(define node.right (lambda (nd) (ref4 nd)))

(define set-node.key!   (lambda (nd key) (set0! nd key)))
(define set-node.value! (lambda (nd value) (set1! nd value)))
(define set-node.color! (lambda (nd color) (set2! nd color)))
(define set-node.left!  (lambda (nd left) (set3! nd left)))
(define set-node.right! (lambda (nd right) (set4! nd right)))

(define red-node?
  (lambda (nd)
    (eq? (node.color nd) 'red)))

(define black-node?
  (lambda (nd)
    (eq? (node.color nd) 'black)))

(define set-node-red!
  (lambda (nd)
    (set-node.color! nd 'red)))

(define set-node-black!
  (lambda (nd)
    (set-node.color! nd 'black)))

(define nil-node?
  (lambda (nd)
    (and (eq? (node.key nd) 'nil)
         (eq? (node.value nd) 'nil)
         (black-node? nd)
         (eq? (node.left nd) 'nil)
         (eq? (node.right nd) 'nil))))

(define node->string
  (lambda (nd)
    (if (nil-node? nd)
      'nil
      (format "(%s %s %s %s %s)"
              (node.key nd)
              (node.value nd)
              (node.color nd)
              (node->string (node.left nd))
              (node->string (node.right nd))))))


(define path-of
  (lambda (tr key)
    (let f ([nd (tree.root tr)] [path '()])
      (cond
        [(nil-node? nd)
         (cons nd path)]
        [((tree.less tr) key (node.key nd))
         (f (node.left nd) (cons nd path))]
        [((tree.greater tr) key (node.key nd))
         (f (node.right nd) (cons nd path))]
        [else
         (cons nd path)]))))

(define left-of?
  (lambda (node1 node2)
    (eq? node1 (node.left node2))))

(define right-of?
  (lambda (node1 node2)
    (eq? node1 (node.right node2))))


;; red-black-tree:
;; #(less greater root)
(define new
  (lambda (less greater)
    (vector less greater (node))))

(define tree.less (lambda (tr) (ref0 tr)))
(define tree.greater (lambda (tr) (ref1 tr)))
(define tree.root (lambda (tr) (ref2 tr)))
(define set-tree.root! (lambda (tr nd) (set2! tr nd)))

(define empty?
  (lambda (tr)
    (nil-node? (tree.root))))

(define present?
  (lambda (tr key)
    (define path (path-of tr key))
    (define nd (car path))
    (not (nil-node? nd))))

(define print
  (lambda (tr)
    (printf "(red-black-tree %s)\n"
            (node->string (tree.root tr)))))


(define left-rotate!
  (lambda (tr path)
    (define x (car path))
    (define y (node.right x))
    (define b (node.left y))
    (set-node.left! y x)
    (set-node.right! x b)
    (cond
      [(= 1 (length path))
       (set-tree.root! tr y)]
      [else
       (define p (get1 path))
       (if (left-of? x p)
         (set-node.left! p y)
         (set-node.right! p y))])))

(define right-rotate!
  (lambda (tr path)
    (define x (car path))
    (define y (node.left x))
    (define b (node.right y))
    (set-node.right! y x)
    (set-node.left! x b)
    (cond
      [(= 1 (length path))
       (set-tree.root! tr y)]
      [else
       (define p (get1 path))
       (if (left-of? x p)
         (set-node.left! p y)
         (set-node.right! p y))])))


(define insert!
  (lambda (tr key value)
    (define path (path-of tr key))
    (define nd (car path))
    (cond
      [(nil-node? nd)
       (set-node.key! nd key)
       (set-node.value! nd value)
       (set-node-red! nd)
       (set-node.left! nd (node))
       (set-node.right! nd (node))
       (insert-fixup! tr path)
       #t]
      [else
       #f])))

(define insert-fixup!
  (lambda (tr path)
    (let f ([path path])
      (when (and (< 2 (length path))
                 (red-node? (get1 path)))
        (define p (get1 path))
        (define pp (get2 path))
        (cond
          [(left-of? p pp)
           (define u (node.right pp))
           (cond
             [(red-node? u)
              (set-node-black! p)
              (set-node-black! u)
              (set-node-red! pp)
              (f (cddr path))]
             [else
              (when (right-of? (car path) p)
                (left-rotate! tr (cdr path))
                (set! p (car path)))
              (right-rotate! tr (cddr path))
              (set-node-black! p)
              (set-node-red! pp)])]
          [else
           (define u (node.left pp))
           (cond
             [(red-node? u)
              (set-node-black! p)
              (set-node-black! u)
              (set-node-red! pp)
              (f (cddr path))]
             [else
              (when (left-of? (car path) p)
                (right-rotate! tr (cdr path))
                (set! p (car path)))
              (left-rotate! tr (cddr path))
              (set-node-black! p)
              (set-node-red! pp)])])))
    (set-node-black! (tree.root tr))))


(define delete!
  (lambda (tr key)
    (define path (path-of tr key))
    (define deleted (car path))
    (if (nil-node? deleted)
      #f
      (let ([color (node.color deleted)]
            [x #f])
        (cond
          [(nil-node? (node.left deleted))
           (set! x (node.right deleted))
           (transplant! tr path x)
           (set! path (cons x (cdr path)))]
          [(nil-node? (node.right deleted))
           (set! x (node.left deleted))
           (transplant! tr path x)
           (set! path (cons x (cdr path)))]
          [else
           (define min-path (minimum (node.right deleted)))
           (define alternate (car min-path))
           (set! color (node.color alternate))
           (set! x (node.right alternate))
           (when (not (right-of? alternate deleted))
             (transplant! tr min-path x)
             (set-node.right! alternate (node.right deleted)))
           (transplant! tr path alternate)
           (set-node.left! alternate (node.left deleted))
           (set-node.color! alternate (node.color deleted))
           (set! path (append (cons x (cdr min-path))
                              (cons alternate (cdr path))))])
        (when (eq? color 'black)
          (delete-fixup! tr path))
        #t))))


(define transplant!
  (lambda (tr path nd)
    (if (= 1 (length path))
      (set-tree.root! tr nd)
      (let ([p (get1 path)])
        (if (left-of? (car path) p)
          (set-node.left! p nd)
          (set-node.right! p nd))))))

(define minimum
  (lambda (nd)
    (let f ([nd nd] [path '()])
      (if (nil-node? nd)
        path
        (f (node.left nd)
           (cons nd path))))))

(define maximum
  (lambda (nd)
    (let f ([nd nd] [path '()])
      (if (nil-node? nd)
        path
        (f (node.right nd)
           (cons nd path))))))

(define delete-fixup!
  (lambda (tr path)
    (define x (car path))
    (let f ([path (cdr path)])
      (cond
        [(null? path)
         (void)]
        [(black-node? x)
         (define p (car path))
         (if (left-of? x p)
           (let ([s (node.right p)])
             (when (red-node? s)
               (left-rotate! tr path)
               (set-node-black! s)
               (set-node-red! p)
               (set! path (cons p (cons s (cdr path))))
               (set! s (node.right p)))
             (cond
               [(and (black-node? (node.left s))
                     (black-node? (node.right s)))
                (set-node-red! s)
                (set! x p)
                (f (cdr path))]
               [else
                (when (black-node? (node.right s))
                  (right-rotate! tr (cons s path))
                  (set-node-red! s)
                  (set-node-black! (node.right p))
                  (set! s (node.right p)))
                (left-rotate! tr path)
                (set-node.color! s (node.color p))
                (set-node-black! p)
                (set-node-black! (node.right s))]))
           (let ([s (node.left p)])
             (when (red-node? s)
               (right-rotate! tr path)
               (set-node-black! s)
               (set-node-red! p)
               (set! path (cons p (cons s (cdr path))))
               (set! s (node.left p)))
             (cond
               [(and (black-node? (node.left s))
                     (black-node? (node.right s)))
                (set-node-red! s)
                (set! x p)
                (f (cdr path))]
               [else
                (when (black-node? (node.left s))
                  (left-rotate! tr (cons s path))
                  (set-node-red! s)
                  (set-node-black! (node.left p))
                  (set! s (node.left p)))
                (right-rotate! tr path)
                (set-node.color! s (node.color p))
                (set-node-black! p)
                (set-node-black! (node.left s))])))]))
    (set-node-black! x)))

  
(define ref
  (lambda (tr key)
    (define path (path-of tr key))
    (define nd (car path))
    (if (nil-node? nd)
      (error 'red-black-tr:ref "key %s is not presented in tr %s"
             key tr)
      nd)))

  
(define sett!
  (lambda (tr key value)
    (define path (path-of tr key))
    (define nd (car path))
    (if (nil-node? nd)
      (error 'red-black-tr:put! "key %s is not presented in tr %s"
             key tr)
      (set-node.value! nd value))))


(define travel
  (lambda (tr)
    (define acc '())
    (let f ([nd (tree.root tr)])
      (when (not (nil-node? nd))
        (f (node.right nd))
        (set! acc (cons (list (node.key nd) (node.value nd)) acc))
        (f (node.left nd))))
    acc))
