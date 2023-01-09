#lang racket


(require "../util.rkt"
         "sequence.rkt")


;; Huffman-Tree
;; node: #(frequence left-node right-node)
;; leaf-node: #(frequence character)
(define leaf?
  (lambda (node)
    (and (= 2 (vector-length node))
         (integer? (ref0 node))
         (char? (ref1 node)))))


(define p (lambda (o1 o2) (< (ref0 o1) (ref0 o2))))


(define init-Seq
  (lambda (ls)
    (let ([S (new-Seq p)])
      (map (lambda (obj)
             (Seq-insert! S obj))
           ls)
      S)))


(define huffman
  (lambda (ls)
    (let ([last (sub1 (length ls))]
          [S (init-Seq ls)]
          [node #f]
          [left #f]
          [right #f])
      (do ([i 0 (add1 i)]) ((= i last))
        (set! left (Seq-extract-head! S))
        (set! right (Seq-extract-head! S))
        (set! node (make-vector 3 0))
        (set0! node (+ (ref0 left) (ref0 right)))
        (set1! node left)
        (set2! node right)
        (Seq-insert! S node))
      (Seq-extract-head! S))))


(define huffman-tree-walk
  (lambda (root)
    (let ([store '()])
      (let f ([node root]
              [code (string)])
        (if (leaf? node)
          (set! store
            (append store
                    (list (list (ref1 node) code))))
          (begin
           (f (ref1 node) (string-append code "0"))
           (f (ref2 node) (string-append code "1")))))
      store)))


;; test
(define data '(#(45 #\a) #(13 #\b) #(16 #\d) #(5 #\f) #(9 #\e) #(12 #\c)))
(huffman-tree-walk
 (huffman data))