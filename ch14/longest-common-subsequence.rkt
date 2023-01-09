#lang racket


(require "../util.rkt"
         "../matrix.rkt")


;; Direction
;; diagonal: 0
;; right: 1
;; bottom: 2
;; bottom and right: 3
;; null: nil
(define lcs
  (lambda (str1 str2)
    (let ([len1 (string-length str1)]
          [len2 (string-length str2)]
          [count    #f]
          [guide    #f]
          [table    #f]
          [right    #f]
          [diagonal #f]
          [bottom   #f])
      (set! table (new-Matrix (add1 len1) (add1 len2) '(0 nil)))
      (do ([i (sub1 len1) (sub1 i)]) ((= i -1))
        (do ([j (sub1 len2) (sub1 j)]) ((= j -1))
          (set! right    (Matrix-ref table i (add1 j)))
          (set! diagonal (Matrix-ref table (add1 i) (add1 j)))
          (set! bottom   (Matrix-ref table (add1 i) j))
          (cond
            [(eq? (string-ref str1 i)
                  (string-ref str2 j))
             (set! count (add1 (0th diagonal)))
             (set! guide 0)]
            [(< (0th bottom) (0th right))
             (set! count (0th right))
             (set! guide 1)]
            [(> (0th bottom) (0th right))
             (set! count (0th bottom))
             (set! guide 2)]
            [else
             (set! count (0th bottom))
             (set! guide 3)])
          (Matrix-set! table i j (list count guide))))
      (lcs-result str1 table))))


(define lcs-result
  (lambda (str1 table)
    (let f ([i 0]
            [j 0])
      (let ([count (0th (Matrix-ref table i j))]
            [guide (1st (Matrix-ref table i j))])
        (cond
          [(= 0 count) '()]
          [(= 0 guide)
           (cons (string-ref str1 i) (f (add1 i) (add1 j)))]
          [(= 1 guide)
           (f i (add1 j))]
          [(= 2 guide)
           (f (add1 i) j)]
          [else
           (list (f i (add1 j))
                 (f (add1 i) j))])))))


;; test
(define str1 "Truth is ever to be found in simplicity, and not in the multiplicity and confusion of things.")
(define str2 "Gravity explains the motions of the planets, but it cannot explain who sets the planets in motion.")
;(lcs str1 str2)
;; Run this code, Drracke consumed many memories but no return.


(define str3 "BDCABAEDCCEABC")
(define str4 "ABCEBDABECBAC")
(lcs str3 str4)