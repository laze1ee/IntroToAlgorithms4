#lang racket


(require "../util.rkt")


;; pseudo double linked list
;; ((previous list) current-object (next list))
;; creat a dlist
(define new-dlist
  (case-lambda
    [()
     '(() ())]
    [(o . objs)
     (list '() o objs)]))


(define dlist-null?
  (lambda (dl)
    (and (= 2 (length dl))
         (null? (0th dl))
         (null? (1st dl)))))


(define dlist-current
  (lambda (dl)
    (1st dl)))


(define dlist-left-shift
  (lambda (dl)
    (let ([pre (0th dl)]
          [cur (1st dl)]
          [nex (2nd dl)])
      (if (null? pre)
        (error 'dlist-left-shift
               "current object is most left in dlist ~s" dl)
        (list (cdr pre)
              (0th pre)
              (cons cur nex))))))


(define dlist-right-shift
  (lambda (dl)
    (let ([pre (0th dl)]
          [cur (1st dl)]
          [nex (2nd dl)])
      (if (null? nex)
        (error 'dlist-right-shift
               "current object is most right in dlist ~s" dl)
        (list (cons cur pre)
              (0th nex)
              (cdr nex))))))


(define dlist-search
  (lambda (o dl)
    (let ([search-pre #f]
          [search-nex #f])
      (set! search-pre
        (lambda (dl)
          (if (equal? o (dlist-current dl))
            dl
            (if (null? (0th dl))
              #f
              (search-pre (dlist-left-shift dl))))))
      (set! search-nex
        (lambda (dl)
          (if (equal? o (dlist-current dl))
            dl
            (if (null? (2nd dl))
              #f
              (search-nex (dlist-right-shift dl))))))
      (let ([pre (search-pre dl)])
        (if (boolean? pre)
          (search-nex dl)
          pre)))))


(define dlist-insert-pre
  (lambda (o dl)
    (let ([pre (0th dl)]
          [cur (1st dl)]
          [nex (2nd dl)])
      (list (cons o pre)
            cur
            nex))))


(define dlist-insert-nex
  (lambda (o dl)
    (let ([pre (0th dl)]
          [cur (1st dl)]
          [nex (2nd dl)])
      (list pre
            cur
            (cons o nex)))))


(define remove
  (lambda (dl)
    (let ([pre (0th dl)]
          [nex (2nd dl)])
      (cond
        [(and (null? pre) (null? nex))
         '(() ())]
        [(null? nex)
         (list (cdr pre) (car pre) '())]
        [else
         (list pre (car nex) (cdr nex))]))))


(define dlist-remove
  (lambda (dl)
    (if (dlist-null? dl)
      (error 'dlist-remove "dlist is empty")
      (remove dl))))


;; test
(define dl (new-dlist 'a 'b 53 90 '2k))
(set! dl (dlist-right-shift dl))
dl