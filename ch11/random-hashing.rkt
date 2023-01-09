#lang racket


(require "../util.rkt")


(define select-a-prime
  (lambda (slot)
    (next-prime (* 2 slot))))


(define random-a
  (lambda (prime)
    (random-integer 1 prime)))


(define random-b
  (lambda (prime)
    (random-integer 0 prime)))


(define hash
  (lambda (key slot a b prime)
    (modulo
     (modulo (+ (* key a) b) prime)
     slot)))


;; test
(define solt 24)
(define p (select-a-prime solt))
(define a (random-a p))
(define b (random-b p))

(map (lambda (key)
       (hash key solt a b p))
     '(3 95 96 95 98 99 100 101 102 103 302 901 1001 5139))