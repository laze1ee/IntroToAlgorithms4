#lang racket


(require "util.rkt")

(provide new-queue enqueue! dequeue! null-queue?)



(define new-queue (lambda () (vector '())))


(define enqueue!
  (lambda (queue any)
    (set0! queue (append (ref0 queue) (list any)))))


(define dequeue!
  (lambda (queue)
    (let ([any (car (ref0 queue))])
      (set0! queue (cdr (ref0 queue)))
      any)))


(define null-queue?
  (lambda (queue)
    (null? (ref0 queue))))