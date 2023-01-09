#lang racket


(require "../util.rkt")

(provide extended-euclid)



(define euclid
  (lambda (a b)
    (if (= b 0)
      a
      (euclid b (modulo a b)))))


(define extended-euclid
  (lambda (a b)
    (if (= b 0)
      (list a 1 0)
      (let* ([dxy (extended-euclid b (modulo a b))]
             [x (1st dxy)]
             [y (2nd dxy)])
        (list (0th dxy) y (- x (* y (quotient a b))))))))


#|
;; test
(extended-euclid 20684 128)
|#