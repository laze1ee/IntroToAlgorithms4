#lang racket


(provide start-timer stop-timer decrement-timer
         make-engine timed-lambda)



;; timer
(define clock 0)
(define handler #f)


(define start-timer
  (lambda (ticks new-handler)
    (set! clock ticks)
    (set! handler new-handler)))


(define stop-timer
  (lambda ()
    (let ([time-left clock])
      (set! clock 0)
      time-left)))


(define decrement-timer
  (lambda ()
    (when (< 0 clock)
      (set! clock (sub1 clock))
      (when (= 0 clock) (handler)))))



;; engine
(define make-engine
  (let ([do-complete #f]
        [do-expire #f]
        [timer-handler #f]
        [new-engine #f])
    (set! timer-handler
      (lambda ()
        (start-timer (call/cc do-expire)
                     timer-handler)))
    (set! new-engine
      (lambda (resume)
        (lambda (ticks complete expire)
          ((call/cc
            (lambda (escape)
              (set! do-complete
                (lambda (ticks value)
                  (escape (lambda () (complete ticks value)))))
              (set! do-expire
                (lambda (resume)
                  (escape (lambda () (expire (new-engine resume))))))
              (resume ticks)))))))
    (lambda (proc)
      (new-engine
       (lambda (ticks)
         (start-timer ticks timer-handler)
         (let* ([value (proc)]
                [ticks (stop-timer)])
           (do-complete ticks value)))))))



(define-syntax timed-lambda
  (syntax-rules ()
    [(_ formals exp1 exp2 ...)
     (lambda formals (decrement-timer) exp1 exp2 ...)]))