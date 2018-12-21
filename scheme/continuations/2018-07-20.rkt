#lang racket

(define saved '())

(define (push-k x)
  (set! saved (append saved (list x))))

(define (poprun-k)
  (if (null? saved)
      #f
      (let ((x (car saved)))
        (set! saved (cdr saved))
        (x))))

(define (c1 x)
  (call/cc (lambda (k)
             (push-k k)))
  (set! x (+ x 1))
  (display "c1 ")(displayln x))

(define (c2 y)
  (call/cc (lambda (k)
             (push-k k)))
  (set! y (* y 2))
  (display "c2 ")(displayln y))

(define (run)
  (c1 0) (c2 2) (poprun-k))

(run)