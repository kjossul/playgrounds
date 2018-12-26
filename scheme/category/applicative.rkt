#lang racket

(define (<*> fs xs)
  (define (go fs xs acc)
    (cond [(null? fs) acc]
          [else (go (cdr fs) xs (append acc (map (car fs) xs)))]))
  (go fs xs '()))

(<*> (list (lambda (x) (+ 1 x))
           (lambda (x) (* 2 x)))
     '(1 2 3))

