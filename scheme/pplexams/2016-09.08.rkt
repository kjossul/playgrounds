#lang racket

(define (genRow n m)
  (define (help i acc)
    (cond [(= i n) acc]
          [(= i m) (help (+ 1 i) (append acc (list 1)))]
          [else (help (+ 1 i) (append acc (list 0)))]))
  (help 0 '()))

(define (genFig n)
  (define (help m acc)
    (if (= n m)
      acc
      (help (+ 1 m) (append acc (list (genRow n m))))))
  (help 0 '()))

(display (genFig 3))
