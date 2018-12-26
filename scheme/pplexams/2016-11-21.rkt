#lang racket

(define (mymap f xs)
  (foldr (lambda (x y)
           (cons (f x) y)) '() xs))

(mymap (lambda (x) (+ x 1)) '(0 1 2 3))

(define (myfilter f xs)
  (foldr (lambda (x y)
           (if (f x)
             (cons x y)
             y)) '() xs))

(myfilter odd? '(0 1 2 3))

(define (cos-min i j)
  (define (go i m)
    (when (< (cos i) (cos m))
      (set! m i))
    (set! i (+ i 1))
    (if (> i j)
      m
      (go i m)))
  (go i i))

(cos-min 1 10)
