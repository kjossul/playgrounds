#lang racket

(define (urmax xs)
  (define (go xs m i)
    (if (null? xs)
      m
      (let ((curr (list-ref (car xs) i)))
        (go (cdr xs) (max m curr) (+ i 1)))))
  (go xs (car (car xs)) 0))

(urmax '((-1)(1 2)(1 2 3)(10 2 3 -4))) 

(define i 0)
(define (getNth xs)
  (set! i (+ i 1))
  (list-ref xs (- i 1)))


(define (urmax1 xs)
  (let* ((xs (map (lambda (xs) (getNth xs)) xs)))
    (foldl max (car xs) xs)))

(urmax1 '((-1)(1 2)(1 2 3)(10 2 3 -4))) 
