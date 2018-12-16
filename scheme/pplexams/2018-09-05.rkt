#lang racket

(define (revlt ls1 ls2 ls3)
  (define (help ls1 ls2 ls3 acc)
    (if (or (null? ls1) (null? ls2) (null? ls3))
      acc
      (help (cdr ls1) (cdr ls2) (cdr ls3) (cons (vector (car ls1) (car ls2) (car ls3)) acc))))
  (help ls1 ls2 ls3 '()))

(revlt '(1 2 3) '(4 5 6 7) '(8 9 10))
