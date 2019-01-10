#lang racket

(define (ftree fs xs)
  (cond [(null? fs) xs]
        [(list? (car fs)) (cons (ftree (car fs) (car xs)) (ftree (cdr fs) (cdr xs)))]
        [else (cons ((car fs) (car xs)) (ftree (cdr fs) (cdr xs)))]))

(define f1 (lambda (x) (+ 1 x)))
(define f2 (lambda (x) (* 2 x)))
(define f3 (lambda (x) (- x 10)))
(define f4 (lambda (x) (string-append "<<" x ">>")))
(define t1 '(1 (2 3 4) (5 (6)) ("hi!" 8)))
(define o1 `(,f1 (,f1 ,f2 ,f1) (,f3 (,f1)) (,f4 ,f3)))
(define o2 `(,f1 () (,f3 (,f1)) (,f4 ,f3)))
(ftree o1 t1)
(ftree o2 t1)
