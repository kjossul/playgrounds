#lang racket

(define-syntax curried
  (syntax-rules ()
    ((_ (arg) (body ...))
       (lambda (arg) (body ...)))
    ((_ (arg args ...) (body ...))
       (lambda (arg) (curried (args ...) (body ...))))))

(define f (curried (x y) (+ x y)))

((f 3) 4)
