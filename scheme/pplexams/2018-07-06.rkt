#lang racket

(define (deepenf xs)
  (foldl (lambda (x y) (append (list y) x)) '() xs))

(define (deepen xs)
  (define (go xs acc)
    (if (null? xs)
      acc
      (go (cdr xs) (append (list acc) (car xs)))))
  (go xs '()))

(define (deepen1 L)
  (foldl (lambda (x y)
           (list y x))
         (list (car L))
         (cdr L)))

(deepenf '(1 2 3))  ; this is wrong
(deepen1 '(1 2 3))  ; this is right

(define-syntax define-with-return:
  (syntax-rules ()
                ((_ m (f args) body ...)
                 (define (f args)
                   (call/cc (lambda (m)
                              body ...))))))

(define-with-return: return (f x)
                     (define a 12)
                     (return (+ x a)))

(f 3)
