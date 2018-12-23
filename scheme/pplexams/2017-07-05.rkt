#lang racket

(struct leaf ((v #:mutable)))
(struct branch (l r))

(define (tmap! f t)
  (if (leaf? t)
    (set-leaf-v! t (f (leaf-v t)))
    (begin
      (tmap! f (branch-l t))
      (tmap! f (branch-r t)))))

(define (reverse! t)
  (define (stack t)
    (if (leaf? t)
      (list (leaf-v t))
      (append (stack (branch-r t)) (stack (branch-l t)))))
  (define (update t stack)
    (if (leaf? t)
      (begin
        (set-leaf-v! t (car stack))
        (cdr stack))
      (let ((stack1 (update (branch-l t) stack)))
        (update (branch-r t) stack1))))
  (update t (stack t)))


(define t1 (branch (branch (leaf 1)(leaf 2))(leaf 3)))
(tmap! (lambda (x) (+ x 1)) t1)
(reverse! t1)
(display (leaf-v (branch-r t1)))

