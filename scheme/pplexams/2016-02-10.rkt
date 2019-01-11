#lang racket

(struct node (
              (data #:mutable)
              (next #:mutable)))

(define (make-clist)
  (let ((sentinel (node #f #f)))
    (set-node-next! sentinel sentinel)
    sentinel))

(define (cons-clist! new clist)
  (begin
    (set-node-next! new (node-next clist))
    (set-node-next! clist new)))

(define (cmap f clist)
  (let loop ((sentinel clist)
             (curr (node-next clist)))
    (unless (eq? sentinel curr)
      (begin
        (set-node-data! curr (f (node-data curr)))
        (loop sentinel (node-next curr))))))

(define clist (make-clist))
(cons-clist! (node 2 #f) clist)
(cons-clist! (node 1 #f) clist)
(cmap (lambda (x) (+ x 1)) clist)
(displayln (node-data (node-next clist)))
(displayln (node-data (node-next (node-next clist))))
