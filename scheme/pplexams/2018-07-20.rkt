#lang racket

(define (fep xs)
  (define (help xs original)
    (if (empty? xs)
      original
      (let ((head (car xs))
            (tail (cdr xs)))
        (append (list head) (list (help tail original)) (list head)))))
  (help xs xs))
    

(fep '(1 2 3))
