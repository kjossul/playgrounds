#lang racket

(define (iter-sublist xs k)
  (let ((ys xs))
    (lambda ()
      (if (< (length ys) k)
        'end
        (let ((y (car ys)))
          (set! ys (cdr ys))
          (take (cons y ys) k))))))

(define i (iter-sublist '(1 2 3 4) 3))
(i)
(i)
(i)

(define (sublist xs n)
  (let ((i (iter-sublist xs n)))
    (define (go acc)
      (let ((cur (i)))
        (if (not (list? cur))
          acc
          (go (cons cur acc)))))
    (go '())))

(sublist '(1 2 3) 2)
