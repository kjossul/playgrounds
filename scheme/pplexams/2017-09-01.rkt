#lang racket

(define (check-this xs)
  (define (go xs stack acc)
    (if (null? xs)
      acc
      (let ((x (car xs))
            (rest (cdr xs)))
        (cond
          [(or (eq? x 'a) (eq? x 1)) (go rest (cons x stack) acc)]
          [(eq? x 'b)
           (if (and (not (null? stack)) (eq? (car stack) 'a))
             (go rest (cdr stack) (+ acc 1))
             #f)]
          [(eq? x 2)
           (if (and (not (null? stack)) (eq? (car stack) 1))
             (go rest (cdr stack) (+ acc 1))
             #f)]
          [else (go rest stack acc)]))))
  (go xs '() 0))

(check-this '(a b a b))
(check-this '(h e l l o))
(check-this '(6 h a b a 1 h h i 2 b z))
(check-this '(6 h a b a 1 h h i b z 2))
