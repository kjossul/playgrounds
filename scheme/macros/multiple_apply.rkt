#lang racket

(define-syntax multiple-apply
  (syntax-rules (to)
                ((_ (f1 ...) to l1 ...)
                 (list (apply f1 l1) ...))))


(multiple-apply (+ - *) to '(1 2 3) '(4 3) '(5 6))
