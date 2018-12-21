#lang racket

; Fixed point analysis
; state = initial state
; equality = equality function
; return = a function that takes a function as parameter and returns a pair (Bool, v),
; where the boolean indicates if the fixed point is reached (prev computation equals to the current one)
(define (ap state equality)
  (let ((local state))
    (lambda (f)
      (let ((new (f local)))  ; first error: we need two lets
        (let ((flag (equality new local)))
          (unless flag        ; second error: update state every time
            (set! local new))
          (cons flag new))))))

(define (g f v equality)
  (let ((alpha (ap v equality)))
    (let beta ((v (alpha f)))  ; named let
      (call/cc
        (lambda (done)
          (when (car v)
            (done (cdr v)))
          (beta (alpha f)))))))

(define (g1 f v equality)
  (let ((alpha (ap v equality)))
    (define (beta)
      (let ((v (alpha f)))
        (if (car v)
          (cdr v)
          (beta))))
    (beta)))

(g1 (lambda (x) (/ x 2)) 21 (lambda (x y) (zero? x)))
