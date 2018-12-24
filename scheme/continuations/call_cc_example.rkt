#lang racket

(define saved-cont #f) ; place to save k

(define (test-cont)
  (let ((x 0))
    (call/cc
     (lambda (k) ; k contains the continuation
       (set! saved-cont k))) ; here is saved
    ;; this *is* the continuation
    (set! x (+ x 1))
    (display x)
    (newline)))

(test-cont) ;; => 1
(saved-cont) ;; => 2
(define other-cont saved-cont)
(test-cont) ;; => 1 ( here we reset saved-cont )
(other-cont) ;; => 3 ( other is still going ...)
(saved-cont) ;; => 2

;; If we put the same instructions inside a function we get an infinite loop
(define (inf-loop)
  (test-cont)
  (display "Called test-cont\n")
  (saved-cont)  ;; saved-cont will also include the first test-cont
  (display "Never called"))

;; (inf-loop)