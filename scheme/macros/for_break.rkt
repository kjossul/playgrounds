#lang racket

;; For/break example
;; MPradella MMXV

;; The idea is to introduce a For loop with a break-like statement, like:
;; (For i from 1 to 10
;;      do
;;      (displayln i)
;;      (when (= i 5) 
;;        (break)) 
;;      )
;; Problem with hygenic macros: we need to be able to access to the parameter
;; containing the escape continuation. This is not so easy with syntax-rules.
;; 
;; A possible solution:

(define-syntax For
  (syntax-rules (from to break: do)
    ((_ var from min to max break: break-sym 
        do body ...)
     (call/cc (lambda (break-sym)  ;; stores the continuation (i.e. the rest of the program)
                (let ((inc (if (< min max) + -)))
                  (let loop ((var min))
                    body ...
                    (unless (= var max)
                      (loop (inc var 1))))))))))

;; let's try it:
(For i from 1 to 10 break: get-out
     do
     (displayln i)
     (when (= i 5) 
       (get-out)) ;; call the continuation
     )

(display "Hey it's me ur continuation!\n")
