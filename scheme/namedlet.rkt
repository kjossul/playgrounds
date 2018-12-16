;; examples for named let semantics 
;; MPradella, MMXIV

#lang racket

(define (loop-no-vars)
  (let ((x 0))
    (let label ()     ;; () because no variables are used inside the loop 
      (when (< x 10)  ;; for (; x < 10; x++) printf("%d\n", x);
        (display x)
        (newline)
        (set! x (+ 1 x))
        (label))
      (display x)     ;; x will be the same in all calls: 10-10-10..
      (display "-")))
  )

(define (loop-1-var)
 (let label ((x 0))   ;; x scope is inside the loop
   (when (< x 10)     ;; for (int x = 0; x < 10; x++) printf("%d\n", x;
     (display x)
     (newline)
     (label (+ x 1)))
   (display x)
   (display "-"))     ;; Every "loop" will have a different value: 10-9-8..
 )

(loop-no-vars)
(display "\n")
(loop-1-var)
