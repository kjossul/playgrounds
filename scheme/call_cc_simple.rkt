#lang racket

(+ 3  ;; this is the continuation
   (call/cc
    (lambda (exit)  ;; exit stores the continuation
      (for-each ( 
                 lambda (x)
                  (when (negative? x)
                    (exit x))) ;; exit == (+ 3) -> (+ 3 -3) = 0
                '(54 0 37 -3 245 19))
      10)))  ;; would get (+ 3 10) if no negative value found
