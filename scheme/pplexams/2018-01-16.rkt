#lang racket

(define test '(#\* 1 2 3 #\* #\$ "hello" #\* 1 #\* 7 "my" #\* 1 2 #\* "world" #\$))

(define (p xs)
  (define (go xs flagn flags accn accs result)
    (if (null? xs)
      result
      (let ((x (car xs)))
        (cond
          [(and (number? x) flagn)
           (go (cdr xs) flagn flags (+ accn x) accs result)]
          [(and (string? x) flags)
           (go (cdr xs) flagn flags accn (string-append accs x) result)]
          [(eq? x #\*)
           (if flagn
             (go (cdr xs) (not flagn) flags 0 accs (append result (list accn)))
             (go (cdr xs) (not flagn) flags 0 accs result))]
          [(eq? x #\$)
           (if flags
             (go (cdr xs) flagn (not flags) accn "" (append result (list accs)))
             (go (cdr xs) flagn (not flags) accn "" result))]
          [else (go (cdr xs) flagn flags accn accs result)]))))
  (go xs #f #f 0 "" '()))

(p test)
