#lang racket

(define (co-sublist xs i j)
  (define (go xs k acc)
    (cond [(empty? xs) acc]
          [(and (>= k i) (<= k j)) (go (cdr xs) (+ k 1) acc)]
          [else (go (cdr xs) (+ k 1) (append (list (car xs)) acc))]))
  (go xs 0 '()))

(co-sublist '(1 2 3 4 5 6) 1 3)

(define-syntax subl
  (syntax-rules (-> <-)
    ((_ -> x ... <-) '(x ...))  ; Note that order matters (this needs to be first)
    ((_ -> x ... y)  (subl -> x ...))
    ((_ x y ...)  (subl y ...))))
(subl 1 -> 2 3 4 <- 5 6)
