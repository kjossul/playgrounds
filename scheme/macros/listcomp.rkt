#lang racket

(define (flatten LL)
  (define (help L acc)
    (if (empty? L)
      acc
      (help (cdr L) (append acc (car L)))))
  (help LL '()))
  
(define (concat-map f xs)
  (flatten (map f xs)))

;(concat-map (lambda (x) (list (+ x 1))) '(1 2 3))  ; '(1 2 3)

(define-syntax listcomp
  (syntax-rules (<-)
    ; base: only one generator
    ((_ expr ((var <- xs)))
     (concat-map (lambda (var) (list expr)) xs))
    ; more than one generator: recursive call, the leftmost becomes a parameter to be used for right generators
    ((_ expr ((var <- xs) . rest))
     (concat-map (lambda (var) (listcomp expr rest)) xs))
    ))

(listcomp (* x x) ((x <- '(1 2 3))))
(listcomp (+ x y z) ((x <- '(100 200 300)) (y <- '(10 20 30)) (z <- '(1 2 3))))
; Think of it as nested for loops: The rightmost is the "most nested" one
; z "unpacks" [[111], [112], [113]] into [111, 112, 113] and so on
; y "unpacks" the higher level, i.e. [[111, 112, 113], [121, 122, 123], [131, 132, 133]] into a list of 9 numbers
; x gives the final result