#lang racket

(define (reverser sequence)
  (foldr (lambda (x y) (append y (list x))) null sequence))

(define (reversel sequence)
  (foldl (lambda (x y) (cons x y)) null sequence))

