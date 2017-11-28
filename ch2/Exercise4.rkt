#lang racket

(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;> (cdr (cdr (cons 1 (cons 2 3))))
;3
;> (car (cdr (cons 1 (cons 2 3))))
;2

