#lang racket

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (divisible-by? n x)
  (= (remainder x n) 0))

(define (car z)
  (if (divisible-by? 3 z)
      (car (/ z 3))
      (/ (log z) (log 2))))

(define (cdr z)
  (if (divisible-by? 2 z)
      (cdr (/ z 2))
      (/ (log z) (log 3))))

(define pair cons)
(define get-x car)
(define get-y cdr)

; TODO: is it possible to do this with integer math (and percision)?

