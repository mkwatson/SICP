#lang racket

(define (same-parity . l)
  (let ((f (if (even? (car l))
               even?
               odd?)))
    (filter f l)))