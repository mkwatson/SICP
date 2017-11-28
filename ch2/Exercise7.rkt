#lang racket

(define (make-interval a b) (cons (min a b) (max a b)))

(define (upper-bound i) (cdr i))

(define (lower-bound i) (car i))

(define (print-interval i)
  (display (lower-bound i))
  (display " to ")
  (display (upper-bound i)))