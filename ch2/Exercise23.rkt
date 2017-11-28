#lang racket

(define (for-each f items)
  (if (null? items)
      null
      (begin
        (f (car items))
        (for-each f (cdr items)))))