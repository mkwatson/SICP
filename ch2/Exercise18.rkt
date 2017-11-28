#lang racket

(define (reverse l)
  (define (reverse-iter in out)
    (if (null? in)
        out
        (reverse-iter (cdr in) (cons (car in) out))))
  (reverse-iter l (list)))

; TODO: is it possible to do this resursively without traversing multiple times?
;(define (reverse items)
;  (if (null? items)
;      items
;      (append (reverse (cdr items))
;              (list (car items)))))