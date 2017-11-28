#lang racket

(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items))
              (list (car items)))))

(define (deep-reverse items)
  (if (or (null? items)
          (not (pair? items)))
      items
      (append (deep-reverse (cdr items))
              (list (deep-reverse (car items))))))

(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1 2) (list 3 4) (list 5 6)))

