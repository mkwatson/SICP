#lang racket

(define (fringe items)
  (if (or (null? items)
          (not (pair? (car items))))
      items
      (append (fringe (car items))
              (fringe (cdr items)))))

(define x (list (list 1 2) (list 3 4)))

