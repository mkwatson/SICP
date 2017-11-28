#lang racket

;(define (tree-map fn tree)
;  (cond ((null? tree) tree)
;        ((not (pair? tree)) (fn tree))
;        (else (cons (tree-map fn (car tree))
;                    (tree-map fn (cdr tree))))))

(define (tree-map fn tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fn sub-tree)
             (fn sub-tree)))
       tree))
