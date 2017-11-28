#lang racket

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (average x y) (/ (+ x y) 2))
(define (midpoint-segment s)
  (let ((start-point (start-segment s))
        (end-point (end-segment s)))
    (make-point (average (x-point start-point) (x-point end-point))
                (average (y-point start-point) (y-point end-point)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))