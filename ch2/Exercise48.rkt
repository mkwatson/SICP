#lang racket

(define (make-vect x y)
  (cons x y))

(define xcor-vect car)
(define ycor-vect cdr)

(define (op-vect op)
  (lambda (v1 v2)
    (make-vect (op (xcor-vect v1) (xcor-vect v2))
               (op (ycor-vect v1) (ycor-vect v2)))))

(define add-vect (op-vect +))
(define sub-vect (op-vect -))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))

(define (make-segment start end)
  (cons start end))
(define start-segment car)
(define end-segment cdr)
