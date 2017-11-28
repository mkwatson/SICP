#lang racket

(define (make-vect x y)
  (cons x y))

(define xcor-vect car)
(define ycor-vect cdr)

; TODO:
; I originally used a list an called (map + v1 v2)
; but thought the overhead wasn't worth it
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))
