#lang racket

(define (make-interval a b) (cons (min a b) (max a b)))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))
(define (print-interval i)
  (display (lower-bound i))
  (display " to ")
  (display (upper-bound i)))

(define (make-center-percent c t)
  (let ((w (* c t)))
    (make-interval (- c w)
                   (+ c w))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (/ (width i)
     (center i)))
