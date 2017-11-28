#lang racket

(define (make-interval a b) (cons (min a b) (max a b)))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))
(define (print-interval i)
  (display (lower-bound i))
  (display " to ")
  (display (upper-bound i)))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; I don't feel like doing this busy work
; Essentially make an interval for all 9 cases
; 9 = 2^4 - 7
; there are 7 cases where the lower bound is pos and
; upper bound neg which is imposisble
(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (let ((xlp (positive? xl))
          (xup (positive? xu))
          (ylp (positive? yl))
          (ylu (positive? yu)))
      (cond
        []
        []
        []
        []
        []
        []
        []
        []
        []))))

(define (div-interval x y)
  (define (spans-zero? i)
    (let ((ub (upper-bound i))
          (lb (lower-bound i)))
      (or
       (= 0 lb)
       (= 0 ub)
       (and (negative? lb)
            (positive? ub)))))
  (if (spans-zero? y)
      (raise "Divide by 0")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))