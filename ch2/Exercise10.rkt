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

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

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

