#lang racket

(define (make-interval a b) (cons (min a b) (max a b)))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))
(define (print-interval i)
  (display "[")
  (display (lower-bound i))
  (display ", ")
  (display (upper-bound i))
  (display "]"))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

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

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define a (make-center-percent 100 0.001))
(define b (make-center-percent 1000 0.001))

;> (print-interval (div-interval a a))
;[0.9980019980019981, 1.002002002002002]
;> (print-interval (div-interval b b))
;[0.998001998001998, 1.002002002002002]
;> (print-interval (div-interval a b))
;[0.0998001998001998, 0.10020020020020019]

;> (print-interval (par1 a b))
;[90.6367269094542, 91.18218218218216]
;> (print-interval (par2 a b))
;[90.81818181818181, 91.00000000000001