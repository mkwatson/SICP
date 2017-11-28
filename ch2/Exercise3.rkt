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

; where make-rectangle's inputs are oppisite corners
(define make-rectangle cons)

(define first-corner car)
(define second-corner cdr)

(define (print-rectangle r)
  (let ((first-point (first-corner r))
        (third-point (second-corner r)))
    (display "(")
    (print-point first-point)
    (display ",")
    (print-point (make-point (x-point first-point) (y-point third-point)))
    (display ",")
    (print-point third-point)
    (display ",")
    (print-point (make-point (x-point third-point) (y-point first-point)))
    (display ")")))

(define (difference x y) (abs (- x y)))
(define (perimeter r)
  (let ((first-point (first-corner r))
        (second-point (second-corner r)))
    (* (+ (difference (x-point first-point)
                      (x-point second-point))
          (difference (y-point first-point)
                      (y-point second-point)))
          2)))
(define (area r)
  (let ((first-point (first-corner r))
        (second-point (second-corner r)))
    (* (difference (x-point first-point)
                   (x-point second-point))
       (difference (y-point first-point)
                      (y-point second-point)))))

; functions:
; (define (make-rect s1 s2)) where s1 and s2 are segments
; (define (make-rect x y)) where x and y are points representing oppisite corners
; what happens if you try to make a rectangle with incoherent line segments?
; what if you want a rectangle for given corners that's rotated?