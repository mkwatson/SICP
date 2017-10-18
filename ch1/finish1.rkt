#lang racket

; Exercise 1.29
;(define (sum term a next b)
;  (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))
(define (cube x) (* x x x))
;(define (integral f a b dx)
;  (define (add-dx x)
;    (+ x dx))
;  (* (sum f (+ a (/ dx 2.0)) add-dx b)
;     dx))
;(integral cube 0 1 0.01)
;0.24998750000000042
;(integral cube 0 1 0.001)
;0.249999875000001

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (plus2h x)
    (+ x (* h 2.0)))
  (define (y k)
    (f (+ a (* k h))))
  (* (+ (y 0.0)
        (* 4.0 (sum f (+ a h) plus2h (- b h)))
        (* 2.0 (sum f (plus2h a) plus2h (- b (* h 2))))
        (y n))
     (/ h 3.0)))

;(integral cube 0 1 100)
;0.23078806666666699
;(integral cube 0 1 1000)
;0.24800798800666748
;(integral cube 0 1 10000)
;0.2499999999999509

; Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; Exercise 1.31
; a
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)
(define (inc x) (+ x 1.0))

(define (factorial n)
  (product identity 2.0 inc n))

; These numbers get too big
;(define (estimate-pi n)
;  (define (double x) (* x 2.0))
;  (define (double-add1 x) (+ (double x) 1.0))
;  (define (square x) (* x x))
;  (* (/ (* 2.0
;           (square (product double 2.0 inc (/ n 2.0)))
;           (double (+ (/ n 2.0) 1.0)))
;        (square (product double-add1 1.0 inc (/ n 2.0))))
;     4))
(define (estimate-pi n)
  (define (term n)
    (if (odd? n)
        (/ (+ n 1.0)
           (+ n 2.0))
        (/ (+ n 2.0)
           (+ n 1.0))))
  (* 4 (product term 1 inc n)))

; b
(define (product-recurse term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recurse term (next a) next b))))

; Exercise 1.32
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum-2 term a next b)
  (accumulate + 0 term a next b))

(define (product-2 term a next b)
  (accumulate * 1 term a next b))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (product-recurse term (next a) next b))))


; Exercise 1.33
(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (filter a)
                           (combiner result (term a))
                           result))))
  (iter a null-value))

; part a
(define (smallest-divisor n)
  (find-divisor n 2))
(define (square x) (* x x))
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-square-primes a b)
  (filtered-accumulate + prime? 0 square a inc b))

; part b
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-relative-prime a)
  (define (relative-prime? n)
    (= 1 (gcd n a)))
  (filtered-accumulate * relative-prime? 1 identity 1 inc a))

; Exercise 1.34
; Is will not halt, the program will endlessly add to the call stack (for each function application) until it overflows.

; Exercise 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define phi (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; Exercise 1.36
(define (fixed-point-debug f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (println next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

; (fixed-point-debug (lambda (x) (/ (log 1000) (log x))) 5.0)
; 28 tries

(define (average x y) (/ (+ x y) 2))
;(fixed-point-debug (lambda (x) (average (/ (log 1000) (log x)) x)) 5.0)
; 8 tries

; Exercise 1.37
(define (dec n) (- n 1))
(define (cont-frac n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter (dec i) (/ (n i)
                         (+ (d i) result)))))
  (iter k 0.0))

; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
; 11 tries

(define (cont-frac-recur n d k)
  (if (< k 1)
      0
      (/ (n k)
         (+ (d k) (cont-frac-recur n d (dec k))))))

; Exercise 1.38
(define (e-cf k)
  (define (n k) 1.0)
  (define (d k)
    (let ((next (inc k)))
      (if (= 0 (remainder next 3))
          (* 2.0 (/ next 3.0))
          1.0)))
  (+ 2
     (cont-frac n d k)))

; Exercsie 1.39
(define (tan-cf x k)
  (cont-frac (lambda (k) (if (> k 1) (* -1 (square x)) x))
             (lambda (k) (- (* k 2) 1))
             k))

; Exercise 1.40
(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x))
                 dx)))
(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x)
                 (* a x x)
                 (* b x)
                 c)))

; Exercise 1.41
(define (double f)
  (lambda (x) (f (f x))))

; inc is applied 2^2^2 times (or 16 times), so the result is 21

; Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

; Exercise 1.43
(define (repeated f n)
  (if (< n 1)
      identity
      (compose f (repeated f (dec n)))))

; Exercise 1.44
(define (smooth f)
  (lambda (x)
    (let ((a (f (- x dx)))
          (b (f (+ x dx))))
      (average a b))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; Exercise 1.45
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
                             1.0))
(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b counter product)
  (cond ((= counter 0) product)
        ((even? counter) (fast-expt-iter (square b)
                                         (/ counter 2)
                                         product))
        (else (fast-expt-iter b
                              (- counter 1)
                              (* b product)))))

;(define (nth-root x n d)
;  (fixed-point ((repeated average-damp d) (lambda (y) (/ x (fast-expt y (dec n)))))
;               1.0))
; sqrt needs one dampening
; cube root needs one
; 4th root needs 2
; 5th needs 2
; 6th needs 2
; 7th needs 2
; 8th neeeds 3

(define (number-of-dampenings r)
  (floor (/ (log r) (log 2))))

(define (nth-root x n)
  (let ((dampenings (number-of-dampenings n)))
    (fixed-point ((repeated average-damp dampenings) (lambda (y) (/ x (fast-expt y (dec n)))))
                 1.0)))

; Exercise 1.46
(define (iterative-improve good-enough? improve-guess)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve-guess guess))))
  iter)

(define (sqrt-2 x)
  (define (good-enough? guess)
    (< ( abs (- (square guess) x)) 0.001))
  (define (improve-guess guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve-guess) 1.0))

(define (fixed-point-2 f first-guess)
  (define (good-enough? guess)
    (let ((next (f guess)))
      (< (abs (- next guess)) tolerance)))
  ((iterative-improve good-enough? f) 1.0))



  

