#lang planet neil/sicp

; Exercise 1.1
; 10
; 12
; 8
; 3
; 6
; nothing?
; nothing?
; 19
; #f
; 4
; 16
; 6
; 16

; Exercise 1.2
;(/ (+ 5
;      4
;      (- 2 (- 3 (+ 6 (/ 4 5)))))
;   (* 3
;      (- 6 2)
;      (- 2 7)))

; Exercise 1.3
; We haven't learned `let` yet, so I'm going to using `define`
(define (sum-square-max a b c)
  (define (sum-square x y)
    (define (square n)
      (* n n))
    (+ (square x)
       (square y)))
  (if (> a b)
      (if (> b c)
          (sum-square a b)
          (sum-square a c))
      (sum-square b c)))

; Exercise 1.4
; `a-plus-abs-b` returns a + |b|

; Exercise 1.5
; Applicative order: the expression never returns because the else clause is always evaluated
; Normal order: the expression returns 0 because the else clause if never evaluated

; Exercise 1.6
; All of the prodecudes arguments will be evaluated first (since new-if isn't a special form like if),
; including (sqrt-iter (improve guess x) x)) so it will never return

; Exercise 1.7
; If a number is the same magnitude (or smaller) as 0.001 (the acceptable error), the procedure
; tolerates errors above 100%.
; With sufficiently large square roots, and a guess whose square is off by slightly more than
; the acceptable error, the term (/ x guess) ~~ sqrt(x), so the guess will not be improved.
; Being off by a very small % will still be more than the tolerated error.

(define (sqrt-iter guess last-guess x)
  (if (good-enough? guess last-guess x)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess last-guess x)
  (< (abs (- last-guess guess))
     (* 0.0001 guess)))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 0.0 x))

; OLD:
; (square (sqrt 100000000000000000000000)) => never returns
; (square (sqrt 0.00000000000000000000001)) => 0.0009765625
; NEW:
; (square (sqrt 100000000000000000000000)) => 1.0000000000033257e+23
; (square (sqrt 0.00000000000000000000001)) => 1.0000000000033254e-23

; Exercise 1.8
(define (cube-iter guess last-guess x)
  (if (good-enough? guess last-guess x)
      guess
      (cube-iter (improve-cube guess x) guess x)))

(define (improve-cube guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cube-root x)
  (cube-iter 1.0 0.0 x))

; Exercise 1.9
;(if (= 4 0) 5 (inc (+ (dec 4) 5)))
;(inc (+ 3 5))
;(inc (if (= 3 0) 5 (inc (+ (dec 3) 5))))
;(inc (inc (+ 2 5)))
;(inc (inc (if (= 2 0) 5 (inc (+ (dec 2) 5)))))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (if (= 1 0) 5 (inc (+ (dec 1) 5))))))
;(inc (inc (inc (inc (+ (dec 1) 5)))))
;(inc (inc (inc (inc (if (= 0 0) 5 (inc (+ (dec 0) 5)))))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9
; Recursive

;(if (= 4 0) 5 (+ (dec 4) (inc 5)))
;(+ 3 6)
;(if (= 3 0) 6 (+ (dec 3) (inc 6)))
;(+ 2 7)
;(if (= 2 0) 7 (+ (dec 2) (inc 7)))
;(+ 1 8)
;(if (= 1 0) 8 (+ (dec 1) (inc 8)))
;(+ 0 9)
;(if (= 0 0) 9 (+ (dec 0) (inc 9)))
;9
; Iterative

; Exercise 1.10
;(A 1 10) => 1024
;(A 2 4) => 65536
;(A 3 3) => 65536

;(f n) computes 2n
;(g n) computes 2^n
;(h n) computes 2^2^2^... where there are n 2s

; TODO with group: count-change generates a tree-recursive process. It is not obvious how to design a better algorithm for computing the result, and we leave this problem as a challenge.

; Exercise 1.11
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (* 1 (f-recursive (- n 1)))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

(define (f-iterative n)
  (f-iter 0 1 2 n))
(define (f-iter a b c count)
  (if (= count 0)
      a
      (f-iter b
              c
              (+ c (* 2 b) (* 3 a))
              (- count 1))))

; Exercise 1.12
(define (pascal r c)
  (if (or (= c 1) (= c r))
      1
      (+ (pascal (- r 1) (- c 1))
         (pascal (- r 1) c))))

; Exercise 1.13
; (on paper, I took a photo though!)

; Exercise 1.14
; steps: O(n^2)
; space: O(n)

; Exercise 1.15
; a:
; 12.15 * (1/3)^x = 0.1
; x > 4.37, so p is called 5 times
; b: input can be 3x before another step/space is needed
; space: O(log n)
; steps: O(log n)

; Exercise 1.16
(define (even? n)
  (= (remainder n 2) 0))

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

; Exercise 1.17
(define (half x) (/ x 2))
(define (double x) (* x 2))

;(define (fast-* a b)
;  (cond ((= b 1) a)
;        ((even? b) (double (fast-* a (half b))))   ; or should this be (fast-* (double a) (half b))?
;        (else (+ a (fast-* a (- b 1))))))

; Exercise 1.18
(define (fast-* a b)
  (fast-*-iter a b 0))

(define (fast-*-iter a counter sum)
  (cond ((= counter 0) sum)
        ((even? counter) (fast-*-iter (double a) (half counter) sum))
        (else (fast-*-iter a (- counter 1) (+ a sum)))))
; should we also handle if a is even?

; Exercsie 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q)) ; p prime
                   (+ (square q) (* 2 q p))  ; q prime
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; Exercise 1.20
; applicative-order:
; (gcd 206 40)
; (gcd 40 (remainer 206 40)) 1
; (gcd 40 6)
; (gcd 6 (remainder 40 6)) 2
; (gcd 6 4)
; (gcd 4 (remainder 6 4)) 3
; (gcd 4 2)
; (gcd 2 (remainder 4 2)) 4 times
; (gcd 2 0)

; normal-order
; this one hurt my eyes
;(gcd 206 40)
;(gcd 40 (remainder 206 40))
;(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;(gcd (remainder 40 (remainder 206 40))
;     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;     (remainder (remainder 40 (remainder 206 40))
;                (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;(gcd (remainder (remainder 40 (remainder 206 40))
;                (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;     (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;                (remainder (remainder 40 (remainder 206 40))
;                           (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))
; 19 times

; Exercise 1.21
;(define (smallest-divisor n)
;   (find-divisor n 2))
;(define (find-divisor n test-divisor)
;   (cond ((> (square test-divisor) n) n)
;         ((divides? test-divisor n) test-divisor)
;         (else (find-divisor n (+ test-divisor 1)))))
;(define (divides? a b)
;   (= (remainder b a) 0))
;(define (prime? n)
;(= n (smallest-divisor n)))
;(smallest-divisor 199)
;199
;(smallest-divisor 1999)
;1999
;(smallest-divisor 19999)
;7

; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
;(define (start-prime-test n start-time)
;  (if (prime? n)
;    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (timed-prime-test start)
  (if (< start end)
      (search-for-primes (+ 2 start) end)))

; Bigger than 1,000: 1009 (4), 1013 (4), 1019 (3)
; Bigger than 10,000: 10007 (9), 10009 (8), 10037 (8)
; Bigger than: 100,000: 100003 (48), 100019 (33), 100043 (33)
; Bigger than 1,000,000: 1000003 (92), 1000033 (92), 1000037 (92)

; Exercise 1.23
(define (smallest-divisor n)
  (find-divisor n 2))

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

; Bigger than 1,000: 1009 (2), 1013 (2), 1019 (3)
; Bigger than 10,000: 10007 (7), 10009 (6), 10037 (7)
; Bigger than: 100,000: 100003 (18), 100019 (18), 100043 (18)
; Bigger than 1,000,000: 1000003 (44), 1000033 (44), 1000037 (44)
; Confirmed! It took ~1/2 as long.

; Exercise 1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-prime (- (runtime) start-time))))

; I would expected searching for primes near 1,000,000 to take 2x as long (log(1,000,000)/log(1,000))
; It appears to be correct!

; Exercise 1.25
; She is correct, it would work.
; However, although both grow logrithmically, expmod never deals with numbers greater than m, so the arithmatic is much quicker.

; Exercise 1.26
; His version never removes half of the tree, square removes ~1/2 of the calls to expmod.

; Exercise 1.27
(define (try-it n test)
  (= (expmod test n n) test))

(define (carmichael? n)
  (carmichael-iter n 1))

(define (carmichael-iter n test)
  (cond ((= test n) #t)
        ((try-it n test) (carmichael-iter n (+ test 1)))
        (else #f)))

; Exercise 1.28
(define (non-trivial-square? x n)
  (if (and (not (or (= x 1)
                    (= x (- n 1))))
           (= (remainder (square x) n)
              1))
      #t
      #f))

(define (miller-rabin-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (if (non-trivial-square? exp m)
                         0
                         (remainder
                          (square (miller-rabin-expmod base (/ exp 2) m))
                          m)))
        (else
         (remainder
          (* base (miller-rabin-expmod base (- exp 1) m))
          m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (miller-rabin-expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))






