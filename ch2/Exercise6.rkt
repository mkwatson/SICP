#lang racket

; zero is defined as the identity function

(define zero (lambda (f) (lambda (x) x)))

;(add-1 zero)
;(lambda (f) (lambda (x) (f ((zero f) x))))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
;(lambda (f) (lambda (x) (f x)))

(define one
  (lambda (f) (lambda (x) (f x))))

;(add-1 one)
;(lambda (f) (lambda (x) (f ((one f) x))))
;(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
;(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
;(lambda (f) (lambda (x) (f (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

;(add-1 n) => (lambda (f) (lambda (x) (f ((n f) x))))
;n => ((n f) x) = (f (f ... (f x)))) n times

; first call f b times, then wrap that with a more calls
(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(define (inc n) (+ n 1))
(define (church->int c)
  ((c inc) 0))

