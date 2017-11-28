#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list j i))
                            (range 1 i)))
           (range 1 (+ n 1))))

(define (unique-triples n)
  (flatmap (lambda (x) (map (lambda (y) (append y (list x)))
                            (unique-pairs (- x 1))))
           (range 1 (+ n 1))))

(define (make-triple-sum triple)
  (list (car triple) (cadr triple) (caddr triple)
        (+ (car triple) (cadr triple) (caddr triple))))

(define (triple-sum-equals n s)
  (define (sum-equals x)
    (lambda (y) (= x (foldl + 0 y))))
  (map make-triple-sum
       (filter (sum-equals s) (unique-triples n))))
