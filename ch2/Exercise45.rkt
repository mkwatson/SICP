#lang racket

(require (planet soegaard/sicp:2:1/sicp))

(define (split main-f minor-f)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split main-f minor-f) painter (- n 1))))
          (main-f painter (minor-f smaller smaller))))))

(define up-split (split below beside))

;(paint (up-split einstein 3))
