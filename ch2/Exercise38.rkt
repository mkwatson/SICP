#lang racket

(foldr / 1 (list 1 2 3))
; 3/2
(foldl / 1 (list 1 2 3))
; 3/2
(foldr list null (list 1 2 3))
; '(1 (2 (3 ())))
(foldl list null (list 1 2 3))
; '(3 (2 (1 ())))

; The op should be associative and commutative