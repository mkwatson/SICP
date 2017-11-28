#lang racket

;; For Addition:

; a = [a1 ... a2]
; wa = a2 - a1

; b = [b1 ... b2]
; wb = b2 - b1

; asb = a - b = [(a1-b2) ... (a2-b1)]
; wasb = (a2-b1) - (a1-b2)
; = a2 - a1 + b2 - b1
; = wa + wb

;; For multiplication:

; a = [-1 ... 1]
; wa = 2
; b = [10 ... 20]
; wb = 10
; ab = [-20 ... 20]
; wab = 40