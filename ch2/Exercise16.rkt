#lang racket

; I think that if you could optimze the forms before
; evaluation, and had every simplification rule at hand,
; you could reduce every expression to a list of "atomic"
; expressions. So that every mathematically equivalent
; expression would reduce to the same expression before
; they are evaluated. In practice this seems difficult,
; if not impossible.