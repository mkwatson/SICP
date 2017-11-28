#lang racket

; a = [Ca*(1-Ta) ... Ca(1+Ta)]
; b = [Cb*(1-Tb) ... Cb(1+Tb)]
; a*b = [Ca*(1-Ta)*Cb*(1-Tb) ... Ca(1+Ta)*Cb(1+Tb)]
; = [Ca*Cb*(1-Ta-Tb-TaTb) ... Ca*Cb*(1+Ta+Tb+TaTb)]

; TaTb ~~ 0
; a*b ~~ [Ca*Cb*(1-Ta-Tb) ... Ca*Cb*(1+Ta+Tb)]

; width(a*B) ~~ (Ca*Cb*(1+Ta+Tb) - Ca*Cb*(1-Ta-Tb)) / 2
; = Ca*Cb*(Ta+Tb)

; center(a*b) ~~ (Ca*Cb*(1+Ta+Tb) + Ca*Cb*(1-Ta-Tb)) / 2
; = Ca*Cb

; tollerance = width / center
; tollerance(a*b) ~~ Ca*Cb*(Ta+Tb) / Ca*Cb
; = Ta+Tb
