#lang racket

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define left-branch car)
(define right-branch (compose car cdr))

(define structure (compose car cdr))

(define (total-weight mobile)
  (if (not (pair? mobile)) ; if mobile is actually a simple weight
      mobile
      (+ (total-weight (structure (left-branch mobile)))
         (total-weight (structure (right-branch mobile))))))

(define (balanced? mobile)
  (if (not (pair? mobile)) ; if mobile is actually a simple weight
      #t
      (let ((left (left-branch mobile))
            (right (right-branch mobile)))
        (let ((left-structure (structure left))
              (right-structure (structure right)))
          ; torques of each branch are equal
          (and (= (* (car left)
                     (total-weight left-structure))
                  (* (car right)
                     (total-weight right-structure)))
               ; and all substructures are balanced
               (balanced? left-structure)
               (balanced? right-structure))))))

; TODO: How did other people solve this? I don't like how large this function is.

; I'd need to change my branch selectors and structure function. I believe those
; are the only functions that destructure mobiles and branches