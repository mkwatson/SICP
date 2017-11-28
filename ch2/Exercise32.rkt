#lang racket

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))


; (subsets '()) => '(())
; (subsets '(1)) => '(() (1))

; (subsets '(1 2) => '(() (1) (2) (1 2))
; rest = (subsets '(2)) = '(() ((2)))
; (append '(() ((2))) (map ? '(() ((2)))) = '(() (1) (2) (1 2))
; (append '(() ((2)))
;         (map (lambda (x)
;                (cons (car s) x))
;              '(() ((2)))))

; It works because if you have a set1, remove one item, and call this set2.
; The set of all subsets of set1 is the union of the set of all sets of
; set2 with the set of all sets of set2 AND the item you removed.
