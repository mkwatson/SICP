#lang racket

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))

(define empty-board null)

(define (make-position row col)
   (cons row col))

(define pos-row car)

(define pos-col cdr)

(define (safe? col queen-positions)
  (let ((test-queen (car (filter (lambda (p) (= col (pos-col p)))
                                 queen-positions)))
        (other-queens (filter (lambda (p) (not (= col (pos-col p))))
                              queen-positions)))
    (define (test-safe other-queen)
      (let ((this-row (pos-row test-queen))
            (this-col (pos-col test-queen))
            (other-row (pos-row other-queen))
            (other-col (pos-col other-queen)))
        (not (or (= this-row other-row)
                 (= this-col other-col)
                 (= (abs (- this-row other-row))
                    (abs (- this-col other-col)))))))
    (foldl (lambda (x y) (and x y))
           true
           (map test-safe other-queens))))

(define (adjoin-position row col rest-of-positions)
  (cons (make-position row col) rest-of-positions))

; this caused me a lot of trouble
(define (enumerate-interval start end)
  (range start (+ end 1)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))