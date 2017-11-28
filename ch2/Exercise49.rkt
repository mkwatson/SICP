#lang racket

(require (planet soegaard/sicp:2:1/sicp))

(define (make-vect x y)
  (cons x y))

(define xcor-vect car)
(define ycor-vect cdr)

(define (op-vect op)
  (lambda (v1 v2)
    (make-vect (op (xcor-vect v1) (xcor-vect v2))
               (op (ycor-vect v1) (ycor-vect v2)))))

(define add-vect (op-vect +))
(define sub-vect (op-vect -))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))

(define (make-segment start end)
  (cons start end))
(define start-segment car)
(define end-segment cdr)

;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame)
;         (start-segment segment))
;        ((frame-coord-map frame)
;         (end-segment segment))))
;     segment-list)))

(define outline
  (segments->painter (list (make-segment (make-vect 0.0 0.0)
                                         (make-vect 0.0 0.99))
                           (make-segment (make-vect 0.0 0.0)
                                         (make-vect 0.99 0.0))
                           (make-segment (make-vect 0.99 0.0)
                                         (make-vect 0.99 0.99))
                           (make-segment (make-vect 0.0 0.99)
                                         (make-vect 0.99 0.99)))))

;(paint outline)

(define x
  (segments->painter (list (make-segment (make-vect 0.0 0.0)
                                         (make-vect 0.99 0.99))
                           (make-segment (make-vect 0.0 0.99)
                                         (make-vect 0.99 0.0)))))

;(paint x)

(define diamond
  (segments->painter (list (make-segment (make-vect 0.5 0.0)
                                         (make-vect 0.99 0.5))
                           (make-segment (make-vect 0.0 0.5)
                                         (make-vect 0.5 0.0))
                           (make-segment (make-vect 0.99 0.5)
                                         (make-vect 0.5 0.99))
                           (make-segment (make-vect 0.0 0.5)
                                         (make-vect 0.5 0.99)))))

;(paint diamond)

; TODO: wave
(define (but-last l)
  (if (null? (cdr l))
      null
      (cons (car l) (but-last (cdr l)))))

(define (connected-segments points)
  (let ((starts (but-last points))
        (ends (cdr points)))
    (map (lambda (start end) (make-segment (make-vect (car start) (cdr start))
                                           (make-vect (car end) (cdr end))))
         starts ends)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define wave
  (let ((left (segments->painter
               (append
                (connected-segments
                 (list (cons 0.4 0.0)
                       (cons 0.7 0.4)
                       (cons 0.7 0.6)
                       (cons 0.4 0.5)
                       (cons 0.0 0.7)))
                (connected-segments
                 (list (cons 0.0 0.8)
                       (cons 0.4 0.6)
                       (cons 0.6 0.7)
                       (cons 0.85 0.7)
                       (cons 0.7 0.85)
                       (cons 0.9 1.0)))
                (connected-segments
                 (list (cons 0.7 0.0)
                       (cons 1.0 0.3)))))))
    (beside left (flip-horiz left))))
  
(paint wave)
