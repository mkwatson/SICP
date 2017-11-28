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

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (below painter1 painter2) 
  (let ((split-point (make-vect 0.0 0.5))) 
    (let ((paint-bottom 
           (transform-painter painter1 
                              (make-vect 0.0 0.0) 
                              (make-vect 1.0 0.0) 
                              split-point)) 
          (paint-top 
           (transform-painter painter2 
                              split-point 
                              (make-vect 1.0 0.5) 
                              (make-vect 0.0 1.0)))) 
      (lambda (frame) 
        (paint-bottom frame) 
        (paint-top frame)))))

;(paint (below einstein einstein))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (below2 painter1 painter2)
  (let ((einstein90 (rotate90 einstein)))
    (rotate270 (beside einstein90 einstein90))))

;(paint (below2 einstein einstein))

