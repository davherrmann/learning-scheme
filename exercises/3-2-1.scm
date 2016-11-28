#lang racket

; differentiate : function number -> function
; finds the derivative of a function f for a small k > 0
(define (differentiate f k)
  (lambda (x)
    (/ (- (f (+ x k))
          (f (- x k)))
       (* 2 k))))

(define (ableiten fkt k) (differentiate fkt k))

(define (sqr x) (* x x))
(define ablsqr (ableiten sqr 0.1))
ablsqr
(ablsqr 1)
(ablsqr 2)
(ablsqr 3)