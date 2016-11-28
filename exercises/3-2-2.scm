#lang racket

; differentiate : function number -> function
; finds the derivative of a function f for a small k > 0
(define (differentiate f k)
  (lambda (x)
    (/ (- (f (+ x k))
          (f (- x k)))
       (* 2 k))))

; next-x : function function number -> number
; finds an x value closer to the zero of function f, given f, its derivative and a value x
(define (next-x f fd x) (- x
                      (/ (f x)
                         (fd x))))

; find-ns : function function number -> number
; finds the zero of a function given the function, its derivative and an x value
(define (find-ns f fd x0)
  (let ((x1 (next-x f fd x0)))
    (if (< (abs (- x0 x1)) 0.0001)
        x1
        (find-ns f fd x1))))

; function -> number -> number
; creates a function for finding the zero of the given function
(define (make-nstf f)
  (lambda (x0)
    (find-ns f (differentiate f 0.1) x0)))

(define (myfunc x) (- (* 3 x x) 2))
(define nstf (make-nstf myfunc))
(nstf 2)
(nstf -2)

