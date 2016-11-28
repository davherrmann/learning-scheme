#lang racket

(define (make-as as)
  (define (dispatch m d)
      (cond ((eq? m 'null?) (null? as))
            (else (error "no handler for message " m))))
  dispatch)

(define (make-empty-as) (make-as '()))

(define a1 (make-empty-as))
(a1 'null? 'dummy)