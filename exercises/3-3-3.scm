#lang racket

; as-find : pair list -> list
; gives a list of values of all pairs in l with the key s
(define (as-find s l)
  (if (null? l)
      '()
      (if (eq? (caar l) s)
          (cons (cadar l) (as-find s (cdr l)))
          (as-find s (cdr l)))))

; creates a message dispatcher for the given associative list
(define (make-as as)
  (define (dispatch m d)
    (let ((f? (lambda (f) (eq? m f))))
      (cond ((f? 'null?) (null? as))
            ((f? 'car) (if (null? as)
                           '()
                           (car as)))
            ((f? 'cdr) (if (null? as)
                           '()
                           (make-as (cdr as))))
            ((f? 'cons) (if (pair? d)
                            (make-as (cons d as))
                            (error "given argument is not a valid (key value) pair")))
            ((f? 'find) (as-find d as))
            (else (error "no handler for message " m)))))
  dispatch)

; creates a message dispatcher for an empty associative list
(define (make-empty-as) (make-as '()))

; returns an associative list without the message dispatching functionality
(define (as-show as)
  (let ((head (as 'car 'd)) (tail (as 'cdr 'd)))
    (if (null? tail)
        head
        (cons head (as-show tail)))))

(define a1 (make-empty-as))
(define a2 (a1 'cons '(b 2)))
(define a3 (a2 'cons '(d 4)))
(define a4 (a3 'cons '(c 5)))
(define a5 (a4 'cons '(a 6)))
(define a6 (a5 'cons '(c 4)))
(define a7 (a6 'cons '(b 7)))
(define a8 (a7 'cons '(a 1)))

(as-show a8)

