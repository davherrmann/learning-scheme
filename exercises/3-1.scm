#lang racket

; insert : list number -> list
; inserts element into sorted non-empty list
(define (insert sorted element)
  (if (null? sorted)
      (list element)
      (let ((head (car sorted)))
      (if (> head element)
          (cons element sorted)
          (cons head (insert (cdr sorted) element))))))

; insert-sort2 : list list -> list
; inserts all elements of unsorted list into sorted list
(define (insert-sort2 uL sL)
  (if (null? uL)
      sL
      (insert-sort2 (cdr uL) (insert sL (car uL)))))

; insert-sort : list -> list
; sorts a list with per insert-sort
(define (insert-sort l) (insert-sort2 l '()))

(insert-sort '(2 4 1 6 4))