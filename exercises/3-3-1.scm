#lang racket

; as-find : pair list -> list
; gives a list of values of all pairs in l with the key s
(define (as-find s l)
  (if (null? l)
      '()
      (if (eq? (caar l) s)
          (cons (cadar l) (as-find s (cdr l)))
          (as-find s (cdr l)))))

(as-find 'b '((a 1) (b 7) (c 4) (a 6) (c 5) (d 4) (b 2)))
(as-find 'a '((a 1) (b 7) (c 4) (a 6) (c 5) (d 4) (b 2)))
(as-find 'e '((a 1) (b 7) (c 4) (a 6) (c 5) (d 4) (b 2)))