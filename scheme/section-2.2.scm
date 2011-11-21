#lang planet neil/sicp

; Exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))

; Exercise 2.18
(define (reverse l)
  (define (iter l a)
    (if (null? l)
      a
      (iter (cdr l) (cons (car l) a))))
  (iter l nil))

