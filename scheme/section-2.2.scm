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

; Exercise 2.20
(define (same-parity head . tail)
  (let ((good? (if (odd? head)
                 odd? 
                 even?)))
    (define (iter l a)
      (if (null? l)
        a
        (iter (cdr l) (if (good? (car l))
                        (cons (car l) a)
                        a))))
    (cons head
          (reverse (iter tail nil)))))

