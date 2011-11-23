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

; Exercise 2.19
(define (no-more? coin-values)
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

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

; Exercise 2.21
(define (square-list items)
  (if (null? items)
    nil
    (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

; Exercise 2.23
(define (for-each func items)
  (cond ((null? items) #t)
        ((func (car items))
         (for-each func (cdr items)))))

; Exercise 2.27
(define (deep-reverse items)
  (display items)(newline)
  (cond ((null? items) nil)
        ((not (pair? items)) items)
        (else (append (deep-reverse (cdr items))
                      (list (deep-reverse (car items)))))))
