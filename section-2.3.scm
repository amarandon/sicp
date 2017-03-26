#lang planet neil/sicp

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


; Exercise 2.53

(list 'a 'b 'c)
; '(a b c)

(list (list 'george))
; '('(george))

(cdr '((x1 x2) (y1 y2)))
; '('(y1 y2))

(cadr '((x1 x2) (y1 y2)))
; '(y1 y2)

(pair? (car '(a short list)))
; #f

(memq 'red '((red shoes) (blue socks)))
; #f

(memq 'red '(red shoes blue socks))
; '(red shoes blue socks)


; Exercise 2.53
(define (equal? a b)
  (cond ((and
           (and (symbol? a) (symbol? b))
           (eq? a b))
         #t)
        ((and (number? a) (number? b))
         (eq? a b))
        ((and (null? a) (null? b))
         #t)
        ((and
           (and (list? a) (list? b))
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
         #t)
        (else #f)))


; Section 2.3.2

; Execises 2.56, 2.57

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))
  

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (cond ((null? (cddr s)) 0)
        (else (cons '+ (cddr s)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (cond ((null? (cddr p)) 1)
        (else (cons '* (cddr p)))))

(define (base p) (cadr p))


(define (make-exponentiation a1 a2)
  (cond ((=number? a2 0) 1)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (expt a1 a2))
        (else (list '** a1 a2))))


(define (make-substraction a1 a2)
  (cond ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (- a1 a2))
        (else (list '- a1 a2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (exponent p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation
                                       (base exp)
                                       (make-substraction exp 1)))
                       (deriv (base exp) var)))
        (else
          (error "unknown expression type -- DERIV" exp))))


; Section 2.3.3

(define (element-of-set? x set)
  (cond((null? set) false)
       ((equal? x (car set)) true)
       (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else 
         (intersection-set (cdr set1) set2))))

; Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons
           (car set1)
           (union-set (cdr set1) set2)))))


; Exercise 2.60
(define (dup-adjoin-set x set)
    (cons x set))


(define (dup-union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (cons
           (car set1)
           (dup-union-set (cdr set1) set2)))))
