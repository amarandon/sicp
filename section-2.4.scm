#lang planet neil/sicp

; Exercise 2.73

; Helpers
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

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

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

; Accessors have been changed because now deriv in invoked only with the
; operand. The operator is not longer part of the expression. This information
; is now represented by the type of the expression.
(define (addend s) (car s))

(define (augend s)
  (cond ((null? (cadr s)) 0)
        (else (cons '+ (cddr s)))))

(define (multiplier p) (car p))

(define (multiplicand p)
  (cond ((null? (cadr p)) 1)
        (else (cons '* (cadr p)))))

(define (base p) (car p))

(define (exponent p) (cadr p))
;
; a. The abstract ``deriv`` operation dispatches to the concrete derivation
; functions implemented for each operator. Expressions that are made of just a
; single number or a single variable don't contain an operator and therefore
; cannot be dispatched this way.
;
; b.
;
(define (put op type item) 'not-implemented)

(define (install-sum-package)
  (define (deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (put 'deriv '(+) deriv))


(define (install-product-package)
  (define (deriv exp var)
      (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp))))
  (put 'deriv '(*) deriv))
;
; c.
;
(define (install-exponent-package)
  (define (deriv exp var)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation
                                       (base exp)
                                       (make-substraction exp 1)))
                       (deriv (base exp) var)))
  (put 'deriv '(**) deriv))

; (install-exponent-package)
; 
; c. We would need to swap the first two arguments of the put method when registering procedures :
;
; For example for product we would register the derivation procedure like this: 
;
; (put '(*) 'deriv deriv))

; Exercise 2.75
(define (make-from-mag-angle r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
            (* r (cos a)))
          ((eq? op 'imag-part)
            (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
            (error "Unknown op -- make-from-real-imag" op))))
  dispatch)

; Exercise 2.76

; Explicit dispatching
;   add new type: add a conditional branch to each operation
;   add new operation: add a procedure that deals with all existing types
; Data-directed style
;   add new type: create a new procedure to install the package
;                 call it to install the package
;   add new operation: add operation to each package
;                      register new operation in each package
;                      define new generic operation that dispatches to the package
; Message-passing
;   add new type: create a new procedure representiong the type
;   add new operation: add operation to each existing procedure
;
; Conclusion: message-passing is most approriate when new types must often be
; added.  Explicit dispatching is most appropriate when new operations must
; often be added.
;
; Correction: it's not very clear here what is the best answer. It's probably
; the answer by torinmr at http://community.schemewiki.org/?sicp-ex-2.76 which
; says that data-directed solves both, but it need to be implemented
; differently from the code in the book so not 100% convincing easier.
; Anyway the good thing to take away from this exercise is the Expression
; Problem: http://wiki.c2.com/?ExpressionProblem

; Exercise 2.77
;
; The z object is structured like this:
;
;   '(complex rectangular 3 4)
;   
; So the call to magnitude:
;
;   (magnitude z)
;
; Is equivalent to this:
;
;   (apply-generic 'magnitude '(complex rectangular 3 4))
; 
; apply-generic gets 'magnitude as it first argument and collects each of its
; subsequent arguments in the args list. Here the args list contains only the z
; object '(complex rectangular 3 4)
; It calls type-tag on that object and gets the 'complex type tag.
; It retrieves the procedure 'magnitude tagged with 'complex, which has been
; registered by adding Alyssa's code to install-complex-package.
; It applies this procedure to the content of the object, which has been
; stripped from the 'complex tag but is still tagged 'rectangular.
; So this is another call to the magnitude procedure which this time is equivalent to:
;
;   (apply-generic 'magnitude '(rectangular 3 4))
;
; apply-generic gets 'magnitude as it first argument and collects each of its
; subsquent arguments in the args list. Here the args list contains the
; object '(rectangular 3 4)
; It calls type-tag on that object and gets the 'rectangular type tag.
; It retrieves the procedure 'magnitude tagged with 'rectangular, which has been registered by install-rectangular-package.
; It applies this procedure to the content of the object, which has been stripped from the all tags.
; The procedure returns the car of the list representing the complex number.
;
; The magnitude procedure registered against the complex package is a generic
; procedure whereas the magnitude registered against the rectangular package is
; a concrete procedure that does actual work.
;
; apply-generic is applied twice, the first time it dispatches to the global
; magnitude procedure, passing it the object stripped from the 'complex tag. 
; The second time it dispatches to the internal magnitude procedure of the
; rectangular package.
