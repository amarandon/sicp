#lang planet neil/sicp
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

; Exercise 2.78
(define (attach-tag type-tag contents)
  (cond ((number? contents) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
          (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (car datum))
        (else
          (error "Bad tagged datum -- TYPE-TAG" datum))))

; Exercice 2.79
(define (put op type item) 'not-implemented)

(put 'equ? '(scheme-number scheme-number) =)

(put 'equ? '(rational rational)
     (lambda (rat1 rat2)
       (let ((reduced1 (make-rat (numer rat1) (denom rat1)))
             (reduced2 (make-rat (numer rat2) (denom rat2))))
         (and (= (numer reduced1) (numer reduced2))
              (= (denom reduced1) (denom reduced2))))))

(put 'equ? '(complex complex)
     (lambda (comp1 comp2)
       (and (= (magnitude compt) (magnitude comp2))
            (= (angle compt) (angle comp2)))))

(define (equ? x y) (apply-generic 'equ? x y))

; Exercise 2.80
(put '=zero? 'scheme-number
     (lambda (x) (= x 0)))

(put '=zero? 'rational
     (lambda (x) (= (numer x) 0)))

(put '=zero? 'complex
     (lambda (x)
       (and (zero? (imag-part a))
            (zero? (real-part a)))))

(define (=zero? x) (apply-generic =zero? x))
