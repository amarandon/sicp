#lang planet neil/sicp

;(define (make-rat n d)
;  (let ((g (gcd n d)))
;    (cons (/ n g) (/ d g))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
      (cons (/ (* n -1) g) (/ (* d -1) g))
      (cons (/ n g) (/ d g)))))

; Section 2.1.3
;(define (cons x y)
;  (define (dispatch m)
;    (cond ((= m 0) x)
;          ((= m 1) y)
;          (else (error "Argument not 0 or 1 -- CONS" m))))
;  dispatch)

;(define (car z) (z 0))
;(define (cdr z) (z 1))

; Exercise 2.2
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (avg a b)
  (/ (+ a b) 2))

(define (coord-average selector segment)
  (avg (selector (start-segment segment)) (selector (end-segment segment))))

(define (midpoint-segment segment)
  (make-point (coord-average x-point segment)
              (coord-average y-point segment)))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


; Exercise 2.3

(define (make-rect segment1 segment2)
  (cons segment1 segment2))

(define (square x) (* x x))

(define (length segment)
  (sqrt (+ (square (- (x-point (start-segment segment))
                      (x-point (end-segment segment))))
           (square (- (y-point (start-segment segment))
                      (y-point (end-segment segment)))))))

(define (rect-height rect)
  (length (car rect)))

(define (rect-width rect)
  (length (cdr rect)))

(define (rect-perim rect)
  (+ (* 2 (rect-width rect))
     (* 2 (rect-height rect))))

(define (rect-area rect)
  (* (rect-width rect) (rect-height rect)))


; Exercise 2.4
;(define (cons x y)
;  (lambda (m) (m x y)))

;(define (car z)
;  (z (lambda (p q) p)))

;(define (cdr z)
;  (z (lambda (p q) q)))


; Exercise 2.5
(define (log-base base n)
  (/ (log n) (log base)))

(define (remove-multiple base n)
  (if (= 0 (remainder n base))
      (remove-multiple base (/ n base))
      n))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (select-element z wanted unwanted)
  (round(log-base wanted (remove-multiple unwanted z))))

(define (car z)
  (select-element z 2 3))

(define (cdr z)
  (select-element z 3 2))
