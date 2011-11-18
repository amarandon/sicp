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
;(define (log-base base n)
;  (/ (log n) (log base)))

;(define (remove-multiple base n)
;  (if (= 0 (remainder n base))
;      (remove-multiple base (/ n base))
;      n))

;(define (cons a b)
;  (* (expt 2 a) (expt 3 b)))

;(define (select-element z wanted unwanted)
;  (round(log-base wanted (remove-multiple unwanted z))))

;(define (car z)
;  (select-element z 2 3))

;(define (cdr z)
;  (select-element z 3 2))

; Exercise 2.6
(define zero 
  (lambda (f)
    (lambda (x) x)))

(define one
  (lambda (f)
    (lambda (x) (f x))))

(define two
  (lambda (f) 
    (lambda (x) (f (f x)))))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define (add n m)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))


; Section 2.14

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; Orginal definition, redefined in Ex. 2.10
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Orginal definition, redefined in Ex. 2.11
;(define (div-interval x y)
;  (mul-interval x
;                (make-interval (/ 1.0 (upper-bound y))
;                               (/ 1.0 (lower-bound y)))))

; Exercise 2.7

(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

; Exercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

; Exercise 2.10
(define (span-zero? y)
  (and (<= (lower-bound y) 0)
       (>= (upper-bound y) 0)))
(define (div-interval x y)
  (if (span-zero? y)
    (error "Interval can't span 0")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

; Exercise 2.11
(define (mul-interval2 x y)
  (let ((lo-x (lower-bound x))
        (up-x (upper-bound x))
        (lo-y (lower-bound y))
        (up-y (upper-bound y)))
    (cond ((and (<= lo-x 0)
                (<= up-x 0)
                (<= lo-y 0)
                (<= up-y 0))
           ; [-, -] * [-, -]
           (make-interval (* up-x up-y) (* lo-x lo-y)))
          ((and (>= lo-x 0)
                (>= up-x 0)
                (>= lo-y 0)
                (>= up-y 0))
           ; [+, +] * [+, +]
           (make-interval (* up-x up-y) (* lo-x lo-y)))
          ((and (<= lo-x 0)
                (>= up-x 0)
                (>= lo-y 0)
                (>= up-y 0))
           ; [-, +] * [+, +]
           (make-interval (* lo-x up-y) (* up-x up-y)))
          ((and (<= lo-x 0)
                (<= up-x 0)
                (>= lo-y 0)
                (>= up-y 0))
           ; [-, -] * [+, +]
           (make-interval (* lo-x up-y) (* up-x lo-y)))
          ((and (<= lo-x 0)
                (<= up-x 0)
                (<= lo-y 0)
                (>= up-y 0))
           ; [-, -] * [- , +]
           (make-interval (* lo-x up-y) (* lo-x lo-y)))
          ((and (<= lo-x 0)
                (>= up-x 0)
                (<= lo-y 0)
                (<= up-y 0))
           ; [-, +] * [-, -]
           (make-interval (* up-x lo-y) (* lo-x lo-y)))
          ((and (<= lo-x 0)
                (>= up-x 0)
                (<= lo-y 0)
                (>= up-y 0))
           ; [-, +] * [- , +]
           (make-interval (min (* up-x lo-y)
                               (* lo-x up-y))
                          (max (* lo-x lo-y)
                               (* up-x up-y))))
          ((and (>= lo-x 0)
                (>= up-x 0)
                (<= lo-y 0)
                (<= up-y 0))
           ; [+, +] * [-, -]
           (make-interval (* up-x lo-y) (* lo-x up-y)))
          ((and (>= lo-x 0)
                (>= up-x 0)
                (<= lo-y 0)
                (>= up-y 0))
           ; [+, +] * [-, +]
           (make-interval (* up-x lo-y) (* up-x up-y))))))

; Exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (/ (* c p) 
              100)))
    (make-center-width c w)))

(define (percent i)
  (let ((c (center i)))
    (* (/ (width i) c) 
       100)))
