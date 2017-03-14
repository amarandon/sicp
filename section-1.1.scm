(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess))) 
  
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))(define (good-enough? guess old-guess)
  (< (abs (- guess old-guess)) 0.0001))

(define (improve guess x)
  (average guess (/ x guess))) 
  
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1 10 x))

(define (sqrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (square x)
  (* x x))
(define (good-enough? guess old-guess)
  (< (abs (- guess old-guess)) 0.0001))

(define (improve y x)
  (/ (+ (* 2 y) (/ x (square y))) 3)) 
  
(define (average x y)
  (/ (+ x y) 2))

(define (cbrt x)
  (cbrt-iter 1 0 x))

(define (cbrt-iter guess old-guess x)
  (if (good-enough? guess old-guess)
      guess
      (cbrt-iter (improve guess x) guess x)))

(define (square x)
  (* x x))
