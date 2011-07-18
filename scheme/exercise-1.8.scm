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
