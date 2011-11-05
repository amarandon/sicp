(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (inc a) (+ a 1))
(define (identity a) a)

(define (sum-int a b)
  (accumulate + 0 identity a inc b))

(define (cube n) (* n n n))
(define (sum-cube a b)
  (accumulate + 0 cube a inc b))

(define (sum-pi a b)
  (accumulate + 0
       (lambda (a) (/ 1.0 (* a (+ a 2))))
       a
       (lambda (a) (+ a 4))
       b))

(define (good-enough? a b)
  (< (abs (- a b)) 0.001))

(define (average a b)
  (/ (+ a b) 2))

(define (search f neg-number pos-number)
  (let ((midpoint (average neg-number pos-number)))  
    (if (good-enough? neg-number pos-number)
        midpoint
        (let ((test-value (f midpoint)))
           (cond ((positive? test-value)
                  (search f neg-number midpoint))
                 ((negative? test-value)
                  (search f midpoint pos-number))
                 (else midpoint))))))

(define (half-interval f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f b a))
          (else
           (error "This is not a fucking valid input")))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          guess
          (try next))))
  (try first-guess))

; Exercise 1.37a
(define (cont-frac n d k)
  (define (frac i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i)
         (+ (d i)         
            (frac (+ i 1))))))
  (frac 1))
  
; Exercise 1.37b
(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (n i)
                 (+ (d i)
                    result)))))
  (iter (- k 1) (/ (n k) (d k))))

; Exercise 1.38
(define (is-one? position)
  (not (= 0 (modulo (+ 1 position) 3))))

(define (e-elem pos)
  (define (iter start non-one-base)
    (if (= start pos)
        (if (is-one? start)
            1
            (* 2 non-one-base))
        (if (is-one? start)
            (iter (+ 1 start) non-one-base)
            (iter (+ 1 start) (+ 1 non-one-base)))))
  (iter 1 1))

(define (d i)
   (if (not (= 0 (remainder (+ i 1) 3)))
       1
       (* 2 (/ (+ i 1) 3))))

(define (compute-e k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
             e-elem
             k)))

; Exercise 1.39
(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (- (* i 2) 1))
             k))

; Section 1.3.4

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square x) (* x x))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (fourth-root x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (cube y)))))
               1.0))

(define (pow x n)
  (if (< n 2)
      x
      (* x (pow x (- n 1)))))
  
(define dx 0.000001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newtons-transform g)
  (lambda (x) (- x (/ (g x)
                      ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newtons-transform g)
               guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (newtons-sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                             newtons-transform
                             1.0))
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                             average-damp
                             1.0))
; Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

; Exercise 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

; Exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; Exercise 1.43
(define (repeated f n)
  (define (iter f n result)
    (if (= n 1)
        result
        (iter f (- n 1) (compose f result))))
  (iter f n f))

; Exercise 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (n-fold-smoothed f n)
  ((repeated smooth n) f))

; Exercise 1.45

; log base n of x = log(x) / log(n)
(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point 
   ((repeated average-damp (floor (log2 n)))
    (lambda (y) (/ x 
                   (pow y (- n 1))))) 
   1.0))

; Exercise 1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (guess) (iter guess)))

; Rewrite sqrt from section 1.1.7
(define (sqrt2 x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))
  
; Rewrite fixed-point from sectin 1.2.3
(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (close-enough-to-next? improve)
    (lambda (guess) (close-enough? guess (improve guess))))
  ((iterative-improve (close-enough-to-next? f) f) first-guess))
