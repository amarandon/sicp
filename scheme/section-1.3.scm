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

(define (tan-cf x k)
  (cont-frac-iter (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (- (* i 2) 1))
             k))