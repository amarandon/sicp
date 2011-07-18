(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 5)
      (report-prime (- (runtime) start-time))))



(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
        (if (<= start end)
          (cond ((even? start)
                 (search-for-primes (+ start 1) end))
                (else (timed-prime-test start)
                      (search-for-primes (+ start 2) end)))))

(define (expt base n)
      (cond ((= n 0) 1)
            (else (* base (expt base (- n 1))))))

(define (fast-expt base n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt base (/ n 2))))
        (else (* base (expt base (- n 1))))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (remainder (square (expmod base (/ exp 2) m))
                      m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                      m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ (random (- n 1)) 1))) 

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (charmichael? n)
  (define (try-number n a)
    (cond ((= a 0) true)
          ((not (= (expmod a n n) a)) false)
          (else (try-number n (- a 1)))))
  (try-number n (- n 1)))