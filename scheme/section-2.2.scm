#lang planet neil/sicp

; Exercise 2.17
(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))

; Exercise 2.18
(define (reverse l)
  (define (iter l a)
    (if (null? l)
      a
      (iter (cdr l) (cons (car l) a))))
  (iter l nil))

; Exercise 2.19
(define (no-more? coin-values)
  (null? coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

; Exercise 2.20
(define (same-parity head . tail)
  (let ((good? (if (odd? head)
                 odd? 
                 even?)))
    (define (iter l a)
      (if (null? l)
        a
        (iter (cdr l) (if (good? (car l))
                        (cons (car l) a)
                        a))))
    (cons head
          (reverse (iter tail nil)))))

; Exercise 2.21
(define (square-list items)
  (if (null? items)
    nil
    (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

; Exercise 2.23
(define (for-each func items)
  (cond ((null? items) #t)
        ((func (car items))
         (for-each func (cdr items)))))

; Exercise 2.27
(define (deep-reverse items)
  (cond ((null? items) nil)
        ((not (pair? items)) items)
        (else (append (deep-reverse (cdr items))
                      (list (deep-reverse (car items)))))))

; Exercise 2.28
(define (fringe items)
  (cond ((null? items) nil)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items))
                      (fringe (cdr items))))))

; Exercise 2.29
(define (make-mobile left right) (cons left right))

(define (left-branch mobile) (car mobile))

(define (right-branch mobile) (cdr mobile))

(define (make-branch length structure) (cons length structure))

(define (branch-length branch) (car branch))

(define (branch-structure branch) (cdr branch))

(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

(define (branch-torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))

(define (balanced? mobile)
  (or (not (pair? mobile))
      (and (= (branch-torque (left-branch mobile))
              (branch-torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

(define mobile1 ; unbalanced, total weight 15
  (make-mobile
    (make-branch 7 (make-mobile
                     (make-branch 4 1)
                     (make-branch 4 2)))
    (make-branch 7 (make-mobile
                     (make-branch 4 (make-mobile
                                      (make-branch 3 3)
                                      (make-branch 2 4)))
                     (make-branch 4 5)))))

(define mobile2 ; balanced
  (make-mobile
    (make-branch 2 (make-mobile
                     (make-branch 8 2)
                     (make-branch 4 4)))
    (make-branch 4 (make-mobile
                     (make-branch 3 2)
                     (make-branch 6 1)))))

