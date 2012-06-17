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

; Exercise 2.30
(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (tree)
         (if (pair? tree)
           (square-tree-map tree)
           (square tree)))
       tree))

; Exercise 2.31
(define (tree-map f tree)
  (map (lambda (tree)
         (if (pair? tree)
           (tree-map f tree)
           (f tree)))
       tree))

; Exercise 2.32
;
(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map (lambda (x) 
                          (cons (car s) x)) 
                        rest)))))

; Exercise 2.33
(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq)
        (accumulate op initial (cdr seq)))))

;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y))
;                nil
;                sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

; Exercise 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (pair? x)
                       (count-leaves x)
                       1))
                   t)))

; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
             nil
             (cons (accumulate op init (map car seqs))
                   (accumulate-n op init (map cdr seqs)))))

; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m v)
  (let ((cols (transpose m)))
    (map (lambda (row)
           (matrix-*-vector cols row))
         m)))

; Exercise 2.38, 2.39
(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial seq))

(define fold-right accumulate)

(define (reverse-foldr seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))

(define (reverse-foldl seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))

; Exercise 2.40
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

(define (filter p s)
  (cond ((null? s) nil)
        ((p (car s))
         (cons (car s) (filter p (cdr s))))
        (else (filter p (cdr s)))))

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap p s)
  (accumulate append nil (map p s)))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum 
       (filter prime-sum? (unique-pairs n))))

; Exercise 2.41
(define (unique-triples n)
  (flatmap (lambda (i) 
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (sum-equals seq int)
  (= int (accumulate + 0 seq)))

(define (int-sum-triples n s)
  (filter (lambda (triple) (sum-equals triple s))
          (unique-triples n)))

; Exercise 2.44
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
