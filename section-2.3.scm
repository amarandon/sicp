#lang planet neil/sicp

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


; Exercise 2.53

; (list 'a 'b 'c)
; ; '(a b c)
;
; (list (list 'george))
; ; '('(george))
;
; (cdr '((x1 x2) (y1 y2)))
; ; '('(y1 y2))
;
; (cadr '((x1 x2) (y1 y2)))
; ; '(y1 y2)
;
; (pair? (car '(a short list)))
; ; #f
;
; (memq 'red '((red shoes) (blue socks)))
; ; #f
;
; (memq 'red '(red shoes blue socks))
; '(red shoes blue socks)


; Exercise 2.53
(define (equal? a b)
  (cond ((and
           (and (symbol? a) (symbol? b))
           (eq? a b))
         #t)
        ((and (number? a) (number? b))
         (eq? a b))
        ((and (null? a) (null? b))
         #t)
        ((and
           (and (list? a) (list? b))
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))
         #t)
        (else #f)))


; Section 2.3.2

; Execises 2.56, 2.57

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))
  

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (cond ((null? (cddr s)) 0)
        (else (cons '+ (cddr s)))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (cond ((null? (cddr p)) 1)
        (else (cons '* (cddr p)))))

(define (base p) (cadr p))


(define (make-exponentiation a1 a2)
  (cond ((=number? a2 0) 1)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (expt a1 a2))
        (else (list '** a1 a2))))


(define (make-substraction a1 a2)
  (cond ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (- a1 a2))
        (else (list '- a1 a2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (exponent p) (caddr p))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation
                                       (base exp)
                                       (make-substraction exp 1)))
                       (deriv (base exp) var)))
        (else
          (error "unknown expression type -- DERIV" exp))))


; Section 2.3.3

(define (element-of-set? x set)
  (cond((null? set) false)
       ((equal? x (car set)) true)
       (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))


(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else 
         (intersection-set (cdr set1) set2))))

; Exercise 2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons
           (car set1)
           (union-set (cdr set1) set2)))))


; Exercise 2.60
(define (dup-adjoin-set x set)
    (cons x set))


(define (dup-union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (cons
           (car set1)
           (dup-union-set (cdr set1) set2)))))

; Sets as ordered lists
(define (element-of-ordered-set? x set)
  (cond ((null? set) false)
        ((= (car set) x) true)
        ((> (car set) x) false)
        (else (element-of-ordered-set? x (cdr set)))))

(define (intersection-ordered-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1
                   (intersection-ordered-set (cdr set1) (cdr set2))))
            ((< x1 x2)
             (intersection-ordered-set (cdr set1) set2))
            ((> x1 x2)
             (intersection-ordered-set (cdr set2) set1))))))


; Exercise 2.61
(define (adjoin-ordered-set x set)
  (cond ((null? set) (cons x '()))
        ((equal? (car set) x) set)
        ((< x (car set)) (cons x set))
        (else
          (cons (car set) (adjoin-ordered-set x (cdr set))))))

; Exercise 2.62
(define (union-ordered-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1
                         (union-ordered-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1
                         (union-ordered-set (cdr set1) set2)))
                  ((< x2 x1)
                   (cons x2
                         (union-ordered-set (cdr set2) set1))))))))

; Sets as binary trees
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-tree-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-tree-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-tree-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-tree-set x (right-branch set))))))

; Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
      (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

; Testing trees from Figure 2.16
(define example-tree-1
  (make-tree 7
             (make-tree 3
                        (make-tree 1 nil nil)
                        (make-tree 5 nil nil))
             (make-tree 9
                        nil
                        (make-tree 11 nil nil))))

(define example-tree-2
  (make-tree 3
             (make-tree 1 nil nil)
             (make-tree 7
                        (make-tree 5 nil nil)
                        (make-tree 9 nil
                                   (make-tree 11 nil nil)))))

(define example-tree-3
  (make-tree 5
             (make-tree 3
                        (make-tree 1 nil nil)
                        nil)
             (make-tree 9
                        (make-tree 7 nil nil)
                        (make-tree 11 nil nil))))

; Both tree->list-1 and tree->list-2 produce the same result. However
; tree->list-1 uses the append function which does more work than cons.
; Therefore tree->list-2 will grow more slowly than tree->list-1.

; Exercise 2.64
(define (list->tree elements)
  (car (partial-tree  elements (length elements))))

(define (partial-tree elements n)
  (if (= n 0)
    (cons '() elements)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elements left-size)))
        (let ((left-tree (car left-result))
              (non-left-elements (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elements))
                (right-result (partial-tree (cdr non-left-elements)
                                            right-size)))
            (let ((right-tree (car right-result))
                  (remaining-elements (cdr right-result)))
              (cons (make-tree this-entry left-tree right-tree)
                    remaining-elements))))))))

; partial-tree identifies the entry of the partial tree. To do this, it takes
; the element at the middle of the list, or just before the middle, in the case
; of a list with an even number of elements. It calls itself recursively with
; the elements before and after that element to build respectively the right
; and left branches of the tree.
;
; Let's draw the tree produced by list->tree for the list (1 3 5 7 8 11)
;
;        5
;       / \
;      /   \
;     /     \
;    1       8
;     \     / \
;      3   7   11

; Exercice 2.66

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) nil)
        ((= given-key (car set-of-records)) (car set-of-records))
        ((< given-key (car set-of-records)) (lookup given-key (cadr set-of-records)))
        ((> given-key (car set-of-records)) (lookup given-key (caddr set-of-records)))))

; Section 2.3.4 - Huffman Encoding Trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf leaf)
  (cadr leaf))

(define (weight-leaf leaf)
  (caddr leaf))

(define (make-code-tree left-branch right-branch)
  (list left-branch
        right-branch
        (append (symbols left-branch) (symbols right-branch))
        (+ (weight left-branch) (weight right-branch))))

(define (huf-left-branch tree)
  (car tree))

(define (huf-right-branch tree)
  (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (choose-branch bit tree)
  (cond ((= bit 0) (huf-left-branch tree))
        ((= bit 1) (huf-right-branch tree))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
      (if (null? bits)
        '()
        (let ((bit (car bits)))
          (let ((next-branch (choose-branch bit current-branch)))
            (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch))))))
  (decode-1 bits tree))

; (define (decode bits tree)
;   (if (null? bits)
;     '()
;     (let ((bit (car bits)))
;       (let ((next-branch (choose-branch bit tree)))
;         (if (leaf? next-branch)
;           (cons (symbol-leaf next-branch) (decode (cdr bits) tree))
;           (decode (cdr bits) next-branch))))))
;
(define (huf-adjoin-set x set)
  (cond ((null? set)
         (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
          (cons (car set) (huf-adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (huf-adjoin-set (make-leaf (car pair)
                             (cadr pair))
                      (make-leaf-set (cdr pairs))))))

; Exercise 2.67

(define hello-tree
  (make-code-tree
      (make-code-tree
        (make-leaf 'H 1)
        (make-leaf 'E 1))
      (make-code-tree
        (make-leaf 'L 1)
        (make-leaf 'O 2))))

(define hello-message '(0 0 0 1 1 0 1 0 1 1))

; (decode hello-message hello-tree)
; gives '(h e l l o)

; Exercise 2.68

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol symbol current-branch)
  (cond ((and (leaf? current-branch)
              (element-of-set? symbol (symbols current-branch)))
         '())
        ((element-of-set? symbol
                          (symbols (huf-left-branch current-branch)))
         (cons 0 (encode-symbol symbol (huf-left-branch current-branch))))
        ((element-of-set? symbol
                          (symbols (huf-right-branch current-branch)))
         (cons 1 (encode-symbol symbol (huf-right-branch current-branch))))
        (else (error "Symbol not in the tree at all"))))

(if (not (equal? (encode (decode hello-message hello-tree)
                         hello-tree)
                 hello-message))
  (error "Encode / decode failed"))

; Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


(define (successive-merge nodes)
  (if (null? (cdr nodes))
    (car nodes)
    (successive-merge (append (cddr nodes)
                              (list (make-code-tree
                                      (car nodes)
                                      (cadr nodes)))))))

(define pairs (list '(h 1) '(e 3) '(l 3) '(o 2) '(p 2)))
(define generated-tree
  (generate-huffman-tree pairs))

(define greeting '(h e l l o p e o p l e))
(define encoded-greeting (encode greeting generated-tree))

(if (not (equal? (decode encoded-greeting generated-tree)
                 greeting))
  (error "Encode / decode failed"))

(if (>= (length encoded-greeting)
        (* 8 (length greeting)))
  (error "Encoded data isn't smaller than original"))
