;; 2.1
(define (make-rat numer denom)
  (cond ((or (and (> numer 0) (> denom 0)) (and (< numer 0) (< denom 0)))
         (cons (abs numer) (abs denom)))
        (else (cons (* -1 (abs numer)) (abs denom)))))

;; 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-point x y)
  (cons x y))
(define (x-point point)
  (car point))
(define (y-point point)
  (cdr point))
(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment))) 2) 
              (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment))) 2)))

;; 2.3
(define (make-rect p1 p2))
(define (make-rect diagonal))
;; ...

;; 2.4
(define (my-cons x y)
  (lambda (m) (m x y)))
(define (my-car z)
  (z (lambda (p q) p)))
(define (my-cdr z)
  (z (lambda (p q) q)))

;; 2.5
(define (pow b n)
  (if (= n 0)
      1
      (* b (pow b (- n 1)))))
(define (num-cons a b)
  (* (pow 2 a)
     (pow 3 b)))
(define (times-in num div)
  (define (times-in-iter num div count)
    (if (not (integer? (/ num div)))
        count
        (times-in-iter (/ num div) div (+ count 1))))
  (times-in-iter num div 0))
(define (num-car z)
  (times-in z 2))
(define (num-cdr z)
  (times-in z 3))

;; 2.6 ============================== NOT FINISHED YET
(define zero
  (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x))))) 

(define one
  (lambda (f) (lambda (x) (f ((lambda (f) (lambda (x) x)) f) x))))


;; 2.17
(define (last-pair lst)
  (if (null? lst) '()
      (if (null? (cdr lst))
          lst
          (last-pair (cdr lst)))))

;; 2.18
(define (appen lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (appen (cdr lst1) lst2))))
(define (revers lst)
  (cond ((null? lst) '())
   ;     ((null? (cdr lst)) (list (car lst)))
        (else (appen (revers (cdr lst)) (cons (car lst) '())))))
(revers (list 1 4 9 16 25))


;; 2.20
(define (same-parity arg . rest)
  (if (even? arg)
      (cons arg (filter even? rest))
      (cons arg (filter odd? rest))))

;; 2.21
(define (sq x) (* x x))
(define (square-list items)
  (if (null? items)
      '()
      (cons (sq (car items)) (square-list (cdr items)))))
(define (square-list items)
  (map sq items))

;; 2.22  
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;; 2.23
(define (for-ech proc lst)
  (if (null? lst)
      '()
      (proc (car lst)))
  (if (null? (cdr lst))
      '()
      (for-ech proc (cdr lst))))

;; 2.27
(define (deep-reverse lst)
  (cond
    ((null? lst) '())
    ((pair? (car lst))
     (append (deep-reverse (cdr lst)) 
             (list (deep-reverse (car lst)))))
    (else
      (append (deep-reverse (cdr lst))
              (list (car lst))))))
(define x (list (list 1 2) (list 3 4)))
x
(revers x)
(deep-reverse x)
;; fromweb
(define (deep-reverse t)
  (if (pair? t)
      (reverse (map deep-reverse t))
      t))

;; 2.28
(define (fringe lst)
  (cond ((null? lst)
         '()) 
        ((list? (car lst))
         (append (fringe (car lst)) (fringe (cdr lst))))
        ((null? (cdr lst))
         (list (car lst)))
        (else
          (append (list (car lst)) (fringe (cdr lst))))))

;; 2.29 a
(define (make-mobile left right)
  (list left right))
(define (make-branch len struct)
  (list len struct))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-struct branch)
  (cadr branch))
;; 2.29 b
(define (total-weight mobile)
  (cond ((null? mobile) 0) 
        ((not (list? (left-branch mobile)))
         (branch-struct mobile))
        (else
         (+ (total-weight (left-branch mobile))
            (total-weight (right-branch mobile))))))
;; 2.29 c
(define (balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (total-weight branch)))
  (if (= (torque (left-branch mobile)) (torque (right-branch mobile)))
      (and (balanced? (left-branch mobile)) (balanced? (right-branch mobile)))
      #f))
;; fromwiki
(define (total-weight mobile)
  (cond
    ((null? mobile) 0)
    ((not (pair? mobile)) mobile)
    (else (+ (total-weight (branch-struct (left-branch mobile)))
             (total-weight (branch-struct (right-branch mobile)))))))
(define (torque branch)
  (* (branch-length branch) (total-weight (branch-struct branch)))) 
(define (balanced? mobile)
  (if (not (pair? mobile))
      #t
      (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
           (balanced? (branch-struct (left-branch mobile)))
           (balanced? (branch-struct (right-branch mobile))))))


;; 2.30
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (* tree tree))
        (else
          (cons (square-tree (car tree)) (square-tree (cdr tree))))))
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7))) ; -> (1 (4 (9 16) 25) (36 49))

;; 2.31 
(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (proc tree))
        (else
          (cons (tree-map proc (car tree))
                (tree-map proc (cdr tree))))))
(define (square-tree tree) (tree-map sq tree))

;; 2.32
(define (subsets s)
  (if (null? s) 
      (list '())
      (let ((rest (subsets (cdr s))))
       (append rest (map (lambda (x) (cons (car s) x)) rest)))))
(subsets '(1 2 3))

;; 2.33
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))
(define (my-map p seq)
  (accumulate (lambda (x y) (cons (p x) y)) nil seq))
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (my-length seq)
  (accumulate (lambda (x y) (+ y 1)) 0 seq))

;; 2.34
(define (horner-eval x coeff-seq)
  (accumulate (lambda (this-coeff higher-terms) 
                (+ this-coeff (* x higher-terms)))
              0
              coeff-seq))

;; 2.35  fromwiki
(define (count-leaves t)
  (accumulate +
              0 
              (map (lambda (x)
                     (cond
                       ((null? x) 0)
                       ((pair? x) (count-leaves x))
                       (else 1)))
                   t)))
(count-leaves '(((1 2) 3 4) ((1 2) 3 4))) ; -> 8

;; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))
(define (transpose mat)
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
   (map (lambda (row) (matrix-*-vector cols row)) m)))

;; 2.38
(define (my-fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

;; 2.39
(define (my-reverse seq)
  (fold-right (lambda (x y) (append y (list x))) '() seq))
(define (my-reverse seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))


;; 
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;; 2.40 
(define (enumerate-interval a b)
  (if (> a b) 
      '()
      (cons a (enumerate-interval (+ a 1) b))))
(define (unique-pairs n)
  (accumulate append
              '()
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-sum-pair
       (filter prime-sum? (unique-pairs n))))

;; 2.41 ==========================================================

;; 2.42
(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position new-row k rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))
;; 2.43

;; 2.54
(define (my-equal? a b)
  (cond ((and (symbol? a) (symbol? b))
         (eq? a b))
        ((and (list? a) (list? b))
         (if (and (null? a) (null? b))
             #t
             (if (or (and (not (null? a)) (null? b)) 
                     (and (null? a) (not (null? b))))
                 #f
                 (if (my-equal? (car a) (car b))
                     (my-equal? (cdr a) (cdr b))
                     #f))))))



;; ========================================

;; 2.73
(define (install-sum-deriv-package)
  ;; internal procedures
  (define (deriv-sum expr var)
    (make-sum (deriv (addend expr) var)) 
              (deriv (augend expr) var)) 
  (define (make-sum a b) 
    (cond ((and? (number? a) (number? b))
           (+ a b))
          ((= a 0) b)
          ((= b 0) a)
          (else (list '+ a b))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  ;; interface
  (put 'deriv '+ deriv-sum))
(define (install-product-deriv-package)
  ;; internals
  (define (deriv-prod expr var)
    (make-sum
      (make-product
        (multiplier expr)
        (deriv (multiplicand expr) var))
      (make-product
        (deriv (multiplier expr) var)
        (multiplicand expr))))
  (define (make-product a b)
    (cond ((and? (number? a) (number? b)) 
           (* a b))
          ((= a 1) b)
          ((= b 1) a)
          ((or? (= a 0) (= b 0)) 0)
          (else (list '* a b))))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  ;; interface
  (put 'deriv '* deriv-prod)
  )

;; 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) 
           (* r (cos a)))
          ((eq? op 'imag-part) 
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "unkown operation")))))
