;;; Derivative rules
(define (deriv expr var)
  (cond ((constant? expr var) 0)
        ((same-var? expr var) 1)
        ((sum? expr) 
         (make-sum 
           (deriv (a1 expr) var) 
           (deriv (a2 expr) var)))
        ((product? expr) 
         (make-sum
           (make-product
             (m1 expr)
             (deriv (m2 expr)))
           (make-product
             (deriv (m1 expr))
             (m2 expr))))
        )
  )

;;; Representation of function like this
(+ (* a (* x x))
   (+ (* b x)
      c))

(define (constant? expr var)
  (and (atom? expr) ;; not a list 
       (not (eq? expr var)))) ;; which is not ident to itself
(define (same-var? expr var)
  (and (atom? expr) ;; not a list
       (eq? expr var))) ;; which *is* ident to itself 
(define (sum? expr)
  (and (not (atom? expr)) ;; a sum is a list
       (eq? (car expr) '+))) ;; and its first element is a +-symbol 
                             ;; therefore + is quoted
(define (make-sum a1 a2)
  (list '+ a1 a2))
(define a1 cadr) ;; car of the cdr
(define a2 caddr) ;; car of the cdr of the cdr
(define (product? expr)
  (and (not (atom? expr))
       (eq? (car expr) '*)))
(define (make-product m1 m2)
  (list '* m1 m2))
(define m1 cadr)
(define m2 caddr)

;;; More algebra
(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2))
         (+ a1 a2))
        ((and (number? a1) (= a1 0))
         a2)
        ((and (number? a2) (= a2 0))
         a1)
        (else (list '+ a1 a2))))
;;; My own make-product betterment
(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2))
         (* m1 m2))
        ((and (number? m1) (= m1 0))
         0)
        ((and (number? m2) (= m2 0))
         0)
        ((and (number? m1) (= m1 1))
         m2)
        ((and (number? m2) (= m2 1))
         m1)
        (else (list '* m1 m2))))
 

