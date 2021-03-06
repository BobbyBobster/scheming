(define (my-cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)
(define (my-car z) (z 0))
(define (my-cdr z) (z 1))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))
(letrec
  ((f (lambda (n)
        (if (< n 2) 1 (* n (f (- n 1)))))))
  (f 20))
(let
 ((f (y (lambda (h)
          (lambda (n) 
            (if (< n 2) 1 (* n (h (- n 1)))))))))
 (f 10))
(let 
 ((g (lambda (h n) 
       (if (< n 2) 1 (* n (h h (- n 1)))))))
 (g g 10))
(letrec
  ((f (lambda (n m) 
        (if (< n 2) m (f (- n 1) (* m n))))))
  (f 10 1))
(letrec
  ((f (lambda (n)
        (lambda (m)
          (if (< n 2) m ((f (- n 1)) (* m n)))))))
  ((f 10) 1))
(let
 ((g (lambda (h)
       (lambda (n)
         (if (< n 2) 1 (* n ((h h) (- n 1))))))))
 ((g g) 10))
(let
 ((g (lambda (h)
       (lambda (n)
         (let 
          ((f (lambda (q)
                (lambda (n)
                  (if (< n 2) 1 (* n (q (- n 1))))))))
          ((f (h h)) n))))))
 ((g g) 10))
(let
 ((f (lambda (q)
       (lambda (n)
         (if (< n 2) 1 (* n (q (- n 1))))))))
 (let ((g (lambda (h)
            (lambda (n)
              ((f (h h)) n)))))
   ((g g) 10)))
(define Y (lambda (f)
            (let ((g (lambda (h)
                       (lambda (x)
                         ((f (h h)) x)))))
              (g g)))) 
