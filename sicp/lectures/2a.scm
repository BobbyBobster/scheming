(define (sum-int a b)
  (if (> a b)
      0
      (+ a (sum-int (1+ a) b))))

(define (sum-sq a b)
  (if (> a b)
      0
      (+ (square a) (sum-sq (1+ a) b))))
(define (square x) (* x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term
              (next a)
              next
              b))))

(define (sum-int a b)
  (define (id a) a)
  (sum id a 1+ b))

(define (sum-int a b)
  (sum (lambda (x) x) a 1+ b))

(define (sum-sq a b)
  (sum square a 1+ b))

(define (pi-sum a b)
  (sum (lambda (i) (/ 1.0 (* i (+ i 2))))
       a
       (lambda (i) (+ i 4))
       b))

;; Fixed points
(define (average a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (fixed-point 
    (lambda (y) (average (/ x y) y))
    1.0))

(define (fixed-point f start)
  (define tol 0.00001)
  (define (close-enuf? u v)
    (< (abs (- u v)) tol))
  (define (iter old new)
    (if (close-enuf? old new)
        new
        (iter new (f new))))
  (iter start (f start)))

(define (sqrt x)
  (fixed-point 
    (average-damp (lambda (y) (/ x y)))
    1.0))

(define average-damp
  (lambda (f)
    (lambda (x) (average (f x) x))))



