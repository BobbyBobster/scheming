;; 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7))
   )

;; 1.3
(define (sum-of-larger-squares a b c)
  (cond ((and (<= a b)
              (<= a c))
         (sum-of-squares b c))
        ((and (<= b a)
              (<= b c))
         (sum-of-squares a c))
        (else (sum-of-squares a b))))
(define (sum-of-squares a b)
  (+ (* a a) (* b b)))

;; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;; 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y))
(test 0 (p))

;; 1.6
(define (sqrt-iter guess x)
  (if (good-enuf? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enuf? guess x)
  (< (abs (- (* guess guess) x)) 0.001))
(define (sqroot x)
  (sqrt-iter 1.0 x))

; NOTE since new-if is a procedure it will evaluate all its arguments
; and then apply (applicative order evaluation)
(define (new-if pred then-clause else-clause)
  (cond (pred then-clause)
        (else else-clause)))
(define (sqrt-iter guess x)
  (new-if (good-enuf? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;; 1.7
(define (sqrt-iter1.7 old new target)
  (if (close-enuf? old new) 
      new
      (sqrt-iter1.7 new (improve new target) 
                    target)))
(define (close-enuf? old new)
  (< (abs (- old new)) 0.0001))

;; 1.8
(define (cbrt-iter guess x)
  (if (good-enuf-cube? guess x)
      guess
      (cbrt-iter (cbrt-improve guess x)
                 x)))
(define (cbrt-improve y x)
  (/ (+ (* 2 y) (/ x (* y y)))
     3))
(define (good-enuf-cube? guess target)
  (< (abs (- target (* guess guess guess))) 
     0.0001))

;; 1.10
(define (ackermann x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (ackermann (1- x)
                         (ackermann x (1- y))))))
(define (f n) (ackermann 0 n))
(define (g n) (ackermann 1 n))
(define (h n) (ackermann 2 n))


;; 1.11
(define (fun n)
  (if (< n 3)
      n
      (+ (fun (- n 1)) (* 2 (fun (- n 2))) (* 3 (fun (- n 3))))))
; werkt niet!?
(define (fun2 n)
  (define (fun-iter a b c d count)
    (if (= count 0)
        a
        (fun-iter (+ b (* 2 c) (* 3 d)) a b c (1- count))
        )
    )
  (if (< n 3) 
      n
      (fun-iter 4 2 1 0 (- n 2))
      )
  )


;; 1.12
(define (pascal depth)
  (define (inter-sum lst)
    (cond ((null? (cdr lst)) (cons 1 nil))
          (else  
                  (cons (+ (car lst) (cadr lst))
                        (inter-sum (cdr lst))))))
  (if (<= depth 1) 
      (list 1)
      (cons 
        1
        (inter-sum (pascal (1- depth))))))

(define (pascal-triangle depth)
  (define (pascal-iter count)
    (if (< count depth)
        (write (pascal count))

        )
    (pascal-iter (1+ count)))
  (pascal-iter 1)
  )
(define (pascal-triangle depth)
  (write (pascal depth))
  (pascal-triangle (1- depth))
  )

;; 1.16
; NOTE not really iterative right?
(define (expon b n)
  (expon-iter b n 1))
(define (expon-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (expon-iter b (/ n 2) (* a (expon b (/ n 2)))))
        (else (expon-iter b (- n 1) (* b a)))))

;; 1.17
(define (mult a b)
  (cond ((or (= a 0) (= b 0)) 0) 
        ((= a 1) b)
        ((= b 1) a)
        ((even? a) (mult (halve a) (double b)))
        ((odd? a) (+ b (mult (1- a) b)))
        )
  )
(define (halve x) (/ x 2))
(define (double x) (+ x x))

;; 1.18
(define (mult-iter a b result)
  (cond ((or (= a 0) (= b 0)) 0) 
        ((= a 1) (+ result b))
        ((= b 1) (+ result a))
        ((even? a) (mult-iter (halve a) (double b) result))
        ((odd? a) (mult-iter (1- a) b (+ result b)))
        (else (mult-iter a b 0))) )
(define (mult a b)
  (mult-iter a b 0))

;; 1.21
(define (smallest-divisor n) 
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n ) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n) (= n (smallest-divisor n)))

;; 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (check-primes-range a b)
  (define (check-iter start end)
    (cond ((even? start) (check-iter (+ start 1) end))
          ((< start end) (timed-prime-test start) (check-iter (+ start 2) end))
          )
    )
  (check-iter a b))


;; 1.29
(define (cube x) (* x x x))
(define (sq x) (* x x))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (if (or (not (even? n)) (> a b))
      0
      (* (/ h 3.0) 
         (+ (y 0) 
            (* 4.0 (sum y 1 (lambda (x) (+ x 2)) (- n 1)))
            (* 2.0 (sum y 2 (lambda (x) (+ x 2)) (- n 2)))            
            (y n)))))

;; 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))
        )
    )
  (iter a 0))

;; 1.31 a
(define (product factor a next b)
  (if (> a b)
      1
      (* (factor a) 
         (product factor (next a) next b))))
(define (factorial n)
  (product (lambda (x) x) 1 (lambda (x) (+ x 1)) n))
; approx pi / 2
(define (numer-fact a)
  (if (even? a) 
      (+ a 2)
      (+ a 1)))
(define (denom-fact a)
  (if (odd? a) 
      (+ a 2)
      (+ a 1)))
(define (num-den-next a) (+ a 1))
(* 4 (/ (* 2.0 (product numer-fact 2 num-den-next 100))
   (product denom-fact 1 num-den-next 100)))
;; 1.31 b
(define (product factor a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (factor a) result))))
  (iter a 1))

;; 1.32 a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner 
        (term a)
        (accumulate combiner null-value term (next a) next b))))
(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (product factor a next b)
  (accumulate * 1 factor a next b))
;; 1.32 b
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;; 1.33
(define (filtered-accumulate predicate combiner null-value term a next b)
  (if (or (> a b) (not (predicate a)))
      null-value
      (combiner
        (term a)
        (filtered-accumulate predicate combiner null-value term (next a) next b)))) 
;; sum of squares of primes in a b 
;; NOTE doesnt work!??!
(filtered-accumulate prime? + 0 (lambda (x) (* x x)) 2 (lambda (a) (+ a 1)) 9)


;; 1.41
(define (double fun)
  (lambda (x) (fun (fun x))))
(((double (double double)) 1+) 5)

;; 1.42 
(define (compose f g)
  (lambda (x) (f (g x))))
((compose sq 1+) 6)

;; 1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))
((repeated sq 2) 5)

;; 1.44
(define (smooth fun)
  (let 
   ((dx 0.001))
   (lambda (x) 
     (/ (+ (fun (- x dx)) (fun x) (fun (+ x dx)))
        3)))) 
(((repeated smooth 4) sq) 4)

