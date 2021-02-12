(define (add x y)
  (if (= x 0)
      y
      (+ (1- x) (1+ y))))

(define (alsoadd x y)
  (if (= x 0)
      y
      (1+ (+ (1- x) y))))

;; Towers of Hanoi
(define (move n from to spare)
  (cond ((= n 0) "done")
        (else 
          (move (1- n) from spare to)
          (print-move from to)
          (move (1- n) spare to from))))

(define (print-move from to)
  (write from)
  (write '-)
  (write to)
  (newline))

