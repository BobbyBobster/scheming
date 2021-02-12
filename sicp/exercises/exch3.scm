;; 3.1
(define (make-accumulator total)
  (lambda (addend)
    (begin
      (set! total (+ total addend))
      total)))

;; 3.2 
(define (make-monitored f)
  (define counter 0)
  (lambda (arg)
    (if (eq? arg 'how-many-calls?)
        counter 
        (begin
          (set! counter (+ counter 1) )
          (f arg)))))

;; 3.3 + 3.4 + 3.7
(define (make-account balance secretkey)
  (define wrong-pass-counter 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops _)
    (error "Call the popo" ))
  (define (correct-password? givenkey)
    (eq? givenkey secretkey))
  (define (dispatch givenkey m)
     (if (> wrong-pass-counter 6) 
         call-the-cops
         (if (not (eq? secretkey givenkey))
         (begin 
           (set! wrong-pass-counter (+ wrong-pass-counter 1))
           (lambda (_)
             (display "Wrong password")))
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              ((eq? m 'correct-password?) correct-password?)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m))))))
  dispatch)
(define (make-joint account password new-password)
  (if ((account password 'correct-password?) password)
      (lambda (key message)
        (if (eq? key new-password)
            (account password message)
            (error "wrong password second account")))
      (error "wrong password first account")))

;; 3.5 
(define (random-in-range low high)
  (let ((range (- high low)))
   (+ low (random range))))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (experiment)
    (pred (random-in-range x1 x2) (random-in-range y1 y2)))
  (monte-carlo trials experiment))
(define (estimate-pi trials)
  (* 4.0
     (estimate-integral 
    (lambda (x y) (< (+ (* x x) (* y y)) 1)) 
    -1.0
    1.0
    -1.0
    1.0
    trials)))

;; 3.8
(define count 0)
(define (f x)
   (if (= count 0)
       (begin
         (set! count 1)
         0)
        x)) 

;; 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle '(a b c)))

;; 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
         (set-cdr! x y)
         (loop temp x))))
  (loop x '()))

;; 3.15
(define x '(a b))
(define z1 (cons x x))
(define z2 (cons '(a b) '(a b)))
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;; 3.18
(define (cycle? l)
  (cond
    ((null? l) #f)
    ((eq? l (car l)) #t)
    ((eq? l (cdr l)) #t)
    )
  )

;; 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue) (car front-ptr))
    (define (insert-queue! item)
      (begin
       (if (empty-queue?)
          (begin
            (set! front-ptr (cons item '()))
            (set! rear-ptr (cons item '())))
          (begin
            (set! front-ptr (cons item front-ptr))
            (set! rear-ptr (cons item '()))))
       front-ptr))
    (define (delete-queue!)
      (begin
        (if (empty-queue?)
            (error "Queue cannot be empty when deleting")
            (begin
              (set! front-ptr (cdr front-ptr))
              ))
        front-ptr))
    (define (dispatch m)
      (cond
        ((eq? m 'empty?)
         (empty-queue?))
        ((eq? m 'front)
         (front-queue))
        ((eq? m 'insert!)
         insert-queue!)
        ((eq? m 'delete!)
         (delete-queue!))
        (else (error "Operation undefined" m))))
    dispatch))

;; 3.23
(define (make-deque)
  (cons '() '()))
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-prt! deque item) (set-cdr! deque item))
(define (empty? deque)
  (null? )
  )
(define (front deque))
(define (rear deque))
(define (front-insert! deque))
(define (rear-insert! deque))
(define (front-delete! deque))
(define (rear-delete! deque))


