;; Map

(define (mymap fun lst)
  (if (null? lst)
      nil
      (cons (fun (car lst)) 
            (mymap fun (cdr lst)))
      )
  )

