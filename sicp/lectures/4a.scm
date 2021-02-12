(define (simplify-expr expr)
  (try-rules
    (if (compound? expr)
        (map simplify-expr expr)
        expr)))
