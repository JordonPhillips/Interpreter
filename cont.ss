(define-datatype continuation continuation?
  (halt-cont)
  (if-cont
   (true-exp expression?)
   (false-exp expression?)
   (cont continuation?)
   (env list?)))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [halt-cont () val]
      [if-cont (if-true-exp if-false-exp env next-cont)
        (if val
            (eval-expression if-true-exp env next-cont)
            (eval-expression if-false-exp env next-cont))])))