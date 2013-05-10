(define-datatype continuation continuation?
  (halt-cont)
  (cons-cont
   (v scheme-value?)
   (cont continuation?))
  (proc-cont
   (cont continuation?))
  (last-element-cont
	(cont continuation?))
  (eval-exps-cont
   (exps (list-of expression?))
   (env scheme-value?)
   (cont continuation?))
  (if-cont
   (true-exp expression?)
   (false-exp expression?)
   (cont continuation?)
   (env list?))
   (if-cont2
   (true-exp expression?)
   (cont continuation?)
   (env list?))
   (apply-proc-cont
	(car-args scheme-value?)
	(cdr-args scheme-value?)
	(env scheme-value?)
	(cont continuation?))
	(map-cont
		(procedure scheme-value?)
		(vals scheme-value?)
		(env scheme-value?)
		(cont continuation?))
	(remove-first-cont
		(cont continuation?)))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [halt-cont () val]
	  [remove-first-cont (cont)
		(apply-cont cont (cdr val))]
	  [eval-exps-cont (exps env cont)
			   (eval-exps exps (cons-cont val cont) env)]
	   [cons-cont (v cont)
		      (apply-cont cont (cons v val))]
	   [proc-cont (cont)
		      (apply-proc (car val) (cdr val) '() cont)]
      [if-cont (if-true-exp if-false-exp next-cont env)
        (if val
            (eval-expression if-true-exp env next-cont)
            (eval-expression if-false-exp env next-cont))]
	  [if-cont2 (if-true-exp next-cont env)
	        (if val
				(eval-expression if-true-exp env next-cont))]
	   [apply-proc-cont (car-args cdr-args env cont)
			(apply-proc car-args cdr-args env cont)]
		[map-cont (procedure vals env cont)
			(if (null? (car vals))
				(apply-cont cont (list val))
				(apply-proc procedure (list (caar vals)) env (map-cont procedure (list (cdar vals)) env (cons-cont val cont))))]
	   [last-element-cont (cont)
			(last-element val cont)])))

(define last-element
	(lambda (val cont)
		(if (null? (cdr val))
			(apply-cont cont (car val))
			(last-element (cdr val) cont))))