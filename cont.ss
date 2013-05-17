(define-datatype continuation continuation?
  (halt-cont)
  (cons-cont
   (v scheme-value?)
   (cont continuation?))
  (proc-cont
   (cont continuation?))
  (last-element-cont
	(cont continuation?))
  (begin-cont
  	(exps (list-of expression?))
  	(env list?)
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
		(cont continuation?))
	(set-cont
		(env list?)
		(var scheme-value?)
		(cont continuation?))
	(set2-cont
		(env list?)
		(var scheme-value?)
		(cont continuation?))
	(letrec-cont
			(vars list?)
			(exprs list?)
			(env list?)
			(cont continuation?))
	(letrec2-cont
			(vars list?)
			(exprs list?)
			(env list?)
			(cont continuation?))
	(call/cc-cont
		(cont continuation?))
	(while-cont
		(test expression?)
		(bodies list?)
		(env list?)
		(cont continuation?))
	(define-contg
		(sym symbol?)
		(env list?)
		(cont continuation?))
	(define-contl
		(sym symbol?)
		(env list?)
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
			(apply-proc car-args cdr-args '() cont)]
		[map-cont (procedure vals env cont)
			(if (null? (car vals))
				(apply-cont cont (list val))
				(apply-proc procedure (list (caar vals)) env (map-cont procedure (list (cdar vals)) env (cons-cont val cont))))]
	   [last-element-cont (cont)
			(last-element val cont)]
		[set-cont (env var cont)
			(eval-expression val env (set2-cont env var cont))]
		[while-cont (test bodies env cont)
			(if val (map (lambda (e) (eval-expression e env cont)) (reverse bodies)))]
		[set2-cont (env var cont)
			(apply-cont cont (change-env env var val))]
		[letrec-cont (var exprs env cont)
			(eval-exps val (letrec2-cont var exprs env cont) env)]
		[letrec2-cont (vars exprs env cont)
			(eval-exps exprs (last-element-cont cont) (extend-env-recur vars val env))]
		[define-contg (sym env cont)
			(apply-cont cont (change-global-env sym val))]
		[define-contl (sym env cont)
			(extend-env (list sym) (list (eval-expression val env cont)) env)]
		[begin-cont (exps env cont)
			(eval-begin exps env cont)]
		[call/cc-cont (cont)
				(cases procd val
					[closure (ids body env)
						(eval-expression body (extend-env ids (list (acontinuation cont)) env) cont)]
					[else (eopl:error "not valid")])]
		[else (eopl:error "Invalid Cont")]
		
		)))

(define last-element
	(lambda (val cont)
		(if (null? (cdr val))
			(apply-cont cont (car val))
			(last-element (cdr val) cont))))