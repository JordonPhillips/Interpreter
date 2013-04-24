
(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (parse-expression exp)]
	   [initial-environment global-env]
	   [result (eval-expression parse-tree initial-environment)])
      result)))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
	   [var-exp (id) (apply-env env id)]
	   [lit-exp (val) val]
	   [lambda-exp (id body)
		       (car (map (lambda (e) (make-closure id e env)) (reverse body)))]
	   [app-exp (operator operand)
		    (let ([procedure (eval-expression operator env)]
			  [arg (map (lambda (e) (eval-expression e env)) operand)])
		      (apply-proc procedure arg env))]
		[if-exp (test-exp true-exp false-exp)
			(if (eval-expression test-exp env)
				(eval-expression true-exp env)
				(eval-expression false-exp env))]
		[if-exp2 (test-exp true-exp)
			(if (eval-expression test-exp env)
				(eval-expression true-exp env))] 
		[begin-exp (exps) (car (map (lambda (e) (eval-expression e env)) (reverse exps)))]		
		[informal-lambda-exp (id body) (car (map (lambda (e) (make-informal-closure id e env)) (reverse body)))]
		[dotted-lambda-exp (id body) 
		(let* ([t (parse-parms id)]
			   [params (car t)]
			   [left (caadr t)])
			   (car (map (lambda (e) (make-dotted-closure params left e env)) (reverse body))))]
		[cond-exp (body)
			(letrec ([helper (lambda (ls)
											(cond 
											[(null? ls) (set! ls '())]
											[(eval-expression (caar ls) env) (eval-expression (cadar ls) env)]
											[else (helper (cdr ls))] ))])
					(helper body))]
		[let-exp (vars vals exprs)]
		[let*-exp (vars vals exprs)]
		[letrec-exp (vars vals exprs)]
		[set!-exp (vars vals exprs)]
		)))
(define *prim-proc-names* '(else car + - * add1 sub1 cons = / zero? not and < <= > >= cdr list null? eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! caar cddr cadr cdar caaar caadr cadar cdaar caddr cdadr cddar cdddr void))

	 
(define make-closure
  (lambda (id body env)
    (closure id body env)))
(define make-informal-closure
	(lambda (id body env)
		(informal-closure id body env)))
(define make-dotted-closure
	(lambda (params leftover body env)
		(dotted-closure params leftover body env)))
	
(define parse-parms
	(lambda (ls)
		(if (symbol? ls)
			ls
			(let ([t (parse-parms (cdr ls))])
				(if (symbol? t)
					(cons (list (car ls)) (list (list t)))
					(cons (cons (car ls) (car t)) (cdr t)))))))
(define parse-args
	(lambda (n ls)
		(if (= n 0)
			(cons #t ls)
			(let ([t (parse-args (- n 1) (cdr ls))])
					(if (eq? (car t) #t)
						(cons (list (car ls)) (list (cdr t)))
						(cons (cons (car ls) (car t)) (cdr t)))))))
(define-datatype procedure procedure?
  [closure
   (id (list-of symbol?))
   (body expression?)
   (env list?)]
   [informal-closure
	(id symbol?)
	(body expression?)
	(env list?)]
	[dotted-closure
	 (parameters list?)
	 (leftover symbol?)
	 (body expression?)
	 (env list?)]
   [primitive 
	(id symbol?)])
	
(define global-env
  (extend-env            
     *prim-proc-names*   
     (map primitive    
          *prim-proc-names*)
     (empty-env)))
(define apply-prim-proc
  (lambda (prim-proc args)
    (case prim-proc
      [(void) (void)]
	  [(else) (#t)]
      [(set-car!) (apply set-car! args)]
      [(set-cdr!) (apply set-cdr! args)]
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(add1) (+ (car args) 1)]
      [(sub1) (- (car args) 1)]
      [(cons) (apply cons args)]
      [(car) (car (car args))]
      [(=) (apply = args)]
      [(/) (apply / args)]
      [(zero?) (zero? (car args))]
      [(not) (not (car args))]
      [(and) (and (car args) (cadr args))]
      [(<) (apply < args)]
      [(<=) (apply <= args)]
      [(>) (apply > args)]
      [(>=) (apply >= args)]
      [(cdr) (cdr (car args))]
      [(list) (car (list args))]
      [(null?) (null? (car args))]
      [(eq?) (apply eq? args)]
      [(equal?) (apply equal? args)]
      [(atom?) (atom? (car args))]
      [(length) (length (car args))]
      [(list->vector) (list->vector (car args))]
      [(list?) (list? (car args))]
      [(pair?) (pair? (car args))]
      [(procedure?) (procedure? (car args))]
      [(vector->list) (vector->list (car args))]
      [(vector) (list->vector args)]
      [(make-vector) (make-vector (car args))]
      [(vector-ref) (vector-ref (car args) (cadr args))]
      [(vector?) (vector? (car args))]
      [(number?) (number? (car args))]
      [(symbol?) (symbol? (car args))]
      [(vector-set!) (vector-set! (car args) (cadr args) (caddr args))]
      [(caar) (caar (car args))]
      [(cddr) (cddr (car args))]
      [(cadr) (cadr (car args))]
      [(cdar) (cdar (car args))]
      [(caaar) (caaar (car args))]
      [(caadr) (caadr (car args))]
      [(cadar) (cadar (car args))]
      [(cdaar) (cdaar (car args))]
      [(caddr) (caddr (car args))]
      [(cdadr) (cdadr (car args))]
      [(cddar) (cddar (car args))]
      [(cdddr) (cdddr (car args))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))
			
(define apply-proc
	(lambda (proc args env)
	(cond 
		[(procedure? proc)
			(cases procedure proc
				[closure (parameters body env)
					(eval-expression body (extend-env parameters args env))]
				[primitive (id)
					(apply-prim-proc id args)]
				[informal-closure (id body env)
					(eval-expression body (extend-env (list id) (list args) env))]
				[dotted-closure (parameters leftover body env)
					(let* ([parsed-args (parse-args (length parameters) args)]
						   [defined (car parsed-args)]
						   [other (cadr parsed-args)])
					(eval-expression body (extend-env (list leftover) (list other) (extend-env parameters defined env))))])]
		[(list? proc)
		(map (lambda (e) (apply-proc e args env)) proc)])))