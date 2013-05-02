(define top-level-eval
  (lambda (form)
    (cases expression form
	   [define-exp (sym val)
	     (extend-global-env sym (eval-expression val (empty-env)))]
	   [else (eval-expression form (empty-env))])))

(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (expand-syntax (parse-expression exp))]
	   [initial-environment empty-env]
	   [result (top-level-eval  parse-tree)])
      result)))
	  

(define eval-expression
  (lambda (exp env)
    (cases expression exp
	   [var-exp (id) (apply-env env id)]
	   [lit-exp (val) val]
	   [lambda-exp (id body)
		       (letrec ([helper (lambda (ls)
							(cond
								[(null? (cdr ls))
									(list (make-closure id (car ls) env))]
								[else
									(cons (make-closure id (car ls) env) (helper (cdr ls)))]))])
						(helper body))]
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
		[begin-exp (exps) (begin-eval exps env)]		
		[informal-lambda-exp (id body) (car (map (lambda (e) (make-informal-closure id e env)) (reverse body)))]
		[dotted-lambda-exp (id body) 
		(let* ([t (parse-parms id)]
			   [params (car t)]
			   [left (caadr t)])
			   (car (map (lambda (e) (make-dotted-closure params left e env)) (reverse body))))]
		
		[and-exp (body)
			(letrec ([helper (lambda (ls)
				(cond
					[(null? ls) #t]
					[(null? (cdr ls)) (eval-expression (car ls) env)]
					[(eval-expression (car ls) env) (helper (cdr ls))]
					[else #f]))])
					(helper body))]
		[or-exp (body)
			(letrec ([helper (lambda (ls)
				(cond
					[(null? ls) #f]
					[(null? (cdr ls)) (eval-expression (car ls) env)]
					[else (let ([evaluated-car (eval-expression (car ls) env)])
						(if evaluated-car
							evaluated-car
							(helper (cdr ls))))]))])
					(helper body))]
		[while-exp (test bodies)
			(letrec ([helper (lambda ()
						(if (eval-expression test env)
							(begin (map (lambda (e) (eval-expression e env)) (reverse bodies)) (helper))))])
							(helper))]
		[set-exp (var val)
			(change-env env var (eval-expression val env)) (list 'special-symbol env)]
		[define-exp (symbol expression)
			 (list 'special-symbol (define-env env env symbol (eval-expression expression env)))]
		[letrec-exp (vars vals exprs)
			(begin-eval exprs (extend-env-recur vars (map (lambda (e) (eval-expression e env)) vals) env))]
		[case-exp (test bodies)]
		[cond-exp (body)]
		[let-exp (vars vals exprs)]
		[let*-exp (vars vals exprs)]
		[set!-exp (vars vals exprs)]
		[named-let-exp (id vars vals exprs)]
		)))

(define begin-eval
	(lambda (exprs env) 
		(cond 
			[(null? exprs) '()]
			[(null? (cdr exprs)) (eval-expression (car exprs) env)]
			[else (let ([result (eval-expression (car exprs) env)])
				(if (and (pair? result) (eq? (car result) 'special-symbol)) (set! env (cadr result)))
					(begin-eval (cdr exprs) env))])))

	 
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
						

(define-datatype procd procd?
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
	
(define apply-prim-proc
  (lambda (prim-proc args env)
	(set! aab prim-proc)
    (case prim-proc
      [(void) (void)]
	  [(else) #t]
      [(set-car!) (set-car! (car args) (cadr args))]
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
	  [(car)  (car (car args))]
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
	  [(append) (apply append args)]
	  [(max)  (apply max args)]
	  [(member) (member (car args) (cadr args))]
      [(assq) (apply assq args)]
	  [(assv) (apply assv args)]
	  [(apply) (apply-proc (car args) (cadr args) env)]
	  [(map)  (letrec 
				([helper (lambda (proc args)
				(cond
				[(pair? args)
					(cons (apply-proc proc (list (car args)) env) (helper proc (cdr args)))]
				[(null? args) '()]
				[else (eopl:error  'parse-expression "Not a proper list: ~s" args)]))])
				(helper (car args) (cadr args)))]

      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))
			
(define apply-proc
	(lambda (proc args env2)
	(cond 
		[(procd? proc)
			(cases procd proc
				[closure (parameters body env)
					(eval-expression body
						(if (null? args)
							env2
						(extend-env parameters args env)))]
				[primitive (id)
					(apply-prim-proc id args env2)]
				[informal-closure (id body env)
					(eval-expression body (extend-env (list id) (list args) env))]
				[dotted-closure (parameters leftover body env)
					(let* ([parsed-args (parse-args (length parameters) args)]
						   [defined (car parsed-args)]
						   [other (cadr parsed-args)])
					(eval-expression body (extend-env (list leftover) (list other) (extend-env parameters defined env))))])]
		[(list? proc)
			(cases procd (car proc)
				[closure (parameters body env)
						(let ([new-env (extend-env parameters args (append env env2))])
							(letrec ([helper (lambda (ls)
								(if (null? (cdr ls))
										(apply-proc (car ls) '() new-env)
										(begin (let ([result (apply-proc (car ls) '() new-env)])
													(if (and (pair? result) (eq? (car result) 'special-symbol))
														(set! new-env (cadr result))
														result)) (helper (cdr ls)))))]) (helper proc)))]		
				[else (letrec ([helper (lambda (ls)
						(if (null? (cdr ls))
								(apply-proc (car ls) args env)
								(begin (apply-proc (car ls) args env) (helper (cdr ls)))))]) (helper proc))]
								)])))
		
