(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (expand-syntax (parse-expression exp))]
       [result (top-level-eval parse-tree)])
      result)))
      
(define top-level-eval
  (lambda (form)
    (cases expression form
     [define-exp (sym val)
       (extend-global-env sym (eval-expression val (empty-env) (halt-cont)))]
     [else (eval-expression form (empty-env) (halt-cont))])))

(define eval-expression
  (lambda (exp env cont)
    (cases expression exp
       [var-exp (id) (apply-cont cont (apply-env env id))]
       [lit-exp (val) (apply-cont cont val)]
       [lambda-exp (id body)
            (apply-cont cont (make-closure id body env))]
       [app-exp (operator operand)
          (eval-exps (cons operator operand) (proc-cont cont) env)]
        [if-exp (test-exp true-exp false-exp)
            (eval-expression test-exp env (if-cont true-exp false-exp cont env))]
        [if-exp2 (test-exp true-exp)
           (eval-expression test-exp env (if-cont2 true-exp cont env))] 
        [begin-exp (exps) (eval-exps exps (last-element-cont cont) env)]      
        [informal-lambda-exp (id body) 
			(apply-cont cont (make-informal-closure id body env))]
        [dotted-lambda-exp (id sym body) 
			(apply-cont cont (make-dotted-closure id sym body env))]
        [while-exp (test bodies)
            (eval-expression test env (if-cont2 (begin-exp (append bodies (list (while-exp test bodies)))) cont env))]
        [set-exp (var val)
		(apply-cont (set-cont env var cont) val)]
		[letrec-exp (vars vals exprs)
			;(eval-begin exprs (extend-env-recur vars (map (lambda (e) (eval-expression e env)) vals) env))
			(apply-cont (letrec-cont vars exprs env cont) vals)]
		[define-exp (sym body)
      (apply-cont (def-cont sym env cont) (eval-expression body env cont))]

    ;(begin (set! env (define-env env env sym (eval-expressions val env cont))) env)]
		[call/cc-exp (receiver)
			(eval-expression receiver env (call/cc-cont cont))]
		[and-exp (body)]
        [or-exp (body)]
        [case-exp (test bodies)]
        [cond-exp (body)]
        [let-exp (vars vals exprs)]
        [let*-exp (vars vals exprs)]
        [set!-exp (vars vals exprs)]
        )))

(define expand-syntax
  (lambda (expr)
    (cases expression expr
       [let-exp (vals vars exprs)
            (app-exp (lambda-exp vals (begin-exp (map expand-syntax exprs)))
                            (map expand-syntax vars))]
       [let*-exp (vals vars exprs)
        (letrec ([helper (lambda (var val)
                                    (cond
                                       [(null? var) (app-exp (lambda-exp '() (being-exp (map expand-syntax exprs))) '())]
                                       [(null? (cdr var)) (app-exp (lambda-exp (list (car var)) (begin-exp (map expand-syntax exprs))) (list (expand-syntax (car val))))]
                                       [else (app-exp (lambda-exp (list (car var)) (begin-exp (list (helper (cdr var) (cdr val))))) (list (expand-syntax (car val))))]))])
                                (helper vals vars))]
            
       [if-exp (conditional if-true if-false)
           (if-exp (expand-syntax conditional)
               (expand-syntax if-true)
               (expand-syntax if-false))]
       [define-exp (sym val) 
          (define-exp sym (expand-syntax val))]
       [app-exp (rator rand)
            (app-exp (expand-syntax rator) (map expand-syntax rand))]
       [lambda-exp (ids bodies)
               (lambda-exp ids (expand-syntax bodies))]
	   [informal-lambda-exp (id body)
               (informal-lambda-exp id (expand-syntax body))]
	   [dotted-lambda-exp (id sym body)
               (dotted-lambda-exp id sym (expand-syntax body))]
        [and-exp (body)
            (letrec ([helper (lambda (ls)
                (cond
                    [(null? ls) (lit-exp #t)]
                    [(null? (cdr ls)) (if-exp2 (expand-syntax (car ls)) (car ls))]
                    [else (if-exp (expand-syntax (car ls)) (helper (cdr ls)) (lit-exp #f))]))])
                    (helper body))]
        [or-exp (body)
            (letrec ([helper (lambda (ls)
                (cond
                    [(null? ls) (lit-exp #f)]
                    [(null? (cdr ls)) (if-exp2 (expand-syntax (car ls)) (car ls))]
                    [else (if-exp (expand-syntax (car ls) (lit-exp #t) (helper (cdr ls))))]))])
                    (helper body))]
        [cond-exp (body)
            (letrec ([helper (lambda (ls)
                                    (if (null? (cdr ls))
                                       (if-exp2 (caar ls) (cadar ls))
                                       (if-exp (caar ls) (cadar ls) (helper (cdr ls)))))])
                                (helper body))]
        [case-exp (test-value cases)
            (letrec ([helper (lambda (ls)
                            (if (null? (cdr ls))
                                (if (and (not (null? (car ls))) (eq? (caar ls) 'else))
                                    (if-exp2 (app-exp (var-exp 'else) '()) (cadar ls))
                                    (if-exp2 (app-exp (var-exp 'member) (list (lit-exp test-value) (lit-exp (caar ls))))
                                    (cadar ls)))
                            (if-exp (app-exp (var-exp 'member) (list (lit-exp test-value) (lit-exp (caar ls))))
                            (cadar ls)
                            (helper (cdr ls)))))])
            (helper cases))]
        [while-exp (test-value bodies)
            (while-exp (expand-syntax test-value) (map expand-syntax bodies))]
		[begin-exp (exprs)
			(begin-exp (map expand-syntax exprs))]
		[letrec-exp (vars vals exprs)
			(letrec-exp vars (map expand-syntax vals) (map expand-syntax exprs))]
		[call/cc-exp (reciever)
			(call/cc-exp (expand-syntax reciever))]
       [else expr])))
     
(define make-closure
  (lambda (id body env)
    (closure id body env)))

(define make-informal-closure
    (lambda (id body env)
        (informal-closure id body env)))

(define make-dotted-closure
    (lambda (params leftover body env)
        (dotted-closure params leftover body env)))
    

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
  [acontinuation
	(cont continuation?)]
  [primitive 
    (id symbol?)])
    


(define apply-prim-proc
  (lambda (prim-proc args env cont)
    (case prim-proc
      [(void) (apply-cont cont (void))]
      [(else) (apply-cont cont #t)]
      [(set-car!) (apply-cont cont (set-car! (car args) (cadr args)))]
      [(set-cdr!) (apply-cont cont (apply set-cdr! args))]
      [(+) (apply-cont cont (apply + args))]
      [(-) (apply-cont cont (apply - args))]
      [(*) (apply-cont cont (apply * args))]
      [(add1) (apply-cont cont (+ (car args) 1))]
      [(sub1) (apply-cont cont (- (car args) 1))]
      [(cons) (apply-cont cont (apply cons args))]
      [(car) (apply-cont cont (car (car args)))]
      [(=) (apply-cont cont (apply = args))]
      [(/) (apply-cont cont (apply / args))]
      [(break) (apply-cont (halt-cont) args)]
      [(zero?) (apply-cont cont (zero? (car args)))]
      [(not) (apply-cont cont (not (car args)))]
      [(and) (apply-cont cont (and (car args) (cadr args)))]
      [(<) (apply-cont cont (apply < args))]
      [(<=) (apply-cont cont (apply <= args))]
      [(>) (apply-cont cont (apply > args))]
      [(>=) (apply-cont cont (apply >= args))]
      [(car)  (apply-cont cont (car (car args)))]
      [(cdr) (apply-cont cont (cdr (car args)))]
      [(list) (apply-cont cont (car (list args)))]
      [(null?) (apply-cont cont (null? (car args)))]
      [(eq?) (apply-cont cont (apply eq? args))]
      [(equal?) (apply-cont cont (apply equal? args))]
      [(atom?) (apply-cont cont (atom? (car args)))]
      [(length) (apply-cont cont (length (car args)))]
      [(list->vector) (apply-cont cont (list->vector (car args)))]
      [(list?) (apply-cont cont (list? (car args)))]
      [(pair?) (apply-cont cont (pair? (car args)))]
      [(procedure?) (apply-cont cont (procedure? (car args)))]
      [(vector->list) (apply-cont cont (vector->list (car args)))]
      [(vector) (apply-cont cont (list->vector args))]
      [(make-vector) (apply-cont cont (make-vector (car args)))]
      [(vector-ref) (apply-cont cont (vector-ref (car args) (cadr args)))]
      [(vector?) (apply-cont cont (vector? (car args)))]
      [(number?) (apply-cont cont (number? (car args)))]
      [(symbol?) (apply-cont cont (symbol? (car args)))]
      [(vector-set!) (apply-cont cont (vector-set! (car args) (cadr args) (caddr args)))]
      [(caar) (apply-cont cont (caar (car args)))]
      [(cddr) (apply-cont cont (cddr (car args)))]
      [(cadr) (apply-cont cont (cadr (car args)))]
      [(cdar) (apply-cont cont (cdar (car args)))]
      [(caaar) (apply-cont cont (caaar (car args)))]
      [(caadr) (apply-cont cont (caadr (car args)))]
      [(cadar) (apply-cont cont (cadar (car args)))]
      [(cdaar) (apply-cont cont (cdaar (car args)))]
      [(caddr) (apply-cont cont (caddr (car args)))]
      [(cdadr) (apply-cont cont (cdadr (car args)))]
      [(cddar) (apply-cont cont (cddar (car args)))]
      [(cdddr) (apply-cont cont (cdddr (car args)))]
      [(append) (apply-cont cont (apply append args))]
      [(member) (apply-cont cont (member (car args) (cadr args)))]
      [(assq) (apply-cont cont (apply assq args))]
      [(assv) (apply-cont cont (apply assv args))]
      [(apply) (apply-cont (apply-proc-cont (car args) (cadr args) env cont) '())]
      [(map)  (apply-cont (map-cont (car args) (cdr args) env (remove-first-cont cont)) '())]
      [(max) (apply-cont cont (apply max args))]

      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))
            
(define apply-proc
    (lambda (proc args env cont)
    (cond 
        [(procd? proc)
            (cases procd proc
                [closure (parameters body env)
                    (eval-expression body (extend-env parameters args env) cont)]
                [primitive (id)
                    (apply-prim-proc id args env cont)]
                [informal-closure (id body env)
                    (eval-expression body (extend-env (list id) (list args) env) cont)]
				[acontinuation (cont)
					(apply-cont cont (car args))] 
                [dotted-closure (parameters leftover body env)
                    (let* ([parsed-args (parse-args (length parameters) args)]
                           [defined (car parsed-args)]
                           [other (cadr parsed-args)])
                    (eval-expression body (extend-env (list leftover) (list other) (extend-env parameters defined env)) (halt-cont)))])]
        [(list? proc)
        (letrec ([helper (lambda (ls)
            (if (null? (cdr ls))
                    (apply-proc (car ls) args env)
                    (begin (apply-proc (car ls) args env) (helper (cdr ls)))))]) (helper proc))])))
        
(define-syntax return-first
  (syntax-rules ()
    [(_ e) e]
    [(_ e1 e2 ...) (let ([a e1]) (begin e2 ...) a)]))

(define-syntax for
  (syntax-rules (:)
       [(_ ( init : test : update) body ...)
     (begin init
        (let loop ()
          (if test
          (begin body ... update (loop)))))]
    ))
	
(define eval-exps
  (lambda (exps cont env)
    (if (null? exps)
	(apply-cont cont '())
	(eval-expression (car exps) env (eval-exps-cont (cdr exps) env cont)))))