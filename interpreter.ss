(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (parse-expression exp)]
       [initial-environment global-env]
       [result (eval-expression (expand-syntax parse-tree) initial-environment (halt-cont))])
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
               (letrec ([helper (lambda (ls)
                            (cond
                                [(null? (cdr ls))
                                    (list (make-closure id (car ls) env))]
                                [else
                                    (cons (make-closure id (car ls) env) (helper (cdr ls)))]))])
                        (helper body))]
       [app-exp (operator operand)
            (let ([procedure (eval-expression operator env cont)]
              [arg (map (lambda (e) (eval-expression e env cont)) operand)])
              (apply-proc procedure arg env))]
        [if-exp (test-exp true-exp false-exp)
            (eval-expression test-exp env (if-cont true-exp false-exp cont env))]
        [if-exp2 (test-exp true-exp)
            (if (eval-expression test-exp env cont)
                (eval-expression true-exp env cont))] 
        [begin-exp (exps) (car (map (lambda (e) (eval-expression e env cont)) (reverse exps)))]      
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
                    [(null? (cdr ls)) (eval-expression (car ls) env cont)]
                    [(eval-expression (car ls) env cont) (helper (cdr ls))]
                    [else #f]))])
                    (helper body))]
        [or-exp (body)
            (letrec ([helper (lambda (ls)
                (cond
                    [(null? ls) #f]
                    [(null? (cdr ls)) (eval-expression (car ls) env cont)]
                    [else (let ([evaluated-car (eval-expression (car ls) env cont)])
                        (if evaluated-car
                            evaluated-car
                            (helper (cdr ls))))]))])
                    (helper body))]
        [while-exp (test bodies)
            (letrec ([helper (lambda ()
                        (if (eval-expression test env cont)
                            (begin (map (lambda (e) (eval-expression e env cont)) (reverse bodies)) (helper))))])
                            (helper))]
        [set-exp (var val)
            (set! var val)]
        [case-exp (test bodies)]
        [cond-exp (body)]
        [let-exp (vars vals exprs)]
        [let*-exp (vars vals exprs)]
        [letrec-exp (vars vals exprs)]
        [set!-exp (vars vals exprs)]
        )))

(define expand-syntax
  (lambda (expr)
    (cases expression expr
       [let-exp (vals vars exprs)
            (app-exp (lambda-exp vals (map expand-syntax exprs))
                            (map expand-syntax vars))]
       [let*-exp (vals vars exprs)
        (letrec ([helper (lambda (var val)
                                    (cond
                                       [(null? var) (app-exp (lambda-exp '() (map expand-syntax exprs)) '())]
                                       [(null? (cdr var)) (app-exp (lambda-exp (list (car var)) (map expand-syntax exprs)) (list (expand-syntax (car val))))]
                                       [else (app-exp (lambda-exp (list (car var)) (list (helper (cdr var) (cdr val)))) (list (expand-syntax (car val))))]))])
                                (helper vals vars))]
            
       [if-exp (conditional if-true if-false)
           (if-exp (expand-syntax conditional)
               (expand-syntax if-true)
               (expand-syntax if-false))]
       [app-exp (rator rand)
            (app-exp (expand-syntax rator) (map expand-syntax rand))]
       [lambda-exp (ids bodies)
               (lambda-exp ids (map expand-syntax bodies))]
        [and-exp (body)
            (and-exp (map expand-syntax body))]
        [or-exp (body)
            (or-exp (map expand-syntax body))]
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
      [(max) (max (car args) (cadr args))]

      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-proc)])))
            
(define apply-proc
    (lambda (proc args env)
    (cond 
        [(procedure? proc)
            (cases procedure proc
                [closure (parameters body env)
                    (eval-expression body (extend-env parameters args env) (halt-cont))]
                [primitive (id)
                    (apply-prim-proc id args env)]
                [informal-closure (id body env)
                    (eval-expression body (extend-env (list id) (list args) env) (halt-cont))]
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