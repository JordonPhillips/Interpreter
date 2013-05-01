(define scheme-value?
  (lambda (v)
    #t))
(define-datatype expression expression? 
  (var-exp
   (id symbol?))
  (informal-lambda-exp
   (id symbol?)
   (body (list-of expression?)))
  (lambda-exp
   (id list?)
   (body list?))
  (app-exp
   (rator expression?)
   (rand list?))
  (lit-exp
   (datum (lambda (x)
      (or (number? x) (string? x) (pair? x) (vector? x) (boolean? x) (symbol? x) (null? x)))))
  (if-exp
   (test-exp expression?)
   (true-exp expression?)
   (false-exp expression?))
  (if-exp2
  (test-exp expression?)
  (true-exp expression?))
  (let-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (exprs (list-of expression?)))
  (let*-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (exprs (list-of expression?)))
  (letrec-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (exprs (list-of expression?)))
   (set!-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (exprs (list-of expression?)))
   (begin-exp
  (exps (list-of expression?)))
   (dotted-lambda-exp
  (id pair?)
  (body (list-of expression?)))
  (cond-exp
   (body (list-of list?)))
  (and-exp
    (body (list-of expression?)))
  (or-exp
    (body (list-of expression?)))
  (case-exp
    (test-val scheme-value?)
    (bodies list?))
  (while-exp
      (test expression?)
      (bodys (list-of expression?)))
  (set-exp
    (var symbol?)
    (val expression?))
  )

(define parse-expression
  (lambda (datum)
    (cond
     [(symbol? datum) (var-exp datum)]
     [(or (null? datum) (number? datum) (string? datum) (vector? datum) (boolean? datum) (null? datum)) (lit-exp datum)]
     [(pair? datum)
       (cond
  [(not (list? datum))
   (eopl:error  'parse-expression "Invalid list structure in ~s" datum)]

  [(eqv? (car datum) 'quote)
   (if (= (length datum) 2)
       (lit-exp (cadr datum))
       (eopl:error  'parse-expression "Invalid quote in ~s" datum))]
  
  [(eqv? (car datum) 'let)
   (if (validApplyingProdcedure? datum)
       (let-exp (map car (cadr datum))
          (map (lambda (expr)
           (parse-expression (cadr expr)))
         (cadr datum))
          (map parse-expression (cddr datum))))]
  
  [(eqv? (car datum) 'let*)
   (if (validApplyingProdcedure? datum)
       (let*-exp (map car (cadr datum))
          (map (lambda (expr)
           (parse-expression (cadr expr)))
         (cadr datum))
          (map parse-expression (cddr datum))))]
    [(eqv? (car datum) 'set!)
       (set-exp (cadr datum)
         (parse-expression (caddr datum)))]   
  [(eqv? (car datum) 'letrec)
   (if (validApplyingProdcedure? datum)
       (letrec-exp (map car (cadr datum))
          (map (lambda (expr)
           (parse-expression (cadr expr)))
         (cadr datum))
          (map parse-expression (cddr datum))))]
      
    [(eqv? (car datum) 'if)
   (cond
    [(= (length datum) 3)
      (apply if-exp2 (map parse-expression (cdr datum)))]
    [(= (length datum) 4)
       (apply if-exp (map parse-expression (cdr datum)))]
       [else (eopl:error  'parse-expression "Invalid if in ~s" datum)])]
  [(eqv? (car datum) 'cond)
    (set! b datum)
    (cond-exp (map list (map (lambda (e) (parse-expression (car e))) (cdr datum))
            (map (lambda (e) (parse-expression (cadr e))) (cdr datum))))]
  [(eqv? (car datum) 'lambda)
   (if (validLambda? datum)
    (cond 
       [(symbol? (cadr datum)) (informal-lambda-exp(cadr datum)
      (map parse-expression (cddr datum)))]
     
     [(list? (cadr datum)) (lambda-exp(cadr datum)
      (map parse-expression (cddr datum)))]
     [(pair? (cadr datum))
     (dotted-lambda-exp (cadr datum) (map parse-expression (cddr datum)))]))]
  [(eqv? (car datum) 'begin)
    (begin-exp (map parse-expression (cdr datum)))]
  [(eqv? (car datum) 'and)
    (and-exp (map parse-expression (cdr datum)))]
  [(eqv? (car datum) 'or)
    (or-exp (map parse-expression (cdr datum)))]
  [(eq? (car datum) 'case)
    (case-exp (cadr datum)
      (letrec ([helper (lambda (ls)
        (if (null? (cdr ls))
          (if (eq? (caar ls) 'else)
            (list (list (caar ls) (parse-expression (cadar ls))))
            (list (list (caar ls) (parse-expression (cadar ls)))))
        (cons (list (caar ls) (parse-expression (cadar ls))) (helper (cdr ls)))))])
  (helper (cddr datum))))]


  [(eqv? (car datum) 'while)
    (while-exp (parse-expression (cadr datum))
        (map parse-expression (cddr datum)))]
  [else (app-exp
         (parse-expression (car datum))
         (map parse-expression (cdr datum)))])]
      
      (else (eopl:error  'parse-expression
              "Invalid concrete syntax ~s" datum)))))

(define unparse-expression
  (lambda (exp)
    (cases expression exp
      [var-exp (id) id]
      [informal-lambda-exp (id body)
         (cons 'lambda
         (cons id (map unparse-expression body)))]
      [lambda-exp (id body) 
      (cons 'lambda
      (cons id (map unparse-expression body)))]
    [dotted-lambda-exp (id body)
    (cons 'lambda 
        (cons id (map unparse-expression body)))]
      [app-exp (rator rand)
         (cons (unparse-expression rator)
         (map unparse-expression rand))]
      [lit-exp (datum)
         (cond
    ((or (number? datum)
         (string? datum)
         (vector? datum)
         (boolean? datum)) datum)
    ((or (pair? datum)
         (symbol? datum)) (list 'quote datum))
    ((null? datum) ''()))]
      [if-exp (test-exp true-exp false-exp)
        (cons 'if
        (map unparse-expression (list test-exp true-exp false-exp)))]
    [if-exp2 (test-exp true-exp)
    (cons 'if (map unparse-expression (list test-exp true-exp)))]
    
      [let-exp (vars vals exprs)
         (cons 'let  (unparse-applying-exp vars vals exprs))]
      [let*-exp (vars vals exprs)
         (cons 'let*  (unparse-applying-exp vars vals exprs))]
      [letrec-exp (vars vals exprs)
         (cons 'letrec  (unparse-applying-exp vars vals exprs))]
    [set!-exp (vars vals exprs)
         (cons 'set!  (unparse-applying-exp vars vals exprs))]
    [begin-exp (exps)
      (cons 'begin (map unparse-expression exps))]
    [cond-exp  (body)
    (cons 'cond (map unparse-expression exps))]
    [and-exp(body)]
    [or-exp(body)]
  [case-exp(test-val bodies)]
  [while-exp(test bodys )]
  [set-exp(var val)])))

(define unparse-applying-exp
  (lambda (vars vals exprs)
    (cons (map (lambda (var val)
     (list var (unparse-expression val)))
         vars vals)
    (map unparse-expression exprs))))

; Validation Functios
(define validApplyingProdcedure?
  (lambda (expr)
  (set! a expr)
    (cond
     ; Check to see that we have at least some form of let, assignments, and expressions
     [(< (length expr) 3)
      (eopl:error  'parse-expression "Incorrect length in ~s" expr)]
     ; Check that the argument list is a valid list
     [(not (list? (cadr expr)))
      (eopl:error  'parse-expression "Bad assignment list in ~s" expr)]
     ; Check that each assignment is a valid list
     [(not (andmap list? (cadr expr)))
      (eopl:error  'parse-expression "Bad assignment in list ~s" expr)]
     ; Check that each assignment is a valid list
     [(not (andmap (lambda (assignment)
         (= (length assignment) 2)) (cadr expr)))
      (eopl:error  'parse-expression "An assignment has invalid length in ~s" expr)]
     ; Check to see if each assignment is a symbol
     [(not (andmap (lambda (itm)
         (symbol? (car itm))) (cadr expr)))
      (eopl:error  'parse-expression "Each assignment must be to a symbol in ~s" expr)]
     ; Check for uniqueness
     [(not (set? (map car (cadr expr))))
      (eopl:error  'parse-expresssion "Each assignment must be to a unique symbol in ~s" expr)]
     [else #t])))

(define validLambda?
  (lambda (expr)
    (cond
     ; Check to see that we have at least lambda, an argument, and expressions
     [(< (length expr) 3)
      (eopl:error  'parse-expression "Incorrect length in ~s" expr)]
     ; Make sure the arguments are valid
     [(not (or (symbol? (cadr expr))
         (null? (cadr expr))
         (and (or (list? (cadr expr)) (pair? (cadr expr)))
        (or (pair? (cadr expr)) (andmap symbol? (cadr expr))))))
      (eopl:error  'parse-expression "Incorrect argument list in ~s" expr)]
     ; Make sure each variable only occurs once
     [(if (list? (cadr expr))
    (not (set? (cadr expr)))
    #f)
      (eopl:error  'parse-expression "Each variable may only occur once in ~s" expr)]
     [else #t])))

; Helper Functions
(define set?
  (lambda (l)
    (if (null? l)
        #t
        (let set?? ((l1 (list (car l))) (l2 (cdr l)))
          (cond
           ((null? l2) #t)
           ((contains? l1 (car l2)) #f)
           (else (set?? (cons (car l2) l1) (cdr l2))))))))

(define contains?
  (lambda (l n)
    (if (null? l)
        #f
        (if (equal? (car l) n)
            #t
            (contains? (cdr l) n)))))