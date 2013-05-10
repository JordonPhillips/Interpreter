;;; Rib cage implementation using:
;;; A list of symbols and
;;; A vector of values

(define empty-env
    (lambda ()
      '()))

(define extend-env
  (lambda (syms vals env)
    (cons (cons syms (list->vector vals)) env)))


(define extend-global-env
    (lambda (sym val)
      (letrec ([helper (lambda (ls)
                          (cond
                              [(null? ls) #f]
                              [(eqv? (caar ls) sym) (set-cdr! (car ls) (list val)) #t]
                              [else (helper (cdr ls))]))])
      (if (not (helper global-env))
          (set! global-env (cons (cons sym (list val)) global-env))))))

(define apply-env
  (lambda (env sym)
    (if (null? env)
        (apply-global-env sym)
    (let ([syms (car (car env))]
          [vals (cdr (car env))]
          [env (cdr env)])
      (let ([pos (find-position sym syms)])
        (if (number? pos)
            (vector-ref vals pos)
            (apply-env env sym)))))))

(define apply-global-env
  (lambda (sym)
    (letrec ([apply (lambda (ls)
                      (cond 
                        [(null? ls) #f]
                        [(eqv? sym (caar ls)) (cadar ls)]
                        [else (apply (cdr ls))]))])
      (let ([return (apply global-env)])
        (if return
          return
          (eopl:error 'apply-env "No binding for ~s" sym))))))

(define find-position
  (lambda (sym ls)
    (cond
      [(null? ls) #f]
      [(eq? sym (car ls)) 0]
      [else (let ([index (find-position sym (cdr ls))])
          (if (number? index)
              (+ index 1)
              #f))])))

(define *prim-proc-names* '(else car + - * add1 sub1 cons = / zero? not and < <= > >= cdr list null? eq? equal?
                            atom? length list->vector list? pair? procedure? vector->list vector make-vector
                            vector-ref vector? number? symbol? set-car! set-cdr! vector-set! caar cddr cadr cdar
                            caaar caadr cadar cdaar caddr cdadr cddar cdddr apply assq assv append map member
                            max void))

(define global-env
  (map (lambda (name)
   (cons name (list (primitive name))))
       *prim-proc-names*))

(define reset-global-env
  (lambda ()
    (set! global-env (map (lambda (name)
                     (cons name (primitive name)))
                         *prim-proc-names*))))



(define extend-env-recur
  (lambda (syms vals env)
    (let* ([vec (list->vector vals)]
     [new-env (cons (cons syms vec) env)])
      (for-each (lambda (item pos)
      (if (proc? item)
          (vector-set! vec
           pos
           (cases proc item
            [closure (ids bodies toss-env)
               (closure ids bodies new-env)]
            [primitive (id)
                 item]))))
    vals
    (make-indices (- (length vals) 1) '()))
      new-env)))

(define make-indices
  (lambda (n accu)
    (if (= n 0)
  (cons 0 accu)
  (make-indices (- n 1) (cons n accu)))))

(define change-env
  (lambda (env sym val)
    (if (null? env)
      (eopl:error 'apply-env "No binding for ~s" sym)
      (let ([syms (caar env)]
            [vals (cdar env)]
            [env (cdr env)])
        (let ((pos (find-position sym syms)))
          (if (number? pos)
            (vector-set! vals pos val)
            (change-env env sym val)))))))