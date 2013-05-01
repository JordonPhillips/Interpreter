
;;; Rib cage implementation using:
;;; A list of symbols and
;;; A vector of values

(define empty-env
    (lambda ()
          '()))

(define extend-env
  (lambda (syms vals env)
    (cons (cons syms (list->vector vals)) env)))

(define apply-env
  (lambda (env sym)
    (if (null? env)
		(eopl:error 'apply-env "No binding for ~s" sym)
	(let ([syms (car (car env))]
	      [vals (cdr (car env))]
	      [env (cdr env)])
	  (let ([pos (find-position sym syms)])
	    (if (number? pos)
		(vector-ref vals pos)
		(apply-env env sym)))))))

(define find-position
  (lambda (sym ls)
    (cond [(null? ls) #f]
	  [(eq? sym (car ls)) 0]
	  [else (let ([index (find-position sym (cdr ls))])
		  (if (number? index)
		      (+ index 1)
		      #f))])))
