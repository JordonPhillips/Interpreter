(load "chez-init.ss")

(load "env.ss")
(load "parser.ss")
(load "interpreter.ss")
(load "cont.ss")

(define (rl) (load "main.ss"))

(define (rep)
  (begin
    (display "--> ")
    (write (eval-one-exp (read)))
    (newline)
    (rep)))
    
    
;BNF definition
;---------------
; <program> ::= <form>*
; <form>        <definition> | <expression>
; <definition> ::=      <variable definition> | (begin <definition>*)
; <variable definition> ::= (define <variable> <expression>)
; <expression> ::=      <constant>
;             |     <variable>
;             |    (quote <datum>)
;             |    (lambda <list> <list>)
;             |    (informal-lambda <symbol> <expressions>)
;             |    (application <expression> <list>)
;             |    (lit <constant>)
;             |    (if <expression> <expression> <expression>)
;             |    (if2  <expression> <expression>)
;             |    (set! <variable> <expression>)
;             |    (let <symbols> <expressions> <expressions>)
;             |    (let* <symbols> <expressions> <expressions>)
;             |    (letrec <symbols> <expressions> <expressions>)
;             |    (set! <symbols> <expressions> <expressions>)
;             |    (begin <expressions>)
;             |    (dotted--lambda <pair> <expressions>)
;             |    (cond <lists>)
;             |    (and <expressions>)
;             |    (or <expressions>)
;             |    (case <constant> <lists>)
;             |    (while <expression> <expressions>)
;             |    (set <symbol> <expression>)
  
; <constant> ::=    <number> | <string> | <pair> | <vector> | <boolean> | <symbol> | <null>
; <formals> ::= <variable>
;               |     (<variable>*)
;               |     (<variable> <variable>* . <variable>)
; <application> ::= (<expression> <expression>*)
; <boolean> ::= #t|#f
; <character> ::=    "A" | "B" | "C" | "D" | "E" | "F" | "G"
;                        | "H" | "I" | "J" | "K" | "L" | "M" | "N"
;                        | "O" | "P" | "Q" | "R" | "S" | "T" | "U"
;                        | "V" | "W" | "X" | "Y" | "Z" 
; <string> ::= "<character>" | "<string><character>"
; <list> ::= (<expression>*)
; <pair> ::= (<expression> . <expression>)
; <vector> ::= #(<datum>*)
; <datum> ::= <boolean>
;             |<symbol>
;             |<string>
;             |<number>
;             |<list>
;             |<vector>

; <Number> ::= <PositiveNumber>|<NegativeNumber>
; <NegativeNumber> ::= <NegativeDigit>|<NonZeroNegativeDigit><Digits>
; <PositiveNumber> ::= <Digit>|<NonZeroDigit><Digits>
; <Digits> ::= <Digit>|<Digits><Digits>
; <Digit> ::= 0|<NonZeroDigit>
; <NonZeroDigit> ::= 1|2|3|4|5|6|7|8|9
; <NonZeroNegativeDigit> ::= -1|-2|-3|-4|-5|-6|-7|-8|-9
; <AllNonZeroDigits> ::= <NonZeroDigit>|<NonZeroNegativeDigit>

; <void> ::= <void>
; <set-car!> ::= (set-car! <pair> <expression>)
; <set-cdr!> ::= (set-cdr! <pair> <expression>)
; <+> ::= (+ <number> <number>)
; <-> ::= (- <number> <number>)
; <*> ::= (* <number> <number>)
; <add1> ::= (+ <number> 1)
; <sub1> ::= (- <number> 1)
; <cons> ::= (cons <list> <list>)
; <car> ::= (car <list>)
; <=> ::= (= <number> <number>)
; </> ::= (/ <number> <number>)
; <zero?> ::= (zero? <number>)
; <not> ::= (not <expression>)
; <and> ::= (and <expression>)
; < <, <=, >, >= > ::= (<, <=, >, >= <number> <number>)
; <cdr> ::= (cdr <list>)
; <list> ::= (list <expression>)
; <null?> ::= (null? <list>)
; <eq?> ::= (eq? <expression> <expression>)
; <equal?> ::= (equal? <expression> <expression>)
; <atom?> ::= (atom? <expression>)
; <length> ::= (length <list>)
; <list->vector> ::= (list->vector <list>)
; <list?> ::= (list? <expression>)
; <pair?> ::= (pair? <expression>)
; <procedure?> ::= (procedure? <expression>)
; <vector->list> ::= (vector->list <vector>)
; <vector> ::= (vector <expression>)
; <make-vector> ::= (make-vector  <expression>)
; <vector-ref> ::= (vector-ref <expression>)
; <vector?>  ::= (vector? <expression>)
; <number?>  ::= (number? <expression>)
; <symbol?> ::= (symbol? <expression>)
; <vector-set!> ::= (vector-set! <vector> <expression>)
; <c**r> ::= (c**r <list>)
; <c***r> ::= (c***r <list>)
; <max> :: (max <number> <number>)






