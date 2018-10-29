#lang racket

; === Parser and Interpreter for the language given in Project 3, CIS505/705, 
;        Kansas State University, Fall 2018

; 

; This is a skeleton, with 10 places where code needs to be changed/inserted.
; Each such place is marked with "CHANGE #k" where k is a number
;  the indicates the suggested priority (lower numbers mean higher priority).
; 
; #1: this is about how to evaluate an identifier;
;      it should be looked up in the environment
; 
; #2: this is about how to evaluate a function definition;
;      the appropriate closure should be created
;
; #3: this is about how to evaluate the argument of an application
;
; #4: this is about how to evaluate a function application,
;       after the function part has been evaluated to a closure
;       and after the argument part has been evaluated;
;      this involves calling the closure body in an environment
;       that implements static scope 
;        (cf. the slides on implementing higher-order-functions
;         that we discussed in detail in class)
;
; #5: this is about how to evaluate a let expression
;       (only a minor change is needed)
;
; #6a: this is about how to evaluate addition of integer expressions
;        which will be similar to subtraction and multiplication
;            (already handled).
;
;  At this point, you can handle programs without if-expressions,
;    and without tables.
;
; #7: this is about how to parse if-expressions;
;      you should take inspiration from how other constructs are parsed
;
; #8: this is about which expressions are considered true and which
;      are considered false (this is specified in the question text).
;
;  Finally, we can embark on how to handle tables:
;
; #9: this is about how to handle the tuple operator which creates a
;      dictionary with a single entry
;
; #6b: this is about how to implement '+' when applied to tables
;         (you will want to append one to the other).
;
; #10: this is about how to implement '*' when applied to two tables;
;         the outline of the code is already there but with several flaws
;

; --- what is visible to the outside

(provide run-lexer)
(provide run-parser)
(provide run)

; --- syntax

; exp ::= id
;      |  num
;      |  "fn" id exp
;      |  "app" exp1 exp2
;      |  "let" id exp1 exp2
;      |  "if" exp1 exp2 exp3
;      |  "tuple" id1 id2
;      |  "select1" id1 exp0
;      |  "select2" id2 exp0
;      |  op exp1 exp2 

;  op ::= "+"  (also union)
;      |  "-"  (also set difference)
;      |  "*"  (also join)
;
; === lexical analysis

; --- tokens

(struct IdentT (string))
(struct NumT (num))
(struct FnT ())
(struct AppT ())
(struct LetT ())
(struct IfT ())
(struct TupleT ())
(struct Select1T ())
(struct Select2T ())
(struct PlusT ())
(struct MinusT ())
(struct TimesT ())

(define (char->digit ch)
  (- (char->integer ch) (char->integer #\0)))
  
(define (lexer chars)
   (if (null? chars)
      null
      (let ([current (first chars)] [remain (rest chars)])
         (cond
	   [(eq? current #\+) (cons (PlusT) (lexer remain))]
	   [(eq? current #\-) (cons (MinusT) (lexer remain))]
           [(eq? current #\*) (cons (TimesT) (lexer remain))]
           [(eq? current #\space) (lexer remain)]	   	   
           [(eq? current #\newline) (lexer remain)]	   	   
	   [(char-numeric? current) (num-state (char->digit current) remain)]
	   [(char-alphabetic? current) (ident-state (list current) remain)]
	   [else (display (string-append "unknown symbol "
	                         (list->string (list current)) "\n"))]
	))))

(define (num-state seen chars)
   (if (and (pair? chars) (char-numeric? (first chars)))
      (num-state (+ (* 10 seen) (char->digit (first chars))) (rest chars))
      (cons (NumT seen) (lexer chars))
    ))

(define (ident-state seen chars)
   (if (and (pair? chars) 
            (or (char-alphabetic? (first chars))
                (char-numeric? (first chars))))
      (ident-state (append seen (list (first chars))) (rest chars))
      (cons (mk-alpha-token (list->string seen)) (lexer chars))
   ))

(define (mk-alpha-token seen)
   (cond
      [(equal? seen "fn") (FnT)]
      [(equal? seen "app") (AppT)]
      [(equal? seen "let") (LetT)]
      [(equal? seen "if") (IfT)]
      [(equal? seen "tuple") (TupleT)]
      [(equal? seen "select1") (Select1T)]
      [(equal? seen "select2") (Select2T)]
      [else (IdentT seen)]
     ))

(define (run-lexer x) (lexer (string->list x)))

; === parsing

; --- syntax trees

(struct IdentExp (string))
(struct NumExp (num))
(struct LamExp (formal body))
(struct AppExp (fun arg))
(struct LetExp (id exp1 exp2))
(struct IfExp (test exp1 exp2))
(struct TupleExp (string1 string2))
(struct Select1Exp (string1 exp0))
(struct Select2Exp (string2 exp0))
(struct PlusExp (exp1 exp2))
(struct MinusExp (exp1 exp2))
(struct TimesExp (exp1 exp2))

(define (parExpectIdent error-msg tks)
   (if (and (pair? tks) (IdentT? (first tks)))
      (values (IdentT-string (first tks)) (rest tks))
      (display error-msg)
   ))

(define (parExp tks)
   (if (pair? tks)
      (let ([tk (first tks)] [tks0 (rest tks)])
         (cond
            [(IdentT? tk)
               (values (IdentExp (IdentT-string tk)) tks0)]
            [(NumT? tk)
               (values (NumExp (NumT-num tk)) tks0)]
 	    [(FnT? tk)
 	       (let*-values (
 	          [(id tks1) (parExpectIdent
 		                 "identifier expected after 'fn'\n" tks0)]
 		  [(e tks2) (parExp tks1)])
 		 (values (LamExp id e) tks2))]
 	    [(AppT? tk)
 	       (let*-values (
 	          [(e1 tks1) (parExp tks0)]
 		  [(e2 tks2) (parExp tks1)])
 		 (values (AppExp e1 e2) tks2))]
 	    [(LetT? tk)
 	       (let*-values (
 	          [(id tks1) (parExpectIdent
 		                 "identifier expected after 'let'\n" tks0)]
 	          [(e1 tks2) (parExp tks1)]
 		  [(e2 tks3) (parExp tks2)])
 		 (values (LetExp id e1 e2) tks3))]
	    [(IfT? tk)
	       (let*-values (
	          [(e1 tks1) (parExp tks0)]
		  [(e2 tks2) (parExp tks1)]
		  [(e3 tks3) (parExp tks2)])
		 (values (IfExp e1 e2 e3) tks3))] ;;; CHANGEddddd #7
            [(TupleT? tk)
               (let*-values (
 	          [(id1 tks1) (parExpectIdent
 		                 "identifier expected after 'tuple'\n" tks0)]
 	          [(id2 tks2) (parExpectIdent
 		                 "two identifiers expected after 'tuple'\n" tks1)])
                 (values (TupleExp id1 id2) tks2))]
            [(Select1T? tk)
               (let*-values (
 	          [(id1 tks1) (parExpectIdent
 		                 "identifier expected after 'select1'\n" tks0)]
                  [(e0 tks2) (parExp tks1)])
                 (values (Select1Exp id1 e0) tks2))]
            [(Select2T? tk)
               (let*-values (
 	          [(id2 tks1) (parExpectIdent
 		                 "identifier expected after 'select2'\n" tks0)]
                  [(e0 tks2) (parExp tks1)])
                 (values (Select2Exp id2 e0) tks2))]
 	    [(PlusT? tk)
 	       (let*-values (
 	          [(e1 tks1) (parExp tks0)]
 		  [(e2 tks2) (parExp tks1)])
 		 (values (PlusExp e1 e2) tks2))]
 	    [(MinusT? tk)
 	       (let*-values (
 	          [(e1 tks1) (parExp tks0)]
 		  [(e2 tks2) (parExp tks1)])
 		 (values (MinusExp e1 e2) tks2))]
 	    [(TimesT? tk)
 	       (let*-values (
 	          [(e1 tks1) (parExp tks0)]
 		  [(e2 tks2) (parExp tks1)])
 		 (values (TimesExp e1 e2) tks2))]
             [else (display "not proper start of expression\n")] ; impossible
	   ))
      (display "expression expected\n")
   ))
   
(define (parse tks)
   (let-values ([(main tks1) (parExp tks)])
      (if (null? tks1)
         main
	 (display "program read but more input given\n"))
    ))

(define (run-parser x) (parse (run-lexer x)))

; === evaluating (abstract) syntax

; --- values

(struct NumVal (num))
(struct ClosureVal (formal body env))
(struct TableVal (table))

(define (extend-env id val env)
  (cons (cons id val) env)
 )

(define (lookup-env env id)
  (cond
     [(null? env)
        (display (string-append "undefined identifier " id "\n"))]
     [(equal? id (car (first env)) )
        (cdr (first env))]
     [else (lookup-env (rest env) id)]
  ))

(define (is-prefix? cs1 cs2) 
   (if (null? cs1)
       #t
       (if (null? cs2)
           #f
           (and (equal? (first cs1) (first cs2))
                (is-prefix? (rest cs1) (rest cs2))))))

(define (eval exp env)
   (cond
      [(IdentExp? exp)
           (lookup-env env (IdentExp-string exp))] ;;; CHANGEd? -- Should it really be a numval? -- there are no other valeus other than num, table and function closures #1
      [(NumExp? exp) 
           (NumVal (NumExp-num exp))]
      [(LamExp? exp)
           (ClosureVal (LamExp-formal exp) (LamExp-body exp) env )] ;;; CHANGEddd #2       THIS IS WHAT NEEDS TO BE RETURNED ClosureVal (formal body env) --- 
      [(AppExp? exp)
          (let ([v1 (eval (AppExp-fun exp) env)])
	     (cond
	        [(ClosureVal? v1)
		   (let ([v2 (eval (AppExp-arg exp) env)]) ;;; CHANGEdddd #3
 	              ;(eval exp env))]     ;;; CHANGEd? #4
                 (eval (ClosureVal-body v1) (extend-env (ClosureVal-formal v1)  v2 (ClosureVal-env v1) ) ))  ] 
	       [(TableVal? v1) (display "table applied as a function\n")]
	        [else (display "integer applied as a function\n")]))]
      [(LetExp? exp) ;;; CHANGEddd #5 (the below lines)
     (eval (AppExp (LamExp (LetExp-id exp) (LetExp-exp2 exp))
                    (LetExp-exp1 exp))
               env)]   
      [(IfExp? exp)
          (let ([v (eval (IfExp-test exp) env)])
          ; (display (NumVal-num v)) ;this display is for debugging purposes
             (if (> (NumVal-num v) 0)  ;;; CHANGEddddd #8
	        (eval (IfExp-exp1 exp) env)
                (eval (IfExp-exp2 exp) env)  ) )]
      [(TupleExp? exp)
          (TableVal (list(cons (TupleExp-string1 exp) (TupleExp-string2 exp))))] ;; CHANGEddd #9
      [(Select1Exp? exp)
          (let ([s1 (Select1Exp-string1 exp)]
                [v0 (eval (Select1Exp-exp0 exp) env)])
            (cond
               [(TableVal? v0) 
                   (TableVal
                      (filter
                         (lambda (tup) 
                             (is-prefix? 
                                (string->list s1)
                                (string->list (car tup))))
                         (TableVal-table v0)))]
               [else (display "second operand to 'select1' must be a table\n")]))]
      [(Select2Exp? exp)
          (let ([s2 (Select2Exp-string2 exp)]
                [v0 (eval (Select2Exp-exp0 exp) env)])
            (cond
               [(TableVal? v0) 
                   (TableVal
                      (filter
                         (lambda (tup) 
                             (is-prefix? 
                                (string->list s2)
                                (string->list (cdr tup))))
                         (TableVal-table v0)))]
               [else (display "second operand to 'select2' must be a table\n")]))]
      [(PlusExp? exp)
          (let ([v1 (eval (PlusExp-exp1 exp) env)]
	        [v2 (eval (PlusExp-exp2 exp) env)])
          (cond
            [(and (NumVal? v1) (NumVal? v2))
             (NumVal (+ (NumVal-num v1) (NumVal-num v2)))]
            [(and (TableVal? v1) (TableVal? v2))
             (TableVal (append (TableVal-table v1) (TableVal-table v2)))]
            [else (display "operands to '+' must be either both numbers or both tables\n")]
            
            ) )];;; CHANGE #6 (#6a and #6b finished
      [(MinusExp? exp)
          (let ([v1 (eval (MinusExp-exp1 exp) env)]
	        [v2 (eval (MinusExp-exp2 exp) env)])
	    (cond
	       [(and (NumVal? v1) (NumVal? v2))
		        (NumVal (- (NumVal-num v1) (NumVal-num v2)))]
	       [(and (TableVal? v1) (TableVal? v2))
                    (let ([table1 (TableVal-table v1)]
                          [table2 (TableVal-table v2)])
                       (TableVal
                          (filter (lambda (tup) (not (member tup table2))) 
                                   table1)))]
               [else (display "operands to '-' must be either both numbers or both tables\n")]))]
      [(TimesExp? exp)
          (let ([v1 (eval (TimesExp-exp1 exp) env)]
	        [v2 (eval (TimesExp-exp2 exp) env)])
	    (cond
	       [(and (NumVal? v1) (NumVal? v2))
		        (NumVal (* (NumVal-num v1) (NumVal-num v2)))]
	       [(and (TableVal? v1) (TableVal? v2))
                    (let ([table1 (TableVal-table v1)]
                          [table2 (TableVal-table v2)])
                       (TableVal
                          (map     ;;; CHANGE #10 (the lines below)
                           ; #10: this is about how to implement '*' when applied to two tables;
                           ;         the outline of the code is already there but with several flaws
                             (lambda (tt) (cons (caar tt) (cddr tt)))
                             (filter 
                                (lambda (tt) (equal? (cdar tt) (cadr tt)))
                                (foldr append '()
                                   (map (lambda (tup1)
                                            (map (lambda (tup2)
                                                    (cons tup1 tup2))
                                                  table2))
                                         table1) )             ))))]
               [else (display "operands to '*' must be either both numbers or both tables\n")]))]
  ))     

(define (run x)
   (let ([main (run-parser x)])
     (let ([v (eval main null)])
       (cond
         [(NumVal? v) (NumVal-num v)]
         [(TableVal? v) (TableVal-table v)]
	 [else (display "program cannot return a function\n")])
   )))



