
;; Syntactical check of statements/expressions

(define define-stmt? 
	(lambda (e)
		;; An expression is a define statement
		;; if it is a list, and the first element
		;; is define, the second element is a symbol,
		;; and the third element is an expression.
		(and (list? e) (equal? (car e) 'define) (symbol? (cadr e)) (= (length e) 3))
	)
)


(define expr?
	(lambda (e)
		(cond
			((number? e) #t)
			((boolean? e) #t)
			((identifier? e) #t)
			((if-stmt? e) #t)
			((let-stmt? e) #t)
			((lstar-stmt? e) #t)
			((lambda-stmt? e) #t)
			((app-stmt? e) #t)
			(else #f)
		)
	)
)


(define if-stmt? 
	(lambda (e)
		(and (list? e) (= (length e) 4) (equal? (car e) 'if) (expr? (cadr e)) (expr? (caddr e)) (expr? (cadddr e)))
	)
)


(define var-binding-list?
	;; if the formal list is empty, it should be recognized by the caller
	;; In other words, this function cannot return false for empty lists
	;; <var_binding_list> -> ( IDENT <expr> ) <var_binding_list>
	(lambda (e)
		(if (not (null? e))
			(and (identifier? (caar e)) (expr? (cadar e)) (var-binding-list? (cdr e)))
			#t
		)
	)
)
		

(define let-stmt? 
	(lambda (e)
		(and (list? e) (= (length e) 3) (equal? (car e) 'let) (expr? (caddr e)) (not (null? (cadr e))) (var-binding-list? (cadr e)))
	)
)


(define lstar-stmt? 
	(lambda (e)
		(and (list? e) (= (length e) 3) (equal? (car e) 'let*) (expr? (caddr e)) (not (null? (cadr e))) (var-binding-list? (cadr e)))
	)
)


(define formal-list?
	;; if the formal list is empty, it should be recognized by the caller
	;; In other words, this function cannot return false for empty lists
	(lambda (e)
		(if (not (null? e))
			(and (identifier? (car e)) (formal-list? (cdr e)))
			#t
		)
	)
)


(define lambda-stmt?
	(lambda (e)
		;; <lambda> -> ( lambda ( <formal_list> ) <expr> )
		(and (list? e) (= (length e) 3) (equal? (car e) 'lambda) (expr? (caddr e)) (not (null? (cadr e))) (formal-list? (cadr e)))
	)
)


(define operand-list?
	(lambda (e)
		;; <operand_list> -> <expr> <operand_list>
		(if (not (null? e))
			(and (expr? (car e)) (operand-list? (cdr e)))
			#t
		)
	)
)


(define operator?
	(lambda (e)
		;; <operator> -> <built_in_operator> | <lambda> | IDENT
		(cond
			((lambda-stmt? e) #t)
			((identifier? e) #t)
			((equal? '+ e) #t)
			((equal? '- e) #t)
			((equal? '* e) #t)
			((equal? '/ e) #t)
			(else #f)
		)
	)
)


(define app-stmt?
	(lambda (e)
		;; <application> -> ( <operator> <operand_list> )
		(if (and (list? e) (>= (length e) 2) (operator? (car e)) (operand-list? (cdr e)))
			(cond
				((lambda-stmt? (car e)) (= (length (cadar e)) (length (cdr e))))
				((equal? (car e) '+) #t)
    			((equal? (car e) '-) (if (>= (length (cdr e)) 2) #t (error "damn-> Needs more than 2 params -->" e)))
    			((equal? (car e) '*) #t)
    			((equal? (car e) '/) (if (>= (length (cdr e)) 2) #t (error "damn-> Needs more than 2 params -->" e)))
				((identifier? (car e)) #t)
				(else #f)
			)
			#f
		)
	)
)


