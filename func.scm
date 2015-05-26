;; Has syntax checing functions
(load "syntax.scm") 
;; Has arithmetic op.s & get-value & extend-env functions
(load "calculate.scm") 

;; Functions that are used to interpret expressions

(define evaluate-expr
	(lambda (e env)
		(cond
			((or (number? e) (boolean? e)) e)
			((if-stmt? e) (eval-if e env))
			((let-stmt? e) (eval-let e env '()))
			((lstar-stmt? e) (eval-lstar e env '()))
			((lambda-stmt? e) (eval-lambda e env))
			((app-stmt? e) (eval-app e env))
			((identifier? 'e) (get-value e env))
		)
	)
)


(define eval-if
	;; Evaluates if statements
	(lambda (e env)
		;; If the 1st expr-which is the condition- evaluates to 0,
		;; execute the 3rd expr, else execute the 2nd expr
		(if (= 0 (evaluate-expr (cadr e) env))
			(evaluate-expr (cadddr e) env)
			(evaluate-expr (caddr e) env)
		)
	)
)


(define bound-vars
	;; int-env -> internal env of let
	(lambda (e env int-env)
		(cond
			;; Get value of the symbol from global environment
			((symbol? (cadar e))
				(let*
					(
						(int-env (extend-env (caar e) (get-value (cadar e) env) int-env))
						(int-env (if (not (null? (cdr e)))
									 (bound-vars (cdr e) env int-env)
									 int-env
								 )
						)
					)
					int-env ;; return the internal environment
				)
			)
			;; Value of the variable is primitive, just extend the environment
			((or (number? (cadar e)) (boolean? (cadar e)))
				(let*
					(
						(int-env (extend-env (caar e) (cadar e) int-env))
						(int-env (if (not (null? (cdr e)))
									 (bound-vars (cdr e) env int-env)
									 int-env
								 )
						)
					)
					int-env ;; return the internal environment
				)
			)
		)
	)
)


(define eval-let
	;; Evaluates let statements
	;; int-env -> internal environment of let
	(lambda (e env int-env)
		(let*
			(
				(int-env (bound-vars (cadr e) env int-env))
				(result (evaluate-expr (caddr e) int-env))
			)
			result ; return the result
		)
	)
)


(define bound-vars-star
	;; int-env -> internal env of let
	(lambda (e env int-env)
		(cond
			;; Get value of the symbol from global environment
			((symbol? (cadar e))
				(let*
					(
						(int-env (extend-env (caar e) (get-value (cadar e) int-env) int-env))
						(int-env (if (not (null? (cdr e)))
									 (bound-vars-star (cdr e) env int-env)
									 int-env
								 )
						)
					)
					int-env ;; return the internal environment
				)
			)
			;; Value of the variable is primitive, just extend the environment
			((or (number? (cadar e)) (boolean? (cadar e)))
				(let*
					(
						(int-env (extend-env (caar e) (cadar e) int-env))
						(int-env (if (not (null? (cdr e)))
									 (bound-vars-star (cdr e) env int-env)
									 int-env
								 )
						)
					)
					int-env ;; return the internal environment
				)
			)
		)
	)
)


(define eval-lstar
	;, Evaluates let* statements
	;; int-env -> internal environment of let*
	(lambda (e env int-env)
		(let*
			(
				(int-env (bound-vars-star (cadr e) env int-env))
				(result (evaluate-expr (caddr e) int-env))
			)
			result ; return the result
		)
	)
)


(define eval-lambda
	;; Evaluates lambda statements
	;; env used by this function is not the global env
	;; but the env of the lambda parameter bindings
	;; By design, lambda will go through the eval-app func.
	;; first, thus easing the job of this function.
	(lambda (e env)
		(evaluate-expr e env) ; Just evaluate the expression
	)
)


(define lambda-env
	;; Binds formal parameters to parameters
	(lambda (formals params)
		(if (not (null? formals))
			(cons (cons (car formals) (car params)) (lambda-env (cdr formals) (cdr params)))
			()
		)
	)
)


(define eval-app
	;; Evaluates operations
	(lambda (e env)
		(cond
			;; Evalaute lambda applications
			((lambda-stmt? (car e))
				;; eval-lambda [expr] [lambda-env(formal-list parameters)]
				(eval-lambda (caddr (car e)) (lambda-env (cadr (car e)) (cdr e)))
			)

			;; Evaluate addition
			((equal? (car e) '+) 
				(if (not (null? (cadr e)))
					(add (cdr e) env)
					(0)
				)			
			)

			;; Evaluate subtraction
			((equal? (car e) '-) (subtract (cdr e) env))

			;; Evaluate multiplication
			((equal? (car e) '*) 
				(if (not (null? (cadr e)))
					(multiply (cdr e) env)
					(1)
				)			
			)

			;; Evaluate division
			((equal? (car e) '/) (divide (cdr e) env))
			
			;; If (car e) is a symbol, get its value from the environment
			;, and recursively call this function to evaluate the expression.
			((symbol? (car e)) (eval-app (cons (get-value (car e) env) (cdr e)) env))
		)
	)
)


(define stmt?
	(lambda (e)
		(cond
			((if-stmt? e) #t)
			((let-stmt? e) #t)
			((lstar-stmt? e) #t)
			((lambda-stmt? e) #t)
			((app-stmt? e) #t)
			(else #f)
		)
	)
)


(define evaluate
	(lambda (e env)
		(cond
			((if-stmt? e) (eval-if e env))
			((let-stmt? e) (eval-let e env '()))
			((lstar-stmt? e) (eval-lstar e env '()))
			((lambda-stmt? e) (eval-lambda e env))
			((app-stmt? e) (eval-app e env))
		)
	)
)


