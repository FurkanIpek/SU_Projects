 
;; Calculate four basic operations, + - * /

;; get-value & extend-env functions are also here!!!

(define add
	(lambda (e env)
		(cond
			((null? e) (0))
			((= 1 (length e)) 
				(if (symbol? (car e))
					(get-value (car e) env)
					(car e)
				)
			)
			((>= (length e) 2)
				(add
					(cons 
						(+ (if (symbol? (car e)) (get-value (car e) env) (car e)) (if (symbol? (cadr e)) (get-value (cadr e) env) (cadr e))) (cddr e)	
					)
					env
				)
			)
		)
	)
)



(define subtract
	(lambda (e env)
		(cond
			((null? e) (0))
			((= 1 (length e)) 
				(if (symbol? (car e))
					(get-value (car e) env)
					(car e)
				)
			)
			((>= (length e) 2)
				(subtract
					(cons 
						(- (if (symbol? (car e)) (get-value (car e) env) (car e)) (if (symbol? (cadr e)) (get-value (cadr e) env) (cadr e))) (cddr e)	
					)
					env
				)
			)
		)
	)
)


(define multiply
	(lambda (e env)
		(cond
			((null? e) (0))
			((= 1 (length e)) 
				(if (symbol? (car e))
					(get-value (car e) env)
					(car e)
				)
			)
			((>= (length e) 2)
				(multiply
					(cons 
						(* (if (symbol? (car e)) (get-value (car e) env) (car e)) (if (symbol? (cadr e)) (get-value (cadr e) env) (cadr e))) (cddr e)	
					)
					env
				)
			)
		)
	)
)


(define divide
	(lambda (e env)
		(cond
			((null? e) (0))
			((= 1 (length e)) 
				(if (symbol? (car e))
					(get-value (car e) env)
					(car e)
				)
			)
			((>= (length e) 2)
				(divide
					(cons 
						(/ (if (symbol? (car e)) (get-value (car e) env) (car e)) (if (symbol? (cadr e)) (get-value (cadr e) env) (cadr e))) (cddr e)	
					)
					env
				)
			)
		)
	)
)


(define get-value (lambda (var env)
    (cond
      ;; If the environment is empty, then we could not find 
      ;; a binding in this environment.
      ((null? env) (error "damn: unbound variable -->" var))

      ;; Check if the first pair is a binding for the
      ;; variable that we are looking for, and if it is
      ;; return the current value of the variable.
      ((equal? (caar env) var) (cdar env))

      ;; Otherwise, search in the remaning of the environment.
      (else (get-value var (cdr env))))))


(define extend-env (lambda (var val old-env)
      ;; Add the new variable binding to the 
      ;; beginning of the old environment.
	(let* ((display val))
      (cons (cons var val) old-env))))


