;; Include the functionality provided to
;; interpret statements
(load "func.scm")

(define get-operator (lambda (op-symbol) 
  (cond 
    ((equal? op-symbol '+) +)
    ((equal? op-symbol '-) -)
    ((equal? op-symbol '*) *)
    ((equal? op-symbol '/) /)
    (else (error "damn: operator not implemented -->" op-symbol)))))


(define repl (lambda (env)
  (let* (
         ; first print out some prompt
         (dummy1 (display "cs305> "))

         ; READ an expression
         (expr (read))

         ; Form the new environment
         (new-env (if (define-stmt? expr)
                      (extend-env (cadr expr) (s6-interpret (caddr expr) env) env)
                      env))

         ; EVALuate the expression read
         (val (if (define-stmt? expr)
                  (cadr expr)
                  (s6-interpret expr env)))

         ; PRINT the value evaluated together
         ; with a prompt as MIT interpreter does
         (dummy2 (display "cs305: "))
         (dummy3 (display val))

         ; get ready for the next prompt
         (dummy4 (newline))
         (dummy4 (newline)))
     (repl new-env))))

(define s6-interpret (lambda (e env)
  (cond 
    ;; If the input expression is a number, then
    ;; the value of the expression is that number.
    ((number? e) e)

	;; If the input expression is a symbol, then
    ;; get the current value binding of this variable.
    ((symbol? e) (get-value e env))

	((equal? (car e) 'lambda) e)
		
	;; Added by furkan - bounds my codes to the r-e-p loop
	((stmt? e) (evaluate e env))

    ;; Otherwise, we must see a list.
    ((not (list? e)) 
          (error "damn-> cannot evaluate -->" e))

    ;; First evaluate the value of the operands
    ;; and the procedure of the expression.
    (else 
       (let ((operands (map s6-interpret (cdr e) (make-list (length (cdr e)) env)))
             (operator (get-operator (car e))))

         ;; And finally apply the operator to the 
         ;; values of the operands
         (apply operator operands))))))

(define cs305 (lambda () (repl '())))


