#| ---- EVAL ---- |#
(define (k/eval exp env)
  (cond ((self-evaluating? exp) exp) ; variables
		((quoted? exp) (quoted-value exp))
		((variable? exp) (lookup-variable-value exp env))

		((definition? exp) (eval-definition exp env)) ; env alteration
		((assignment? exp) (eval-assignment exp env))

		((lambda? exp) (make-procedure exp env)) ; procedure
		
		((conditional? exp) (eval-cond (clauses exp) env)) ; special forms

		((fun-application? exp) (k/apply
								   (k/eval (operator exp) env)
								   (list-of-values (operands exp) env)))))
#|
l'env non viene passato ad k/apply in quanto
è compito della funzione tenere traccia dell'ambiente di definizione
data la natura lexically scoped del linguaggio
|#

#| ---- APPLY ---- |#
(define (k/apply procedure args)
  (cond ((primitive-procedure? procedure)
		 (apply-primitive-prodecure procedure args))
		((compound-procedure? procedure)
		 (eval-sequence (procedure-body procedure)
					   (extend-env
						(parameters procedure)
						args
						(procedure-environment procedure))))))

(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
		(else (cons (k/eval (first-operand exps) env)
					(list-of-values (rest-operands exps) env)))))

(define (eval-cond cond-list env)
  (cond ((no-clauses? cond-list) #nil)
		((or (else-clause? (first-clause cond-list))
			 (true? (k/eval (clause-predicate (first-clause cond-list)) env)))
		 (eval-sequence (clause-body (first-clause cond-list)) env))
		(else (eval-cond (rest-clauses cond-list) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (k/eval (first-exp exps) env))
		(else (k-eval (first-exp exps) env)
			  (eval-sequence (rest-exps exps) env))))

#| ---- ENVIRONMENT et  al ---- |#
(define (eval-assignment exp env)
  (let ((assigned-value (k/eval (assignment-body exp) env)))
	(set-variable-value! (assigment-variable exp)
						 assigned-value
						 env)
	assigned-value))

;; k/... perchè =define-variable!= mi sfotte l'indentazione
(define (eval-definition exp env)
  (k/define-variable! (definition-variable exp)
					  (eval (definition-body exp) env)
					  env)
  (definition-variable exp))

#| ---- QUI IL CROLLO DI ASTRAZIONE CHE DETERMINA COME FUNZIONA TUTTO ---- |#
(define (self-evaluating? exp) (number? exp))

(define (car-check sym) (lambda (exp) (and (pair? exp) (eq? (car exp) sym))))

;; una form quote ha forma (quote <val>)
(define (quoted? exp) ((car-check 'quote) exp))
(define (quoted-value exp) (cadr exp))

;; un assegnamento fa (set! <sym> <val>)
(define (assignment? exp) ((car-check 'set!) exp))
(define (assigment-variable exp) (cadr exp))
(define (assigment-body exp) (caddr exp))

;; una definizione fa (define <sym> <val>)
(define (definition? exp) ((car-check 'define) exp))
(define (definition-variable exp) (cadr exp))
(define (definition-body exp) (caddr exp))

;; (lambda <args> <corpus>)
(define (lambda? exp) ((car-check 'lambda) exp))
(define (lambda-args exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

;; (cond <clausole>*)
(define (conditional? exp) ((car-check 'cond) exp))

;; la lista <clausole>* è, per l'appunto una lista
(define (clauses exp) (cadr exp))
(define (no-clauses? clauses) (null? clauses))
(define (first-clause clauses) (car clauses))
(define (rest-clauses clauses) (cdr clauses))

;; una clausola fa (<predicato> <corpus>)
(define (clause-predicate clause) (car clause))
(define (clause-body clause) (cdr clause))
(define (else-clause? clause) (eq? (predicate clause) 'else))
(define (true? x) (not (null? x)))
		 
;; una sequenza è una lista di espressioni
(define (last-exp? seq) (and (not (null? (car seq))) (null? (cdr seq))))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))

(define (application? exp) (pari? exp))
(define (operator app) (car app))
(define (operands app) (cdr app))
(define (no-operands? args) (null? args))
;; per evlist
(define (first-operand args) (car args))
(define (rest-operand args) (cdr args))

;; ora la definizione delle procedure
(define (make-procedure? lambda-exp env)
  (list 'procedure lambda-exp env))
(define (compound-procedure? proc) ((car-check 'procedure) proc))
;; ricordando che una <lambda-exp> fa (lambda <args> <corpus>)
;; questa fa (procedure <lambda-exp> <env>)
(define (procedure-lambda proc) (cadr proc))
(define (procedure-environment proc) (caddr proc))
(define (parameters proc) (lambda-args (procedure-lambda proc)))
(define (procedure-body proc) (lambda-body (procedure-lambda proc)))
