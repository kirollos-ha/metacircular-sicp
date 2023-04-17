#lang sicp

(define (k/eval expr env)
  (cond ((self-evaluating? expr) expr)
		((variable? expr) (lookup-value expr env))
		((eq? (car expr) 'print) (display (eval-list (cdr expr) env)) (newline))
		((eq? (car expr) '+) (apply + (eval-list (cdr expr) env)))
		((lambda-expr? expr) (make-procedure expr env))
		((definition? expr) (eval-definition expr env))
		((application? expr) (k/apply (k/eval (procedure-operator expr) env)
									  (eval-list (procedure-operands expr) env)))))

(define (k/apply proc args)
  (eval-sequence (procedure-body proc)
				 (env-add-frame (procedure-env proc) 
								(frame-make (procedure-args proc)
											args))))

(define (eval-sequence seq env)
  (define (single? x) (and (pair? x) (null? (cdr x))))
  (cond ((single? seq) (k/eval (car seq) env))
		(else (k/eval (car seq) env)
			  (eval-sequence (cdr seq) env))))

(define (eval-list lst env)
  (cond ((null? lst) '())
		(else (cons (k/eval (car lst) env)
					(eval-list (cdr lst) env)))))

#| --- SELF EVALS --- |#
(define (self-evaluating? expr) (number? expr))
(define (variable? expr) (symbol? expr))

#| --- ENVIRONMENT --- |#
#|
un ambiente è una lista/sequenza/stack/boh di frame
un frame è una qualsiasi mappa nome -> valore
qui famo alists
|#

;; per la alist
(define (pair-list new-binds new-vals)
  "input (a b c d) (1 2 3 4), output ((a . 1) (b . 2) (c . 3) (d . 4))"
  (cond ((and (null? new-binds) (null? new-vals)) '())
		((null? new-binds) (error "too many values given to procedure"))
		((null? new-vals) (error "too many bindings given to procedure"))
		(else (cons (cons (car new-binds) (car new-vals))
					(pair-list (cdr new-binds) (cdr new-vals))))))

;;; binding
(define (binding-make var val) (cons var val))
(define (binding-var bind) (car bind))
(define (binding-value bind) (cdr bind))
(define no-binding #f)
(define (no-binding? b) (not b))

(define (binding-set-value! bind new-val)
  (set-cdr! bind new-val))

;;; frame
(define (frame-make binds vals)
  (pair-list binds vals))

(define (frame-binding-for-var var frame)
  "(var . value) se la variabile ha un valore, else #f"
  (assoc var frame))

(define (frame-add-binding frame new-var new-val)
  (cons (cons new-var new-val)
		frame))

;;; ambiente
(define (env-make-empty) '(()))
(define (env-add-frame orig new-frame)
  (cons new-frame orig))
(define (env-first-frame env) (car env))
(define (env-rest-frames env) (cdr env))
(define (env-no-frames? env) (null? env))

(define (set-first-frame! env new-frame)
  (set-car! env new-frame))

(define (env-binding-for-var var env)
  (if (env-no-frames? env)
	  no-binding
	  (let ((bind (frame-binding-for-var var (env-first-frame env))))
		(if (no-binding? bind)
			(env-binding-for-var var (env-rest-frames env))
			bind))))

(define (lookup-value var env)
  (let ((bind (env-binding-for-var var env)))
	(if (no-binding? bind)
		(error "unbound variable" var)
		(binding-value bind))))

(define (set-variable-value! var new-val env)
  (let ((bind (env-binding-for-var var env)))
	(if (no-binding? bind)
		(error "can't set unbound variable" var)
		(binding-set-value! bind new-val))))

(define (define-variable-value! var val env)
  (define (first-frame-add-bind! env var val)
	(set-first-frame! env (frame-add-binding (env-first-frame env)
											 var
											 val)))
  (let ((bind (env-binding-for-var var env)))
	(if (no-binding? bind)
		(first-frame-add-bind! env var var)
		(binding-set-value! bind val))))

;;; definizioni e assegnamenti
;;; una definizione fa (define <var> <val>)
(define (eval-definition expr env)
  (let ((definition-var (cadr expr))
		(definition-val (k/eval (caddr expr) env)))
	(define-variable-value! definition-var
	  definition-val
	  env)))
	  

#| --- CHECKS FOR THE DISPATCH --- |#
;; I don't know why this keeps not working, but it keeps not working
(define (car-check? sym) (lambda (x) (and (pair? x) (eq? (car x) sym))))
(define (lambda-expr? expr) ((car-check? 'lambda) expr))
(define (definition? expr) ((car-check? 'define) expr))
(define (application? expr) (pair? expr))

#| --- LAMBDAS --- |#
;; lambda -> (lambda <args> <body>)
(define (lambda-args lambda-expr) (cadr lambda-expr))
(define (lambda-body lambda-expr) (cddr lambda-expr))

#| --- PROCEDURES, GETTERS ET AL --- |#
(define (make-procedure lambda-expr env) (cons lambda-expr env))
(define (procedure-lambda proc) (car proc))
(define (procedure-env proc) (cdr proc))
(define (procedure-body proc) (lambda-body (procedure-lambda proc)))
(define (procedure-args proc) (lambda-args (procedure-lambda proc)))

(define (procedure-operator proc) (car proc))
(define (procedure-operands proc) (cdr proc))

  
(k/eval '(+ x x) '(((x . 5))))
(k/eval '((lambda (x) (+ x x)) 5) '())
(k/eval '((lambda (x) (+ x x)) 5) '(env-make-empty))
(k/eval '(define x 10) (env-make-empty))

(eval-sequence '(
                 ((lambda (x) (print (+ x x))) 5)
                 ((lambda (x) (print x) (print x)) 10)
				 )
			   (env-make-empty))

(eval-sequence '(
                 (define x 40)
                 (print x)
                 )
               (env-make-empty))

(eval-sequence '(
                 (define x (lambda (x) (print (+ x x))))
                 (x 10)
                 )
               (env-make-empty))

(eval-sequence '(
                 (define print-double (lambda (x) (print (+ x x))))
                 (define print-twice (lambda (x) (print x) (print x)))
                 (define x 40)
                 (print-double x)
                 (print-twice (+ x x x x))
				 )
			   (env-make-empty))

(eval-sequence '(
                 )
               (env-make-empty))
#|
(k/eval-sequence '((define print-double (lambda (x) (print (+ x x))))
				   (print-double 10)))
|#
