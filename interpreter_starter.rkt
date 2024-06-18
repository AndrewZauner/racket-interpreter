#lang racket
(provide (all-defined-out))

; Chalondra Jewell, Andrew Zauner

;; Variable bindings  

(define (make-binding var val)
 (list var val))

(define (binding-variable binding)
  (first binding))

(define (binding-value binding)
  (first (rest binding)))


;; Frames  

(define (make-frame variables values)
  (map list variables values))

(define (first-binding frame)
  (car frame))

(define (rest-of-bindings frame)
  (cdr frame))

(define (empty-frame? frame)
  (null? frame))

(define (adjoin-binding binding frame)
  (cons binding frame))

(define (binding-in-frame var frame)
  (let ((binding (assoc var frame equal?)))
    (if (and binding (pair? binding))
        (let ((value (cdr binding)))
          (if (list? value)
            (car value)value))
        #f)))


;; Environments  

(define (empty-env)
  (list))

(define (empty-env? env)
  (empty? env))

(define (first-frame env)
  (if (empty-env? env)
      '()
      (mcar env)))

(define (rest-of-frames env)
  (if (empty-env? (mcdr env))
      '()
      (mcdr env))) 

(define (set-first-frame! env new-frame)
  (if (empty-env? env)
      (mcons new-frame env)
  (set-mcar! env new-frame)))

(define (adjoin-frame frame env)
  (if (empty-env? env)
      (mcons frame '())
      (mcons (mcar env) (adjoin-frame frame (mcdr env)))))

(define (extend-env vars vals env)
  (adjoin-frame (make-frame vars vals) env))


(define (binding-in-env var env)
  (let loop ((frames env))
    (cond ((empty-env? frames) #f)
          ((let ((current-frame (first-frame frames))
                 (binding (binding-in-frame var (first-frame frames))))
             (if binding
                 binding
                 (loop (rest-of-frames frames)))))
          (else #f))))


(define (lookup-variable var env)
  (let ((binding (binding-in-env var env)))
    (if (void? binding)
        (error "unbound in environment!")
        binding)))


(define make-primitive
        (lambda (name proc)
                (list 'primitive name proc)))

;; Global environment definition 
(define setup-env
  (lambda ()
    (let ((env (extend-env '(null + - * / = cons first rest null? > < >= <= display newline) (list '()
                                                  (make-primitive '+ +)
                                                  (make-primitive '- -)
                                                  (make-primitive '* *)
                                                  (make-primitive '/ /)
                                                  (make-primitive '= =)
                                                  (make-primitive 'cons cons)
                                                  (make-primitive 'first first)
                                                  (make-primitive 'rest rest)
                                                  (make-primitive 'null? null?)
                                                  (make-primitive '> >)
                                                  (make-primitive '< <)
                                                  (make-primitive '>= >=)
                                                  (make-primitive '<= <=)
                                                  (make-primitive 'display display)
                                                  (make-primitive 'newline newline)
                                                  ) (empty-env))))
      env)))

(define global-env (setup-env))

;; i-eval

(define i-eval
  (lambda (exp env)
    (cond
      ((definition? exp) (eval-definition exp env))
      ((begin? exp) (eval-begin exp env))
      ((if? exp) (eval-if exp env))
      ((cond? exp) (eval-cond exp env))
      ((variable? exp) (lookup-variable exp env))
      ((quoted? exp) (text-of-quotation exp))
      ((boolean? exp) exp)
      ((number? exp) exp)
      ((string? exp) exp)    
      ((lambda? exp) (make-closure exp env))
      ((let? exp) (let->lambda exp) env)
      ((application? exp) (eval-application exp env))
      ((closure? exp) (apply-closure exp env))
      ((closure? (lookup-variable (first exp) env) (apply-closure exp (rest exp))))      
      (else (error "i-eval::unknown expression type" exp)))))




;; REPL  

(define read-eval-print-loop
  (lambda ()
    (let loop ()
      (display "INTERPRETER> ")
      (let ((user-input (read)))
        (if (eq? user-input 'exit)
            (display "INTERPRETER done")
            (begin
              (let ((result (i-eval user-input global-env)))
                (if (definition? user-input)
                    (begin
                      (display (definition-variable user-input))
                      (newline))
                    (begin
                      (display result)
                      (newline))))
              (loop)))))))



; Shortcut method
(define repl read-eval-print-loop)




;; Quote and quoted expressions  

(define (quoted? exp)
  (and (pair? exp) (eq? (car exp) 'quote)))

(define (text-of-quotation exp)
  (if (quoted? exp)
      (if (pair? (cadr exp))
          (cadr exp)
          (cadr exp)) 
      (error "Not a quoted expression")))




;; define and variables  

(define (variable? exp)
    (symbol? exp))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (first exp) tag)))

(define (definition? exp)
  (if (tagged-list? exp 'define)
  (tagged-list-length-n? exp 'define 2)
  #f))

(define (definition-variable exp)
  (first (rest exp)))

(define (definition-value exp)
  (first (rest (rest exp))))

(define (eval-definition exp env)
  (let ((var (definition-variable exp))
        (val (i-eval (definition-value exp) env)))
    (define-variable! var val env)
    val))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (if (assoc var frame)
        (error "duplicate definition for identifier")
        (set-first-frame! env (cons (cons var (list val)) frame)))))


; Syntax Checker  
(define (tagged-list-length-n? exp tag n)
  (if (and (pair? exp) (eq? (first exp) tag) (= (length exp) (+ 1 n)))
      #t
      (begin
        (display "Bad syntax in ") (display exp)
        (display ". Expected ") (display n) (display " elements.")
        (newline) 
        #f)))


(define (tagged-list-minlength-n? exp tag n)
  (if (and (pair? exp) (eq? (first exp) tag) (>= (length exp) (+ 1 n)))
      #t
      (begin
        (display "Bad syntax in ") (display exp)
        (display ". Expected at least ") (display n) (display " elements.")
        (newline) 
        #f)))




; Implementing i-apply  

(define (application? exp)
  (pair? exp))

(define (operator exp)
  (first exp))

(define (operands exp)
  (rest exp))

(define eval-operands
  (lambda (operands env)
    (map (lambda (operand) (i-eval operand env)) operands)))



; Primitive  

(define (primitive-procedure? proc)
  (let ((proc (lookup-variable proc global-env)))
  (and (list? proc)
       (eq? (first proc) 'primitive))))

(define (primitive-name proc)
  (if (primitive-procedure? proc)
      (cadr (lookup-variable proc global-env))
      (error "Not a primitive procedure")))

(define (primitive-implementation proc)
  (if (primitive-procedure? proc)
      (last (lookup-variable proc global-env))
      (error "Not a primitive procedure")))

(define eval-application
  (lambda (exp env)
    (let ((operator-exp (operator exp))
          (operands-exp (operands exp)))
      (if (pair? exp)
          (cond ((primitive-procedure? operator-exp) (apply-primitive-procedure operator-exp (eval-operands operands-exp env)))
                ((lambda? operator-exp) (apply-closure (i-eval operator-exp env) (eval-operands operands-exp env)))
                ((closure? (lookup-variable operator-exp env)) (apply-closure (lookup-variable operator-exp env) (eval-operands operands-exp env)))
                (else exp))
          (error "Operator expression is not a pair")))))



(define i-apply
  (lambda (proc vals)
    (cond ((primitive-procedure? proc)
           (apply-primitive-procedure proc vals))
          (else (error "Error: unknown procedure type")))))

(define apply-primitive-procedure
  (lambda (proc vals)
    (apply (primitive-implementation proc) vals)))

; Begin  
(define (begin? exp)
  (and (pair? exp)            
       (eq? (car exp) 'begin)))

(define (begin-expressions exp)
  (cdr exp))

(define (eval-begin exp env)
  (let ((expressions (begin-expressions exp)))
    (if (null? expressions)            
        (void)                        
        (let loop ((exps expressions)) 
          (if (null? (cdr exps))      
              (i-eval (car exps) env)  
              (begin                    
                (i-eval (car exps) env)
                (loop (cdr exps))))))))

; If  
(define (if? exp)
  (and (pair? exp)            
       (eq? (car exp) 'if)))
(define (if-test exp) (cadr exp))
(define (if-then exp) (caddr exp))
(define (if-else exp)
  (if (null? (cdddr exp))
      'void 
      (cadddr exp)))
(define (eval-if exp env)
  (if (i-eval (if-test exp) env)          
      (i-eval (if-then exp) env)          
      (if (eq? (if-else exp) 'void)      
          'void                          
          (i-eval (if-else exp) env)))) 


; Cond  
(define (cond? exp)
  (and (pair? exp)
       (eq? (car exp) 'cond)))

(define (cond-clauses exp)
  (if (empty? (first exp))
      (cond-clauses (rest exp))
      (cdr exp)))

(define (first-cond-exp exp)
  (if (null? exp)
      (error "No clauses found in cond")
      (let ((clause (car exp)))
        (if (null? (cdr clause))
            (error "No expressions found after test in cond clause")
            (car (cdr clause))))))


(define (rest-of-cond-exps exp)
  (if (null? exp)
      (error "No clauses found in cond")
      (let ((clause (car exp)))
        (if (null? (cdr clause))
            (error "No expressions found after test in cond clause")
            (cdr (cdr clause))))))

(define (eval-cond exp env)
  (if (null? (cond-clauses exp))
      (error "No clauses found in cond")
      (cond-eval (cond-clauses exp) env)))

(define (i-eval-sequence exps env)
  (if (null? (cdr exps))
      (i-eval (car exps) env)
      (begin (i-eval (car exps) env)
             (i-eval-sequence (cdr exps) env))))


(define (cond-eval clauses env)
  (cond ((null? clauses) (error "No matching clauses in cond"))
        ((null? (car clauses)) (error "Empty clause in cond"))
        ((null? (caar clauses))
         (if (null? (cdar clauses))
             (error "No resulting expression in cond clause")
             (i-eval (cadar clauses) env)))
        ((eq? (caar clauses) 'else) 
         (if (null? (cdar clauses))
             (error "No resulting expression in cond clause")
             (i-eval (cadar clauses) env)))
        ((i-eval (caar clauses) env)
         (if (null? (cdar clauses))
             (i-eval (caar clauses) env) 
             (eval-next (cdar clauses) env)))
        (else (cond-eval (cdr clauses) env))))

(define (eval-next exps env)
  (if (null? exps)
      (void)
      (if (null? (cdr exps))
          (i-eval (car exps) env)
          (begin (i-eval (car exps) env)
                 (eval-next (cdr exps) env)))))


(define (check-one-exp exp)
  (if (null? exp)
      (void)
      (if (pair? exp)
          (if (eq? (car exp) 'begin)
              (check-all-exps (cdr exp))
              (void))
          (void))))

(define (check-all-exps exps)
  (if (null? exps)
      (void)
      (begin
        (check-one-exp (car exps))
        (check-all-exps (cdr exps)))))


; lambda  
(define lambda?
  (lambda (exp)
    (and (pair? exp)
         (eq? (car exp) 'lambda))))


; closure  
 (define make-closure
        (lambda (lambda-exp env)
          (list 'closure lambda-exp env)))

(define (closure? exp)
  (if (tagged-list? exp 'closure)
      #t 
      #f))

(define procedure-parameters
  (lambda (closure)
    (second (second closure))))

(define procedure-body
  (lambda (closure)
    (rest (rest (first (rest closure))))))

(define procedure-env
  (lambda (closure)
    (last closure)))

(define apply-closure
  (lambda (closure args)
    (let ((param (procedure-parameters closure))
          (body (procedure-body closure))
          (env (procedure-env closure)))
      (let ((new-env (mcons (make-frame param args) global-env)))
        (i-eval (first body) new-env)))))

           


; let
(define (let? exp)
  (and (pair? exp)
       (eq? (car exp) 'let)
       (list? (cadr exp))))

(define (let->lambda exp)
  (if (let? exp)
      (let* ((bindings (cadr exp))
            (body (cddr exp)))
        (let ((vars (map car bindings))
              (lambda-exp (cons 'lambda (cons (map car bindings) body))))
          lambda-exp))
      (error "Not a valid let expression")))




; higher-order functions

; map
(define (map? exp)
  (and (list? exp)
       (eq? (car exp) 'map)))

(define (eval-map exp env)
  (if (map? exp)
      (let ((proc (eval (cadr exp) env)) 
            (lst (eval (caddr exp) env))) 
        (map proc lst))
      (error "Invalid map expression" exp)))


; filter
(define (filter? exp)
  (and (list? exp)
       (eq? (car exp) 'filter)))

(define (eval-filter exp env)
  (if (filter? exp)
      (let ((pred (eval (cadr exp) env)) 
            (lst (eval (caddr exp) env))) 
        (filter pred lst))
      (error "Invalid filter expression" exp)))

; fold
  (define (fold? exp)
  (and (list? exp)
       (eq? (car exp) 'fold)))

(define (foldl proc initial lst)
  (if (null? lst)
      initial
      (foldl proc (proc initial (car lst)) (cdr lst))))


(define (eval-fold exp env)
  (if (fold? exp)
      (let ((proc (eval (cadr exp) env))
            (init (eval (caddr exp) env))
            (lst (eval (cadddr exp) env)))
        (foldl proc init lst))
      (error "Invalid fold expression" exp)))

