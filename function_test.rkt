#lang racket
(require rackunit
         "interpreter_starter.rkt")

; Andrew Zauner

; Test cases for Variable Bindings  
(define sample (make-binding 'today 'monday))
(check-equal? sample (list 'today 'monday) "Make binding - symbols case")
(check-equal? (binding-variable sample) 'today "Binding variable - symbols case")
(check-equal? (binding-value sample) 'monday "Binding value - symbols case")

(define sample2 (make-binding 'a-pair (list 1 2)))
(check-equal? sample2 (list 'a-pair (list 1 2)) "Make binding - pair case")
(check-equal? (binding-variable sample2) 'a-pair "Binding variable - pair case")
(check-equal? (binding-value sample2) (list 1 2) "Binding value - pair case")

(define sample3 (make-binding 'a-list (cons 1 (cons 2 null))))
(check-equal? sample3 (list 'a-list (list 1 2)) "Make binding - list case")
(check-equal? (binding-variable sample3) 'a-list "Binding variable - list case")
(check-equal? (binding-value sample3) (list 1 2) "Binding value - list case")


; Test cases for the frame abstraction  

(define frame (make-frame '(a b c) '(1 2 3)))
(check-equal? (empty-frame? frame) #f "empty frame check")
(check-equal? (first-binding frame) '(a  1) "first binding check")
(check-equal? (rest-of-bindings frame) '((b 2) (c 3)) "rest of bindings check")
(define new-frame (adjoin-binding '(d . 4) frame))
(check-equal? (binding-in-frame 'a frame) 1 "binding for 'a'")
(check-equal? (binding-in-frame 'z frame) #f "binding for 'z'")
(check-equal? (binding-in-frame 'b frame) 2 "binding for 'b'")
(check-equal? new-frame '((d . 4) (a  1) (b  2) (c  3)) "adjoin binding check")


; Test cases for the environment abstraction  

; Empty environment initialization
(define env0 (empty-env))
(check-equal? env0 '() "Empty environment initialization")
(check-equal? (empty-env? env0) #t "Empty environment test")

; Test environments initialization
(define env1 (extend-env '(a b c) '(1 2 3) env0))
(define env2 (extend-env '(a c d e) '(red blue green yellow) env1))
(define env3 (extend-env '(a f) '(#t #f) env2))
(define env4 env3)

; first-frame test
(check-equal? (first-frame env1) '((a 1) (b 2) (c 3)) "First frame in env1")
(check-equal? (first-frame env2) '((a 1) (b 2) (c 3)) "First frame in env2")
(check-equal? (first-frame env3) '((a 1) (b 2) (c 3)) "First frame in env3")

; rest-of-frames test
(check-equal? (rest-of-frames env1) '() "Rest of frames in env1")
(check-equal? (rest-of-frames env2) (mcons '((a red) (c blue) (d green) (e yellow)) '()) "Rest of frames in env2")
(check-equal? (rest-of-frames env3) (mcons '((a red) (c blue) (d green) (e yellow)) (mcons '((a #t) (f #f)) '())) "Rest of frames in env3")

; set-first-frame! test
(set-first-frame! env4 '((this 7) (that 8) (other 9)))
(check-equal? (first-frame env4) '((this 7) (that 8) (other 9)) "Set first frame in env4")
(check-equal? (rest-of-frames env4) (mcons '((a red) (c blue) (d green) (e yellow)) (mcons '((a #t) (f #f)) '())) "Rest of frames in env4 with set-first-frame method")

; binding-in-env test
(check-equal? (binding-in-env 'a env1) 1 "a in env1")
(check-equal? (binding-in-env 'e env2) 'yellow "e in env2")
(check-equal? (binding-in-env 'c env3) 'blue "c in env3")
(check-equal? (binding-in-env 'z env3) #f "z in env3")
(check-equal? (binding-in-env 'that env4) 8 "that in env4")

; lookup-variable test
(check-equal? (lookup-variable 'c env3) 'blue "c in env3")
(check-equal? (lookup-variable 'other env4) 9 "other in env4")
; (lookup-variable 'g env3) ; will throw an error which is expected



; Global environment test cases

(define (setup-env)
  (list (null? null)
        (first '(1 2 3))  
        (rest '(1 2 3))   
        (list 1 2 3)       
        (cons 1 '(2 3))      
        ))
(check-equal? (setup-env)
              '(#t 1 (2 3) (1 2 3) (1 . (2 3))) "Interpreter test with check-equal?")



; i-eval test  
(check-equal? (i-eval #t'())#t "Evaluate boolean true")
(check-equal? (i-eval #f'())#f "Evaluate boolean false")
(check-equal? (i-eval 13 '())13 "Evaluate 13")
(check-equal? (i-eval 7 '())7 "Evaluate 7")
(check-equal? (i-eval "read" '())"read" "Evaluates a string 'read'")
(check-equal? (i-eval "i am cool" '())"i am cool" "Evaluates a string 'i am cool'")

;; Quote and quoted expressions  
(check-equal? (quoted? '(quote x)) #t "Test quoted? with (quote x)")
(check-equal? (quoted? '(quote y)) #t "Test quoted? with (quote y)")
(check-equal? (quoted? ''x) #t "Test quoted? with ''x")
(check-equal? (quoted? ''y) #t "Test quoted? with ''y")
(check-equal? (quoted? '(1 2 3)) #f "Test quoted? with (1 2 3)")
(check-equal? (quoted? '(4 5 6)) #f "Test quoted? with (4 5 6)")

(check-equal? (text-of-quotation '(quote x)) 'x "Test text-of-quotation with (quote x)")
(check-equal? (text-of-quotation '(quote y)) 'y "Test text-of-quotation with (quote y)")
(check-equal? (text-of-quotation ''x) 'x "Test text-of-quotation with ''x")
(check-equal? (text-of-quotation ''y) 'y "Test text-of-quotation with ''y")



; Define and varibles tests  

; tagged list test
(check-equal? (tagged-list? '(define x 8) 'define) #t "tagged-list #t")
(check-equal? (tagged-list? "some string" 'define) #f "tagged-list #f")
(check-equal? (tagged-list? '(quote x 6) 'define) #f "tagged-list #f")
(check-equal? (tagged-list? '(quote x 6) 'quote) #t "tagged-list #t")

; definition? test
(check-equal? (definition? '(define y 3)) #t "definition? #t")
(check-equal? (definition? 5) #f "definition #f")
(check-equal? (definition? '(quote x 6)) #f "definition? #f")


; definition-variable test
(check-equal? (definition-variable '(define x 6)) 'x "definition variable")
(check-equal? (definition-variable '(define water "splash")) 'water "definition variable")


; definition-value test
(check-equal? (definition-value '(define x 6)) 6 "definition value - number")
(check-equal? (definition-value '(define water "splash")) "splash" "definition value - string")
(check-equal? (definition-value '(define lst (list 1 2))) '(list 1 2) "definition value - list")


; REPL tests for rest of methods:

#|
> (repl)
INTERPRETER> (define pi 3.1415)
pi
INTERPRETER> pi
3.1415
INTERPRETER> (define a (quote apple))
a
INTERPRETER> a
apple
INTERPRETER> (define j (quote jacks))
j
INTERPRETER> j
jacks
INTERPRETER> (define x 3)
x
INTERPRETER> x
3
INTERPRETER> (define q x)
q
INTERPRETER> q
3
INTERPRETER> (define m 'q)
m
INTERPRETER> m
q
|#



;; i-apply tests  

; application
(check-equal? (application? '(+ x 6)) #t "application + with variable and number")
(check-equal? (application? '(+ 2 6)) #t "application + with two numbers")
(check-equal? (application? '+) #f "application false case with symbol")
(check-equal? (application? '(= 1 1)) #t "application with =")

; operator
(check-equal? (operator '(+ x 6)) '+ "operator +")
(check-equal? (operator '(* x 6)) '* "operator *")
(check-equal? (operator '(- x 6)) '- "operator -")
(check-equal? (operator '(/ x 6)) '/ "operator /")
(check-equal? (operator '(cons x 6)) 'cons "operator cons")

; operands
(check-equal? (operands '(+ 1 6)) '(1 6) "operands + numbers")
(check-equal? (operands '(* x 6)) '(x 6) "operands * variable and number")
(check-equal? (operands '(- x y)) '(x y) "operands - variables")
(check-equal? (operands '(cons "this" "that")) '("this" "that") "operands cons strings")

; eval-operands
(check-equal? (eval-operands '(1 6) global-env) '(1 6) "eval-operands numbers")
(check-equal? (eval-operands '("hello" "world") global-env) '("hello" "world") "eval-operands strings")
(check-equal? (eval-operands '('hello 'world) global-env) '(hello world) "eval-operands symbols")



; Primitive Tests  

; primitive-procedure?
(check-equal? (primitive-procedure? '+) #t "primtive-procedure? +")
(check-equal? (primitive-procedure? '-) #t "primtive-procedure? -")
(check-equal? (primitive-procedure? '*) #t "primtive-procedure? *")
(check-equal? (primitive-procedure? '/) #t "primtive-procedure? /")
(check-equal? (primitive-procedure? '=) #t "primtive-procedure? =")
(check-equal? (primitive-procedure? 'cons) #t "primtive-procedure? cons")
(check-equal? (primitive-procedure? '(1 2)) #f "primtive-procedure? list")

; primitive-name
(check-equal? (primitive-name '+) '+ "primtive-name +")
(check-equal? (primitive-name '-) '- "primtive-name -")
(check-equal? (primitive-name 'cons) 'cons "primtive-name cons")
; (check-equal? (primitive-name 'this) (error "Not a primtive procedure!") "primtive-name error case") Will throw error

; primitive-implementation
(check-equal? (procedure? (primitive-implementation '+)) #t "primitive-implementation +")
(check-equal? (procedure? (primitive-implementation '-)) #t "primitive-implementation -")
(check-equal? (procedure? (primitive-implementation 'cons)) #t "primitive-implementation cons")
(check-equal? (procedure? (primitive-implementation '=)) #t "primitive-implementation =")
; (check-equal? (procedure? (primitive-implementation 'this)) #f "primitive-implementation this") Will throw an error

; eval-application
(check-equal? (eval-application '(+ 1 2) global-env) 3 "eval-application +")
(check-equal? (eval-application '(- 4 2) global-env) 2 "eval-application -")
(check-equal? (eval-application '(* 3 2) global-env) 6 "eval-application *")
(check-equal? (eval-application '(= 1 1) global-env) #t "eval-application =")
(check-equal? (eval-application '(= 1 2) global-env) #f "eval-application =")
(check-equal? (eval-application '(cons 1 2) global-env) '(1 . 2) "eval-application =")






; Begin  
(define (test-eval-begin)
  (check-equal? (eval-begin '(begin) '()) "Evaluate empty begin expression")
  (check-equal? (eval-begin '(begin (+ 2 3)) '()) 5 "Evaluate single expression")
  (check-equal? (eval-begin '(begin (define x 5) (+ x 3)) '((x . 2))) 8 "Evaluate multiple expressions"))

; If  
(define (test-eval-if)
  (check-equal? (eval-if '(if #t 5) '()) 5 "Basic if-then")
  (check-equal? (eval-if '(if #f 5) '()) 'void "Basic if-else")
  (check-equal? (eval-if '(if #t 5 10) '()) 5 "If-then-else - true condition")
  (check-equal? (eval-if '(if #f 5 10) '()) 10 "If-then-else - false condition"))

; Cond  

(define (test-eval-cond)
  (check-equal? (eval-cond '(cond ((= 3 5) "this is false so we skip this one")
                                  ((= 3 3) "this is true so we return this message")
                                  ((= 5 5) "this is also true but since an earlier test was true this never gets returned")))
                "this is true so we return this message")
               
  (check-equal? (eval-cond '(cond (1 "since 1 is not #f, this evaluates to be true")
                                  (else "and once again, we never get here")))
                "since 1 is not #f, this evaluates to be true")
                
  (check-equal? (eval-cond '(cond ((< (+ 1 1) 0) 'nope)
                                  ((< 1 0) 'still-no)
                                  ((= 4 5) 'no-matches)))
                'no-matches)
               
  (check-equal? (eval-cond '(cond ((< (+ 1 1) 0) 'nope)
                                  ((< 1 0) 'still-no)
                                  (else 'matched-else)))
                'matched-else)
              
  (check-equal? (eval-cond '(cond ((+ 1 1))))
                2)
              
  (check-equal? (eval-cond '(cond))
                '()))
               

(define (test-check-one-exp)
  (check-equal? (check-one-exp '()) '() "Empty expression")
  (check-equal? (check-one-exp '((= 3 5) "this is false so we skip this one")
                                '((= 3 3) "this is true so we return this message")
                                '((= 5 5) "this is also true but since an earlier test was true this never gets returned"))
                '()
                "Non-empty expression"))


; lambda
 (check-equal? (lambda? '(+ 10 10)) #f "lambda? #f")
 (check-equal? (lambda? '(lambda (x) (+ x 30) (/ -50 5))) #t "lambda? #t")
 (check-equal? (lambda? '(lambda x y)) #t "lambda? #t")



; closure
(check-equal? (closure? (make-closure '(lambda (x) (+ x y)) global-env)) #t "closure? #t")
(check-equal? (closure? (make-closure '(lambda (x) (+ x y) (+ x 50)) global-env)) #t "closure? #t")
(check-equal? (closure? '(closure (lambda (x) (+ x y)) (((y 100) (x 200))))) #t "closure? #t")

(check-equal? (procedure-parameters (make-closure '(lambda (x z) (+ x y)) global-env)) '(x z) "procedure-parameters")
(check-equal? (procedure-parameters (make-closure '(lambda (x) (+ x y)) global-env)) '(x) "procedure-parameters")

(check-equal? (procedure-body (make-closure '(lambda () (newline))
                        global-env)) '((newline)) "procedure body")
(check-equal? (procedure-body (make-closure '(lambda () (display 5) (newline))
                        global-env)) '((display 5) (newline)) "procedure body")

(check-equal? (procedure-env (make-closure '(lambda () (display 5) (newline))
                     (empty-env))) '() "procedure environment empty")
(check-equal? (procedure-env (make-closure '(lambda () (display 5) (newline))
                     global-env)) global-env "procedure environment global")

#|
Also tested several lambda functions in i-eval to ensure proper functionality

(define x 200)
    (define y 100)
    (define f
        (lambda (x)
                (+ x y)))
    (f 50) ; returns 150
    x ; still 200

(define g
        (lambda (y)
            (f y)))
(g 50) ; returns 150
y ; still 100

((lambda (x) (+ x 30)) (/ -50 5))
20


|#


; let
(check-equal? (let? '(let ((x 10) (y 20)) (+ x y))) #t "let? #t")

(check-equal? (let? '(define x 10)) #f "let? #f")

; let-> lambda
(check-equal? (let->lambda '(let ((x 10) (y 20)) (+ x y))) '(lambda (x y) (+ x y)) "Valid let expression")

(begin
  (define error-raised #f)
  (with-handlers ([exn:fail? (lambda (exn) (set! error-raised #t))])
    (let->lambda '(define x 10)))
  (check-true error-raised "Invalid let expression should raise an error"))
; map
 (check-equal? (eval-map '(map add1 '(4 5 6)) (make-base-namespace)) '(5 6 7))
 (check-equal? (eval-map '(map (lambda (x) (* x x)) '(4 5 6)) (make-base-namespace)) '(16 25 36))
 (check-equal? (eval-map '(map add1 '()) (make-base-namespace)) '())

;filter

(check-equal? (eval-filter '(filter even? '(1 2 3 4 5 6)) (make-base-namespace)) '(2 4 6))
(check-equal? (eval-filter '(filter (lambda (x) (> x 3)) '(1 2 3 4 5)) (make-base-namespace)) '(4 5))
(check-equal? (eval-filter '(filter even? '()) (make-base-namespace)) '())


