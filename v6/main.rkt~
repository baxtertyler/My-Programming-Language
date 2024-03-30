#lang typed/racket
(require typed/rackunit)

;language definition
(define-type ExprC (U NumC BinopC IdC Ifleq0C AppC LamC IfC StrC ErrC))
(struct NumC    ([n : Real]) #:transparent)                                   ;numbers
(struct BinopC  ([operation : Symbol] [l : ExprC] [r : ExprC]) #:transparent) ;+-*/
(struct AppC    ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)         ;Function call
(struct IdC     ([id : Symbol]) #:transparent)                                ;Variable
(struct Ifleq0C ([c : ExprC] [y : ExprC] [n : ExprC]) #:transparent)          ;Simple conditional
(struct IfC ([c : ExprC] [then : ExprC] [else : ExprC]) #:transparent)        ;If conditional statement
(struct StrC ([s : String]) #:transparent)                                    ;Simple string
(struct LamC ([arg : (Listof Symbol)] [body : ExprC]) #:transparent)          ;Lambda function
(struct ErrC ([v : Any]) #:transparent)                                       ;Error

;value definition
(define-type Value (U NumV BoolV StrV ClosV PrimopV ErrV))
(struct NumV ([n : Real]) #:transparent)
(struct BoolV ([b : Boolean]) #:transparent)
(struct StrV ([s : String]) #:transparent)
(struct ClosV ([arg : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimopV ([op : Symbol]) #:transparent)
(struct ErrV ([v : Any]) #:transparent)

;environment definition
(define-type Env (Listof Binding))
(struct Binding ([name : Symbol] [val : Value])) 
;environment init
(define mt-env '())
(define extend-env cons)
(define top-env (list
                 (Binding '+ (PrimopV '+))
                 (Binding '- (PrimopV '-))
                 (Binding '* (PrimopV '*))
                 (Binding '/ (PrimopV '/))
                 (Binding '<= (PrimopV '<=))
                 (Binding 'equal? (PrimopV 'equal?))
                 (Binding 'true (BoolV #t))
                 (Binding 'false (BoolV #f)))) 







;TOP-INTERP
;in: list of oazo5 syntax functions fun-sexps
;out: the evaluation of main function in fun-sexps
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))




;SERIALIZE
;in: a Value from interp
;out: the string representation of that value
(define (serialize [val : Value]) : String
  (match val
    [(NumV n) (number->string n)]
    [(BoolV #t) "true"]
    [(BoolV #f) "false"]
    [(StrV val) (format "~v" val)]
    [(ClosV _ _ _) "#<procedure>"]
    [(PrimopV _) "#<primop>"]))




;INTERP
;in: ExprC exp, list of FundefC lst
;out: evaluation of exp as a Real
(define (interp [exp : ExprC] [env : Env]) : Value
  (match exp
    [(NumC n) (NumV n)]
    [(StrC s) (StrV s)]
    [(ErrC msg) (error 'interp "user-error ~v" msg)]
    [(IdC id) (lookup id env)]
    [(LamC args body) (ClosV args body env)]
    [(IfC if then else) (match (interp if env)
                          [(BoolV b) (cond [b (interp then env)]
                                           [else (interp else env)])]
                          [else (error 'interp "OAZO5 if must be a truth value")])
                        (cond [(BoolV-b (cast (interp if env) BoolV)) (interp then env)]
                              [else (interp else env)])]
    [(AppC fun (list args ...)) (define f (interp fun env))
                                (define arguments (map (lambda ([a : ExprC])
                                                         (interp a env)) args))
                                (match f
                                  [(PrimopV op) (operation op (first arguments) (first (rest arguments)))]
                                  [(ClosV (list args ...) body env)
                                   (cond [(= (length arguments) (length args))
                                          (interp body (extend arguments args env))]
                                         [else (error 'interp "OAZO5 incorrect argument length")])]
                                  [(NumV n) (error 'interp "OAZO5 incorrect argument type of ~v" n)])]))





;LOOKUP
;in: a symbol and the current environment
;returns the symbols value in the environment, erros if not found
(define (lookup [for : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "OAZO5 name not found: ~e" for)]
    [(cons (Binding name val) r) (cond
                                   [(symbol=? for name) val]
                                   [else (lookup for r)])]))





;OPERATION
;in: the operation as a symbol and the two values
;out: values applied to the racket operation based on that symbol
(define (operation [op : Symbol] [l : Value] [r : Value]) : Value
  (cond [(and (NumV? l) (NumV? r))
         (match op
           ['+ (NumV (+ (NumV-n l) (NumV-n r)))]
           ['- (NumV (- (NumV-n l) (NumV-n r)))]
           ['* (NumV (* (NumV-n l) (NumV-n r)))]
           ['/ (cond [(equal? (NumV-n r) 0) (error 'operation "OAZO5 div by 0")]
                     [else (NumV (/ (NumV-n l) (NumV-n r)))])]
           ['<= (BoolV (<= (NumV-n l) (NumV-n r)))]
           ['equal? (BoolV (equal? l r))])]
        [else (cond [(equal? op 'equal?)(BoolV (equal? l r))]
                    [else (error 'parse "OAZO5 operation invalid")])]))





;EXTEND
;in: a list or agumenets, list of parameters, and current environment
;out: the new environment that has the parameters with the values of arguments
(define (extend [arg : (Listof Value)] [param : (Listof Symbol)] [env : Env]) : Env
  (match arg
    ['() env]
    [a (extend (rest arg) (rest param) (extend-env (Binding (first param) (first arg)) env))]))


 
 

;PARSE
;in: s-expression code
;out: the parsed ExprC representation of code
(define (parse [code : Sexp]) : ExprC
  (match code 
    [(? real? n)   (NumC n)]
    [(? string? s) (StrC s)]
    [(list 'error msg) (ErrC msg)]
    ['error (ErrC "")]
    [(list 'if i 'then t 'else e) (IfC (parse i) (parse t) (parse e))]
    [(list 'let (list (? symbol? (? is-allowed? var)) '<- val) ... body)
     (parse (cast (cons (list 'anon var ': body) val) Sexp))]
    [(? symbol? s) (cond [(is-allowed? s) (IdC s)]
                         [else (error 'parse "OAZO5 keyword error: ~e" s)])]
    [(list 'anon (list (? symbol? (? is-allowed? args)) ...) ': body)
     (cond [(and (not-has-duplicates? (cast args (Listof Symbol)))
                 (cast args (Listof Symbol))) (LamC (cast args (Listof Symbol)) (parse body))]
           [else (error 'interp "OAZO5 two args with the same name")])]
    [(list func args ...) (AppC (parse func) (for/list ([item (in-list args)]) 
                                               (parse (cast item Sexp))))]
    [other (error 'parse "OAZO5 syntax error in ~e" other)]))





;IS-ALLOWED
;in: symbol s
;out: boolean represntation of if the symbol is not a keyword
(define (is-allowed? [s : Sexp]) : Boolean
  (match s
    ['if #f]
    ['let #f]
    ['then #f]
    ['anon #f]
    [': #f]
    ['<- #f]
    [else #t]))




;HAS-NOT-DUPLICATES
;in: a list of symbols
;out: not (boolean reprentation of if the symbol contains duplicates)
(define (not-has-duplicates? [lst : (Listof Symbol)]) : Boolean
  (define sorted-list : (Listof Symbol)
    (sort lst symbol<?)) ; Sort the list in ascending order
  (define (check-duplicates [lst : (Listof Symbol)]) : Boolean
    (cond
      [(or (empty? lst) (empty? (rest lst))) #t] ; Base case: no duplicates found
      [(equal? (first lst) (second lst)) #f] ; Found a duplicate
      [else (check-duplicates (rest lst))])) ; Recur with the rest of the list
  (check-duplicates sorted-list))







;------------------------ TESTING ----------------------------------
;basic functions
(check-equal? (top-interp '{let [w <- 5] [x <- 7] [y <- 5] [z <- 7] {/ {- {* {+ x y} z} w} 1}}) "79")
(check-equal? (top-interp '{{anon {x} : {+ x 1}} 8}) "9")
(check-equal? (top-interp '{{anon {x} : {<= x 9}} 8}) "true")
(check-equal? (top-interp '{{anon {x} : {<= x 9}} 80}) "false")
(check-equal? (top-interp '{{anon {h} : {h 8}} {anon {x} : {+ x 1}}}) "9") 
(check-equal? (top-interp '{{{anon {f g} : {anon {x} : {f {g x}}}} {anon {x} : {+ x 10}}
                                                                   {anon {x} : {* x 2}}} 10}) "30") 
(check-equal? (top-interp '{{anon {x} : {if {<= x 9} then {- 1 2} else {+ 1 2}}} -1}) "-1")
(check-equal? (top-interp '{let
                               {z <- {+ 9 14}}
                             {y <- 98} 
                             {p <- 44}
                             {- {+ z y} p}}) "77")
(check-equal? (top-interp '{{anon {x} : {if {equal? x "hello"} then {- 1 1} else {+ 1 2}}} "hello"}) "0")
(check-equal? (top-interp '{{anon {x} : {if {equal? x 2} then {- 1 1} else {+ 1 2}}} 1}) "3")
(check-equal? (top-interp '{{anon {x} : {if {equal? x "hello"} then {- 1 1} else {+ 1 2}}} "yes"}) "3")
(check-equal? (top-interp '{{anon {x} : {if {equal? x "hello"} then "yes" else {+ 1 2}}} "hello"})  "\"yes\"")
(check-equal? (serialize (ClosV '(x y) (NumC 0) mt-env)) "#<procedure>")
(check-equal? (serialize (PrimopV '-)) "#<primop>")
(check-equal? (parse "hello") (StrC "hello"))

;error testing
(check-exn #rx"name not found" (lambda () (top-interp '{{anon {x} : {<= y 9}} 8})))
(check-exn #rx"argument length" (lambda () (top-interp '{{anon {x y} : {<= y 9}} 8})))
(check-exn #rx"syntax" (lambda () (top-interp '{})))
(check-exn #rx"keyword" (lambda () (top-interp '{{anon {if y} : {<= y 9}} 8})))
(check-exn #rx"keyword" (lambda () (parse ':)))
(check-exn #rx"keyword" (lambda () (top-interp '{{anon {let y} : {<= y 9}} 8})))
(check-exn #rx"keyword" (lambda () (top-interp '{{anon {anon y} : {<= y 9}} 8})))
(check-exn #rx"keyword" (lambda () (top-interp '{{anon {: y} : {<= y 9}} 8})))
(check-exn #rx"keyword" (lambda () (top-interp '{{anon {<- y} : {<= y 9}} 8})))
(check-exn #rx"OAZO" (lambda () (top-interp '(+ + +))))
(check-exn #rx"keyword" (lambda () (top-interp '(+ then 4))))
(check-exn #rx"truth" (lambda () (top-interp '{{anon {x} : {if {+ 1 2} then {- 1 2} else {+ 1 2}}} -1})))
(check-exn #rx"div" (lambda () (top-interp '(/ 1 (- 3 3)))))
(check-exn #rx"OAZO" (lambda () (parse '(anon (x x) : 3)))) 
(check-exn #rx"OAZO" (lambda () (parse '(anon (x x) : 3))))
(check-exn #rx"user-error" (lambda () (top-interp '{{anon {x} : {error "whats going on"}} 8}))) 
(check-exn #rx"user-error" (lambda () (top-interp '(+ 4 (error "1234")))))
(check-exn #rx"user-error" (lambda () (top-interp '((anon (e) : (e e)) error))))
(check-exn #rx"OAZO5 incorrect argument type of" (lambda () (top-interp '{3 4 5})))





