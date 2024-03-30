#lang typed/racket
(require typed/rackunit)

(provide file-interp)
 
(define-type Simple-Sexp
  (U Symbol Number String (Listof Simple-Sexp)))
 
(define-predicate sexp? Simple-Sexp)
 
(define (file-interp [f : Path-String]) 
  (define v (file->value f))
  (cond [(sexp? v) (top-interp v)]
        [else (error 'file-interp "expected file to contain s-expression, got: ~e" v)]))


;language definition
(define-type ExprC (U NumC IdC AppC LamC IfC StrC ErrC))
(struct NumC    ([n : Real]) #:transparent)                                   ;numbers
(struct AppC    ([fun : ExprC] [arg : (Listof ExprC)]) #:transparent)         ;Function call
(struct IdC     ([id : Symbol]) #:transparent)                                ;Variable
(struct IfC     ([c : ExprC] [then : ExprC] [else : ExprC]) #:transparent)        ;If conditional statement
(struct StrC    ([s : String]) #:transparent)                                    ;Simple string
(struct LamC    ([arg : (Listof Symbol)] [body : ExprC]) #:transparent)          ;Lambda function
(struct ErrC    ([v : Any]) #:transparent)                                       ;Error


;value definition
(define-type Value (U NumV BoolV StrV ClosV PrimopV ErrV))
(struct NumV    ([n : Real]) #:transparent)
(struct BoolV   ([b : Boolean]) #:transparent)
(struct StrV    ([s : String]) #:transparent)
(struct ClosV   ([arg : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimopV ([op : Symbol]) #:transparent)
(struct ErrV    ([v : Any]) #:transparent)

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
                 (Binding 'false (BoolV #f))
                 (Binding '++ (PrimopV '++))
                 (Binding 'println (PrimopV 'println))
                 (Binding 'read-num (PrimopV 'read-num))
                 (Binding 'seq (PrimopV 'seq)))) 
 






;TOP-INTERP
;in: list of OAZO6 syntax functions fun-sexps
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
                          [else (error 'interp "OAZO6 if must be a truth value")])]
    [(AppC fun (list args ...)) (define f (interp fun env))
                                (define arguments (map (lambda ([a : ExprC])
                                                         (interp a env)) args))
                                (match f
                                  [(PrimopV op) (operation op arguments)]
                                  [(ClosV (list args ...) body env)
                                   (cond [(= (length arguments) (length args))
                                          (interp body (extend arguments args env))]
                                         [else (error 'interp "OAZO6 incorrect argument length")])]
                                  [(NumV n) (error 'interp "OAZO6 incorrect argument type of ~v" n)])]))




;LOOKUP
;in: a symbol and the current environment
;returns the symbols value in the environment, erros if not found 
(define (lookup [for : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "OAZO6 name not found: ~e" for)]
    [(cons (Binding name val) r) (cond
                                   [(symbol=? for name) val]
                                   [else (lookup for r)])]))




;OPERATION
;in: the operation as a symbol and the two values
;out: values applied to the racket operation based on that symbol
(define (operation [op : Symbol] [args : (Listof Value)]) : Value
  (cond [(equal? (length args) 2)
         (define l (first args))
         (define r (first (rest args)))
         (cond [(and (NumV? l) (NumV? r))
                (match op
                  ['+ (NumV (+ (NumV-n l) (NumV-n r)))]
                  ['- (NumV (- (NumV-n l) (NumV-n r)))]
                  ['* (NumV (* (NumV-n l) (NumV-n r)))]
                  ['/ (cond [(equal? (NumV-n r) 0) (error 'operation "OAZO6 div by 0")]
                            [else (NumV (/ (NumV-n l) (NumV-n r)))])]
                  ['<= (BoolV (<= (NumV-n l) (NumV-n r)))]
                  ['equal? (BoolV (equal? l r))]
                  ['seq (last args)]
                  ['++ (define str (combine args)) str]
                  [else (error 'parse "OAZO6 operation invalid")])]
               [(and (StrV? l) (StrV? r))
                (match op
                  ['equal? (BoolV (equal? l r))]
                  ['++ (define str (combine args)) str]
                  [else (error 'parse "OAZO6 operation invalid")])]
               [else (match op
                       ['++ (define str (combine args)) str]
                       ['equal? (BoolV (equal? l r))]
                       ['seq (last args)]

                       [else (error 'parse "OAZO6 operation invalid ~e" op)])])]  
        [(equal? (length args) 1) (match op 
                                    ['println
                                     (match (first args)
                                       [(? StrV?) (displayln (StrV-s (cast (first args) StrV))) (BoolV #t)]
                                       [else (error 'println "OAZO6 error ~e" (first args))])]
                                    ['++ (define str (combine args)) str]
                                    [else (error 'parse "OAZO6 operation invalid")])]
        [(equal? (length args) 0) (match op
                                    ['read-num (NumV (read))]
                                    [else (error 'parse "OAZO6 operation invalid")])] 
        [else (match op 
                ['seq (last args)]
                ['++ (define str (combine args)) str]   
                [else (error 'parse "OAZO6 operation invalid ~e" op)])]))
 


(define (combine [input : (Listof Value)]) : StrV
  (match input 
    ['() (StrV "")]
    [(cons inp rest-input)
     (StrV (string-append
            (cond
              [(StrV? inp) (StrV-s inp)]
              [(NumV? inp) (format "~a" (NumV-n inp))]
              [else (error '++ "OAZO: expected either a Real or String")])
            (StrV-s (combine rest-input))))]))



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
                         [else (error 'parse "OAZO6 keyword error: ~e" s)])]
    [(list 'anon (list (? symbol? (? is-allowed? args)) ...) ': body)
     (cond [(and (not-has-duplicates? (cast args (Listof Symbol)))
                 (cast args (Listof Symbol))) (LamC (cast args (Listof Symbol)) (parse body))]
           [else (error 'interp "OAZO6 two args with the same name")])]
    [(list func args ...) (AppC (parse func) (for/list ([item (in-list args)]) 
                                               (parse (cast item Sexp))))]
    [other (error 'parse "OAZO6 syntax error in ~e" other)]))


(define (read) : Real
  (display "> ")
  (define input (read-line (current-input-port)))
  (cond
    [(eof-object? input) (error 'read "Reached end of file while reading")]
    [(string? input)
     (define num (string->number input))
     (cond
       [(and (real? num) (not (nan? num))) num]
       [else (error 'read "Tried to read a non-number")])]
    [else (error 'read "Invalid input type")]))



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
    (sort lst symbol<?))
  (define (check-duplicates [lst : (Listof Symbol)]) : Boolean
    (cond
      [(or (empty? lst) (empty? (rest lst))) #t] 
      [(equal? (first lst) (second lst)) #f] 
      [else (check-duplicates (rest lst))]))
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
(check-exn #rx"OAZO6 incorrect argument type of" (lambda () (top-interp '{3 4 5})))



;; ---------------------- EXAMPLE PROGRAM -------------------------------------

#;{seq
   {println ""}
   {println "~~~ Welcome to your virtual golf score card ~~~"}
   {println "Enter each hole's par and hits, and we will calculate your score"}
   {seq
    {println "How many holes did you play?"}
    {let [num_holes <- {read-num}]
      {let [golf <-
                 {anon
                  {self hole par score} :
                  {if
                   {<= hole num_holes}
                   then
                   {seq
                    {println ""}
                    {println (++ "----- HOLE: " hole " -----")}
                    {println "What was the par?"}
                    {let [p <- {read-num}]
                      {seq
                       {println (++ "How did you hit?")}
                       {let [s <- {read-num}]
                         {seq

                          {if
                           {equal? 0 (- s p)}
                           then
                           {println "Par, nice!"}
                           else
                           {if
                            {equal? -1 (- s p)}
                            then
                            {println "Birdie!"}
                            else
                            {if
                             {equal? -2 (- s p)}
                             then
                             {println "Eagle! Woohoo!"}
                             else
                             {if
                              {equal? 1 {- s p}}
                              then
                              {println "Bogey, pretty good"}
                              else
                              {if
                               {equal? 2 {- s p}}
                               then {println "Double bogey, keep working"}
                               else {if {equal? 1 s}
                                        then {println "HOLE IN ONE!"}
                                        else { if {<= 3 {- p s}}
                                                  then {println "I think you're lying..."}
                                                  else {println "You suck..."}}}}}}}}

                          {println ""}
                          {self self {+ hole 1} {+ p par} {+ s score}}}}}}
                    }
                   else {seq
                         {if
                          {equal? 0 (- score par)}
                          then
                          {println {++ "You shot even! Wow! Give your caddie a raise!"}}
                          else
                          {if {<= 0 (- par score)}
                              then
                              {println {++ "You shot " (- par score) " under par! You belong on the tour!"}}
                              else
                              {if {<= (- score par) 5}
                                  then
                                  {println {++ "You shot " (- score par) " over par! Getting good!"}}
                                  else
                                  {if {<= (- score par) 10}
                                      then
                                      {println {++ "You shot " (- score par) " over par... Get to the range"}}
                                      else
                                      {if {<= (- score par) 15}
                                          then {println
                                                {++ "You shot " (- score par)
                                                    " over par... have you thought about tennis?"}}
                                          else {println "You suck....."}}}}}}
                         {println ""}}}}]
        {golf golf 1 0 0}}}}

   }
#;(
   ~~~ Welcome to your virtual golf score card ~~~
       Enter each hole's par and hits, and we will calculate your score
       How many holes did you play?
       > 9

       ----- HOLE: 1 -----
       What was the par?
       > 4
       How did you hit?
       > 5
       Bogey, pretty good


       ----- HOLE: 2 -----
       What was the par?
       > 3
       How did you hit?
       > 3
       Par, nice!


       ----- HOLE: 3 -----
       What was the par?
       > 5
       How did you hit?
       > 3
       Eagle! Woohoo!


       ----- HOLE: 4 -----
       What was the par?
       > 4
       How did you hit?
       > 9
       You suck...


       ----- HOLE: 5 -----
       What was the par?
       > 4
       How did you hit?
       > 3
       Birdie!


       ----- HOLE: 6 -----
       What was the par?
       > 8
       How did you hit?
       > 1
       HOLE IN ONE!


       ----- HOLE: 7 -----
       What was the par?
       > 4
       How did you hit?
       > 6
       Double bogey, keep working


       ----- HOLE: 8 -----
       What was the par?
       > 5
       How did you hit?
       > 2
       I think you're lying...


       ----- HOLE: 9 -----
       What was the par?
       > 10
       How did you hit?
       > 10
       Par, nice!

       You shot 5 under par! You belong on the tour!

       )





