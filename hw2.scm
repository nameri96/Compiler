(load "qq.scm") 
(load "hw1.scm") 


(define pr pattern-rule)

    
(define unique
    (lambda(lst)
        (if (null? lst) #t
        (if (pair? lst)
            (and (not (member (car lst) (cdr lst))))
            (unique (cdr lst))))))
            
(define is-vs-unique
    (lambda(lst) (unique (map car lst))))

(define const-patterns
    (compose-patterns
        (pr `() (lambda() `(const ())))
        (pr `,(? 'vec vector?) (lambda(vec) `(const ,vec)))
        (pr `,(? 'bool boolean?) (lambda(bool) `(const ,bool)))
        (pr `,(? 'char char?) (lambda(ch) `(const ,ch)))
        (pr `,(? 'num number?) (lambda(num) `(const ,num)))
        (pr `,(? 'str string?) (lambda(str) `(const ,str)))
        (pr `(quote ,(? 'dtc)) (lambda(dtc) `(const ,dtc)))
        ))

(define *reserved-words*
    '(and begin cond define do else if lambda
      let let* letrec or quasiquote unquote
      unquote-splicing quote set!))

(define var? 
    (lambda(x) (and (symbol? x) (not (member x *reserved-words*)))))
    
(define special-form
    (lambda(sf)(lambda(lst)(eq? (car lst) sf))))
      
(define var-patterns
    (pr `,(? 'var atom? var?) (lambda(var) `(var ,var)))) 

(define disj-patterns
    (compose-patterns
        (pr `(or) (lambda()`(const #f))) 
        (pr
            `,(? 'or-exps list? (lambda(lst)(eq? (car lst) 'or))) 
            (lambda(exps) (if (equal? 1 (length (cdr exps))) (tag-parse (cadr exps)) `(or ,(map tag-parse (cdr exps))))))
        (pr `(and) (lambda()`(const #t))) 
        (pr
            `,(? 'and-exps list? (lambda(lst)(eq? (car lst) 'and))) 
            (lambda(exps) (if 
                            (= 2 (length exps)) 
                                (tag-parse (cadr exps))
                                (tag-parse `(if ,(cadr exps) ,(cons 'and (cddr exps)) #f)))))
                            ))

(define if-patterns
    (compose-patterns
        (pr
            `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
            (lambda(test dit dif)
                (let ((tagedtest (tag-parse test)))
                    (if (equal? (car tagedtest) 'const)
                        (if (cadr tagedtest) (tag-parse dit) (tag-parse dif)) 
                        `(if3 ,(tag-parse test) ,(tag-parse dit) ,(tag-parse dif))))))
        (pr
            `(if ,(? 'test) ,(? 'dit))
            (lambda(test dit) 
                `(if3 ,(tag-parse test) ,(tag-parse dit) (const ,(void)))))
                ))
                
(define identify-lambda
	(lambda (argl ret-simple ret-opt ret-var)
		(cond 
			((null? argl) (ret-simple '()))
			((var? argl) (ret-var argl)) 
			(else (identify-lambda (cdr argl)
					(lambda (s) (ret-simple `(,(car argl) ,@s))) 
					(lambda (s opt) (ret-opt `(,(car argl) ,@s) opt)) 
					(lambda (var) (ret-opt `(,(car argl)) var)))))))

					
(define lambda-patterns
    (pr 
        `(lambda ,(? 'argl) ,(? 'body) . ,(? 'bods))
        (lambda(argl body bods)
            (identify-lambda argl
                (lambda(args)
                    (if (not (unique args)) (error 'lambda "ERROR")
                    `(lambda-simple ,args ,(tag-parse `(begin ,body ,@bods)))))
                (lambda(args rest) 
                    `(lambda-opt ,args ,rest ,(tag-parse `(begin ,body ,@bods))))
                (lambda(args) 
                    `(lambda-var ,args ,(tag-parse `(begin ,body ,@bods))))
                    ))))
                    
(define def-patterns
    (compose-patterns
        (pr 
        `(define ,(? 'lst pair? (lambda(id)(var? (car id)))) ,(? 'body) . ,(? 'bods)) 
                    (lambda(nameNargs body bods)
                        `(def ,(tag-parse (car nameNargs))
                         ,(tag-parse `(lambda ,(cdr nameNargs) (begin ,body ,@bods))))))
        (pr 
        `(define ,(? 'var var?) ,(? 'body) . ,(? 'bods))  
                (lambda(var body bods) 
                    `(def ,(tag-parse var) ,(tag-parse `(begin ,body ,@bods)))))))
                    
(define ass-pattern
    (pr `(set! ,(? 'var var?) ,(? 'expr)) (lambda(var expr) 
        `(set ,(tag-parse var) ,(tag-parse expr)))
    ))
                    
(define app-patterns
    (pr `,(? 'app list?)
                  (lambda(app) `(applic ,(tag-parse (car app))
                                        ,(map tag-parse (cdr app))))))
(define cond-cont
    (lambda(exprs)
          (let((rest (cdr exprs)));((x 1 2)(else 5))
            (if (= 1 (length rest))
                (if(eq? (caar rest) 'else)
                    (tag-parse `(begin ,@(cdar rest)))
                (tag-parse
                `(if ,(caar rest) (begin ,@(cdar rest)))))
                (tag-parse 
                    `(if ,(caar rest) (begin ,@(cdar rest))
                                (cond ,@(cdr rest))))
                ))))
                
(define cond-pattern ;(cond (x 1 2) (else 5))
    (pr `,(? 'exprs list? 
            (lambda(lst)(eq? (car lst) 'cond))) 
                cond-cont
                ))
                                        
(define list-patterns
    (compose-patterns
        (pr `,(? 'lst list?) (lambda(lst) (map tag-parse lst)))
        (pr `,(? 'lst pair?) 
            (lambda(lst) (cons (tag-parse (car lst)) (tag-parse (cdr lst))
            )))))

(define mtp
    (lambda(lst)
        (map tp lst)))
        
(define mc
    (lambda(lst)
        (map car lst)))
        
(define md
    (lambda(lst)
        (map cdr lst)))
            
(define b-app
    (lambda(ps vs body)
        `(applic (lambda-simple (,@ps) ,body) ,vs)))

(define b-e-app
    (lambda(body)
        `(applic (lambda-simple () ,body) ())))
        
(define let->app
    (lambda(exprs body)
        (if (null? exprs) 
        `(applic (lambda-simple () ,(tag-parse body)) ())
        `(applic (lambda-simple ,(map car exprs) ,(tag-parse body)) ,(map (lambda(expr)(tag-parse (cadr expr))) exprs)))))
        
(define letrec->app
    (lambda(exprs body)
        (if (null? exprs) 
        `(applic (lambda-simple ()
                    ,(tag-parse `(let () ,body))) ())
        `(applic (lambda-simple ,(map car exprs) 
            (seq (,@(map (lambda(expr)
        (tag-parse `(set! ,(car expr) ,(cadr expr)))) exprs)
            ,(tag-parse `(let () ,body))))) 
        ,(map (lambda(expr)'(const #f)) exprs)))))

(define let*->app
    (lambda(exprs body)
        (cond ((null? exprs) 
        `(applic (lambda-simple (),(tag-parse body)) ()))
            ((= 1 (length exprs))
        `(applic (lambda-simple(,(caar exprs)),(tag-parse body)) (,(tag-parse (cadar exprs)))))
        (else 
        `(applic (lambda-simple (,(caar exprs)),(tag-parse `(let* ,(cdr exprs) ,body)))(,(tag-parse (cadar exprs)))))
         )))
        
(define ^some-let
    (lambda(f es b bs) (f es `(begin ,b ,@bs))))

        
(define let-patterns
    (compose-patterns
        (pr `(let* ,(? 'exprs list?) ,(? 'body) . ,(? 'bods)) 
            (lambda(es b bs) 
            (^some-let let*->app es b bs)))
        (pr `(letrec ,(? 'exprs list? is-vs-unique) 
                ,(? 'body) . ,(? 'bods)) 
            (lambda(es b bs)
                (^some-let letrec->app es b bs)))
        (pr `(let ,(? 'exprs list? is-vs-unique) 
                ,(? 'body) . ,(? 'bods)) 
            (lambda(es b bs)
                (^some-let let->app es b bs)))
            ))

(define fold
    (lambda(lst)
        (if (null? lst) (list)
            (append (car lst) (fold (cdr lst))))))
            
(define begin-pattern
    (compose-patterns
        (pr `(begin) 
            (lambda() `(const ,(void))))
        (pr `(begin ,(? 'expr) . ,(? 'other-exprs)) 
            (lambda(expr others)
            (if (null? others) (tag-parse expr)
            `(seq 
            ,(fold (map (lambda(exp)
                    (let ((taged (tag-parse exp)))
                        (if (eq? 'seq (car taged))
                            (cadr taged)
                            (list taged))))
                    (cons expr others)))))))))

(define quasi-pattern
        (compose-patterns
            (pr `(quasiquote ,(? 's) . ,(? 't))
                (lambda(s t)
                    (tag-parse (expand-qq s))))))

(define tag-parse
    (let ((run
            (compose-patterns
                quasi-pattern
                const-patterns
                var-patterns
                if-patterns
                disj-patterns
                lambda-patterns
                def-patterns
                ass-pattern
                let-patterns
                cond-pattern
                begin-pattern
                app-patterns

        )))
        (lambda(sexpr)
            (run sexpr (lambda() (error 'tag-parse "ERROR"))))))
    
(define tp tag-parse)

(define parse tag-parse)
    
(define hw2 (lambda(str) (map tag-parse (hw1 (string->list  str)))))
