(load "qq.scm") 
(define pr pattern-rule)

;;;;;;;;;;;;;;;;;;;;
(define cntList '())
(define symList '())
(define finalSymList '())
(define generate-symList
        (lambda()
            (set! symList (filter (lambda(expr) (not (null? expr))) 
            (map (lambda(pair) (if (< 1 (unbox (cdr pair)))  (list (gensym) (car pair)) '())) cntList)))))
        
(define replace
    (lambda (expr)
        (let ((exist (filter (lambda(pair) (equal? (cadr pair) expr)) symList)))
            (if (null? exist) expr
                (caar exist))
        )))
        
(define cnt 
    (lambda(expr)
    (letrec((runner (lambda(lst expr)
        (if (null? lst) (set! cntList (append cntList (list (cons expr (box 1))))) 
            (if (equal? (caar lst) expr) (set-box! (cdar lst) (+ 1 (unbox (cdar lst)))) 
                (runner (cdr lst) expr))))))
        (runner cntList expr)
        expr
    ))) 

(define search
    (lambda(x lst cont)
        (map (lambda(y) (if (list? y) (search x y cont) (if (equal? x y) (cont y) y))) lst)))
(define countOccur
    (lambda (item lst)
        (let ((tmpCnt (box 0))
              (counter (box 0)))
            (map (lambda(x) (search (car item) x (lambda(_)(set-box! tmpCnt (+ 1 (unbox tmpCnt)))))
                            (if (> (unbox tmpCnt) 0) (set-box! counter (+ 1 (unbox counter))))
                            (set-box! tmpCnt 0)) lst)
            (unbox counter))))
(define countOccurInCode
    (lambda (item code)
        (let ((counter (box 0)))
            (search (car item) code (lambda(_)(set-box! counter (+ 1 (unbox counter)))))
            (unbox counter))))

(define putMeBack
    (lambda (item lst)
            (map (lambda(x) (search (car item) x (lambda(_)(cdr item)))) lst)))


(define minimizeSymList
    (lambda(code)
        (letrec ((minimize (lambda(lst)
                                (if (null? lst) '()
                                (let* ((curr (car lst))
                                       (rest (cdr lst))
                                       (amount (countOccur curr rest))
                                       (amountinCode (countOccurInCode curr code)))
                                    (if (and (= 1 amount) (= 0 amountinCode)) (minimize (putMeBack curr rest))
                                                                              (cons curr (minimize rest))))))))
            (set! finalSymList (minimize finalSymList))
            finalSymList
            )))
    
(define func cnt)

(define simple? (lambda(expr) (or (not (list? expr)) (equal? (car expr) 'quote))))
(define simplify (lambda (exprs) (map simplifyone exprs)))
(define simplifyone (lambda(x) (if (simple? x) x (tag-p x))))
(define all-simple? (lambda (exprs) (andmap simple? exprs)))
;;;;;;;;;;;;;;;;;;;


(define const-patterns
    (compose-patterns
        (pr `() (lambda() `()))
        (pr `,(? 'vec vector?) (lambda(vec) vec))
        (pr `,(? 'bool boolean?) (lambda(bool) bool))
        (pr `,(? 'char char?) (lambda(ch) ch))
        (pr `,(? 'num number?) (lambda(num) num))
        (pr `,(? 'str string?) (lambda(str) str))
        (pr `(quote ,(? 'dtc)) (lambda(dtc) dtc))
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
    (pr `,(? 'var atom? var?) (lambda(var) var))) 

(define disj-patterns
    (compose-patterns
        (pr `(or) (lambda() (func '(or))))
        (pr
            `,(? 'or-exps list? (lambda(lst)(eq? (car lst) 'or))) 
            (lambda(exps) (if (all-simple? exps) (func `(or ,@(cdr exps))) `(or ,@(simplify (cdr exps))))))
        (pr `(and) (lambda() (func '(and))))
        (pr
            `,(? 'and-exps list? (lambda(lst)(eq? (car lst) 'and))) 
            (lambda(exps) (if (all-simple? exps) (func `(and ,@(cdr exps))) `(and ,@(simplify (cdr exps))))))
                            ))

(define if-patterns
    (compose-patterns
        (pr
            `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
            (lambda(test dit dif)
                (let ((newparts (list test dit dif)))
                (if (all-simple? newparts) (func `(if ,@newparts)) `(if ,@(simplify newparts))))))
        (pr
            `(if ,(? 'test) ,(? 'dit))
            (lambda(test dit)
                (let ((newparts (list test dit)))
                (if (all-simple? newparts) (func `(if ,@newparts)) `(if ,@(simplify newparts)))))))
                )
            
(define lambda-patterns
    (pr 
        `(lambda ,(? 'argl) ,(? 'body) . ,(? 'bods))
        (lambda(argl body bods)
            (let ((newbody (cons body bods)))
                (if (all-simple? newbody) (func `(lambda ,argl ,@newbody)) `(lambda ,argl ,@(simplify newbody)))))))
                    
(define def-patterns
    (compose-patterns
        (pr 
        `(define ,(? 'lst pair? (lambda(id)(var? (car id)))) ,(? 'body)) 
                    (lambda(nameNargs body)
                        (if (simple? body) (func `(define ,nameNargs ,body)) `(define ,nameNargs ,(simplifyone body)))))
        (pr 
        `(define ,(? 'var var?) ,(? 'body))  
                (lambda(var body) 
                    (if (simple? body) (func `(define ,var ,body)) `(define ,var ,(simplifyone body)))))))
                    
(define ass-pattern
    (pr `(set! ,(? 'var var?) ,(? 'expr)) (lambda(var body) 
        (if (simple? body) (func `(set! ,var ,body)) `(set! ,var ,(simplifyone body))))
    ))
                    
(define app-patterns
    (pr `,(? 'app list?)
                  (lambda(app) (if (all-simple? app) (func app) (simplify app)))))

(define cond-pattern
    (pr `,(? 'exprs list? 
            (lambda(lst)(eq? (car lst) 'cond))) 
        (lambda(exprs) (if (andmap all-simple? (cdr exprs)) (func exprs) `(cond ,@(map simplify (cdr exprs)))))
                ))

(define conslet 
    (lambda(es b bs sign) 
            (let ((newbody (cons b bs)))
                (if (and (all-simple? newbody) (andmap (lambda(expr) (simple? (cadr expr))) es)) 
                (func `(,sign ,es ,@newbody)) 
                `(,sign ,(map (lambda(expr) 
                                (list (car expr) (if (simple? (cadr expr)) (cadr expr)
                                                                           (simplifyone (cadr expr)))))
                                es) ,@(simplify newbody))))))
                
(define let-patterns
    (compose-patterns
        (pr `(let* ,(? 'exprs list?) ,(? 'body) . ,(? 'bods)) 
            (lambda(es b bs) (conslet es b bs 'let*)))
        (pr `(letrec ,(? 'exprs list?) ,(? 'body) . ,(? 'bods)) 
            (lambda(es b bs)
                (conslet es b bs 'letrec)))
        (pr `(let ,(? 'exprs list?) ,(? 'body) . ,(? 'bods)) 
        (lambda(es b bs)
            (conslet es b bs 'let)))
            ))

         
(define begin-pattern
    (compose-patterns
        (pr `(begin) 
            (lambda() (func '(begin))))
        (pr `(begin ,(? 'expr) . ,(? 'other-exprs)) 
            (lambda(body bods) (let ((newbody (cons body bods)))
                (if (all-simple? newbody) (func `(begin ,@newbody)) `(begin ,@(simplify newbody))
                ))))))


	    
;############################################################################;

            
(define tag-p
    (let ((run
            (compose-patterns
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
            (run sexpr (lambda() 'fail)))))
        
(define cse-h
        (lambda(expr)
            (letrec ((optimize (lambda(oexpr)
                                (set! cntList '())
                                (set! symList '())
                                (set! func cnt)
                                (tag-p oexpr)
                                (generate-symList)
                                (if (null? symList) oexpr
                                 (begin
                                    (set! func replace)
                                    (set! finalSymList (append finalSymList symList))
                                    (optimize (tag-p oexpr)))))))
            (set! finalSymList '())
            (let* ((finalexpr (optimize expr))
                  (theUltimateSymList (minimizeSymList finalexpr)))
                (cond ((null? theUltimateSymList) expr) 
                      ((= 1 (length theUltimateSymList)) `(let ,theUltimateSymList ,finalexpr)) 
                      (else `(let* ,theUltimateSymList ,finalexpr)))
            ))))
(define cse (lambda(expr) (display (cse-h expr))))  