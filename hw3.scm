(load "hw2.scm")
(load "cse.scm")

;;;;;;;;util;;;;;;;;;;;
(define find-first
    (lambda(var vlst)
        (cond ((null? vlst) #f)
              ((equal? var (caar vlst)) (car vlst))
              (else (find-first var (cdr vlst))))))

(define dislst
    (lambda(lst)
        (map (lambda(e) (display e)(newline)) lst)
        (newline)))
	
(define contains
    (lambda(e lst)
        (if (null? lst) #f (if (equal? (car lst) e) e (contains e (cdr lst))))))
	
(define search
    (lambda(pred lst cont)
        (if (and (list? lst) (not (null? lst)))
        (if (pred lst) (cont lst)
        (map (lambda(y) (if (and (list? y) (not (null? y))) (search pred y cont) y)) lst))
        lst)))

(define search-one-level
    (lambda(pred lst cont)
        (myMap (lambda(y) (if (pred y) (cont y) y)) lst)))
        
(define is-lambda
    (lambda(tag)
        (contains tag (list 'lambda-simple 'lambda-opt 'lambda-var))))
;;;;;;;;util;;;;;;;;;;;

;;;FINISHED;;;
;;;;;;;;;nested define;;;;;;;;;
(define create-letrec
    (lambda(defs regs)
        `(applic (lambda-simple ,(map cadadr defs)
                (seq (,@(map (lambda(def) 
                                `(set ,(cadr def) 
                                      ,(eliminate-nested-defines (caddr def)))) defs)
                      ,@(map eliminate-nested-defines regs))))
                      ,(map (lambda(def)'(const #f)) defs))
                ))
                            
(define def-help
    (lambda(seq)
        (let ((def-exprs '())
              (reg-exprs '()))
            (search-one-level 
                    (lambda(y) #t) 
                    seq 
                    (lambda(y)(if (equal? (car y) 'def)
                                (set! def-exprs 
                                    (append def-exprs (list y)))
                                (set! reg-exprs 
                                    (append reg-exprs (list y))))))
            (if (null? def-exprs) `(seq ,(eliminate-nested-defines seq))
                (create-letrec def-exprs reg-exprs))
                      )))
                            
(define eliminate-nested-defines
    (lambda(taged)
        (search (lambda(y) (equal? (car y) 'seq)) 
                taged 
                (lambda(y)(def-help (cadr y))))))
;;;;;;;;;nested define;;;;;;;;;

;;;FINISHED;;;
;;;;;;;;;redundant app;;;;;;;;;
(define redundant
    (lambda(lbda)
        (and (equal? (car lbda) 'lambda-simple) (null? (cadr lbda)))))

(define rem-help
    (lambda(app)
        (if (and (redundant (car app)) (null? (cadr app))) 
            (remove-applic-lambda-nil (caddar app))
            `(applic ,(remove-applic-lambda-nil (car app))
                     ,(remove-applic-lambda-nil (cadr app))))))
            

(define remove-applic-lambda-nil
    (lambda(taged)
        (search (lambda(y) (equal? (car y) 'applic)) 
                taged 
                (lambda(y)(rem-help (cdr y))))))
;;;;;;;;;redundant app;;;;;;;;;

;;;;;;;;;boxing var;;;;;;;;;
;;boxing;;
(define *box-exp* (list 'var 'set 'box-set 'box-get 'lambda-simple 'lambda-opt 'lambda-var))

(define minus
    (lambda(mylst conlst)
        (if (null? mylst) (list) 
            (if (contains (car mylst) conlst) 
                (minus (cdr mylst) conlst)
                (cons (car mylst) (minus (cdr mylst) conlst))))))
                
(define replace
    (lambda(body to-box)
        (search (lambda(y) (contains (car y) *box-exp*)) 
                body 
                (lambda(y)(cond ((equal? (car y) 'var)
                                    (let((to-replace (contains (cadr y) to-box)))
                                        (if to-replace `(box-get ,y) y)))
                                ((equal? (car y) 'set)
                                    (let((to-replace (contains (cadadr y) to-box)))
                                        (if to-replace 
                                            `(box-set ,(cadr y) ,(replace (caddr y) to-box)) `(set ,(cadr y) ,(replace (caddr y) to-box)))))
                                ((equal? (car y) 'box-get) y)
                                ((equal? (car y) 'lambda-simple) 
                                    `(lambda-simple ,(cadr y) 
                                        ,(replace (caddr y) (minus to-box (cadr y)))))
                                ((equal? (car y) 'lambda-opt)  
                                    `(lambda-opt ,(cadr y) ,(caddr y)
                                        ,(replace (cadddr y) 
                                        (minus to-box (append (cadr y)(list (caddr y)))))))
                                ((equal? (car y) 'lambda-var) 
                                    `(lambda-var ,(cadr y) 
                                        ,(replace (caddr y) (minus to-box (list (cadr y))))))
                                ((equal? (car y) 'box-set)
                                    `(box-set ,(cadr y) ,(replace (caddr y) to-box))))))))

(define sets
    (lambda(params)
        (map (lambda(p)`(set (var ,p) (box (var ,p)))) params)))
                                
(define replace-boxing
    (lambda(body to-box)
        (let((sets-exp (sets to-box)))
        (if (equal? (car body) 'seq) 
                    `(seq ,(append sets-exp (replace (cadr body) to-box)))
                    `(seq ,(append sets-exp (list (replace body to-box))))))))
;;boxing;;

;;find what to box;;
(define push
    (lambda(stack to-add)
        (append to-add stack)))
        
(define pop
    (lambda(stack n-of-e)
        (if (= n-of-e 0) (list) (cons (car stack) (pop (cdr stack) (- n-of-e 1)))))) 
        
(define cons-ps
    (lambda(parms)
        (myMap  
            (lambda(e)
                (let ((bound #f)(read-bound (box #f))(seted (box #f))(read (box #f))
                                                                    (seted-bound (box #f)))
                    (list e bound read-bound seted read seted-bound))) 
                    parms)))

(define to-bound
    (lambda(e) (append (list (car e) #t) (cddr e)))) 
            
(define update
    (lambda(stack)
        (myMap (lambda(p)(to-bound p)) stack)))
        
(define to-box
    (lambda(param)
        (or (and (unbox (car param)) (unbox (cadr param))) 
            (and (unbox (caddr param)) (unbox (cadddr param)))
            (and (unbox (car param)) (unbox (cadddr param))))))
        
(define what-to-box
    (lambda(params)
        (cond ((null? params) (list))
              ((to-box (cddar params)) (cons (caar params) (what-to-box (cdr params))))
              (else (what-to-box (cdr params))))))
            
(define handle-lambda-simple 
    (lambda(lmda stack)
        (let* ((next-stack (push (update stack) (cons-ps (car lmda))))
                (after-check (in-box-set (cadr lmda) next-stack))
                (to-box (what-to-box (pop next-stack (length (car lmda))))))
            (if (null? to-box) `(lambda-simple ,(car lmda) ,after-check)
                    `(lambda-simple ,(car lmda) ,(replace-boxing after-check to-box))))))

(define handle-lambda-opt 
    (lambda(lmda stack)
        (let* ((next-stack (push (update stack) 
                                (cons-ps (append (car lmda)(list (cadr lmda))))))
                (after-check (in-box-set (caddr lmda) next-stack))
                (to-box (what-to-box (pop next-stack (+ (length (car lmda)) 1)))))
            (if (null? to-box) `(lambda-opt ,(car lmda) ,(cadr lmda) ,after-check)
                    `(lambda-opt ,(car lmda) ,(cadr lmda) ,(replace-boxing after-check to-box))))))
                    
(define handle-lambda-var 
    (lambda(lmda stack)
        (let* ((next-stack (push (update stack) (cons-ps (list (car lmda)))))
                (after-check (in-box-set (cadr lmda) next-stack))
                (to-box (what-to-box (pop next-stack 1))))
            (if (null? to-box) `(lambda-var ,(car lmda) ,after-check)
                    `(lambda-var ,(car lmda) ,(replace-boxing after-check to-box))))))
                    
(define founded-ocu
    (lambda(var)
        (if (car var) (set-box! (cadr var) #t)  (set-box! (cadddr var) #t))))

(define founded-set-ocu
    (lambda(bound? var)
        (if bound? (set-box! (cadddr var) #t)  (set-box! (cadr var) #t))))
        
(define handle-var
    (lambda(var stack)
        (let((var-ocu (find-first var stack)))
            (if var-ocu (founded-ocu (cdr var-ocu)) (void))
            `(var ,var))))
            
(define handle-set
    (lambda(set stack)
        (let((var-ocu (find-first (cadar set) stack)))
            (if var-ocu (founded-set-ocu (cadr var-ocu) (cddr var-ocu)) (void))
            `(set ,(car set) ,(in-box-set (cadr set) stack)))))
            
(define *to-box-exp* (list 'lambda-simple 'lambda-opt 'lambda-var 'var 'set))
            
(define in-box-set
    (lambda(taged stack)
        (search (lambda(y) (contains (car y) *to-box-exp*))
                taged
                (lambda(y)(cond ((equal? (car y) 'var)
                                    (handle-var (cadr y) stack))
                                ((equal? (car y) 'lambda-simple) 
                                    (handle-lambda-simple (cdr y) stack))
                                ((equal? (car y) 'lambda-opt) 
                                    (handle-lambda-opt (cdr y) stack))
                                ((equal? (car y) 'lambda-var) 
                                    (handle-lambda-var (cdr y) stack))
                                ((equal? (car y) 'set) 
                                    (handle-set (cdr y) stack)))))))
                
(define box-set (lambda(taged)(in-box-set taged '())))
;;find what to box;;
;;;;;;;;;boxing var;;;;;;;;;

;;;FINISHED;;;
;;;;;;;;;lexical addressing;;;;;;;;;
(define search-var
    (lambda(var plist blist)
        (let* ((serp (find-first var plist))
               (serv (if serp #f (find-first var blist))))
            (cond (serp (cons 'pvar serp))
                  (serv (cons 'bvar serv))
                  (else (list 'fvar var))))))
      
(define myMap
    (lambda(func lst)
        (if (null? lst) lst
            (cons (func (car lst)) (myMap func (cdr lst))))))
      
(define create-body
    (lambda(old-body params old-params bounds)
      (let ((pcounter (box -1)))
        (in-pe->lex-pe 
                old-body 
                (myMap (lambda(var)
                        (set-box! pcounter (+ (unbox pcounter) 1))
                        (list var (unbox pcounter))) params)
                (append 
                 (myMap (lambda(p) (list (car p) 0 (cadr p))) old-params)
                 (myMap (lambda(b) (list (car b) (+ 1 (cadr b)) (caddr b))) bounds)
                  )))))

(define lex-help
    (lambda(tag taged plist blist)
        (cond ((equal? tag 'lambda-simple)
                `(lambda-simple ,(car taged) ,(create-body (cadr taged) (car taged) plist blist)))
              ((equal? tag 'lambda-var)
                `(lambda-var ,(car taged) ,(create-body (cadr taged) (list (car taged)) plist blist)))
              ((equal? tag 'lambda-opt)
                (let((params (append  (car taged) (list (cadr taged)))))
                `(lambda-opt ,(car taged) ,(cadr taged) ,(create-body (caddr taged) params plist blist)))))))
                  
(define pe->lex-pe
    (lambda(taged)
        (in-pe->lex-pe taged '() '())))
                                  
(define in-pe->lex-pe
    (lambda(taged plist blist)
        (search (lambda(y) (or (is-lambda (car y))(equal? (car y) 'var))) 
                taged 
                (lambda(y)(if (equal? (car y) 'var)
                              (search-var (cadr y) plist blist) 
                              (lex-help (car y) (cdr y) plist blist))))))
;;;;;;;;;lexical addressing;;;;;;;;;

;;;;;;;;;annotate call;;;;;;;;;
(define *tc-exp* (list 'applic 'lambda-simple 'lambda-opt 'lambda-var 'seq 'if3 'or 'def 'set 'box-set))

(define tail-last
    (lambda(lst tc?)
        (if (null? (cdr lst)) 
            (list (in-annotate-tc (car lst) tc?)) 
            (cons (in-annotate-tc (car lst) #f) (tail-last (cdr lst) tc?))))) 

(define cases
    (lambda(tag exp tc?)
        (cond ((equal? tag 'applic) 
                    (if tc? `(tc-applic ,(in-annotate-tc (car exp) #f) ,(in-annotate-tc (cadr exp) #f)) 
                            `(applic ,(in-annotate-tc (car exp) #f) ,(in-annotate-tc (cadr exp) #f))))
              ((equal? tag 'lambda-simple) `(lambda-simple ,(car exp) ,(in-annotate-tc (cadr exp) #t)))
              ((equal? tag 'lambda-opt) `(lambda-opt ,(car exp) ,(cadr exp) ,(in-annotate-tc (caddr exp) #t)))
              ((equal? tag 'lambda-var) `(lambda-var ,(car exp) ,(in-annotate-tc (cadr exp) #t)))
              ((equal? tag 'def) `(def ,(car exp) ,(in-annotate-tc (cadr exp) #f)))
              ((equal? tag 'seq) `(seq ,(tail-last (car exp) tc?)))
              ((equal? tag 'set) `(set ,(car exp) ,(in-annotate-tc (cadr exp) #f)))
              ((equal? tag 'box-set) `(box-set ,(car exp) ,(in-annotate-tc (cadr exp) #f)))
              ((equal? tag 'or) `(or ,(tail-last (car exp) tc?)))
              ((equal? tag 'if3) `(if3 ,(in-annotate-tc (car exp) #f) 
                                       ,(in-annotate-tc (cadr exp) tc?) 
                                       ,(in-annotate-tc (caddr exp) tc?)))
              )))

(define in-annotate-tc
    (lambda(taged tc?)
        (search (lambda(y) (contains (car y) *tc-exp*)) 
                taged 
                (lambda(y) (cases (car y) (cdr y) tc?)))))
    
(define annotate-tc
    (lambda(taged)
        (in-annotate-tc taged #f)))
;;;;;;;;;annotate call;;;;;;;;;


(define hw3
    (lambda(str)
        (map (lambda(taged) 
                (annotate-tc (pe->lex-pe (box-set (remove-applic-lambda-nil (eliminate-nested-defines taged))))))
             (hw2 str))))