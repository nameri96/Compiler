(load "hw3.scm")
(load "remww.scm")

;;;;;static;;;;;
(define output (box (string)))
(define closures 0)
(define major 0)
(define constTable (list));(val val)
(define constCode (box (string)))
(define global-env (list 
;;                          ;other
                         (cons 'eq? "ispointereq")
                         (cons 'procedure? "isprocedure")
                         (cons 'apply "apply")
;;                          ;boolean-done
                         (cons 'boolean? "boolean")
                         (cons 'not "not")
;;                          ;pair-done
                         (cons 'car "car")
                         (cons 'cdr "cdr")
                         (cons 'cons "cons")
                         (cons 'pair? "pair")
                         (cons 'set-car! "setcar")
                         (cons 'set-cdr! "setcdr")
;;                          ;char-done
                         (cons 'char->integer "chartointeger")
                         (cons 'char? "ischar")
;;                          ;numbers-done
                         (cons 'number? "isnumber")
;;                          ;rational-done
                         (cons 'numerator "numerator")
                         (cons 'denominator "denominator")
                         (cons 'rational? "isrational")
;;                          ;integer-done
                         (cons 'integer? "isinteger")
                         (cons 'integer->char "integertochar")
                         (cons 'zero? "isazero")
;;                          ;list-done
                         (cons 'null? "null")
                         (cons 'reverse "reverse")
                         (cons 'listtovector "listtovector")
;;                          ;string
                         (cons 'make-string "makeastring")
                         (cons 'string-length "stringlength")
                         (cons 'string-ref "stringref")
                         (cons 'string-set! "stringset")
;;                          (cons 'string->symbol "stringtosymbol")
                         (cons 'string? "isstring")
;;                          ;vector-done
                         (cons 'make-vector "makeavector")
                         (cons 'vector-length "vectorlength")
                         (cons 'vector-ref "vectorref")
                         (cons 'vector-set! "vectorset")
                         (cons 'vector? "isvector")
;;                          ;symbol-done
                         (cons 'symbol? "issymbol")
                         (cons 'symbol->string "symboltostring")
                         (cons 'getsymboltable "getsymboltable")
                         (cons 'makesymbolfromstring "makesymbolfromstring")
;;                          ;arithmetic
                         (cons 'binaryplus "binaryplus")
                         (cons 'binarymul "binarymul")
                         (cons 'binaryminus "binaryminus")
                         (cons 'binarydiv "binarydiv")
                         (cons 'remainder "remainder");done
                         (cons 'binarylt "binarylt")
                         (cons 'binaryeq "binaryeq")
                         (cons 'binarygt "binarygt")
                         (cons 'integerminus "integerminus")
                            ))
(define global-code (box (string)))
;;;;;static;;;;;
;;;;;labels;;;;;
(define error-label "L_error")
(define or-label "L_or_exit")
(define if-exit-label "L_if_exit")
(define dif-label "L_dif")
(define clos-body-label (lambda(n) (string-append "L_clos_body_" (number->string n))))
(define clos-exit-label (lambda(n) (string-append "L_clos_exit_" (number->string n))))
(define inc-closures (lambda()(set! closures (+ closures 1))))
(define inc-major (lambda()(set! major (+ major 1))))
(define dec-major (lambda()(set! major (- major 1))))
;;;;;labels;;;;;
;;;;;util;;;;;
(define add-to-code-start 
    (lambda(code-lines boxed-code) (set-box! boxed-code (string-append code-lines (unbox boxed-code))))) 
(define add (lambda(code-line) (set-box! output (string-append (unbox output) "    " code-line "\n")))) 

(define get-label-code 
    (lambda(label to-jump) 
        (if to-jump 
            (add (string-append to-jump "(" label ");"))
            (add (string-append label ":")))))
                        
(define begin-local
    (lambda labels
        (add (string-append "BEGIN_LOCAL_LABELS " 
                            (car labels) 
                            (list->string 
                                (apply append (myMap (lambda(lbl)(string->list (string-append ", " lbl))) (cdr labels))))
                            ";"))))
                            
(define end-local (lambda() (add "END_LOCAL_LABELS;")))
;;;;;util;;;;;
        
;;;;handle const table and its code;;;;;;;;;;;
(define add-to-constCode 
    (lambda(code-line) (set-box! constCode (string-append (unbox constCode) "    " code-line "\n")))) 

(define push-set-elements
    (lambda(the-set length-func ref-func index)
        (if (= index (length-func the-set)) (void)
            (begin  (add-to-constCode 
                        (string-append "PUSH(INDD(R15," (search-const (ref-func the-set index) constTable 0) "));"))
                    (push-set-elements the-set length-func ref-func (+ index 1))))))
        
(define add-const;TODO: add symbol SOB
    (lambda(const str-index)
         (set! constTable (append constTable (list const)))
         (cond ((null? const)
                (begin  (add-to-constCode "CALL(MAKE_SOB_NIL);")
                        (add-to-constCode (string-append "MOV(INDD(R15," str-index "),R0);"))))
               ((eq? (void) const)
                (begin  (add-to-constCode "CALL(MAKE_SOB_VOID);")
                        (add-to-constCode (string-append "MOV(INDD(R15," str-index "),R0);"))))
               ((vector? const) 
                (begin  (push-set-elements const vector-length vector-ref 0)
                        (add-to-constCode (string-append "PUSH(IMM(" (number->string (vector-length const)) "));"))
                        (add-to-constCode "CALL(MAKE_SOB_VECTOR);")
                        (add-to-constCode "POP(R1);")
                        (add-to-constCode "DROP(R1);")
                        (add-to-constCode (string-append "MOV(INDD(R15," str-index "),R0);"))))
               ((boolean? const)
                (begin  (add-to-constCode (string-append "PUSH(IMM(" (number->string (if const 1 0)) "));"))
                        (add-to-constCode "CALL(MAKE_SOB_BOOL);")
                        (add-to-constCode "DROP(1);")
                        (add-to-constCode (string-append "MOV(INDD(R15," str-index "),R0);"))))
               ((char? const)
                (begin  (add-to-constCode (string-append "PUSH(IMM(" (number->string (char->integer const)) "));"))
                        (add-to-constCode "CALL(MAKE_SOB_CHAR);")
                        (add-to-constCode "DROP(1);")
                        (add-to-constCode (string-append "MOV(INDD(R15," str-index "),R0);"))))
               ((integer? const)
                (begin  (add-to-constCode (string-append "PUSH(IMM(" (number->string const) "));"))
                        (add-to-constCode "CALL(MAKE_SOB_INTEGER);")
                        (add-to-constCode "DROP(1);")
                        (add-to-constCode (string-append "MOV(INDD(R15," str-index "),R0);"))))
               ((string? const) 
                (begin  (push-set-elements const string-length string-ref 0)
                        (add-to-constCode (string-append "PUSH(IMM(" (number->string (string-length const)) "));"))
                        (add-to-constCode "CALL(MAKE_SOB_STRING);")
                        (add-to-constCode "POP(R1);")
                        (add-to-constCode "DROP(R1);")
                        (add-to-constCode (string-append "MOV(INDD(R15," str-index "),R0);"))))
               ((pair? const) 
                (begin  (add-to-constCode 
                            (string-append "PUSH(INDD(R15," (search-const (cdr const) constTable 0) "));"))
                        (add-to-constCode 
                            (string-append "PUSH(INDD(R15," (search-const (car const) constTable 0) "));"))
                        (add-to-constCode "CALL(MAKE_SOB_PAIR);")
                        (add-to-constCode  "DROP(2);")
                        (add-to-constCode (string-append "MOV(INDD(R15," str-index "),R0);"))))
               ((rational? const) 
                (begin  (add-to-constCode 
                            (string-append "PUSH(INDD(R15," (search-const (denominator const) constTable 0) "));"))
                        (add-to-constCode 
                            (string-append "PUSH(INDD(R15," (search-const (numerator const) constTable 0) "));"))
                        (add-to-constCode "CALL(MAKE_SOB_RATIONAL);")
                        (add-to-constCode  "DROP(2);")
                        (add-to-constCode (string-append "MOV(INDD(R15," str-index "),R0);"))))
               ((symbol? const)
                (begin  (let((index (search-const (symbol->string const) constTable 0)))
                        (add-to-constCode (string-append "PUSH(INDD(R15," index "));"))
                        (add-to-constCode "CALL(MAKE_SOB_SYMBOL);")
                        (add-to-constCode "DROP(1);")
                        (add-to-constCode (string-append "MOV(INDD(R15," str-index "),R0);"))
                        (add-to-constCode (string-append "PUSH(R13);"))
                        (add-to-constCode (string-append "PUSH(INDD(R15," str-index "));"))
                        (add-to-constCode "CALL(MAKE_SOB_PAIR);")
                        (add-to-constCode "DROP(2);")
                        (add-to-constCode "MOV(R13,R0);")))))
         str-index))
        
(define search-const
    (lambda(const consts index)
        (if (null? consts) (add-const const (number->string index))
            (if (equal? const (car consts)) (number->string index)
                (search-const const (cdr consts) (+ index 1))))))
        
(define write-The-ConstTable
    (lambda()
        (add-to-code-start 
            (string-append "    //start create the const table into R15\n"
                           "    PUSH(IMM(" (number->string (length constTable)) "));\n" 
                           "    CALL(MALLOC);\n" 
                           "    DROP(1);\n"
                           "    MOV(R15,R0);\n"
                           "    CALL(MAKE_SOB_NIL);\n"
                           "    MOV(R13,R0);\n")
            constCode)
        (add-to-constCode "//finished create the const table into R15")))
;;;;handle const table and its code;;;;;;;;;;;        

;;;;handle global table and its code;;;;;;;;;;;
(define add-to-globalCode 
    (lambda(code-line) (set-box! global-code (string-append (unbox global-code) "    " code-line "\n")))) 
    
(define is-equal?
    (lambda(fvar glob-var)
        (if (pair? glob-var) (equal? fvar (car glob-var)) (equal? fvar glob-var))))  
    
(define search-global
    (lambda(fvar globals index)
        (if (null? globals) (void)
            (if (is-equal? fvar (car globals)) (number->string index)
                (search-global fvar (cdr globals) (+ index 1))))))

(define add-to-global-env (lambda(fvar) (set! global-env (append global-env (list fvar)))))

(define search-in-global
    (lambda(fvar globals index)
        (if (null? globals) (begin (add-to-global-env fvar) (number->string index))
            (if (is-equal? fvar (car globals)) (number->string index)
                (search-in-global fvar (cdr globals) (+ index 1))))))
                
(define add-to-global (lambda(fvar) (set! global-env (union global-env (list (cadr fvar))))))

(define set-up-global-env
    (lambda(taged) (map (lambda(prog)(if (equal? (car prog) 'def) (add-to-global (cadr prog)))) taged)))

(define allocate-funcs
    (lambda(globals index)
        (if (null? globals) (void)
            (begin
            (if (pair? (car globals))
                (begin
                (add-to-globalCode (string-append "PUSH(LABEL(" (cdar globals) "));"))
                (add-to-globalCode "PUSH(R9);")
                (add-to-globalCode "CALL(MAKE_SOB_CLOSURE);")
                (add-to-globalCode "DROP(2);"))
                (add-to-globalCode "MOV(R0,R9);"))
            (add-to-globalCode (string-append "MOV(INDD(R14," (number->string index) "),R0);"))
            (allocate-funcs (cdr globals) (+ index 1))))))
            
            
    
(define write-the-global-env
    (lambda()
       (if(null? global-env) (void)
        (begin
        (add-to-globalCode "//start create the global env into R14")
        (add-to-globalCode (string-append "PUSH(IMM(" (number->string (length global-env)) "));")) 
        (add-to-globalCode "CALL(MALLOC);") 
        (add-to-globalCode "DROP(1);")
        (add-to-globalCode "MOV(R14,R0);")
        (add-to-globalCode "CALL(MAKE_SOB_NIL);")
        (add-to-globalCode "MOV(R9,R0);")
        (allocate-funcs global-env 0)
        (add-to-globalCode "//finished create the global env into R14")))))       
;;;;handle global table and its code;;;;;;;;;;;

;;;;handle lambda exp;;;;;;;
(define extended-env-to-R2;do first
    (lambda(arg-count)
      (if (> major 0)
        (begin
        (add "//start extend the env") 
        ;allocate new env into R2
        (add "//allocate place for the new env")
        (add (string-append "PUSH(IMM(" (number->string major) "));"))
        (add "CALL(MALLOC);") 
        (add "POP(R7);")
        (add "MOV(R2,R0);");R2=new env ,|new env| = |old env|+1
        
        (if (> major 1)
            (begin
            ;copy the old env to the new one
            (add "//copy the old env to new env")
            (add "MOV(R1,FPARG(0));");R1=old env
            (begin-local "copy_loop" "add_loop")
            (add "MOV(R3,0);")
            (add "MOV(R4,1);")
            (get-label-code "copy_loop" #f);while(R4<major)
            (add "MOV(INDD(R2,R4),INDD(R1,R3));");new env[i+1] = old env[i]
            (add "INCR(R3);")
            (add "INCR(R4);")
            (add "CMP(R4,R7);")
            (get-label-code "copy_loop" "JUMP_LT")
            (end-local)))
        
            ;copy the params to the new extended env
            (add "//add the params to new env")
            (begin-local "add_loop" "noparams" "enterparamsend")
            (add "MOV(R5,FPARG(1));");R5 = |args|
            (add "CMP(R5,IMM(0));")
            (get-label-code "noparams" "JUMP_EQ")
            (add "PUSH(R5);")
            (add "CALL(MALLOC);")
            (add "DROP(1)")
            (add "MOV(INDD(R2,0),R0);")
            (add "MOV(R3,INDD(R2,0));")
            (add "MOV(R4,0);")
            (add "MOV(R6,2);")
            (get-label-code "add_loop" #f);while(R4<|args|)
            (add "MOV(INDD(R3,R4),FPARG(R6));");new env[0][i] = args[i]
            (add "INCR(R4);")
            (add "INCR(R6);")
            (add "CMP(R4,R5);")
            (get-label-code "add_loop" "JUMP_LT")
            (get-label-code "enterparamsend" "JUMP")
            (get-label-code "noparams" #f)
            (add "//create empty paramlist")
            (add "PUSH(IMM(1));")
            (add "CALL(MALLOC);")
            (add "DROP(1)")
            (add "MOV(INDD(R2,0),R0);")
            (add "MOV(R3,INDD(R2,0));")
            (add "CALL(MAKE_SOB_VOID);")
            (add "MOV(INDD(R3,0),R0);")
            (get-label-code "enterparamsend" #f)
            (add "NOP;")
            (end-local)
            (add "//done create empty paramlist")
        (add "//done extend the env"))
        (begin
        (add "//create empty env")
        (add "CALL(MAKE_SOB_NIL);") 
        (add "MOV(R2,R0);")
        (add "//done create empty env")))))
        
(define create-var-list
    (lambda();R3 = the element to add added, R1 previus pair,R6 base
         (add "//create the variadian param")
         (begin-local "list_create" "list_end")
         (add "CMP(FPARG(1),IMM(0));")
         (get-label-code "list_end" "JUMP_EQ")
         (get-label-code "list_create" #f);while(R3>=0)(cons R3 R1)
         (add "PUSH(R1);")
         (add "PUSH(FPARG(R3+2));")
         (add "CALL(MAKE_SOB_PAIR);")
         (add "DROP(2);")
         (add "MOV(R1,R0);")
         (add "DECR(R3);")
         (add "CMP(R3,R6);")
         (get-label-code "list_create" "JUMP_GE")
         (get-label-code "list_end" #f)
         (add "NOP;")
         (end-local)
         (add "//done creatting the variadian param")))
        
(define fix-the-stack ;do third
    (lambda(type args-count)
        (cond ((equal? type 'lambda-simple)
                    ;copy the old env to the new one
                    (add "//start fixing the stack of simple lambda")
                    (begin-local "copy_loop")
                    (add "MOV(R3,FPARG(1));")
                    (add "ADD(R3,IMM(2));")
                    (add "MOV(R4,R3);")
                    (add "SUB(R4,IMM(1));")
                    ;R3 = last arg position R4 start at the added nil at the end 
                    (add "//copy all elements in stack one step down")
                    (get-label-code "copy_loop" #f);while(R3>-2)
                    (add "MOV(FPARG(R3),FPARG(R4));")
                    (add "DECR(R3);")
                    (add "DECR(R4);")
                    (add "CMP(R3,IMM(-2));")
                    (get-label-code "copy_loop" "JUMP_GT")
                    (add "DROP(1);")
                    (add "DECR(FP);")
                    (end-local)
                    (add "//done fixing the stack of simple lambda"))
              ((equal? type 'lambda-var)
                    (add "//start fixing the stack of variadian lambda")
                    (add "CALL(MAKE_SOB_NIL);")
                    (add "MOV(R1,R0);")
                    (add "MOV(R3,FPARG(1)-1);")
                    (add "MOV(R5,FPARG(1));")
                    (add "MOV(R6,IMM(0));")
                    (add "MOV(R8,FPARG(1));")
                    ;R5 = last arg position R3 = the element to add added R1 previus pair  
                    (create-var-list)
                    (add "MOV(FPARG(2+R5),R1);")
                    (add "DECR(R5);")
                    (add "MOV(R4,IMM(1));")
                    (begin-local "copy_loop")
                    ;R5 copy to, R4 copy from
                    (get-label-code "copy_loop" #f);while(R4>-3)
                    (add "MOV(FPARG(R5+2),FPARG(R4));")
                    (add "DECR(R4);")
                    (add "DECR(R5);")
                    (add "CMP(R4,IMM(-3));")
                    (add "//mov the stack down by the params count")
                    (get-label-code "copy_loop" "JUMP_GT")
                    (add "//fixing the sp and fp simple var")
                    (add "DROP(R8);")
                    (add "SUB(FP,R8);")
                    (add "MOV(FPARG(1),IMM(1));")
                    (end-local)
                    (add "//done fixing the stack of variadian lambda"))
              ((equal? type 'lambda-opt)
                    (add "//start fixing the stack of optional lambda")
                    (add "CALL(MAKE_SOB_NIL);")
                    (add "MOV(R1,R0);")
                    (add "MOV(R3,FPARG(1)-1);")
                    (add "MOV(R5,FPARG(1));")
                    (add (string-append "MOV(R6,IMM(" (number->string (- args-count 1)) "));"))
                    (add "MOV(R8,FPARG(1));")
                    ;R5 = last arg position R3 = the element to add added R1 previus pair  
                    (create-var-list)
                    (add "MOV(FPARG(2+R5),R1);")
                    ;todo fix the stack and the opt lambda
                    (add "DECR(R5)")
                    (add (string-append "MOV(R4,IMM(1+" (number->string (- args-count 1)) "))"))
                    (begin-local "copy_loop")
                    ;R5 copy to, R4 copy from
                    (get-label-code "copy_loop" #f);while(R4>-3)
                    (add "MOV(FPARG(R5+2),FPARG(R4));")
                    (add "DECR(R4);")
                    (add "DECR(R5);")
                    (add "CMP(R4,IMM(-3));")
                    (get-label-code "copy_loop" "JUMP_GT")
                    ;fix fp and n
                    (add (string-append "DROP(R8-" (number->string (- args-count 1)) ")"))
                    (add (string-append "SUB(FP,R8-" (number->string (- args-count 1)) ")"))
                    (add (string-append "MOV(FPARG(1),IMM(" (number->string args-count) "))"))
                    (end-local)
                    (add "//done fixing the stack of optional lambda")))))
        
(define create-closure-and-make-body;do second
    (lambda(arg-count body type)
        (add "//start create the closure and its body")
        (extended-env-to-R2 arg-count)
        (let ((my-clos closures))
        ;create the closure
        (add "//create the closure")
        (add (string-append "PUSH(LABEL(" (clos-body-label my-clos) "));"))
        (add "PUSH(R2);")
        (add "CALL(MAKE_SOB_CLOSURE);")
        (add "DROP(2);")
        ;jump to the end when the closure is in R0
        (get-label-code (clos-exit-label my-clos) "JUMP")
        ;define the body of the procedure
        (add "//create the body")
        (get-label-code (clos-body-label my-clos) #f);body start
        (add "PUSH(FP);")
        (add "MOV(FP,SP);")
        ;fix the stack
        (fix-the-stack type arg-count)
        (add (string-append "CMP(FPARG(1),IMM(" (number->string arg-count) "));"))
        (get-label-code error-label "JUMP_NE")
        (inc-major)
        (inc-closures)
        (in-code-gen body)
        (dec-major)
        (add "POP(FP);")
        (add "RETURN;")
        (get-label-code (clos-exit-label my-clos) #f))
        (add "//done create the closure and the body")))
;;;;handle lambda exp;;;;;;;

;;;;getVeriableAddress;;;;;;
(define SetVar;new val at R1
    (lambda(var)
      (let((type (car var))(value (cadr var)) (extraData (cddr var)))
        (cond ((equal? type 'fvar)
                    (add "//set free var")
                    (add (string-append "MOV(INDD(R14," (search-in-global value global-env 0) "),R1);"))
                    (add "//done setting free var"))
              ((equal? type 'pvar)
                    (add "//set param var address")
                    (add (string-append "MOV(FPARG(" (number->string (+ 2 (car extraData))) "),R1);"))
                    (add "//done setting param var"))
              ((equal? type 'bvar)
                    (add "//set bound var")
                    (add "MOV(R0,FPARG(0));")
                    (add (string-append "MOV(R0,INDD(R0," (number->string (car extraData)) "));"))
                    (add (string-append "MOV(INDD(R0," (number->string (cadr extraData)) "),R1);"))
                    (add "//done setting bound var"))))))

;;;;getVeriableAddress;;;;;;
(define in-code-gen-const
    (pr `(const ,(? 'constant)) 
        (lambda(constant) 
            (let((index (search-const constant constTable 0)))
            (add "//get constant")
            (add (string-append "MOV(R0,INDD(R15," index "));"))
            (add "//done getting constant")))))
      
(define in-code-gen-fvar
    (pr `(fvar ,(? 'fvar)) 
        (lambda(var)
            (add "//get free var val")
            (add (string-append "MOV(R0,INDD(R14," (search-in-global var global-env 0) "));"))
            (add "//done getting free var val")))) 

(define in-code-gen-pvar
    (pr `(pvar ,(? 'pvar) ,(? 'minor)) 
        (lambda(var minor)
            (add "//get param var val")
            (add (string-append "MOV(R0,FPARG(" (number->string (+ minor 2)) "));"))
            (add "//done getting param var val"))))
    
(define in-code-gen-bvar
    (pr `(bvar ,(? 'var) ,(? 'major) ,(? 'minor)) 
        (lambda(var major minor)
            (add "//get bound var val")
            (add "MOV(R0,FPARG(0));")
            (add (string-append "MOV(R0,INDD(R0," (number->string major) "));"))
            (add (string-append "MOV(R0,INDD(R0," (number->string minor) "));"))
            (add "//done getting bound var val"))))
    
(define in-code-gen-or
    (pr `(or ,(? 'or-exps list?)) 
        (lambda(exps) (begin-local or-label)
                      (myMap (lambda(exp) (in-code-gen exp)
                                          (add "CMP(INDD(R0,0),IMM(T_BOOL));")
                                          (get-label-code or-label "JUMP_NE")
                                          (add "CMP(INDD(R0,1),IMM(1));")
                                          (get-label-code or-label "JUMP_EQ")) 
                            exps)
                      (get-label-code or-label #f)
                      (add "NOP;")
                      (end-local))))

(define in-code-gen-if
    (pr `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
        (lambda(test dit dif) (add "//begin if")
                              (begin-local if-exit-label dif-label)
                              (add "//the test")
                              (in-code-gen test)
                              (add "CMP(INDD(R0,0),IMM(T_BOOL));")
                              (get-label-code dif-label "JUMP_NE")
                              (add "CMP(INDD(R0,1),IMM(1));")
                              (get-label-code dif-label "JUMP_EQ")
                              (add "//do if false")
                              (in-code-gen dif)
                              (get-label-code if-exit-label "JUMP") 
                              (get-label-code dif-label #f)
                              (add "//do if true")
                              (in-code-gen dit)
                              (get-label-code if-exit-label #f)
                              (add "NOP;")
                              (end-local)
                              (add "//end if"))))

(define in-code-gen-lambda-simple
    (pr `(lambda-simple ,(? 'args list?) ,(? 'body))
        (lambda(args body)
            (create-closure-and-make-body (length args) body 'lambda-simple))))
                
(define in-code-gen-lambda-var
    (pr `(lambda-var ,(? 'arglst) ,(? 'body))
        (lambda(arglst body) (create-closure-and-make-body 1 body 'lambda-var))))

(define in-code-gen-lambda-opt
    (pr `(lambda-opt ,(? 'args list?) ,(? 'arg) ,(? 'body))
        (lambda(args arg body) (create-closure-and-make-body (+ (length args) 1) body 'lambda-opt))))
                
(define in-code-gen-def
    (pr `(def ,(? 'var) ,(? 'body))  
            (lambda(var body) (add "//start define")
                              (in-code-gen body)
                              (add "MOV(R1,R0);")
                              (SetVar var)
                              (add "CALL(MAKE_SOB_VOID);")
                              (add "//done define"))))
                    
(define in-code-gen-set
    (pr `(set ,(? 'var) ,(? 'expr)) 
        (lambda(var expr) (add "//setting var")
                          (in-code-gen expr)
                          (add "MOV(R1,R0);")
                          (SetVar var)
                          (add "CALL(MAKE_SOB_VOID);")
                          (add "//done setting var"))))
  
(define in-code-gen-box
    (pr `(box ,(? 'var))
        (lambda(var) (add "//start boxing")
                     (in-code-gen var)
                     (add "MOV(R1,R0);")
                     (add "PUSH(IMM(1));")
                     (add "CALL(MALLOC);")
                     (add "DROP(1);")
                     (add "MOV(IND(R0),R1);")
                     (add "//done boxing with R0 the pointer to the box "))))
        
(define in-code-gen-boxset
    (pr `(box-set ,(? 'var) ,(? 'expr)) 
        (lambda(var expr) (add "//setting box")
                          (in-code-gen expr)
                          (add "MOV(R1,R0);")
                          (in-code-gen var)
                          (add "MOV(IND(R0),R1);")
                          (add "CALL(MAKE_SOB_VOID);")
                          (add "//done setting box"))))
        
(define in-code-gen-boxget
    (pr `(box-get ,(? 'var)) 
        (lambda(var) (add "//getting box inner val")
                     (in-code-gen var)
                     (add "MOV(R0,IND(R0));")
                     (add "//done getting box inner val"))))
                    
(define in-code-gen-applic
    (pr `(applic ,(? 'proc) ,(? 'params list?)) 
        (lambda(proc params) 
            (add "//start applic") 
            (add "CALL(MAKE_SOB_NIL);")
            (add "PUSH(R0);")
            (myMap (lambda(p) (in-code-gen p) (add "PUSH(R0);")) (reverse params))
            (add (string-append "PUSH(" (number->string (length params)) ");"))
            (in-code-gen proc)
            (add "MOV(R1,R0);")
            (add "CMP(INDD(R1,0),IMM(T_CLOSURE));")
            (add "JUMP_NE(L_error);")
            (add "PUSH(INDD(R1,1));")
            (add "CALLA(INDD(R1,2));")
            (add "POP(R1);")
            (add "POP(R1);")
            (add "DROP(R1);")
            (add "//done applic"))))
            
;; (define in-code-gen-tc-applic
;;     (pr `(tc-applic ,(? 'proc) ,(? 'params list?)) 
;;         (lambda(proc params) 
;;             (add "//start applic") 
;;             (add "CALL(MAKE_SOB_NIL);")
;;             (add "PUSH(R0);")
;;             (myMap (lambda(p) (in-code-gen p) (add "PUSH(R0);")) (reverse params))
;;             (add (string-append "PUSH(" (number->string (length params)) ");"))
;;             (in-code-gen proc)
;;             (add "MOV(R1,R0);")
;;             (add "CMP(INDD(R1,0),IMM(T_CLOSURE));")
;;             (add "JUMP_NE(L_error);")
;;             (add "PUSH(INDD(R1,1));")
;;             (add "CALLA(INDD(R1,2));")
;;             (add "POP(R1);")
;;             (add "POP(R1);")
;;             (add "DROP(R1);")
;;             (add "//done applic"))))
            
(define in-code-gen-tc-applic
    (pr `(tc-applic ,(? 'proc) ,(? 'params list?)) 
        (lambda(proc params) 
            (add "//start tc-applic")
            (add "//set the stack and call the tc-applic given func")
            (add "PUSH(IMM(0));")
            (myMap (lambda(p) (in-code-gen p) (add "PUSH(R0);")) (reverse params))
            (add (string-append "PUSH(" (number->string (length params)) ");"))
            (in-code-gen proc)
            (add "MOV(R1,R0);")
            (add "CMP(INDD(R1,0),IMM(T_CLOSURE));")
            (add "JUMP_NE(L_error);")
            (add "PUSH(INDD(R1,1));")
            (add "PUSH(FPARG(-1));")
            (fix-tc-applic-stack)
            (add "JUMPA(INDD(R1,2));")
            (add "//done tc-applic"))))

(define fix-tc-applic-stack
    (lambda()
            (add "//start change the stack in tc-applic")
            (add "MOV(R2,FPARG(-2));")
            (add "MOV(R4,FP);")
            (add "SUB(R4,SP);")
            (add "SUB(R4,IMM(2));")
            (add "MOV(R3,FPARG(1));")
            (add "ADD(R3,IMM(4));")
            (add "MOV(SP,FP)")
            (add "SUB(SP,R3)")
            ;loop until R4 = SP
            (begin-local "tcapplicstackfixingloop" "tcapplicstackfixingend")
            (add "MOV(R5,IMM(-3));")
            (get-label-code "tcapplicstackfixingloop" #f)
            (add "CMP(R5,R4);")
            (get-label-code "tcapplicstackfixingend" "JUMP_LT")
            (add "PUSH(FPARG(R5));")
            (add "DECR(R5);")
            (get-label-code "tcapplicstackfixingloop" "JUMP")
            (get-label-code "tcapplicstackfixingend" #f)
            (add "NOP;")
            (end-local)
            (add "MOV(FP,R2);")
            (add "//done change the stack in tc-applic")))
            
(define in-code-gen-seq (pr `(seq ,(? 'exprs list?)) 
        (lambda(exprs)
                (add "//start seq")
                (myMap (lambda(expr)(add "//seq element start")(in-code-gen expr)(add "//seq element end")) exprs)
                (add "//done seq"))))

(define in-code-gen-program 
    (pr `(program ,(? 'exprs list?)) 
        (lambda(exprs) 
            (set-up-global-env exprs)
            (write-the-global-env)
            (myMap (lambda(expr) 
                        (in-code-gen expr)
                        (begin-local "next")
                        (add "CMP(INDD(R0,0),IMM(T_VOID));")
                        (get-label-code "next" "JUMP_EQ")
                        (add "PUSH(R0);")
                        (add "CALL(WRITE_SOB);")
                        (add "POP(R0);")
                        (add "CALL(NEWLINE);")
                        (get-label-code "next" #f)
                        (add "NOP")
                        (end-local))
                        exprs)
            (write-The-ConstTable)
            (add-to-code-start 
                (string-append  "#include <stdio.h>\n"
                                "#include <stdlib.h>\n"
                                "#define DO_SHOW 1\n"
                                "#include \"arch/cisc.h\"\n"
                                "int main()\n"
                                "{\n"
                                "    START_MACHINE;\n"
                                "    JUMP(CONTINUE);\n"
                                "    #include \"arch/char.lib\"\n"
                                "    #include \"arch/io.lib\"\n"
                                "    #include \"arch/math.lib\"\n"
                                "    #include \"arch/string.lib\"\n"
                                "    #include \"arch/system.lib\"\n"
                                "    #include \"arch/scheme.lib\"\n"
                                "    #include \"arch/my.lib\"\n"
                                "    CONTINUE:\n"
                                (unbox global-code)
                                (unbox constCode)) 
                output)
            (add "JUMP(L_no_error);")
            (get-label-code error-label #f)
            (add "SHOW(\"error: \",INDD(R0,0));")
            (add "L_no_error:")
            (add "STOP_MACHINE;\n")
            (add "return 0;\n}\n"))))

(define dis (lambda(to-display) (display to-display)(newline)))
        
(define in-code-gen
    (let ((run
            (compose-patterns
                in-code-gen-program
                in-code-gen-const
                in-code-gen-fvar
                in-code-gen-pvar
                in-code-gen-bvar
                in-code-gen-or
                in-code-gen-if
                in-code-gen-lambda-simple
                in-code-gen-lambda-var
                in-code-gen-lambda-opt
                in-code-gen-def
                in-code-gen-set
                in-code-gen-box
                in-code-gen-boxset
                in-code-gen-boxget
                in-code-gen-applic
                in-code-gen-tc-applic
                in-code-gen-seq)))
        (lambda(after-stage-three)
            ;(dis after-stage-three)
            (run after-stage-three (lambda() (error 'code-gen "L_error"))))))
            
(define code-gen (lambda(taged) (in-code-gen taged)))
            
(define file->string
    (lambda (in-file)
        (let ((in-port (open-input-file in-file)))
            (letrec ((run
                    (lambda ()
                        (let ((ch (read-char in-port)))
                            (if (eof-object? ch)
                                (begin (close-input-port in-port) '())
                                (cons ch (run)))))))
        (list->string (run))))))
        
(define string->file
    (lambda (out-file to-write)
        (let ((out-port (open-output-file out-file 'replace)))
            (display (string->symbol (unbox output)) out-port)
            (close-output-port out-port))))

(define compile-scheme-file
    (lambda(in out)
        (string->file out (code-gen (cons 'program (list (hw3 (string-append (file->string "my.scm")(file->string in)))))))))