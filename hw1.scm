(load "pc.scm")

(define delayedSexpr (*delayed (lambda () <sexpr>)))
(define delayedInfix (*delayed (lambda()<InfixExpression>)))

;;;;;;;;comment section;;;;;;;;;
(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
	 (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done)))
    (new (*parser (char #\;))
	 
	 (*parser <any-char>)
	 (*parser <end-of-line-comment>)
	 *diff *star

	 (*parser <end-of-line-comment>)
	 (*caten 3)
	 done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       delayedSexpr
       (*caten 2)
       done))
       
(define <infix-comment>
  (new (*parser (word "#;"))
       delayedInfix
       (*caten 2)
       done))

(define <comment>
  (disj <line-comment>
	<sexpr-comment>))
	
(define <incomment>
  (disj <line-comment>
	<infix-comment>))

(define <skip>
  (disj <comment>
	<whitespace>))
	
(define <inskip>
  (disj <incomment>
	<whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (_left e _right) e))
	   done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

(define ^<inskipped*> (^^<wrapped> (star <inskip>)))
;;;;;;end of comment section;;;;;;;

;;;;;;;;numbers section;;;;;;;;;
(define <digit-0-9>
    (range #\0 #\9))

(define <digit-1-9>
    (range #\1 #\9))
    
(define <Natrual>
    (new (*parser (char #\0))
         (*pack (lambda(_) 0))
         (*parser <digit-1-9>)
         (*parser <digit-0-9>) *star
         (*caten 2)
         (*pack-with
            (lambda (a s)
                (string->number
                    (list->string
                        `(,a ,@s)))))
         (*disj 2)
         done))
         
(define <Integer>
    (new (*parser (char #\+))
         (*parser <Natrual>)
         (*caten 2)
         (*pack-with
            (lambda (++ n) n))
         (*parser (char #\-))
         (*parser <Natrual>)
         (*caten 2)
         (*pack-with
            (lambda (-- n)(- n)))
            
         (*parser <Natrual>)
         
         (*disj 3)
         done))
         
(define <Fraction>
    (new (*parser <Integer>)
         (*parser (char #\/))
         (*parser <Natrual>)
         (*guard (lambda(n) (not (zero? n))))
         (*caten 3)
         (*pack-with
            (lambda (int div nat)(/ int nat)))
         done))
 
(define <Number>
    (disj <Fraction> <Integer>))
;;;;;;;;end of numbers section;;;;;;;;;
    
(define ^<MetaChar>
  (lambda (str ch)
    (new (*parser (word-ci str))
	 (*pack (lambda (_) ch))
	 done)))

(define <StringVisibleChar> <any-char>)
	 
(define <HexDigit>
  (let ((zero (char->integer #\0))
	(lc-a (char->integer #\a))
	(uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
	 (*pack
	  (lambda (ch)
	    (- (char->integer ch) zero)))

	 (*parser (range #\a #\f))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) lc-a))))

	 (*parser (range #\A #\F))
	 (*pack
	  (lambda (ch)
	    (+ 10 (- (char->integer ch) uc-a))))

	 (*disj 3)
	 done)))
	 
(define <XX>
  (new (*parser <HexDigit>)
       (*parser <HexDigit>)
       (*caten 2)
       (*pack-with
	(lambda (h l)
	  (+ l (* h 16))))
       done))

(define <XXXX>
  (new (*parser <XX>)
       (*parser <XX>)
       (*caten 2)
       (*pack-with
	(lambda (h l)
	  (+ l (* 256 h))))
       done))

(define <HexChar>
  (new (*parser <XXXX>)
       (*parser <XX>)
       (*disj 2)
       done))
	 
(define <StringHexChar> 
    (new (*parser (word-ci "\\x"))
         (*parser <HexChar>)
         (*parser (char #\;) )
         (*caten 3)
	 (*pack-with
		(lambda(pre num end) (integer->char num)))
         done))
 
(define <StringMetaChar>
    (disj (^<MetaChar> "\\\\" #\\) (^<MetaChar> "\\\"" #\")
          (^<MetaChar> "\\n" #\newline) (^<MetaChar> "\\r" #\return)
          (^<MetaChar> "\\t" #\tab) (^<MetaChar> "\\f" #\page)))
 
(define <StringChar>
    (new (*parser <StringMetaChar>)
         (*parser <StringHexChar>)
         (*parser <StringVisibleChar>)
         (*parser (char #\"))
         (*parser (char #\\))
         (*disj 2)
         *diff
         (*disj 3)
         done))
  
(define <String>
    (new (*parser (char #\"))
         (*parser <StringChar>)*star
         (*parser (char #\"))
         (*caten 3)
         (*pack-with
            (lambda (od chars cd) (list->string chars)))
         done))
         
(define <SymbolChar>
    (new (*parser (range #\0 #\9))
         (*parser (range-ci #\a #\z))
         (*parser (char #\!))
         (*parser (char #\$))
         (*parser (char #\^))
         (*parser (char #\*))
         (*parser (char #\-))
         (*parser (char #\_))
         (*parser (char #\=))
         (*parser (char #\+))
         (*parser (char #\<))
         (*parser (char #\>))
         (*parser (char #\?))
         (*parser (char #\/))
         (*parser (char #\:))
         (*disj 15)
         done))
         
(define ^symbolBuilder
    (lambda(parse)
        (new (*parser parse) *plus
            (*pack-with
                (lambda chars
                    (if (andmap (lambda (ch)
                                    (and (char<=? #\0 ch)
                                    (char<=? ch #\9))) chars) 
                    (string->number (list->string chars))  
                    (string->symbol 
                        (string-downcase (list->string chars))))
                    ))
         done)))
         
(define <Symbol> (^symbolBuilder <SymbolChar>))
         
(define <Boolean>
  (new (*parser (word-ci "#t"))
       (*pack
	(lambda (_) #t))

       (*parser (word-ci "#f"))
       (*pack
	(lambda (_) #f))

       (*disj 2)
       done))
         
(define <CharPrefix> 
    (caten (char #\#)(char #\\)))
         
(define <VisibleSimpleChar> <any-char>)


(define <NamedChar>
    (disj (^<MetaChar> "lambda" #\λ)(^<MetaChar> "newline" #\newline)
        (^<MetaChar> "nul" #\nul)(^<MetaChar> "page" #\page)
        (^<MetaChar> "return" #\return)(^<MetaChar> "space" #\space)
        (^<MetaChar> "tab" #\tab)
         ))
         

         
(define <Char>
    (new (*parser <CharPrefix>)
         (*parser <NamedChar>)
         (*parser <VisibleSimpleChar>)
         (*disj 2)
         (*caten 2)
         (*pack-with (lambda(pre char) char))
         done))

;;;;;;;;collections section;;;;;;;;;
(define <ProperList>
    (new (*parser (char #\())
         delayedSexpr
         *star
         (*parser (char #\)))
         (*caten 3)
         (*pack-with 
            (lambda (the-start sexprs the-end) sexprs))
         done))

(define make-list
	(lambda(starts end)
		(if (null? starts) end
			(cons (car starts) (make-list (cdr starts) end)))))

(define <ImproperList>
    (new (*parser (char #\( ))
         delayedSexpr
         *plus
         (*parser (char #\.))
         delayedSexpr
         (*parser (char #\)))
         (*caten 5)
         (*pack-with 
            (lambda (the-start sexprs point sexpr the-end) 
                (make-list sexprs sexpr)))
         done))
         
(define <Vector>
    (new (*parser (char #\#))
         (*parser <ProperList>)
         (*caten 2)
         (*pack-with (lambda(char lst) (list->vector lst) ))
         done))
;;;;;;;;end of collections section;;;;;;;;;
         
;;;;;;;;end of qoute section;;;;;;;;;
(define qoute-parser
    (lambda(qmark qm)
        (new (*parser (word qmark))
             delayedSexpr
             (*caten 2)
             (*pack-with (lambda(q exp) (list qm exp)))
            done)
    ))

(define <Qouted> (qoute-parser "'" 'quote))
(define <QuasiQouted> (qoute-parser "`" 'quasiquote))
(define <Unqouted> (qoute-parser "," 'unquote))
(define <UnqoutedAndSpliced> (qoute-parser ",@" 'unquote-splicing))
;;;;;;;;end of qoute section;;;;;;;;;         



;;;;;;infix section;;;;;;;

(define delayedNotMath (*delayed 
    (lambda()<InfixNotMath>)))
    
(define delayedEscape (*delayed 
    (lambda()<InfixSexprEscape>)))
    
(define make
 (lambda(an any);any = (("f"(4 5 6 7))("v"(3))("v"(4)))
    (let((first (car any))(rest (cdr any)))
        (if (null? rest)
            (if (string=? (car first) "v")
                (list (string->symbol "vector-ref") an (cdr first))
                (cons an (cdr first)))
            (if (string=? (car first) "v")
                (list (string->symbol "vector-ref") 
                      (make an rest)
                      (cdr first))
                (cons (make an rest)
                      (cdr first)))))))


                          
(define <InfixSymbol> 
    (^symbolBuilder (diff <SymbolChar>
        (disj (char #\^) (char #\*) (char #\-) (char #\+) (char #\/))))) 

(define <InfixParen>
    (new (*parser (char #\())
         delayedInfix
         (*parser (char #\)))
         (*caten 3)
         (*pack-with (lambda(st body end) body))
         done))

(define <InfixArgList>
    (new delayedInfix
         (*parser (char #\,))
         delayedInfix
         (*caten 2)
         *star
         (*caten 2)
         (*pack-with 
            (lambda(first rest);rest = ((, (exp))(, (exp))
                (cons first (map cadr rest))))
         (*parser (^<inskipped*> <epsilon>))
         (*disj 2)
         done))

         
(define ^<accessor>
    (lambda(st parse end kind)
        (new (*parser (char st))
             parse
             (*parser (char end))
             (*caten 3)
             (*pack-with (lambda(begining value ending)
                (cons kind value))) 
             done)
            ))

            
(define <funcallaccessor>
    (^<accessor> #\( (*delayed(lambda()<InfixArgList>)) #\) "f"))

(define <arrayaccessor>
    (^<accessor> #\[ delayedInfix #\] "v"))


(define <InfixArrayNFunc>
    (new (*parser (^<inskipped*> <InfixSymbol>))
         (*parser <arrayaccessor>)
         (*parser <funcallaccessor>)
         (*disj 2)
         *plus
         (*caten 2)
         (*pack-with (lambda(afn values)
                        (make afn (reverse values))))
         done))
         
(define <ToTheLeftD>
    (disj <InfixArrayNFunc> <InfixParen> <InfixSymbol>))

    
(define process-left
    (lambda(sups end)
        (let ((first (car sups)) (rest (cdr sups)))
           (let((firststr (list->string (cadr first))))
            (let ((op (string->symbol firststr))
                  (firstval (car first)))
            (if (null? rest) 
                (list op firstval end)
                (list op (process-left rest firstval) end)))))))
            
(define ^<InfixAction>
    (lambda (pred mysup1 mysup2 pw)
      (let ((newsup mysup1))
        (new
        (*parser (^<inskipped*> newsup))
        (*parser pred)
        (*caten 2)
        *star
        (*parser (^<inskipped*> mysup2))
        (*caten 2)
        (*pack-with pw)
        done))))
        
(define benaym-pack-with
    (lambda (lefts end)
        (if (null? lefts) end (process-left (reverse lefts) end ))))
                    
(define ^<Benaym>
    (lambda (c sup)
        (^<InfixAction> c sup sup benaym-pack-with)))

(define <InfixNotMath> 
    (new delayedEscape
        (*parser <InfixParen>)
        (*parser <InfixArrayNFunc>)
        (*parser <Number>)
        (*parser <InfixSymbol>)
        (*disj 5)
        done))
        
(define <PowerSymbol> (disj (word "^") (word "**")))

(define make-power
    (lambda(left end)
        (if (null? left) end 
            (list 'expt (caar left) (make-power (cdr left) end)))))
    
(define pow-pack-with
    (lambda (lefts end)
            (if (null? lefts) end (make-power lefts end))))
    
(define <InfixPow>
    (^<InfixAction> <PowerSymbol> <InfixNotMath> <InfixNotMath> 
        pow-pack-with))

(define make-neg-with-n
    (lambda(lefts end)
        (if (null? lefts) end 
            (if (null? (caar lefts))
                    (list '- (make-neg-with-n (cdr lefts) end))
                    (list 'expt (caaar lefts) 
                        (list '- (make-neg-with-n (cdr lefts) end)))))
        ))
        
(define make-neg
    (lambda(lefts end)
        (if (null? lefts) end 
            (if (null? (caar lefts))
                    (list '- (make-neg (cdr lefts) end))
                    (make-power (caar lefts) 
                        (list '- (make-neg (cdr lefts) end)))))
        ))
    
(define neg-pack-with
    (lambda (lefts end)
        (if (null? lefts) end (make-neg lefts end))))
    
(define <InfixNeg>
    (^<InfixAction> (word "-") 
        (star (caten  (^<inskipped*> <InfixNotMath>) <PowerSymbol>)) <InfixPow> neg-pack-with))
    
(define <InfixMulNDiv>
    (^<Benaym> (disj (word "*") (word "/")) <InfixNeg>))
        
(define <InfixAddNSub>
    (^<Benaym> (disj (word "+") (word "-")) <InfixMulNDiv>))
    
(define <InfixPrefixExtensionPrefix>
    (disj (word "##") (word "#%")))

(define ^<ExtensionBuilder>
   (lambda(parse) 
     (new (*parser <InfixPrefixExtensionPrefix>) 
         parse
         (*caten 2)
         (*pack-with (lambda(pre fix) fix))
         done)))    

(define <InfixExtension> 
    (^<ExtensionBuilder> delayedInfix))

(define <InfixSexprEscape>  
    (^<ExtensionBuilder> delayedSexpr))

(define <InfixExpression> (^<inskipped*> <InfixAddNSub>))
;;;;;;end of infix section;;;;;;;

(define <sexpr>
    (^<skipped*> (disj <Boolean> <String> <Char> <Number> <Symbol>  <ProperList> <ImproperList> <Vector> <Qouted> <QuasiQouted> 
    <Unqouted> <UnqoutedAndSpliced> <InfixExtension>)))
    
(define hw1 
    (lambda(str)
        (<sexpr> str 
            (lambda (e s)(if (null? s) (list e) (cons e (hw1 s)))) 
            (lambda(w) ((error 'tag-parse `(ERROR: ,@w)))))))
