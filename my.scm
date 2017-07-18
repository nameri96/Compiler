;list
(define list (lambda lst lst));done
(define mlg (lambda(first rest)(lambda(el) (first (rest el)))))
(define caar (mlg car car))
(define cdar (mlg cdr car))
(define cadr (mlg car cdr))
(define cddr (mlg cdr cdr))
(define caaar (mlg car caar))
(define cdaar (mlg cdr caar))
(define cadar (mlg car cdar))
(define cddar (mlg cdr cdar))
(define caadr (mlg car cadr))
(define cdadr (mlg cdr cadr))
(define caddr (mlg car cddr))
(define cdddr (mlg cdr cddr))
(define caaaar (mlg caar caar))
(define cdaaar (mlg cdar caar))
(define cadaar (mlg cadr caar))
(define cddaar (mlg cddr caar))
(define caadar (mlg caar cdar))
(define cdadar (mlg cdar cdar))
(define caddar (mlg cadr cdar))
(define cdddar (mlg cddr cdar))
(define caaadr (mlg caar cadr))
(define cdaadr (mlg cdar cadr))
(define cadadr (mlg cadr cadr))
(define cddadr (mlg cddr cadr))
(define caaddr (mlg caar cddr))
(define cdaddr (mlg cdar cddr))
(define cadddr (mlg cadr cddr))
(define cddddr (mlg cddr cddr))
(define myawesomemap 
    (lambda(func lst)
        (if (null? lst) (list) (cons (func (car lst)) (myawesomemap func (cdr lst))))));done
(define map 
    (lambda(func . lsts)
        (if (null? (car lsts)) (list) 
            (cons (apply func (myawesomemap car lsts)) 
                  (apply map (cons func (myawesomemap cdr lsts)))))));done
(define binaryappend
    (lambda(list1 list2)
        (if (null? list1) list2 (cons (car list1) (binaryappend (cdr list1) list2)))));done
(define append 
    (lambda lists
        (if (null? (cdr lists)) (car lists)
            (binaryappend (car lists) (apply append (cdr lists)))))) 
(define vector (lambda lst (listtovector lst)));done
(define abstract-positive-arithmatic
    (lambda(func init params)
        (if (null? params) init 
            (let((rest (abstract-positive-arithmatic func init (cdr params))))
                (func (numerator (car params)) (denominator (car params)) 
                        (numerator rest) (denominator rest))))))
(define abstract-negative-arithmatic
    (lambda(func init pos-func params)
        (let((firstnum (numerator (car params)))(firstdeno (denominator (car params))))
        (if (null? (cdr params)) 
            (func (numerator init) (denominator init) firstnum firstdeno) 
            (let((rest (abstract-positive-arithmatic pos-func init (cdr params))))
                    (func firstnum firstdeno (numerator rest) (denominator rest)))))))
(define * (lambda args (try-red-rat (abstract-positive-arithmatic binarymul 1 args))))
(define + (lambda args (try-red-rat (abstract-positive-arithmatic binaryplus 0 args))))
(define - (lambda args (try-red-rat (abstract-negative-arithmatic binaryminus 0 binaryplus args))))
(define / (lambda args (try-red-rat (abstract-negative-arithmatic binarydiv 1 binarymul args))))
(define gcd-in (lambda(x y) (if (zero? y) x (gcd-in y (remainder x y)))))
(define gcd 
    (lambda(x y) 
        (let ((x (if (> x 0) x (- x)))(y (if (> y 0) y (- y))))
            (if (< x y) (gcd-in y x) (gcd-in y x)))))
(define simple-div
    (lambda(x y)
      (let ((sign (if (or (and (< x 0) (> y -1)) (and (< y 0) (> x -1))) #t #f))
            (x (if (> x 0) x (- x)))
            (y (if (> y 0) y (- y))))
        (letrec ((loop (lambda(x res) (if (= x 0) res (loop (- x y) (+ res 1))))))
            (if sign (- (loop x 0)) (loop x 0))))))
(define try-red-rat 
    (lambda(rat)
        (if (integer? rat) rat
            (let* ((num (numerator rat))(deno (denominator rat))(theirgcd (gcd num deno)))
                (let ((sign (if (or (and (< num 0) (> deno -1)) 
                                    (and (< deno 0) (> num -1))) #t #f))
                      (num (if (> num 0) num (integerminus num)))
                      (deno (if (> deno 0) deno (integerminus deno))))
                      (cond ((= num 0) 0) 
                            ((= deno 1) (if sign (integerminus num) num))
                            ((= num deno) (if sign -1 1))
                            ((= theirgcd 1) 
                                (if sign (binarydiv (integerminus num) 1 deno 1) rat))
                            (else (let ((res (binarydiv (simple-div num theirgcd) 1 
                                                        (simple-div deno theirgcd) 1)))
                                        (if (integer? res)
                                            (if sign (integerminus res) res)
                                            (if sign 
                                                (binarydiv (integerminus (numerator res)) 1
                                                    (denominator res) 1) res))))
                        ))))))
(define abstract-comparator
    (lambda(func params)
        (if (null? (cdr params)) #t 
            (let((first (car params) ) (second (cadr params)))
                (and (func (numerator first) (denominator first) (numerator second) (denominator second)) 
                     (abstract-comparator func (cdr params)))))))
(define = (lambda args (abstract-comparator binaryeq args)))
(define < (lambda args (abstract-comparator binarylt args)))
(define > (lambda args (abstract-comparator binarygt args)))
(define mycomplilerinitialsymbolvalueforitnottobenull 'init)
(define str-eq?
    (lambda(str1 str2)
        (letrec 
            ((loop (lambda(index)
                    (if (= index (string-length str1)) #t
                        (and (= (char->integer (string-ref str1)) 
                                (char->integer (string-ref str2)))
                             (loop (+ index 1)))))))
        (if (= (string-length str1) (string-length str2)) (loop 0) #f))))
(define search-in-symtab
    (lambda(str flag)
        (let ((symtab (getsymboltable)))
            (letrec((loop (lambda(lst)
                            (cond ((str-eq? str (symbol->string (car lst))) (car lst))
                                  ((null? (cdr lst)) 
                                    (if flag 
                                        (let((sym (makesymbolfromstring str)))
                                            (set-cdr! lst (cons sym '())) sym)
                                        #t))
                                  (else (loop (cdr lst)))))))
                (loop symtab)))))
(define string->symbol
    (lambda(str)
        (let ((symtab (getsymboltable)))
            (letrec((loop (lambda(lst)
                            (cond ((str-eq? str (symbol->string (car lst))) (car lst))
                                  ((null? (cdr lst)) 
                                    (let((sym (makesymbolfromstring str)))
                                        (set-cdr! lst (cons sym '())) 
                                        sym))
                                  (else (loop (cdr lst)))))))
                (loop symtab)))))
(define symcounter 0)
(define gensym
    (lambda()
        (let ((newsym (make-string symcounter #\g)))
        (set! symcounter (+ symcounter 1))
        (if (search-in-symtab newsym) (string->symbol newsym) (gensym)))))