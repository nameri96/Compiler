(define members
    (lambda(to from)
        (sub (union to from) (union (sub to from) (sub from to)))))

(define sub
    (lambda(to from)
        (if (null? from) to
            (if (member (car from) to) 
                (sub (remq (car from) to) (cdr from))
                (sub to (cdr from)))))) 
            
(define union
    (lambda(to from)
        (if (null? from) to
            (if (member (car from) to) 
                (union to (cdr from))
                (union (cons (car from) to) (cdr from)))))) 

(define searchAndDelete
    (lambda(code result stack)
        (if (null? code) result
            (let* ((last-ins (car code))
                   (last-ins-name (car last-ins))
                   (last-ins-read (cadr last-ins))
                   (last-ins-write (caddr last-ins))
                   (rest-ins (cdr code))
                   (in-stack (members last-ins-write stack)))
                (if (null? in-stack)
                    (searchAndDelete rest-ins result stack)
                    (searchAndDelete rest-ins 
                                    (cons (list last-ins-name last-ins-read last-ins-write) result)
                                    ;if need to reduce the reg which are reduntant swich last-ins-write with in-stack 
                                    (union (sub stack in-stack) last-ins-read)))))))

                
(define get-all-regs
    (lambda(code stack)
        (if (null? code) stack
            (get-all-regs (cdr code) (union stack (union (cadar code) (caddar code))))))) 
        
(define remww
    (lambda(code)
        (searchAndDelete (reverse code) (list) (get-all-regs code (list)))))
        