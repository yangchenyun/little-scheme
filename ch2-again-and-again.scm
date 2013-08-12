; introduce lat?
; every lat is a list of atoms
(define lat? 
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l))) ; recursively ask for (cdr l)
      (else #f))))
; lat? checks the S-expression of a list in turn
; ask for if the S-expression is an atom
; if all the S-expression is true, it returns true
; otherwise, it returns false

(lat? '()) ; #t
(lat? '(a b c)) ; #t
(lat? '(a (b c) d)) ; #f

; introduce or and member?
(define member?
  (lambda (a l)
    (cond 
      ((null? l) #f) 
      (else 
        (or 
          (eq? a (car l)) 
          (member? a (cdr l)))))))

(define a 'tea)
(define lat '(coffee tea or milk))
(member? a lat) ; #t
