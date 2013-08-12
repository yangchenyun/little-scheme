; definition for atoms
(atom? 'atom)
(atom? 'turkey)
(atom? 'u)
(atom? '1492)
(atom? '*abc$)

; definition for lists
(atom? '(atom))
(atom? '(atom turkey or))

; definitions for S-expressions
; all atoms are S-expressions
; all lists are S-expressions
; () is an null S-expression

; introduce car
(car '(a b c))
(car '((a b c) x y z))
(car 'hotdog) ; couldn't ask car for an atom
(car '()) ; couldn't ask car for null list

(car '(((hotdogs)) (and) (pickle) relish))
(car (car '(((hotdogs)) (and) (pickle) relish))) 

; introduce cdr
; "cdr" is pronounced "could-er."
(cdr '(a b c))
(cdr '(hamberger)) ; could return null list
(cdr '())

(define l '((b) (x y) (c)))
(car (cdr l))
(cdr (cdr l))

(define l '(a (b (c)) d))
(cdr (car l)) ; (car l) is an atom

; introduce cons
; cons adds a S-expression to the front of a list.
(define a 'peanut)
(define l '(butter and jelly))
(cons a l)

(define s '(banana and))
(define l '(peanut butter and jelly))
(cons s l)

(define s 'a)
(define l '((b) c d))
(cons s (car l)) ; (a b)
(cons s (cdr l)) ; (a c d)

; introduce null?
; only defined for list
(null? '())

; reintroduce atom?
(define atom? 
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; introduce eq?
; both arguments should be non-numeric atoms
(eq? 'Harry 'Harry)
(eq? 'butter 'fly)

(define l '(Mary had a little lamb chop))
(define a 'Mary)
(eq? (car l) a)

(define l '(soured milk))
(define a 'milk)
(eq? (cdr l) a)

(define l '(beans beans we need jelly beans))
(eq? (car l) (car (cdr l)))
