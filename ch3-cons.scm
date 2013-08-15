; introduce rember
; (rember atom lat)
; return a new list with first occurence of atom removed
; it returns empty list if the list is empty
; it check the atom in turn in the list
;   if it equals the atom occurs in the list it returns the list with the atom removed
;   otherwise, it saves this atom and try to rember atom from the rest of the list
(define rember
  (lambda (a lat)
    (cond 
      ((null? lat) '())
      ((eq? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))

(define a 'cup)
(define lat '(tea coffee cup))
(rember a lat)

(define a 'sauce)
(define lat '(soy sauce and tomato sauce))
(rember a lat)

; introduct firsts
; it takes a list as argument
; the list is either an empty list or contains ONLY non-empty list
; returns a new list composed of the first S-expression of each lists
(define firsts
  (lambda (lat) 
    (cond 
      ((null? lat) '())
      (else (cons (car (car lat)) (firsts (cdr lat)))))))

(define l '(((five plums) fours) (eleven green oranges) ((no) more)))
(firsts '((a b) (c d)))
(firsts l)

; similarly, the seconds
(define seconds
  (lambda (lat) 
    (cond 
      ((null? lat) '())
      (else (cons (car (cdr (car lat))) (seconds (cdr lat)))))))
(seconds l)

; introduce insertR
; it takes two atoms - new, old and a list
; it returns a new list with the new atom inserted to the right of the old atom
(define insertR
  (lambda (new old lat) 
    (cond    
      ((null? lat) '())
      ((eq? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(define new 'jalapeno)
(define old 'and)
(define lat '(tacos tamales and salsa))
(insertR new old lat)

(define new 'e)
(define old 'd)
(define lat '(a b c d f g d h))
(insertR new old lat)

; similarly insertL and subst
(define insertL
  (lambda (new old lat) 
    (cond    
      ((null? lat) '())
      ((eq? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat) 
    (cond    
      ((null? lat) '())
      ((eq? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

; replace multiple occurence of old
(define subst2
  (lambda (new old1 old2 lat) 
    (cond    
      ((null? lat) '())
      ((or (eq? old1 (car lat)) (eq? old2 (car lat))) 
          (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new old1 old2 (cdr lat)))))))

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

; introduce of multirember
; it takes a atom and a list
; if the list is empty returns the empty list
; otherwise it returns a new list with all occurence of atoms removed
(define multirember
  (lambda (a lat) 
    (cond ((null? lat) '())
          ; drop the (car lat) value
          ((eq? a (car lat)) (multirember a (cdr lat))) 
          ; concate the (car lat) in the head of list
          (else (cons (car lat) (multirember a (cdr lat)))))))

(define a 'cup)
(define lat '(coffee cup tea cup and hick cup))
(multirember a lat)

; similarly
(define multiinsertL
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) 
            (cons new 
              (cons old 
                (multiinsertL new old (cdr lat)))))
          (else (cons (car lat) 
                  (multiinsertL new old (cdr lat)))))))

(define new 'e)
(define old 'd)
(define lat '(a b c d f g d h))
(multiinsertL new old lat)

; similarly
(define multisubst
  (lambda (new old lat)
    (cond ((null? lat) '())
          ((eq? old (car lat)) 
            (cons new 
                (multisubst new old (cdr lat))))
          (else (cons (car lat) 
                  (multisubst new old (cdr lat)))))))
