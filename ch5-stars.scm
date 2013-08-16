;;; This chapter expands the recurring functions applied to list
;;; to apply to S-expression as well
; introduce rember*
; it takes an atom and a list
; it returns a new list with all the occurence of atom in the list removed
(define rember*
  (lambda (a lat)
    (cond 
      ((null? lat) '())
      ((atom? (car lat)) 
        (cond 
          ((eq? a (car lat)) (rember* a (cdr lat)))
          (else (cons (car lat) (rember* a (cdr lat))))))
      ; the first occurence is a list
      (else (cons 
              (rember* a (car lat)) 
              (rember* a (cdr lat)))))))

(define a 'cup)
(define l '((coffee) cup ((tea) cup) (and (hick)) cup))
(rember* a l)

; insertR*
(define insertR*
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((atom? (car lat))
        (cond 
          ((eq? old (car lat)) 
            (cons old 
              (cons 
                new 
                (insertR* new old (cdr lat)))))
          (else (cons 
                  (car lat) 
                  (insertR* new old (cdr lat))))))
      (else (cons
              (insertR* new old (car lat))
              (insertR* new old (cdr lat)))))))

(define new 'roast)
(define old 'chuck)
(define l '((how much (wood)) 
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))

(insertR* new old l)

; occur*
(define occur*
  (lambda (a lat)
    (cond 
      ((null? lat) 0)
      ((atom? (car lat)) 
        (cond 
          ((eq? a (car lat)) 
            (add1 (occur* a (cdr lat))))
          (else 
            (occur* a (cdr lat)))))
      (else (+ (occur* a (car lat))
               (occur* a (cdr lat)))))))

(define a 'banana)
(define l '((banana)
            (split ((((banana ice)))
                   (cream (banana))
                   sherbet))
            (banana)
            (bread)
            (banana brandy)))
(occur* a l)

; subst*
(define subst*
  (lambda (new old lat)
    (cond 
      ((null? lat) '()) 
      ((atom? (car lat)) 
        (cond 
          ((eq? old (car lat)) 
            (cons 
              new 
              (subst* new old(cdr lat)))) 
          (else (cons 
                  (car lat) 
                  (subst* new old (cdr lat)))))) 
      (else (cons 
              (subst* new old (car lat))
              (subst* new old (cdr lat)))))))

(define new 'orange)
(define old 'banana)
(subst* new old l)

; insertL*
(define insertL*
  (lambda (new old lat)
    (cond 
      ((null? lat) '())
      ((atom? (car lat))
        (cond 
          ((eq? old (car lat)) 
            (cons new 
              (cons 
                old 
                (insertL* new old (cdr lat)))))
          (else (cons 
                  (car lat) 
                  (insertL* new old (cdr lat))))))
      (else (cons
              (insertL* new old (car lat))
              (insertL* new old (cdr lat)))))))

(define new 'pecker)
(define old 'chuck)
(define l '((how much (wood)) 
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))

(insertL* new old l)

; member*
; returns true if any S-expression of the list contains the atom a
(define member*
  (lambda (a lat)
    (cond 
      ; use boolean value as terminal value
      ((null? lat) #f)
      ((atom? (car lat)) 
        (or 
          (eq? a (car lat))
          (member* a (cdr lat))))
      (else (or 
            (member* a (car lat))
            (member* a (cdr lat)))))))

(member* 'chips '((potato) (chips ((with) fish) (chips))))

; leftmost
; it finds the leftmost atom appears a non-empty list 
; of S-expression that doesn't contain empty list
(define leftmost
  (lambda (a lat)
    (cond 
      ((atom? (car lat)) (car lat))
      (else (leftmost (cdr lat))))))

; eqlist?
; it takes two lists as arguments
; and returns #t only if the two list contains the same S-expressions
(define eqlist?
  (lambda (lat1 lat2)
    (cond 
      ((null? lat1) 
        (cond 
          ((null? lat2) #t)
          (else #f)))
      ((atom? (car lat1)) 
        (cond
          ((atom? (car lat2)) 
            (and 
              (eqan? (car lat1) (car lat2))
              (eqlist? (cdr lat1) (cdr lat2))))
          (else #f)))
      (else 
        (cond
          ((or (null? lat2) (atom? (car lat2))) #f)
          (else (and 
                  (eqlist? (car lat1) (car lat2))
                  (eqlist? (cdr lat1) (cdr lat2)))))))))


(define l1 '(beef ((sausage)) (and (soda))))
(define l2 '(beef ((salami)) (and (soda))))
(eqlist? l1 l1)
(eqlist? l1 l2)

; equal?
; it takes two S-expressions as arguments
; and returns #t only if the two are equal S-expression
(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

; simplify eqlist? with equal?
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ; we know neigher l1 nor l2 is null
      (else 
        (and 
          (equal? (car l1) (car l2)) 
          (eqlist? (cdr l1) (cdr l2)))))))

; There is a bit confusing between the two intervening recurring functions equal? and eqlist?
; why eqlist? determines result of null lists
; and equal? determines result of atoms

(define l1 '(beef ((sausage)) (and (soda))))
(define l2 '(beef ((salami)) (and (soda))))
(eqlist? l1 l1)
(eqlist? l1 l2)

; a more generic rember for S-expression
(define rember
  (lambda (s l)
    (cond 
      ((null? l) '())
      ((atom? (car l))
        (cond 
          ((equal? (car l) s) (cdr l)) 
          (else (cons (car l)
                  (rember s (cdr l))))))
      (else (cond 
              ((equal? (car l) s) (cdr l))
              (else (cons (car l)
                      (rember s (cdr l)))))))))

(rember '((salami)) l2)

; simplify rember use equal?
(define rember
  (lambda (s l)
    (cond 
      ((null? l) '()) 
      ((equal? s (car l)) (cdr l))
      (else (cons (car l)
              (rember s (cdr l)))))))

(rember '((salami)) l2)
