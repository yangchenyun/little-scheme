; define set?
; a list contains only unique atoms
(define member?
  (lambda (atom lat)
    (cond 
      ((null? lat) #f)
      ((equal? atom (car lat)) #t)
      (else (member? atom (cdr lat))))))

(define set?
  (lambda (lat)
    (cond 
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(set? '())
(set? '(apple peaches apple plum))
(set? '(apple peaches banana plum))
(set? '(apple 3 banana 4 9 3 plum))

; makeset
; it builds a set from a list
(define makeset
  (lambda (lat)
    (cond 
      ((null? lat) '())
      ((member? (car lat) (cdr lat)) 
        (makeset (cdr lat)))
      (else (cons 
              (car lat) 
              (makeset (cdr lat)))))))

(makeset '(apple peaches apple banana plum apple peaches))

; use multirember to rewrite makeset
(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons 
              (car lat)
              (makeset 
                (multirember 
                  (car lat) 
                  (cdr lat))))))))

(makeset '(apple 3 banana 4 9 3 plum))

; subset?
(define set1 '(5 chicken wings))
(define set2 '(5 hamburgers 2 pieces fried chicken and light duckling wings))
(subset? set1 set2) ; #t

(define subset?
  (lambda (set1 set2)
    (cond 
      ((null? set1) #t)
      ((member? (car set1) set2) 
        (subset? (cdr set1) set2))
      (else #f))))

; an improvement with (and ..)
(define subset?
  (lambda (set1 set2)
    (cond 
      ((null? set1) #t)
      (else 
        (and 
          ; because member? itself determines the false value
          (member? (car set1) set2) 
          (subset? (cdr set1) set2))))))

; set comparison with eqset?
(define set1 '(6 large chickens with wings))
(define set2 '(6 chickens with large wings))
(eqset? set1 set2) ; #t

(define eqset?
  (lambda (set1 set2)
    (cond 
      ((and (null? set1) (null? set2)) #t)
      ((or (null? set1) (null? set2)) #f)
      ; neigher set1 nor set2 is null now
      (else (eqset? 
              (cdr set1) 
              (multirember (car set1) set2))))))

; an simplified version using subset?
(define eqset?
  (lambda (set1 set2)
    (and 
      (subset? set1 set2)
      (subset? set2 set1))))

; intersect?
; at least one atom in set1 is in set2
(define intersect?
  (lambda (set1 set2)
    (cond 
      ((null? set1) #f)
      ((member? (car set1) set2) #t)
      (else (intersect? (cdr set1) set2)))))

; use (or ...) to simplify
(define intersect?
  (lambda (set1 set2)
    (cond 
      ((null? set1) #f)
      (else (or 
        (member? (car set1) set2) 
        (intersect? (cdr set1) set2))))))

(define set1 '(stewed tomatoes and macaroni))
(define set2 '(macaroni and cheese))
(intersect? set1 set2) ; #t

; intersect
(define intersect
  (lambda (set1 set2)
    (cond 
      ((null? set1) '())
      ((member? (car set1) set2)
        (cons 
          (car set1) 
          (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(intersect set1 set2)

; union
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) 
        (union (cdr set1) set2))
      (else (cons 
              (car set1)
              (union (cdr set1) set2))))))

(union set1 set2)

; xxx? -> difference
(define xxx
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
        (xxx (cdr set1) set2))
      (else (cons 
              (car set1)
              (xxx (cdr set1) set2))))))

(xxx set1 set2)

; intersectall
; performs set operation on list
(define intersectall
  (lambda (lset)
    (cond 
      ; the terminal conditions stops one step before null? set
      ((null? (cdr lset)) (car lset))
      (else (intersect 
              (car lset) 
              (intersectall (cdr lset)))))))

(intersectall '(
  (6 pears and)
  (3 peaches and 6 peppers)
  (8 pears and 6 plums)
  (and 6 prunes with some apples)))


; pair - a list with two S-expressions
(define a-pair?
  (lambda (l)
    (cond
      ((atom? l) #f)
      ((null? l) #f)
      ((null? (cdr l)) #f)
      (else
        (null? (cdr (cdr l)))))))

(a-pair? '((3) (7)))
(a-pair? '((2) (house)))
(a-pair? '(full (house)))
(a-pair? '((house)))
(a-pair? '(house))
(a-pair? 'house)
(a-pair? '())

; pair related helper functions
(define first
  (lambda (p) (car p)))

(define second
  (lambda (p) (car (cdr p))))

(define build
  (lambda (s1 s2) 
    (cons s1 (cons s2 '()))))


; rel: a set of pairs
; fun: a rel with the (firsts rel) being a set

; define fun?
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

; revrel
; returns a new rel with atoms in all pairs reversed
(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons 
        (revpair (car rel)) 
        (revrel (cdr rel)))))))

(revrel '((a 8) (pumpkin pie) (got sick)))

; fullfun, a fun with the (seconds rel) being a set too
(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(fullfun? '((grape raisin)
            (plum prune)
            (stewed prune)))

(fullfun? '((grape raisin)
            (plum prune)
            (stewed grape)))

; use revrel
(define fullfun?
  (lambda (fun)
    (fun? (revrel fun))))

