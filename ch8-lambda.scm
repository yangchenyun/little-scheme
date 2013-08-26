; a more agostic rember function
(define rember-f 
  (lambda (test? a l) 
    (cond 
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else (cons 
              (car l) 
              (rember-f test? a (cdr l)))))))

(rember-f = 5 '(6 2 5 3)) ; (6 2 3)
(rember-f eq? 'jelly '(jelly beans are good))
(rember-f equal? '(pop corn) '(lemonadae (pop corn) and (cake)))

; introduce function which could return functions
(define eq-c?
  (lambda (a)
    (lambda (x)
      (eq? x a))))

((eq-c? 3) 3) ;#t
((eq-c? 3) 1) ;#f

; make rember-f a high-order function
(define rember-f 
  (lambda (test?) 
    (lambda (a l) 
      (cond 
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons 
                (car l) 
                ((rember-f test?) a (cdr l))))))))

((rember-f =) 5 '(6 2 5 3)) ; (6 2 3)
((rember-f eq?) 'jelly '(jelly beans are good))
((rember-f equal?) '(pop corn) '(lemonadae (pop corn) and (cake)))

; transform insertL as well
(define insertL-f
  (lambda (test?) 
    (lambda (new old lat) 
      (cond    
        ((null? lat) '())
        ((test? old (car lat)) 
          (cons new lat))
        (else (cons 
                (car lat) 
                ((insertL-f test?) new old (cdr lat))))))))

((insertL-f =) 10 5 '(6 2 5 5 3)) ; (6 2 3)
((insertL-f eq?) 'chocolate 'jelly '(jelly beans are good))
((insertL-f equal?) '(hello kitty) '(pop corn) '(lemonadae (pop corn) and (cake)))


; ...for insertR as well
(define insertR-f
  (lambda (test?) 
    (lambda (new old lat) 
      (cond    
        ((null? lat) '())
        ((test? old (car lat)) 
          (cons old 
            (cons new (cdr lat))))
        (else (cons 
                (car lat) 
                ((insertR-f test?) new old (cdr lat))))))))

((insertR-f =) 10 5 '(6 2 5 5 3)) ; (6 2 3)
((insertR-f eq?) 'chocolate 'jelly '(jelly beans are good))
((insertR-f equal?) '(hello kitty) '(pop corn) '(lemonadae (pop corn) and (cake)))

; a more generic insert-g func
; step1 - extract the difference part
; step2 - use a func to express the difference
(define setL
  (lambda (new old lat)
    (cons new lat)))

(define setR
  (lambda (new old lat)
    (cons old (cons new (cdr lat)))))

; write a high-order func use this func to combine similarities
(define insert-g
  (lambda (seq)
    (lambda (new old lat)
      (cond 
        ((null? lat) '())
        ((eq? old (car lat))
          (seq new old lat))
        (else 
          (cons 
            (car lat)
            ((insert-g seq) new old (cdr lat))))))))

((insert-g setL) 10 5 '(6 2 5 5 3)) ; (6 2 3)
((insert-g setR) 10 5 '(6 2 5 5 3)) ; (6 2 3)

; also do for subst
(define seqS
  (lambda (new old lat)
    (cons new (cdr lat))))

((insert-g seqS) 10 5 '(6 2 5 5 3)) ; (6 2 3)

; more
(define seqrem
  (lambda (new old lat) (cdr lat)))

((insert-g seqrem) #f 5 '(6 2 5 5 3))

; refactor the value function
(define extract-operator
  (lambda (atom)
    (cond 
      ((eq? atom '+) +)
      ((eq? atom 'x) x)
      ((eq? atom '^) ^))))

(define value
  (lambda (aexp) 
    (cond
      ((atom? aexp) aexp)
      (else 
        ((extract-operator (operator aexp))
          (value (first_sub_exp aexp))
          (value (second_sub_exp aexp)))))))

(value '(+ 1 2))
(value '(+ 1 (x 2 3)))
(value '(+ (^ 5 2) (x 2 3)))


; now the multi rember
(define multirember-f 
  (lambda (test?) 
    (lambda (a l) 
      (cond 
        ((null? l) '())
        ((test? a (car l)) 
          ((multirember-f test?) a 
            (cdr l)))
        (else (cons 
                (car l) 
                ((multirember-f test?) a 
                  (cdr l))))))))

; a modification
(define eq-tuna
  (eq-c? 'tuna))

(define multiremberT
  (lambda (test? lat)
    (cond 
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else 
        (cons 
          (car lat) 
          (multiremberT test? (cdr lat)))))))

(multiremberT eq-tuna '(tuna hello world tuna))

; a complex example
