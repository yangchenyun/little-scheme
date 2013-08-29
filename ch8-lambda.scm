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
(define a-friend
  (lambda (x y)
    (null? y)))

(define multiremberco
  (lambda (a lat col)
    (cond
      ((null? lat) 
        (col '() '()))
      ((eq? (car lat) a)
        (multiremberco a
          (cdr lat)
          (lambda (newlat seen)
            (col newlat
              (cons (car lat) seen)))))
      (else
        (multiremberco a
          (cdr lat)
          (lambda (newlat seen)
            (col (cons (car lat) newlat)
              seen)))))))

; the simpliest recursion
(multiremberco 'tuna '() a-friend) ; #t

; the one step up in recursion
(multiremberco 'tuna '(tuna) a-friend) ; #f
; resolves to
(multiremberco 'tuna
          '()
          (lambda (newlat seen)
            (a-friend newlat
              (cons 'tuna seen))))
; resolves to
((lambda (newlat seen)
            (a-friend newlat
              (cons 'tuna seen))) '() '())
; resolves to
(a-friend '() (cons 'tuna '())) ;#f


; the one step up into recursion
(multiremberco 'tuna '(and tuna) a-friend)

(multiremberco 'tuna '(tuna) 
  (lambda (newlat seen)
              (a-friend (cons 'and newlat)
                seen)))

; build recursive functions to collection more than one value at one time
; use lambda to build recursive functions, passed by parameters with changes

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond 
      ((null? lat) '())
      ((eq? (car lat) oldL) 
        (cons new 
          (cons oldL
            (multiinsertLR new oldL oldR 
              (cdr lat)))))
      ((eq? (car lat) oldR) 
        (cons oldR 
          (cons new 
            (multiinsertLR new oldL oldR
              (cdr lat)))))
      (else 
        (cons (car lat)
          (multiinsertLR new oldL oldR (cdr lat)))))))


; turning it into using a recursive collector
(define multiinsertLRco
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) 
        (col '() 0 0))
      ((eq? (car lat) oldL)
        (multiinsertLRco new oldL oldR 
          (cdr lat)
          (lambda (newlat L R)
          ; why the first param is a new lat built by cons?
            (col (cons new (cons oldL newlat)) (+ L 1) R))))
      ((eq? (car lat) oldR)
        (multiinsertLRco new oldL oldR 
          (cdr lat)
          (lambda (newlat L R)
            (col (cons oldR (cons new newlat)) L (+ R 1)))))
      (else
        (multiinsertLRco new oldL oldR
          (cdr lat)
          (lambda (newlat L R)
            (col 
              (cons 
                (car lat) newlat) L R)))))))

(define even?
  (lambda (n) 
  (= (mod n 2) 0)))


(define evens-only*
  (lambda (lat)
    (cond 
      ((null? lat) '())
      ((atom? (car lat)) 
        (cond 
          ((even? (car lat)) (cons (car lat) (evens-only* (cdr lat))))
          (else (evens-only* (cdr lat)))))
      (else 
        (cons 
          (evens-only* (car lat)) 
          (evens-only* (cdr lat)))))))

(evens-only* '((2 5) 3 4 5 (2 3 4)))

; use a collector with the evens-only*
; the collector multiplies the even numbers and sums up the odd numbers
(define evens-only*co
  (lambda (lat col)
    (cond
      ((null? lat) (col '() 1 0))
      ((atom? (car lat) 
        (cond
          ((even? (car lat) 
            (evens-only*co (cdr lat) 
              (lambda (newlat product sum)
                (col 
                  ; the result is collected here
                  (cons (car lat) newlat)
                  (* product (car lat))
                  sum)))))
          (else
            (evens-only*co (cdr lat)
              (lambda (newlat product sum)
                (col newlat
                  product
                  (+ 1 sum))))))))
      (else 
        (evens-only*co (car l)
          (lambda (al ap as)
            (evens-only*co (cdr l)
              (lambda (dl dp ds)
                (col 
                  (cons al dl)
                  (* ap dp)
                  (+ ap dp))))))))))

