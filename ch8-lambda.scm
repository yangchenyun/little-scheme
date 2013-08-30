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

((insert-g setL) 10 5 '(6 2 5 5 3))
((insert-g setR) 10 5 '(6 2 5 5 3))

; combine the previous two function variations
(define insertg-f
  (lambda (test? seq)
    ; it returns a lambda
    (lambda (new old lat)
      (cond
        ((null? lat) '())
        ((test? old (car lat))
          (seq new old lat))
        (else
          (cons (car lat)
            ((insertg-f test? seq) new old (cdr lat))))))))
((insertg-f equal? setR) 'hello '(a b c) '((a b c) 2 cat (a papa) 3))

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
(define multirember-col
  (lambda (a lat col)
    (cond ((null? lat) (col '() '()))
          ((eq? a (car lat))
            (multirember-col a (cdr lat)
              ; the collection happens in the collector function
              (lambda (removed left)
                (col (cons (car lat) removed) left))))
          (else
            (multirember-col a (cdr lat)
              (lambda (removed left)
                (col removed (cons (car lat) left))))))))

(define removal-col
  (lambda (rem-lat remain-lat)
    (cons rem-lat remain-lat)))
(define a 'cup)
(define lat '(coffee cup tea cup and hick cup))
(multirember-col a lat (lambda (x y) (length y)))

(define a-friend
  (lambda (x y)
    (null? x)))

; the simpliest recursion
(multirember-col 'tuna '() a-friend) ; #t

; the one step up in recursion
(multirember-col 'tuna '(tuna) a-friend) ; #f

; build recursive functions to collection more than one value at one time
; use lambda to build recursive functions, passed by parameters with changes

; =================== USE FUNCTION TO COLLECT RESULTS ===================
; combine the multiinsertL and multiinsertR

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond ((null? lat) '())
          ((eq? oldL (car lat))
            (cons new
              (cons oldL
                (multiinsertLR new oldL oldR (cdr lat)))))
          ((eq? oldR (car lat))
            (cons oldR
              (cons new
                (multiinsertLR new oldL oldR (cdr lat)))))
          (else (cons (car lat)
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
            ; newlat here is the lat produced by
            ; (inner recursion) apply multiinsertLR-col on new oldL oldR and (cdr lat)
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

; with another collection algorithm
(define multiinsertLR-col
  (lambda (new oldL oldR lat col)
    (cond ((null? lat) (col '() '() '()))
      ((eq? oldL (car lat))
        (multiinsertLR-col new oldL oldR (cdr lat)
          (lambda (newlat left right)
            (col
              (cons new (cons oldL newlat))
              (cons oldL left)
              right))))
      ((eq? oldR (car lat))
        (multiinsertLR-col new oldL oldR (cdr lat)
          (lambda (newlat left right)
            (col
              (cons oldR (cons new newlat))
              left
              (cons oldR right)))))
      (else
        (multiinsertLR-col new oldL oldR (cdr lat)
          (lambda (newlat left right)
            (col
              (cons (car lat) newlat)
              left
              right)))))))

(define watcher
  (lambda (lat left right)
    (cons lat (cons left right))))

(define new 'e)
(define oldL 'd)
(define oldR 'g)
(define lat '(a b c d f g d h))
(multiinsertLR-col new oldL oldR lat watcher)

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
(define even-only*-col
  (lambda (lat col)
    (cond
      ((null? lat) (col '() 1 0))
      ((atom? (car lat))
        (cond
          ((even? (car lat))
            (even-only*-col (cdr lat)
              (lambda (newlat p s)
              (col
                (cons (car lat) newlat)
                (* p (car lat))
                s))))
          ; odd number
          (else (even-only*-col (cdr lat)
            (lambda (newlat p s)
              (col newlat p (+ (car lat) s)))))))
      ; recursive into (car lat)
      (else (even-only*-col (car lat)
        ; recursively use collector
        (lambda (al ap as)
          (even-only*-col (cdr lat)
            (lambda (dl dp ds)
              (col
                (cons al dl)
                (* ap dp)
                (+ as ds))))))))))

(even-only*-col '((2 5) 3 4 5 (2 3 4))
  (lambda (lat p s)
    (cons p (cons s lat))))
