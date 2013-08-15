; numbers
(define add1
  (lambda (n) 
    (+ n 1)))

(add1 42)

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 42)

(zero? 0)

; define + from add1, sub1, zero?
(define +
  (lambda (a b)
    (cond 
      ((zero? b) a)
      (else (+ (add1 a) (sub1 b))))))

; so is -
(define -
  (lambda (a b)
    (cond 
      ((zero? b) a)
      (else (- (sub1 a) (sub1 b))))))

; tuples - list of numbers
(define addtup
  (lambda (tup)
    (cond 
      ((null? tup) 0) 
      (else (+ (car tup) (addtup (cdr tup)))))))

(addtup '(1 2 3))

; define x
(define x
  (lambda (a b)
    (cond 
      ((zero? b) 0)
      (else (+ a (x a (sub1 b)))))))

; define tup+
; it takes two tups with the same length
; and it returns a new tup by add the first number in tup1 on first number in tup2
; so on and so forth..
(define tup+
  (lambda (tup1 tup2)
    (cond 
      ((and (null? tup1) (null? tup2)) '())
      (else (cons 
              (+ (car tup1) (car tup2))
              (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(1 2 3) '(4 5 6))

; improved to handle unequal lengths of two tuples
(define tup+
  (lambda (tup1 tup2)
    (cond 
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons 
              (+ (car tup1) (car tup2))
              (tup+ (cdr tup1) (cdr tup2)))))))

(tup+ '(2 3) '(4 5 6 8))

; define >
(define >
  (lambda (a b)
    (cond 
      ((zero? b) #f)
      ((zero? a) #t)
      (else (> (sub1 a) (sub1 b))))))

; define <
(define <
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)
      (else (< (sub1 a) (sub1 b))))))

; redefine = with > and <
(define =
  (lambda (a b)
    (cond
      ((or (< a b) (> a b)) #f)
      (else #t))))

; define ^
(define ^
  (lambda (a b)
    (cond 
      ((zero? b) 1)
      (else (x a (^ a (sub1 b)))))))


; self-defined quotient
(define quotient
  (lambda (a b)
    (cond
      ((< a b) 0)
      (else (add1 (quotient (- a b) b))))))


; mix numbers with list operations
; define length
(define length
  (lambda (lat)
    (cond 
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

; define pick
; (pick 0 '(a)) doesn't have answer
(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))


; define rempick
(define rempick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

; no-nums
; remove all numbers from lat
(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(no-nums '(5 pears 6 prunes 9 dates))

; on the contrast all-nums
(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(all-nums '(5 pears 6 prunes 9 dates))

; eqan?
; a generalized equality funcs take care of numbers
(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (= a b))
      ((or (number? a) (number? b)) #f)
      (else (eq? a b)))))

; occur
; count the number of times an atom a appears in a lat
(define occur
  (lambda (a lat)
    (cond 
      ((null? lat) 0)
      ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))

; define one?
(define one?
  (lambda (n) 
    (cond ((= 1 n) #t) (else #f))))

(define one? (lambda (n) (= 1 n)))

; rewrite rempick using one?
(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))) 
