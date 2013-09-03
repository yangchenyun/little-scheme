; looking will keep looking for atom pointed by number
; when it finds one, it compares with a and returns #t
; if it eq?s a.
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond 
      ((number? sorn)
        (keep-looking a (pick sorn lat) lat))
      ; sorn is an non-numeric number
      (else (eq? a sorn)))))

(looking 'caviar '(6 2 grits caviar 5 7 3))

; introduce the partial functions
; 1. it doesn't recur on lat
; 2. it doesn't reach its goal for some of its arguments
(define eternity
  (lambda (x)
    (eternity x)))


(shift '((a b) c)) ; => (a (b c))
(shift '((a b) (c d))) ; => (a (b (c d)))

; shift takes a pair whose first component is a pair
; and returns a new pair by shift the 2nd component of the 
; first pair into the second components
(define shift
  (lambda (pair)
    (cons (car (car pair))
      (cons (cons 
        (car (cdr (car pair))) 
        (cdr pair)) '()))))

; change representations
(define shift
  (lambda (pair)
    (build (first (first pair))
      (build (second (first pair))
        (second pair)))))

; the align functions
(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
        (align (shift pora)))
      (else (build (first pora)
              (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else
        (+ 
          (length* (first pora))
          (length* (second pora)))))))

(length* '((a b) (c d)))

; but the first components are becoming simplier for each recursion.

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
        (shuffle (revpair pora)))
      (else (build (first pora)
              (shuffle (second pora)))))))

(shuffle '(a (b c)))
(shuffle '((a b) (c d)))

; Lothar Collatz
(define C
  (lambda (n)
    (cond
      ((one? n) 1)
      ((even? n) (C (/ n 2)))
      (else (C (add1 (x 3 n)))))))

; Wilhelm Ackermann
(define A
  (lambda (n m)
    (cond 
      ((zero? n) (add1 m))
      ((zero? m) (A (sub1 n) 1))
      (else (A (sub1 n)
              (A n (sub1 m)))))))

; write functions to detect partial / total functions
(define will-stop?
  (lambda (f) 
    (...)))

; failed attempt for function:
(define try-to-stop
  (lambda (x)
    (and (will-stop? try-to-stop)
      (eternity x))))

; fun only returns for length 0
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))

; fun only returns for length <= 1
(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (length0 (cdr l))))))

; replace length0 with its definition
(lambda (l)
  (cond
    ((null? l) 0)
    (else 
      (add1 
        ((lambda (l) 
          (cond
            ((null? l) 0)
            (else (add1 (eternity (cdr l)))))) 
        (cdr l))))))

; do it again for length <= 2
(lambda (l)
  (cond
    ((null? l) 0)
    (else
      (add1 
        ((lambda (l)
          (cond
            ((null? l) 0)
            (else 
              (add1 
                ((lambda (l) 
                  (cond
                    ((null? l) 0)
                    (else (add1 
                            (eternity 
                              (cdr l)))))) 
                (cdr l))))))
        (cdr l))))))

; length 0
((lambda (length) 
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
eternity)

; length <= 1
((lambda (length) 
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
  ((lambda (length) 
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
  eternity))

; length <= 2
((lambda (length) 
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
  ((lambda (length) 
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
    ((lambda (length) 
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    eternity)))

; extract with mk-length
; takes length as a function
; and returns a similar function

((lambda (mk-length)
  (mk-length eternity))
 (lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

; for legnth <= 1
((lambda (mk-length)
  (mk-length
    (mk-length eternity)))
 (lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

; for length <= 2
((lambda (mk-length)
  (mk-length
    (mk-length 
      (mk-length eternity))))
 ; apply the function on itself
 (lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

; pass mk-length the arbitary function initially
; this is still length0, why?
; because the arbitary function passed to mk-length initially will not invoked at all
((lambda (mk-length)
  (mk-length mk-length)) 
 (lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 
              (length (cdr l))))))))

; it returns a functions
; where length is an arbitary function which won't be invoked at all
;(lambda (l)
;  (cond
;    ((null? l) 0)
;    (else (add1 
;            (length (cdr l))))))


; now, rename length to mk-length to remind us 
; mk-length is passed in as first function
((lambda (mk-length)
  (mk-length mk-length)) 
 (lambda (mk-length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 
              (mk-length (cdr l))))))))

; we apply mk-length one more time to get 
; fn: length <= 1
(((lambda (mk-length)
    (mk-length 
      (mk-length
        mk-length))) 
   (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 
                (mk-length (cdr l)))))))) 
'(apples))

; recursively apply to mk-length to itself
; just about to expire
((lambda (mk-length)
    (mk-length mk-length)) 
   (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 
                ((mk-length mk-length) (cdr l))))))))

; extract the part to build 'length'
; but now it never stops
((lambda (mk-length)
    (mk-length mk-length))  
   (lambda (mk-length)
    ((lambda (length) 
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 
                  (length (cdr l)))))))
    (mk-length mk-length))))

; a step walk through
; first recursion, applies the arbitary func (lambda (mk-length) ...)
((lambda (mk-length)
    ((lambda (length) 
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 
                  (length (cdr l)))))))
    (mk-length mk-length)))
  (lambda (mk-length)
      ((lambda (length) 
        (lambda (l)
          (cond
            ((null? l) 0)
            (else (add1 
                    (length (cdr l)))))))
      (mk-length mk-length))))
; now it becomes 
((lambda (length) 
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 
                  (length (cdr l)))))))
    (mk-length mk-length))
; where mk-length is
(lambda (mk-length)
      ((lambda (length) 
        (lambda (l)
          (cond
            ((null? l) 0)
            (else (add1 
                    (length (cdr l)))))))
      (mk-length mk-length)))

; and again and again...


; let's try out another apporach
((lambda (mk-length)
    (mk-length mk-length)) 
   (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 
                ((lambda (x) 
                  ((mk-length mk-length) x)) 
                 (cdr l))))))))

; extract the part of length
((lambda (mk-length)
    (mk-length mk-length)) 
   (lambda (mk-length)
    ((lambda (length) 
      (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 
                (length (cdr l)))))))
      (lambda (x) 
        ((mk-length mk-length) x)))))

; extract the part that 'mk-length'
((lambda (le)
  ((lambda (mk-length)
    (mk-length mk-length))
  (lambda (mk-length)
    (le (lambda (x)
          ((mk-length mk-length) x))))))
 (lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))
