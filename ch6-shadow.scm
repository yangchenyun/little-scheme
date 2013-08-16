; definition of arithmetic expression
; - atom of numbers
; - atom of numbers with operation +, x, ^

; introduce numbered?
; it determines whether a list is a representation of arithmetic expression

(numbered? '(1 + 2)) ; #t
(numbered? '(1 + (2 x 3))) ; #t
(numbered? '((1 ^ 2) + (2 x 3))) ; #t
(numbered? '(+ (2 x 3))) ; #f

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? (car aexp)) (number? (car aexp)))
      (else (and 
              (numbered? (car aexp))
              (numbered? (car (cdr (cdr aexp)))))))))

; introduce value?
; it takes an numbered expression and returns its natural value
(define value
  (lambda (aexp) 
    (cond
      ((atom? aexp) aexp)
      ((eq? '+ (car (cdr aexp))) 
        (+ 
          (value (car aexp))
          (value (car (cdr (cdr aexp))))))
      ((eq? 'x (car (cdr aexp))) 
        (x 
          (value (car aexp))
          (value (car (cdr (cdr aexp))))))
      ((eq? '^ (car (cdr aexp))) 
        (^ 
          (value (car aexp))
          (value (car (cdr (cdr aexp)))))))))

(value '(1 + 2)) ; #t
(value '(1 + (2 x 3))) ; #t
(value '((5 ^ 2) + (2 x 3))) ; #t

; another value functions that takes form 
; (+ 1 2)
(define newvalue
  (lambda (aexp) 
    (cond
      ((atom? aexp) aexp)
      ((eq? '+ (car aexp)) 
        (+ 
          (value (car (cdr aexp)))
          (value (car (cdr (cdr aexp))))))
      ((eq? 'x (car aexp)) 
        (x 
          (value (car (cdr aexp)))
          (value (car (cdr (cdr aexp))))))
      ((eq? '^ (car aexp)) 
        (^ 
          (value (car (cdr aexp)))
          (value (car (cdr (cdr aexp)))))))))

(newvalue '(+ 1 2)) ; #t
(value '(1 + (2 x 3))) ; #t
(value '((5 ^ 2) + (2 x 3))) ; #t

; extract helper functions to hide representations
(define first_sub_exp
  (lambda (aexp) 
    (car (cdr aexp))))

(define second_sub_exp
  (lambda (aexp) 
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

; simplify the new value
(define newvalue
  (lambda (aexp) 
    (cond
      ((atom? aexp) aexp)
      ((eq? '+ (operator aexp))
        (+ 
          (value (first_sub_exp aexp))
          (value (second_sub_exp aexp))))
      ((eq? 'x (operator aexp))
        (x 
          (value (first_sub_exp aexp))
          (value (second_sub_exp aexp))))
      ((eq? '^ (operator aexp))
        (^ 
          (value (first_sub_exp aexp))
          (value (second_sub_exp aexp)))))))

; build another number representation system using ()
; (): 0
; (()): 1
(define sero?
  (lambda (n) (null? n)))

(define edd1
  (lambda (n) (cons '() n)))

(define zub1
  (lambda (n) (cdr n)))

(define +
  (lambda (a b)
    (cond 
      ((sero? b) a)
      (else (+ (edd1 a) (zub1 b))))))

(define -
  (lambda (a b)
    (cond 
      ((sero? b) a)
      (else (- (zub1 a) (zub1 b))))))

(define x
  (lambda (a b)
    (cond 
      ((sero? b) '())
      (else (+ a (x a (zub1 b)))))))

(+ '(()) '(() ()))
(- '(() () ()) '(() ()))
(x '(() () () () ()) '(() () () ()))
