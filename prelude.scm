; assumed functions
(define atom? (lambda (x)
  (and (not (pair? x)) (not (null? x)))))

(atom? (quote ())) ; => #f