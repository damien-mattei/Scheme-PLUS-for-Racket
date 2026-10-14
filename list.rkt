(module list racket/base

	(provide singleton-list?
		 replace-many-deep
                 replace-many-deep-equality)

	(define (singleton-list? lst)
	  (and (list? lst)
	       (not (null? lst))
	       (null? (cdr lst))))

        ; AI generated :
	; (replace-many-deep '(1 (2 3 (a 2)) 3) '((2 . 99) (3 . 42) (a . z)))
	; '(1 (99 42 (z 99)) 42)
	(define (replace-many-deep s rules)
	  (define (lookup x rules)
	    (cond
	     [(null? rules) x]
	     [(equal? x (caar rules)) (cdar rules)]
	     [else (lookup x (cdr rules))]))
	  
	  (cond
	   [(pair? s)
	    (cons (replace-many-deep (car s) rules)
		  (replace-many-deep (cdr s) rules))]
	   [else (lookup s rules)]))

        (define (replace-many-deep-equality s rules equality?)
	  (define (lookup x rules)
	    (cond
	     [(null? rules) x]
	     [(equality? x (caar rules)) (cdar rules)]
	     [else (lookup x (cdr rules))]))
	  
	  (cond
	   [(pair? s)
	    (cons (replace-many-deep-equality (car s) rules equality?)
		  (replace-many-deep-equality (cdr s) rules equality?))]
	   [else (lookup s rules)]))

	)




