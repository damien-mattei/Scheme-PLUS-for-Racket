(module list racket/base

	(provide singleton-list?)

	(define (singleton-list? lst)
	  (and (list? lst)
	       (not (null? lst))
	       (null? (cdr lst)))))




