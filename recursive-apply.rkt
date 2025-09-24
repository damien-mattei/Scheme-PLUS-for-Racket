(module recursive-apply racket/base


	(provide recursive-apply)

	(require Scheme+/atom)


	(define (recursive-apply L)

	  (cond ((atom? L) L)
		;; now should be a list
		((null? L) L)
		((procedure? (car L)) ; something to evaluate
		 (apply (car L)
			(map recursive-apply (cdr L))))
		(else L))) ; a simple list of data
	
) ;  end library
