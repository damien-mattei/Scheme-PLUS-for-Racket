(module conjunction racket/base

	(provide && )

	(define (&& f . r)
	     
	   (cond ((null? r) f)
		 (f (apply && r))
		 (else #f)))
	
) ;  end library
