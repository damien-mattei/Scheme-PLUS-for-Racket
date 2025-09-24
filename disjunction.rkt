(module disjunction racket/base


	(provide ∣∣ )

	(define (∣∣ f . r)
	     
	   (cond ((null? r) f)
		 (f #t)
		 (else
		  (apply ∣∣ r))))

) ;  end library
