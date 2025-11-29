(module disjunction racket/base


	(provide ∣∣ )  ; this symbol is probably composed by other than | which is a reserved char in Racket, do not trust the apparence !

	(define (∣∣ f . r) ; there exist many vertical pipe in the charset !
	     
	   (cond ((null? r) f)
		 (f #t)
		 (else
		  (apply ∣∣ r))))

) ;  end library
