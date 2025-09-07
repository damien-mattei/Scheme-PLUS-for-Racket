(module conjonction racket/base


	(provide && )

	(require Scheme+/def)


	(def (&& f . r)
	     
	   (when (null? r)
	     (return f))
	     
	   (if f
	       (apply && r)
	       #f))

) ;  end library
