(module in-equalities racket/base


	(provide in-equality?)

	(require SRFI-105/SRFI-105-curly-infix
		 Scheme+/operators) ; for alternating-parameters


  ;; predicate to test if an infix expression is an in/equality, ex: (x < 3 < y <= z)
  (define (in-equality? s)
    (and (>= (length? s) 3)
	 (andmap IN-EQUALITY-op? (alternating-parameters s))))


  ) ; end module
