(module def+ racket/base


	(provide def+ return return-rec) ; warning return and return-rec also provide elsewhere ! but it works....

	(require srfi/31 ;; for 'rec in def*
		 Scheme+/return
		 racket/stxparam
		 (for-syntax racket/base))


;;(def+ (foo) (when (2 < 3) (return "vrai")))

;;(foo)
;;"vrai"

(define-syntax def+

  (lambda (stx)
    
      (syntax-case stx ()

	;; multiple definitions without values assigned
	;; (def+ (x y z))
	;; TODO: remove? redundant with (declare x y z)
	((_ (var1 ...)) #`(begin (define var1 '()) ...))
	

	((_ (<name> <arg> ...) <body> <body>* ...)
	 
         
	 #'(define (<name> <arg> ...)

	     (call/cc

	      (lambda (ret-rec-id) ;(#,ret-rec-id)
		;; In the body we adjust the 'return-rec' keyword so that calls
		;; to 'return-rec' are replaced with calls to the escape
		;; continuation.

		(syntax-parameterize
		 ([return-rec (syntax-rules ()
				[(return-rec vals (... ...))
				 (ret-rec-id vals (... ...))])])
		 
		 (apply (rec <name> (lambda (<arg> ...)
				      
				      (call/cc

				       (lambda (ret-id) ;(#,ret-id)
					 ;; In the body we adjust the 'return' keyword so that calls
					 ;; to 'return' are replaced with calls to the escape
					 ;; continuation.
					 (syntax-parameterize
					  ([return (syntax-rules ()
						     [(return vals (... ...))
						      (ret-id vals (... ...))])])
					  ($nfx$ <body>)
					  ($nfx$ <body>*)
					  ...)))))
			
			(list <arg> ...)))))))



	;; variadic arguments in list
	((_ (<name> <arg> . L) <body> <body>* ...)
	 
         
	 #'(define (<name> <arg> . L)

	     (call/cc

	      (lambda (ret-rec-id) ;(#,ret-rec-id)
		;; In the body we adjust the 'return-rec' keyword so that calls
		;; to 'return-rec' are replaced with calls to the escape
		;; continuation.

		(syntax-parameterize
		 ([return-rec (syntax-rules ()
				[(return-rec vals (... ...))
				 (ret-rec-id vals (... ...))])])
		 
		 (apply (rec <name> (lambda (<arg> . L)
				      
				      (call/cc

				       (lambda (ret-id) ;(#,ret-id)
					 ;; In the body we adjust the 'return' keyword so that calls
					 ;; to 'return' are replaced with calls to the escape
					 ;; continuation.
					 (syntax-parameterize
					  ([return (syntax-rules ()
						     [(return vals (... ...))
						      (ret-id vals (... ...))])])
					  ($nfx$ <body>)
					  ($nfx$ <body>*)
					  ...)))))
			
			(cons <arg> . L)))))))



	

	;; single definition without a value assigned
	;; (def x)
	((_ var) #`(define var '()))

	;; (def x 7)
	((_ var expr) #`(define var expr))

	((_ err ...) #`(syntax-error "Bad def+ form"))

	)))


) ; end module
