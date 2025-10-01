(module lambda+ racket/base

	(provide lambda+)

	(require srfi/31 ;; for 'rec
		 Scheme+/return
		 racket/stxparam
		 Scheme+/nfx
		 (for-syntax racket/base))


	;; (define foo (lambda+ () 7))
	;; (foo)
	;; 7

	;; > (define foo (lambda+ () (return (3 * 5 + 2))
	;;                        'not_good))
	;; (define foo (lambda+ () (return (3 * 5 + 2)) 'not_good))
	;; #<eof>
	;; > (foo)
	;; (foo)
	;; 17
	;; #<eof>
	(define-syntax lambda+

	  (lambda (stx)
    
	    (syntax-case stx ()

	      ((_ (<arg> ...) <body> <body>* ...)
	 
         
	       #'(lambda (<arg> ...)

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
						;;(display "def+.rkt : def+ : <body> =") (display <body>) (newline)
						($nfx$-rec <body>)
						($nfx$-rec <body>*)
						...)))))
			      
			      (list <arg> ...)))))))



	      ;; variadic arguments in list
	      ((_ (<arg> . L) <body> <body>* ...)
	       
               
	       #'(lambda (<arg> . L)

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
						($nfx$-rec <body>)
						($nfx$-rec <body>*)
						...)))))
			      
			      (cons <arg> . L))))))) ; this way we get the full list of arguments
	      
	))))


;; > (define loo (lambda+ ()
;;                        ((lambda+ () 3))))                
;; (define loo (lambda+ () ((lambda+ () 3))))
;; #<eof>
;; > (loo)
;; (loo)
;; 3





;; > (define x 3)


;; (define x 3)


;; #<eof>
;; > (define foo (rec bar (lambda+ ()
;;                                 (when (x = 3)
;;                                   {x := x + 1}
;;                                   (display "super!")(newline)
;;                                   (bar))
;;                                 (return (3 * 5 + 2))
;;                                 'not_good)))


;; (define foo
;;   (rec
;;    bar
;;    (lambda+
;;     ()
;;     (when (x = 3) (:= x (+ x 1)) (display "super!") (newline) (bar))
;;     (return (3 * 5 + 2))
;;     'not_good)))


;; #<eof>
;; > (foo)


;; (foo)
;; super!
;; 17


;; #<eof>
;; > 


 ;; (define x 3)

 ;; (define foo (rec bar (lambda+ ()
 ;;                                (when (x < 5)
 ;;                                  {x := x + 1}
 ;;                                  (display "super!")(newline)
 ;;                                  (bar))
 ;;                                (return-rec (3 * 5 + 2))
 ;;                                'not_good)))


;;(foo)
;;super!
;;super!
;;17
