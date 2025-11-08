(module lambda+ racket/base

	(provide lambda+)

	(require srfi/31 ;; for 'rec
		 Scheme+/return
		 racket/stxparam
		 Scheme+/nfx
		 Scheme+/def+
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
			(lambda (ret-id) ;(#,ret-id)
			  ;; In the body we adjust the 'return' keyword so that calls
			  ;; to 'return' are replaced with calls to the escape
			  ;; continuation.
			  (syntax-parameterize
			   ([return (syntax-rules ()
				      [(_ vals (... ...))
				       (ret-id vals (... ...))])])
			   ($nfx$-rec <body>)
			   ($nfx$-rec <body>*)
			   ...)))))	      

	      ;; variadic arguments in list
	      ((_ (<arg> . L) <body> <body>* ...)
         
	       #'(lambda (<arg> . L)
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

	      ;; variadic arguments in list
	      ((_ L <body> <body>* ...)
         
	       #'(lambda L
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
		       ...))))))))

	) ; end module


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
;;                     (when (< x 5)
;;                       (set! x (+ x 1))
;;                       (display "super!")(newline)
;;                       (bar))
;;                     (display "returning") (newline)
;;                     (return 17)
;;                     'not_good)))


;; (define foo
;;   (rec
;;    bar
;;    (lambda+
;;     ()
;;     (when (< x 5) (set! x (+ x 1)) (display "super!") (newline) (bar))
;;     (display "returning")
;;     (newline)
;;     (return 17)
;;     'not_good)))


;; #<eof>
;; > (foo)


;; (foo)
;; super!
;; super!
;; returning
;; returning
;; returning
;; 17

;; #<eof>
;; > 
