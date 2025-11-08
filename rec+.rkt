(module rec+ racket/base

	(provide rec+) 

	(require srfi/31
		 Scheme+/nfx
		 Scheme+/lambda+)

;; (define f (rec+ foo (lambda () (2 + 3))))
;; (f)
;; 5
	
(define-syntax rec+
  (syntax-rules ()
   
    ((_ (name . formals) body ...)                ; procedure
     (letrec ((name (lambda+ formals body ...)))
       name))
    ((_ name expr)                                ; arbitrary object
     (letrec ((name ($nfx$-rec expr)))
       name))))
)
