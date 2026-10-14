(module type racket/base

	(provide type
		 is-type?
		 is-type-expression?
		 all-numerical-types?
		 numerical-type?
		 numerical-types
		 all-numerical-types-or-unknow?
		 some-numerical-types?
		 litteral?
		 litteral-list?
		 all-numbers?
		 ANNOTATION-ON
		 ANNOTATION-OFF)

	(require Scheme+/operators)
	
	(define-syntax type
	  
	  (syntax-rules ()
	    
	    ((_ arg1 ...)
	     (begin
	       (display "type: Warning : some type annotation has been skipped by the type annotation parser. Parser code must be checked and fixed , here is the critical annotation: ") (display '(type arg1 ...)) (newline) ; quoted : a composite type like (flonum?) must not be evaluated
	     #;(void)))))

	(define err-port (current-error-port))
	
	(define (is-type? s)
	  (eq? s 'type))

	(define (is-type-expression? s)
	  (and (pair? s) (is-type? (car s))))

	;; in annot+ integers (exact integer) are "filtered" before reals
	;;(define integer exact-integer?)
	;;(define real inexact?)

	(define numerical-types '(exact-integer? integer? number? flonum? real? rational? complex? inexact? fixnum? exact?))

	(define (numerical-type? t)
	  (memq t numerical-types))

	(define (numerical-type-or-unknow? t)
	  (or (not t)
	      (memq t numerical-types)))

	(define (some-numerical-types? L)
	  (ormap numerical-type? L))
	
	(define (all-numerical-types? L)
	  ;(display "type : L=" err-port) (display L err-port) (newline err-port)
	  (define rv (andmap numerical-type? L))
	  ;(display "type : rv=" err-port) (display rv err-port) (newline err-port)
	  rv)


	(define (all-numerical-types-or-unknow? L)
	  (andmap numerical-type-or-unknow? L))

	(define (all-numbers? L)
          (andmap number? L))

	;; litteral? : test if s is a Scheme litteral, that is a self-evaluating datum
	;; (a datum which is not evaluated as a variable reference or a procedure call)
	;; or an explicitly quoted/quasiquoted datum.
	;; examples: (litteral? 3)        --> #t
	;;           (litteral? 3.14)     --> #t
	;;           (litteral? "hello")  --> #t
	;;           (litteral? #t)       --> #t
	;;           (litteral? #\a)      --> #t
	;;           (litteral? #(1 2 3)) --> #t
	;;           (litteral? '())      --> #t
	;;           (litteral? 'x)       --> #f  ; x is a symbol/variable, not a litteral by itself
	;;           (litteral? '(quote x))       --> #t  ; quoted datum is a litteral at runtime
	;;           (litteral? '(+ 1 2))         --> #f  ; procedure call, not a litteral
	(define (litteral? s)
	  (or (boolean? s)
	      (number? s)
	      (string? s)
	      (char? s)
	      (vector? s)
	      (hash? s)
	      (bytes? s)
	      (null? s)
	      (and (pair? s)
		   (quote-or-quasi? (car s)))))


	;; litteral-list? : test if lst is a list whose elements are all litterals
	;; examples: (litteral-list? '(1 2.0 "a" #t))  --> #t
	;;           (litteral-list? '(1 x 3))         --> #f  ; x is a symbol/variable
	(define (litteral-list? lst)
	  (andmap litteral? lst))

	(define ANNOTATION-ON 'ANNOTATION-ON)
	(define ANNOTATION-OFF 'ANNOTATION-OFF)


	)

