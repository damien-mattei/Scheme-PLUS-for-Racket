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
		 ANNOTATION-OFF
		 :print-type
		 is-print-type?
		 ℕ ℤ ℝ ℚ ℂ
		 natural-fixnum?
		 natural-type-name?
		 integer-type-name?
		 real-type-name?)

	(require Scheme+/operators
		 (for-syntax racket/base))

	;; reserved names of Scheme+ for the types : ℕ and ℤ are fixnum? (with the negative ones , bounded : not the sets of
	;; mathematics) , ℝ is flonum? ; as values for the predicates of the overloads chosen at run time ,
	;; (overload-operator + (vec?) add-vec (vec? ℝ)) ; the annotation reads them as fixnum? and flonum? (annot+.rkt)
	;; a natural number that is a fixnum : the type ℕ (an index of a vector or a string , without the negative indexes)
	(define (natural-fixnum? x)
	  (and (fixnum? x) (>= x 0)))

	#;(define ℕ fixnum?)
	(define ℕ natural-fixnum?)
	(define ℤ fixnum?)
	(define ℝ flonum?)
	;; only defined for now , not used by the annotation
	(define ℚ rational?)
	(define ℂ complex?)

	;; the names of the types fixnum? and flonum? (for the annotation , annot+.rkt , which works on symbols)
	#;(define (fixnum-type-name? s)
	  (and (memq s '(ℕ ℤ)) #t))

	#;(define (flonum-type-name? s)
	  (eq? s 'ℝ))

	;; one predicate for each name , to distinguish ℕ and ℤ later
	(define (natural-type-name? s) ; today the type fixnum?
	  (eq? s 'ℕ))

	(define (integer-type-name? s) ; today the type fixnum?
	  (eq? s 'ℤ))

	(define (real-type-name? s) ; today the type flonum?
	  (eq? s 'ℝ))

	;; (:print-type expr) is a command of the REPL , done by the reader (SRFI-105 or SRFI-110) with the tables of the type
	;; annotation , as :print-type of Typed Racket ; elsewhere (in a module) it is an error
	(define (is-print-type? s) ; for the readers : (:print-type expr) typed at the REPL
	  (eq? s ':print-type))

	(define-syntax (:print-type stx)
	  (raise-syntax-error ':print-type "works only at the REPL (with ANNOTATION-ON)" stx))
	
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

	(define numerical-types '(exact-integer? integer? number? flonum? real? rational? complex? inexact? fixnum? exact?
				  natural-fixnum?))

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

