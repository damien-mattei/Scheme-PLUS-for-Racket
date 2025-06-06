#lang reader SRFI-105


(module test-overload racket


	(require Scheme+)
	
	(define-overload-existing-n-arity-operator +)

	(define (add-n-lists . vn-lst) (implementation-add-n-lists vn-lst))

	(define (implementation-add-n-lists vn-lst)
	  (define plus +)
	  {map-args <- (cons plus vn-lst)}
	  (apply map map-args))

	(overload-existing-n-arity-operator + add-n-lists (list? list?))

	(display "+ =") (display +) (newline)

	
	 ;; {expr <- '(⊕ (· B1 B0) (· C1 (⊕ B1 B0)))}
	 ;; (display expr)
	 (newline)

	 (+ '(1 2) '(3 4))



	)
