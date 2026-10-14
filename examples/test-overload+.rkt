#lang reader SRFI-105


(module test-overload racket


	(require Scheme+)

	ANNOTATION-ON

	(define (add-n-lists . vn-lst) (implementation-add-n-lists vn-lst))

	(define (implementation-add-n-lists vn-lst)
	  (define plus +)
	  {map-args <- (cons plus vn-lst)}
	  (apply map map-args))

	(overload-operator + (list?) add-n-lists (list?)) ; n-ary : any number of lists, returns a list

	(display "+ =") (display +) (newline)

	
	 ;; {expr <- '(⊕ (· B1 B0) (· C1 (⊕ B1 B0)))}
	 ;; (display expr)
	 (newline)

	 (+ '(1 2) '(3 4))



	)
