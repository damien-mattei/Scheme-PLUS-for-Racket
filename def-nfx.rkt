;; This file is part of Scheme+

;; Copyright 2021-2024 Damien MATTEI

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.





(module def-nfx racket/base


	(provide define
		 define-infix)

	(require (only-in racket/base [define define-scheme]) ;; backup original Scheme 'define'
		 Scheme+/nfx)


		

	;; > (define z  3 * 5 + 2)

	;; (define z 3 * 5 + 2)
	;; $nfx$ : parsed-args=(.#<syntax +> (.#<syntax *> .#<syntax 3> .#<syntax 5>) .#<syntax 2>)

	;; #<eof>
	;; > z

	;; z
	;; 17

	;;  (define x   - 3)
	;; x
	;; -3


	
	
	(define-syntax define 

	  (syntax-rules ()

	    ;; original define for procedures
	    ((define (name arg ...) body ...)
	     (define-scheme (name arg ...) body ...))

	    ((define (name arg ... . rest-id) body ...)
	       (define-scheme (name arg ... . rest-id) body ...))



	    ;; new define for infix

	    ;; (define x   - 3)
	    ;; x
	    ;; -3

	    

	    ;; do not work for unknown reason (in R6RS, will check in Racket)
	    ;; ((define name expr)
	    ;;  ;;(define-scheme name expr))
	    ;;  (define-scheme name ($nfx$ expr)))
	    ;;  ;; (define-scheme name (recall-infix-parser expr
	    ;;  ;; 					      infix-operators-lst-for-parser
	    ;;  ;; 					      (lambda (op a b) (list op a b)))))


	    ;; infix define
	    ;; ((define name arg1 arg2 arg3 ...)
	    ;;  (define-scheme name ($nfx$ arg1 arg2 arg3 ...)))))

	    ;; (define x   3 + 2 + 1)
	    ;; x
	    ;; 6

	    ;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	    ;; (define a  (cdr (cons + 1)) + 2)
	    ;; exception raised by error display handler: srcloc: contract violation
	    ;;   expected: (or/c exact-positive-integer? #f)
	    ;;   given: 0; original exception raised: +: contract violation
	    ;;   expected: number?
	    ;;   given: #<procedure:mcons>

	    ;; instead use:
	    ;;  (define a  (+ (cdr (cons + 1)) 2))
	    ;; a
	    ;; 3

	    ;; !!!!!!!!!!!!!!!!!!!
	    ;; (define y sin 0.34)
	    ;; y
	    ;; 0.3334870921408144
	    
	    ;; at least 2 arguments
	    ((define name   arg1 arg2 arg3 ...)
	     (define-scheme name ($nfx$ arg1 arg2 arg3 ...)))

	    ;; recall the 'define' of Scheme RNRS
	    ((define name arg1)
	     (define-scheme name arg1))
	    
	    ;; zero argN, juste the name
	    ((define name)
	     (define-scheme name '()))

	    ))


	
	;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	;; (define-infix a   (cdr (cons + 1)) + 2)
	;; ../../../../../../racket/collects/racket/private/kw.rkt:1260:25: +: contract violation
	;; expected: number?
	;; given: #<procedure:cons>

	;; (define op+ +)
	;; (define-infix a   (cdr (cons op+ 1)) + 2)
	;; a
	;; 3

	(define-syntax define-infix

	  (syntax-rules ()

	    ;; not original define for procedures
	    ((define-infix (name arg ...) body ...)
	     (define-scheme (name arg ...) ($nfx$ body) ...))

	    ((define-infix (name arg ... . rest-id) body ...)
	     (define-scheme (name arg ... . rest-id) ($nfx$ body) ...))


	    ;; new define for infix
	    
	    ;; (define-infix a  (cos (3 * .3 * 4 / 5)))
	    ;; a
	    ;; 0.751805729140895

	    ;; (define-infix x   - 3)
	    ;; x
	    ;; -3

	    ;; (define-infix x (3 + 2 + 1))
	    ;; x
	    ;; 6

	    ;; (define-infix x   3 + 2 + 1)
	    ;; x
	    ;; 6

	    ;; (define-infix a  (cos (2 * .3)))
	    ;; a
	    ;; 0.8253356149096783
	   	    
	    ;; at least 1 arguments
	    ((define-infix name    arg1 arg2 ...)
	     (define-scheme name ($nfx$ arg1 arg2 ...)))

	    
	    ;; zero argN, juste the name
	    ((define-infix name)
	     (define-scheme name '()))

	    ))

	)
  
