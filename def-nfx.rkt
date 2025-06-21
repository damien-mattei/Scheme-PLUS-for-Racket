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
		 define+)

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

	;; (define+ x (5 - 3) ⁴)
	;; 16

	;; (define x (5 - 3) ⁴)
	;; 16
	
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



	;; rationale of define+ is ,first for function definitions, and when only one argument (after symbol) that require to be parsed
	;; (not possible with define without rejecting then a few normal scheme expression)
	;; see examples below

	
	;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	;; (define+ a   (cdr (cons + 1)) + 2)
	;; ../../../../../../racket/collects/racket/private/kw.rkt:1260:25: +: contract violation
	;; expected: number?
	;; given: #<procedure:cons>

	;; (define op+ +)
	;; (define+ a   (cdr (cons op+ 1)) + 2)
	;; a
	;; 3

	;; (define+ (foo x) ((x - 3) ²))
	;; (foo 5)
	;; 4

	;; (define+ (foo x y) ((x - 3) ² + (y - 2) ²))


	(define-syntax define+

	  (syntax-rules ()

	    ;; not original define for procedures
	    ((define+ (name arg ...) body ...)
	     (define-scheme (name arg ...) ($nfx$ body) ...))

	    ((define+ (name arg ... . rest-id) body ...)
	     (define-scheme (name arg ... . rest-id) ($nfx$ body) ...))


	    ;; new define for infix
	    
	    ;; (define+ a  (cos (3 * .3 * 4 / 5)))
	    ;; a
	    ;; 0.751805729140895

	    ;; (define+ x   - 3)
	    ;; x
	    ;; -3

	    ;; (define+ x (3 + 2 + 1))
	    ;; x
	    ;; 6

	    ;; (define+ x   3 + 2 + 1)
	    ;; x
	    ;; 6

	    ;; (define+ a  (cos (2 * .3)))
	    ;; a
	    ;; 0.8253356149096783
	   	    
	    ;; at least 1 arguments, so (define+ name arg1) is parsed here
	    ((define+ name    arg1 arg2 ...)
	     (define-scheme name ($nfx$ arg1 arg2 ...)))

	    
	    ;; zero argN, just the name
	    ((define+ name)
	     (define-scheme name '()))

	    ))

	)
  
;; > (define x  1 + 2 + 3)


;; (define x 1 + 2 + 3)


;; #<eof>
;; > x


;; x
;; 6


;; #<eof>
;; > (define y  - (sin .3))


;; (define y - (sin 0.3))


;; #<eof>
;; > y


;; y
;; -0.29552020666133955


;; #<eof>
;; > (define z  3 · (2 ³ - 1))


;; (define z 3 · (2 ³ - 1))


;; #<eof>
;; > z


;; z
;; 21


;; #<eof>
;; > (define k  10.0 - 3.0 - 4.0 + 1 - 5.0 * 2.0 ** 3.0 / 7.0 ** 3.0)


;; (define k 10.0 - 3.0 - 4.0 + 1 - 5.0 * 2.0 ** 3.0 / 7.0 ** 3.0)


;; #<eof>
;; > k


;; k
;; 3.883381924198251


;; #<eof>






;; Welcome to DrRacket, version 8.17 [cs].
;; Language: racket, with debugging; memory limit: 14000 MB.
;; > (require Scheme+)

;; Scheme+ v10.0 by Damien Mattei

;; > (define x  3 * 5 + 2)
;; > x
;; 17
;; > (define x  1 + 2 + 3)
;; > x
;; 6
;; > (define k  10.0 - 3.0 - 4.0 + 1 - 5.0 * 2.0 ** 3.0 / 7.0 ** 3.0)
;; > k
;; 3.883381924198251
;; > (define z  (3 + 1) * (2 * (+ 2 1) - (sin 0.3)) + ((* 2 5) - 5))
;; > z
;; 27.817919173354642
;; > (define t 3 * (+ 2 4) - 1)
;; > t
;; 17
;; > (define a 7)
;; > (define b 3)
;; > (define r  a * - b)
;; > r
;; -21
;; >



;; > (define s  3 ² + 2 · 3 · 5 + 5 ²)
;; > s
;; 64
;; > (define s  (3 ² + 2 · 3 · 5 + 5 ²))
;; . . ²: undefined;
;;  cannot reference an identifier before its definition
;; > (define s  3 ² + 2 * 3 * 5 + 5 ²)
;; > s
;; 64
