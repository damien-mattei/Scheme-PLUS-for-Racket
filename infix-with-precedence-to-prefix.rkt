;; infix with precedence to prefix

;; This file is part of Scheme+

;; Copyright 2024 Damien MATTEI

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


;; code from Scheme+R6RS


(module infix-with-precedence-to-prefix racket/base

  (provide !*prec-generic-infix-parser
	   recall-infix-parser)

  (require ;(only-in srfi/1 any)
	   Scheme+/syntax
	   Scheme+/prefix
	   Scheme+/operators-list
	   Scheme+/operators
	   Scheme+/infix
	   SRFI-105/SRFI-105-curly-infix) ; for alternating-parameters
	  

  ;; procedures work with quoted expression and syntax expressions



  
;; evaluate one group of operators in the list of terms
(define (!**-generic-infix-parser terms stack operators #;odd? creator)

  ;; (display "!** : terms = ") (display terms) (newline)
  ;; (display "!**-generic-infix-parser : operators = ") (display operators) (newline)
  ;; (display "!** : stack = ") (display stack) (newline)
  ;;(display "!** : odd? = ") (display odd?) (newline) (newline)

					; why `odd?`? because scheme's list-iteration is forwards-only and
					; list-construction is prepend-only, every other group of operators is
					; actually evaluated backwards which, for operators like / and -, can be a
					; big deal! therefore, we keep this flipped `odd?` counter to track if we
					; should flip our arguments or not


  ;; inner definition ,odd? is variable like a parameter
  (define (calc-generic-infix-parser op a b)
    ;;(if odd? (list op a b) (list op b a)))
    
    (define rv ;;(if odd?
		   (creator op a b))
		   ;;(creator op b a))) ; at the beginning odd? is #f
    ;; (display "calc-generic-infix-parser : rv =") (display rv) (newline)
    rv)
  

  ;; executed body of procedure start here
  
  (cond ((and (null? terms)
	      (not (memq 'expt operators))
	      (not (member-syntax #'expt operators)))

	  ;; (display "!**-generic-infix-parser cond case 1 : stack =")
	  ;; (display stack)
	  ;; (newline)
	  ;; (display "!**-generic-infix-parser : terms =")
	  ;; (display terms)
	  ;; (newline)
	  (let ((rs (reverse stack))) ; base case, stack is the result, we return the reverse because
					; scheme's list-iteration is forwards-only and
					; list-construction is prepend-only
	    ;; (display "!**-generic-infix-parser : rs =")
	    ;; (display rs)
	    ;; (newline)
	    rs))

	((null? terms)
	 ;; (display "!**-generic-infix-parser cond case 2 : stack =")
	 ;; (display stack)
	 ;; (newline)
	 ;; (display "!**-generic-infix-parser : terms =")
	 ;; (display terms)
	 ;; (newline)
	 stack) ; here we get 'expt (see previous test) then we do not reverse because we
	        ; start reversed and then went right->left

	
	;; condition
	;; operator we can evaluate -- pop operator and operand, then recurse
	((and (> (length stack) 1) ; stack length at least 2 : b op
	      ;; (begin
	      ;;   (display "!** : operators=") (display operators) (newline)
	      ;;   (let* ((op (car stack))
	      ;; 	  (mres (memq op operators)))
	      ;;     (display "op=") (display op) (newline)
	      ;;     (display "mres=") (display mres) (newline) (newline)
	      ;;     mres)))

	      ;; test the finding of operator in precedence list
	      (or
	       (memq (car stack) operators) ; find an operator of the same precedence
	       (member-syntax (car stack) operators)))	 ;  syntaxified !

	 
	 ;; body if condition is true : found an operator of the same precedence
	 (let* ((op (car stack)) ; get back the operator from the stack ... a op
		(b (car terms)) ; b
		(a (cadr stack)) ; a
		(calculus (begin
			    ;;(display "!**-generic-infix-parser : a=") (display a) (newline)
			    ;;(display "!**-generic-infix-parser : b=") (display b) (newline)
			    ;;(display "checking exponential for calculus...")(newline)
			    (if (or (memq 'expt operators) ; testing for exponential (expt or **)
				    (member-syntax #'expt operators))
			      (calc-generic-infix-parser op b a) ; op equal expt or **
			      (calc-generic-infix-parser op a b)))))
	   
	   ;;(display "op=") (display op) (newline)
	   
	   (!**-generic-infix-parser (cdr terms) ; forward in terms
			(cons calculus ; put the result in prefix notation on the stack
			      (cddr stack)) 
			operators
			;;odd? ;(not odd?)
			creator)))


	
	;; otherwise just keep building the stack, push at minima : a op from a op b  
	(else
       
	 (!**-generic-infix-parser (cdr terms) ;  forward in expression
		      (cons (car terms) stack) ; push first sub expression on stack
		      operators ; always the same operator group
		      ;;odd?;(not odd?)
		      creator))))



;; deal with simple infix with same operator n-arity
;; check we really have infix expression before
;; wrap a null test
(define (pre-check-!*-generic-infix-parser terms operator-precedence creator)

  ;; pre-check we have an infix expression because parser can not do it
  (when (not (infix? terms operators-lst-syntax))
 	(error "pre-check-!*-generic-infix-parser  : arguments do not form an infix expression :terms:"
	       terms))
  

  (if (null? terms) ;; never for infix as there is e1 op1 e2 op2 e3 at least
	terms
	(!*-generic-infix-parser (reverse terms) ; start reversed for exponentiation (highest precedence operator)
		    operator-precedence
		    creator)))





;; evaluate a list of groups of operators in the list of terms - forward in operator groups
(define (!*-generic-infix-parser terms operator-groups #;odd? creator)
  ;; (display "!*-generic-infix-parser : terms = ") (display terms) (newline)
  ;; (display "!*-generic-infix-parser : operator-groups = ") (display operator-groups) (newline) (newline)
  (if (or (null? operator-groups) ; done evaluating all operators
	  (null? (cdr terms)))    ; only one term left
      terms ; finished processing operator groups
      
      ;; evaluate another group -- separating operators into groups allows
      ;; operator precedence

      ;; recursive tail call
      (let ((rv-tms (!**-generic-infix-parser terms '() (car operator-groups) #;odd? creator) ))
	;; (display "!*-generic-infix-parser : rv-tms =")
	;; (display rv-tms)
	;; (newline)
	
	(!*-generic-infix-parser rv-tms; this forward in terms 
		    (cdr operator-groups) ;  rest of precedence list , this forward in operator groups of precedence ,check another group
		    ;;(not odd?)
		    creator))))




(define (recall-infix-parser expr operator-precedence creator)

    (define expr-inter #f) ; intermediate variable

    ;;(display "recall-infix-parser : expr =") (display expr) (newline)

    (when (syntax? expr)
      ;;(display "recall-infix-parser : detected syntax,passing from syntax to list (will be used if it is a list)") (newline)
      (set! expr-inter (syntax->list expr))
      (when expr-inter
	;;(display "recall-infix-parser : got a list") (newline)
	(set! expr expr-inter)))

    
    ;;(display "recall-infix-parser : expr= ") (display expr) (newline)
    ;;(display "recall-infix-parser : (list? expr)= ") (display (list? expr)) (newline)


    (cond ((not (list? expr)) ; atom
	   ;;(display "recall-infix-parser : expr not list.") (newline)
	   expr)
	  
	  ((null? expr)
	   expr)

	  ((null? (cdr expr))
	   expr)

	  ;; could have be replaced by next case (prefix? ...)
	  ((datum=? '$nfx$ (car expr)) ; test {e1 op1 e2 ...}
	   expr)
	       
	  ((prefix? expr) ; test (proc1 arg0 arg1 ...)
	   (cons (car expr)
		 (map (lambda (x) (recall-infix-parser x operator-precedence creator))
		      (cdr expr))))
	   ;;expr)

	  (else
	   ;;(define expr-d
	     (car ;  probably because the result will be encapsuled in a list !
	      (!*prec-generic-infix-parser expr operator-precedence creator))) ; recursive call to the caller
	  ;;(display "expr-d=")(display expr-d)(newline)
	  ;;expr-d)
	  ))






;; this is generally the main entry routine
(define (!*prec-generic-infix-parser terms  operator-precedence creator)   ;; precursor of !*-generic-infix-parser

  ;;(display "!*prec-generic-infix-parser : terms=") (display terms) (newline)
  ;;(display "!*prec-generic-infix-parser : operator-precedence=") (display operator-precedence) (newline)

  (when (not (list? terms))
    (display "!*prec-generic-infix-parser : WARNING , terms is not a list, perheaps expander is not psyntax (Portable Syntax)") (newline)
    (display "!*prec-generic-infix-parser : terms=") (display terms) (newline))


  
  (define deep-terms (map (lambda (x) (recall-infix-parser x operator-precedence creator)) #;recall-infix-parser terms))

  
  
  (define rv

    (begin
      ;;(display "!*prec-generic-infix-parser rv : deep-terms:") (display deep-terms) (newline)
      ;; test for simple-infix (no operator precedence)
      (if (simple-infix-list-syntax? deep-terms)
	  (begin
	    ;;(display "!*prec-generic-infix-parser : deep-terms is a simple infix list") (newline)
	    ;;(display "!*prec-generic-infix-parser : deep-terms=") (display deep-terms) (newline)
	    (list
	     (cons (cadr deep-terms) (alternating-parameters deep-terms)))) ; we put it in a list because nfx take the car...

	  (begin
	    ;;(display "!*prec-generic-infix-parser : deep-terms is not a simple infix list") (newline)
            (pre-check-!*-generic-infix-parser  deep-terms ;terms
				   operator-precedence
				   creator)))))

  ;;(display "!*prec-generic-infix-parser : rv=") (display rv) (newline)

  ;;(newline)
  
  rv)




) ; end module



;; Welcome to DrRacket, version 8.13 [cs].
;; Language: r6rs, with debugging; memory limit: 8192 MB.
;; > (!*prec-generic-infix-parser '(x <- 10.0 - 3.0 - 4.0 + 1 - 5.0 * 2.0 ** 3.0 / 7.0 ** 3.0)   infix-operators-lst-for-parser (lambda (op a b) (list op a b)))
;; ((<- x (- (+ (- (- 10.0 3.0) 4.0) 1) (/ (* 5.0 (** 2.0 3.0)) (** 7.0 3.0)))))
;; > (- (+ (- (- 10.0 3.0) 4.0) 1) (/ (* 5.0 (** 2.0 3.0)) (** 7.0 3.0)))
;; > (define ** expt)
;; > (- (+ (- (- 10.0 3.0) 4.0) 1) (/ (* 5.0 (** 2.0 3.0)) (** 7.0 3.0)))
;; 3.883381924198251

;; Python:
;; 10.0 - 3.0 - 4.0 + 1 - 5.0 * 2.0 ** 3.0 / 7.0 ** 3.0
;; 3.883381924198251


;; > (!*prec-generic-infix-parser '(a ** b ** c)  infix-operators-lst-for-parser (lambda (op a b) (list op a b)))
;; ((** a (** b c)))

;; >  (!*prec-generic-infix-parser '(a - b - c) infix-operators-lst-for-parser (lambda (op a b) (list op a b)))
;; ((- (- a b) c))


;;   > {(3 * 5 + {2 * (sin .5)}) - 4 * 5}

;; ($nfx$ (3 * 5 + ($nfx$ 2 * (sin 0.5))) - 4 * 5)
;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:65:69 ((3 * 5 + ($nfx$ 2 * (sin 0.5...>
;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax (3 * 5 + ($nfx$ 2 * (sin 0.5)))> .#<syntax -> .#<syntax 4> .#<syntax *> .#<syntax 5>)
;; $nfx$ : parsed-args=.#<syntax (- (+ (* 3 5) ($nfx$ 2 * (sin...>
;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:65:69 (2 * (sin 0.5))>
;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax 2> .#<syntax *> .#<syntax (sin 0.5)>)
;; $nfx$ : parsed-args=.#<syntax (* 2 (sin 0.5))>
;; -4.041148922791594




;;   > {(3 * 5 + (2 * (sin .5))) - 4 * 5}

;; ($nfx$ (3 * 5 + (2 * (sin 0.5))) - 4 * 5)
;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:65:69 ((3 * 5 + (2 * (sin 0.5))) - ...>
;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax (3 * 5 + (2 * (sin 0.5)))> .#<syntax -> .#<syntax 4> .#<syntax *> .#<syntax 5>)
;; $nfx$ : parsed-args=.#<syntax (- (+ (* 3 5) (* 2 (sin 0.5))...>
;; -4.041148922791594

;; > {(3 + 1) * (2 * (2 + 1) - 1) + (2 * 5 - 5)}


;; ($nfx$ (3 + 1) * (2 * (2 + 1) - 1) + (2 * 5 - 5))
;; $nfx$: #'(e1 op1 e2 op2 e3 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:63:76 ((3 + 1) * (2 * (2 + 1) - 1) ...>
;; $nfx$: (syntax->list #'(e1 op1 e2 op2 e3 op ...))=(.#<syntax (3 + 1)> .#<syntax *> .#<syntax (2 * (2 + 1) - 1)> .#<syntax +> .#<syntax (2 * 5 - 5)>)
;; $nfx$ : parsed-args=.#<syntax (+ (* (+ 3 1) (- (* 2 (+ 2 1)...>
;; 25

;; > {x <- #(1 2 3)[1] + 1}


;; ($nfx$ x <- ($bracket-apply$ #(1 2 3) 1) + 1)
;; $nfx$: #'(e1 op1 e2 op2 e3 op ...)=.#<syntax:Users/mattei/Library/CloudStorage/Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:63:76 (x <- ($bracket-apply$ #(1 2 ...>
;; $nfx$: (syntax->list #'(e1 op1 e2 op2 e3 op ...))=(.#<syntax x> .#<syntax <-> .#<syntax ($bracket-apply$ #(1 2 3) 1)> .#<syntax +> .#<syntax 1>)
;; $nfx$ : parsed-args=.#<syntax (<- x (+ ($bracket-apply$ #(1...>

;; bracket-apply : #'parsed-args=.#<syntax (list 1)>



;; > x
;; x
;; 3
