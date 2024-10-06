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

(module infix-with-precedence-to-prefix racket

  (provide !*prec-generic)
	   ;;!0-generic)

  (require (only-in srfi/1 any)
	   Scheme+/syntax)
	  

  ;; procedures work with quoted expression and syntax expressions


;; evaluate one group of operators in the list of terms
(define (!**-generic terms stack operators #;odd? creator)

  ;; (display "!** : terms = ") (display terms) (newline)
  ;; (display "!** : operators = ") (display operators) (newline)
  ;; (display "!** : stack = ") (display stack) (newline)
  ;;(display "!** : odd? = ") (display odd?) (newline) (newline)

					; why `odd?`? because scheme's list-iteration is forwards-only and
					; list-construction is prepend-only, every other group of operators is
					; actually evaluated backwards which, for operators like / and -, can be a
					; big deal! therefore, we keep this flipped `odd?` counter to track if we
					; should flip our arguments or not


  ;; inner definition ,odd? is variable like a parameter
  (define (calc-generic op a b)
    ;;(if odd? (list op a b) (list op b a)))
    
    (define rv ;;(if odd?
		   (creator op a b))
		   ;;(creator op b a))) ; at the beginning odd? is #f
    ;; (display "calc-generic : rv =") (display rv) (newline)
    rv)
  

  ;; executed body of procedure start here
  
  (cond ((and (null? terms)
	      (not (memq 'expt operators))
	      (not (member-syntax #'expt operators)))

	  ;; (display "!**-generic cond case 1 : stack =")
	  ;; (display stack)
	  ;; (newline)
	  ;; (display "!**-generic : terms =")
	  ;; (display terms)
	  ;; (newline)
	  (let ((rs (reverse stack))) ; base case, stack is the result, we return the reverse because
					; scheme's list-iteration is forwards-only and
					; list-construction is prepend-only
	    ;; (display "!**-generic : rs =")
	    ;; (display rs)
	    ;; (newline)
	    rs))

	((null? terms)
	 ;; (display "!**-generic cond case 2 : stack =")
	 ;; (display stack)
	 ;; (newline)
	 ;; (display "!**-generic : terms =")
	 ;; (display terms)
	 ;; (newline)
	 stack) ; here we get 'expt (see previous test) then we do not reverse because we
					;start reversed and then went right->left

	
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

	 
	 ;; body if condition is true : ; found an operator of the same precedence
	 (let* ((op (car stack)) ; get back the operator from the stack ... a op
		(b (car terms)) ; b
		(a (cadr stack)) ; a , get back the operand from the stack ... a op
		(calculus (begin
			    ;;(display "checking exponential for calculus...")(newline)
			    (if (or (memq 'expt operators) ; testing for exponential (expt or **)
				    (member-syntax #'expt operators))
			      (calc-generic op b a) ; op equal expt or **
			      (calc-generic op a b)))))
	   
	   ;;(display "op=") (display op) (newline)
	   
	   (!**-generic (cdr terms) ; forward in terms
			(cons calculus ; put the result in prefix notation on the stack
			      (cddr stack)) 
			operators
			;;odd? ;(not odd?)
			creator)))


	
	;; otherwise just keep building the stack, push at minima : a op from a op b  
	(else
       
	 (!**-generic (cdr terms) ;  forward in expression
		      (cons (car terms) stack) ; push first sub expression on stack
		      operators ; always the same operator group
		      ;;odd?;(not odd?)
		      creator))))

;; wrap a null test
(define (check-null-and-!*-generic terms operator-precedence creator)

  (if (null? terms) ;; never for infix as there is e1 op1 e2 op2 e3 at least
	terms
	(!*-generic (reverse terms) ; start reversed for exponentiation (highest precedence operator)
		    operator-precedence
		    creator)))


;; evaluate a list of groups of operators in the list of terms - forward in operator groups
(define (!*-generic terms operator-groups #;odd? creator)
  ;; (display "!*-generic : terms = ") (display terms) (newline)
  ;; (display "!*-generic : operator-groups = ") (display operator-groups) (newline) (newline)
  (if (or (null? operator-groups) ; done evaluating all operators
	  (null? (cdr terms)))    ; only one term left
      terms ; finished processing operator groups
      ;; evaluate another group -- separating operators into groups allows
      ;; operator precedence

      ;; recursive tail call
      (let ((rv-tms (!**-generic terms '() (car operator-groups) #;odd? creator) ))
	;; (display "!*-generic : rv-tms =")
	;; (display rv-tms)
	;; (newline)
	
	(!*-generic rv-tms; this forward in terms 
		    (cdr operator-groups) ;  rest of precedence list , this forward in operator groups of precedence ,check another group
		    ;;(not odd?)
		    creator))))



(define (!*prec-generic terms  operator-precedence creator)   ;; precursor of !*-generic

  ;;(display "!*prec-generic : terms=") (display terms) (newline)
  ;;(display "!*prec-generic : operator-precedence=") (display operator-precedence) (newline)

  (when (not (list? terms))
    (display "!*prec-generic : WARNING , terms is not a list, perheaps expander is not psyntax (Portable Syntax)") (newline)
    (display "!*prec-generic : terms=") (display terms) (newline))

  (define rv
    (check-null-and-!*-generic  terms ; start reversed for exponentiation (highest precedence operator)
				operator-precedence
				creator))

  ;; (display "!*prec-generic : rv=") (display rv) (newline)
  ;; (newline)
  
  rv)


;; DEPRECATED
;;; evaluates `terms` symbolically or numerically as a basic infix expression
;; no more called by nfx
(define (!0-generic terms  operator-precedence creator) 

  ;; (newline)
  ;; (display "!0-generic : terms=") (display terms) (newline)
  ;; (display "!0-generic : operator-precedence=") (display operator-precedence) (newline)
  (when (not (list? terms))
    (display "!0-generic : WARNING , terms is not a list, perheaps expander is not psyntax (Portable Syntax)") (newline)
    (display "!0-generic : terms=") (display terms) (newline))

  (define rv
    (if (null? terms) ;; i added this null case but with correct input this should not be necessary
	terms
	(car (!*-generic (reverse terms) ; for exponentiation evaluated from right to left
			 operator-precedence
			 #;#f
			 creator))))

  (display "!0-generic : rv=") (display rv) (newline)
  (newline)
  rv

  )





) ; end module



;; Welcome to DrRacket, version 8.13 [cs].
;; Language: r6rs, with debugging; memory limit: 8192 MB.
;; > (!*prec-generic '(x <- 10.0 - 3.0 - 4.0 + 1 - 5.0 * 2.0 ** 3.0 / 7.0 ** 3.0)   infix-operators-lst-for-parser (lambda (op a b) (list op a b)))
;; ((<- x (- (+ (- (- 10.0 3.0) 4.0) 1) (/ (* 5.0 (** 2.0 3.0)) (** 7.0 3.0)))))
;; > (- (+ (- (- 10.0 3.0) 4.0) 1) (/ (* 5.0 (** 2.0 3.0)) (** 7.0 3.0)))
;; > (define ** expt)
;; > (- (+ (- (- 10.0 3.0) 4.0) 1) (/ (* 5.0 (** 2.0 3.0)) (** 7.0 3.0)))
;; 3.883381924198251

;; Python:
;; 10.0 - 3.0 - 4.0 + 1 - 5.0 * 2.0 ** 3.0 / 7.0 ** 3.0
;; 3.883381924198251


;; > (!*prec-generic '(a ** b ** c)  infix-operators-lst-for-parser (lambda (op a b) (list op a b)))
;; ((** a (** b c)))

;; >  (!*prec-generic '(a - b - c) infix-operators-lst-for-parser (lambda (op a b) (list op a b)))
;; ((- (- a b) c))
