;; infix with precedence to prefix

;; This file is part of Scheme+

;; Copyright 2024-2025 Damien MATTEI

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
		 !*prec-generic-infix-parser-rec
		 !*prec-generic-infix-parser-prepare-runtime
		 !*prec-generic-infix-parser-runtime
		 !*-generic-infix-parser
		 superscript-operator-loop
		 begin-operators+-)

	(require ;(only-in srfi/1 any)
	 Scheme+/syntax
	 Scheme+/operators-list
	 Scheme+/operators
	 Scheme+/def
	 ;;SRFI-105/SRFI-105-curly-infix ; for alternating-parameters
	 Scheme+/alternating-parameters
	 Scheme+/superscript-parser
	 Scheme+/block
	 Scheme+/infix-prefix
	 Scheme+/plus-minus-parser
	 Scheme+/atom
	 Scheme+/in-equalities
	 Scheme+/n-arity
	 Scheme+/conjonction ; perheaps useless TODO test
	 Scheme+/recursive-apply
	 ) 
	  

  ;; procedures work with quoted expression and syntax expressions




  ;; original parser routines
  
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
	      (not (exponential-operators-group? operators)))

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
	    rs
	    ;;stack
	    ))

	((null? terms)
	 ;; (display "!**-generic-infix-parser cond case 2 : stack =")
	 ;; (display stack)
	 ;; (newline)
	 ;; (display "!**-generic-infix-parser : terms =")
	 ;; (display terms)
	 ;; (newline)
	 stack
	 ;;(reverse stack)
	 ) ; here we get 'expt (see previous test) then we do not reverse because we
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
	       (member-generic (car stack) operators)))	 ;  syntaxified !

	 
	 ;; body if condition is true : found an operator of the same precedence
	 (let* ((op (car stack)) ; get back the operator from the stack ... a op
		(b (car terms)) ; b
		(a (cadr stack)) ; a
		(calculus (begin
			    ;;(display "!**-generic-infix-parser : a=") (display a) (newline)
			    ;;(display "!**-generic-infix-parser : b=") (display b) (newline)
			    ;;(display "checking exponential for calculus...")(newline)
			    (if (exponential-operators-group? operators) ; testing for exponential (expt or **)
				(calc-generic-infix-parser op b a) ; op equal expt or **
				(calc-generic-infix-parser op a b)))))
	   
	   ;;(display "op=") (display op) (newline)
	   
	   (!**-generic-infix-parser (cdr terms) ; forward in terms
				     (cons calculus ; put the result in prefix notation on the stack
					   (cddr stack)) 
				     operators ; always the same operator group
				     ;;odd? ;(not odd?)
				     creator)))


	
	;; otherwise just keep building the stack, push at minima : a op from a op b  
	(else
       
	 (!**-generic-infix-parser (cdr terms) ;  forward in expression terms
				   (cons (car terms) stack) ; push first sub expression on stack
				   operators ; always the same operator group
				   ;;odd?;(not odd?)
				   creator))))

;; end of !**-generic-infix-parser





;; evaluate a list of groups of operators in the list of terms - forward in operator groups
(define (!*-generic-infix-parser terms operator-groups #;odd? creator)
  ;;(display "!*-generic-infix-parser : terms = ") (display terms) (newline)
  ;;(display "!*-generic-infix-parser : operator-groups = ") (display operator-groups) (newline) (newline)

  ;; should not be reached as there is a check of canonical infix expression, TODO: check this for a long time
  (when (null? operator-groups) ; done evaluating all operators
    (error "!*-generic-infix-parser : no more operator precedence groups , resting terms to parse:" terms))
  
  (if ;(or
       ;(null? operator-groups) ; done evaluating all operators
       (null? (cdr terms)) ; only one term left
       ;)    
      terms ; finished processing operator groups
      
      ;; evaluate another group -- separating operators into groups allows
      ;; operator precedence

      ;; recursive tail call
      (let* ((current-operator-group (car operator-groups))
	     (rv-tms (if (exponential-operators-group? current-operator-group) ; testing for exponential (expt or **)
			 (begin
			   ;;(display  "!*-generic-infix-parser : expo detected") (newline)
			   ;;(display "!*-generic-infix-parser : current-operator-group = ") (display current-operator-group) (newline)
			   (!**-generic-infix-parser (reverse terms) '() current-operator-group #;odd? creator)  ; start reversed for exponentiation (highest precedence operator)
			   )
			 (begin
			   ;;(display  "!*-generic-infix-parser : expo NOT detected") (newline)
			   ;;(display "!*-generic-infix-parser : current-operator-group = ") (display current-operator-group) (newline)
			   (!**-generic-infix-parser terms '() current-operator-group #;odd? creator)))))  ; this forward in terms
	     
	;; (display "!*-generic-infix-parser : rv-tms =")
	;; (display rv-tms)
	;; (newline)
	
	(!*-generic-infix-parser rv-tms
				 (cdr operator-groups) ;  rest of precedence list , this forward in operator groups of precedence ,check another group
				 ;;(not odd?)
				 creator))))





;; deal with simple infix with same operator n-arity,why same operator ???
;; check we really have infix expression before
;; wrap a null test
(def (pre-check-!*-generic-infix-parser terms creator)

  ;;(display "pre-check-!*-generic-infix-parser : terms = ") (display terms) (newline)


  ;; check the whole expression for infix (canonical) because parser infix->prefix can not do it
  (when (not (infix-canonical? terms))
    ;; no more error but runtime parsing
    (error "infix-with-precedence-to-prefix.rkt : pre-check-!*-generic-infix-parser : not a canonical infix expression: " terms))
    
    ;; (define mult *)
    ;; (define add +)
    ;; {3 mult 5 add 2}
    ;; (car
    ;;  (!*-generic-infix-parser
    ;;   (list 3 mult 5 add 2)
    ;;   infix-operators-lst-for-parser
    ;;   (lambda (op a b) (op a b))))
    ;; 17

    ;; (define (mult) *)
    ;; {3 (mult) 5 add 2}
    ;; (car
    ;;  (!*-generic-infix-parser
    ;;   (list 3 (mult) 5 add 2)
    ;;   infix-operators-lst-for-parser
    ;;   (lambda (op a b) (op a b))))
    ;; 17

    ;; note: more external parenthesis ( ) are because !*prec-generic-infix-parser-prepare-runtime take the 'car' at some point (rv return value)
    ;; inner 'car' is because !*-generic-infix-parser return value in a list.
    ;; note: one can not remove ((car ...) thinking it auto-annihilate because ( ) which is like 'list' is evaluated in parser stage and 'car' at runtime !!!
    ;; (return `((car (!*-generic-infix-parser ,(cons 'list terms) ; ah ah ! not only terms
    ;; 					    infix-operators-lst-for-parser
    ;; 					    (lambda (op a b) (op a b))))))) ; whaouuuu !

  ;; (define (cinque) 5)
  ;; {3 (mult) (cinque) add 2}
  
  #;(car
     (!*-generic-infix-parser
      (list 3 (mult) (cinque) add 2)
      infix-operators-lst-for-parser
      (lambda (op a b) (op a b))))
  ;;17

  
  (if (null? terms) ;; never for infix as there is e1 op1 e2 op2 e3 at least
	terms
	(!*-generic-infix-parser terms ; (reverse terms) ; will now reverse only later when expo
				 ;; as we are in REPL/parsing generating source code we can use symbols for parsing
				 infix-operators-lst-for-parser;-syntax
				 creator)))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main entry routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; precursor generic infix parser
;; (this is generally one of the main entry routines of this module)
(def (!*prec-generic-infix-parser terms creator )   ;; precursor of !*-generic-infix-parser

  ;;(display "!*prec-generic-infix-parser : start terms=") (display terms) (newline)
  ;; (newline)

  (var-syntax2list terms)

  ;; (newline)
  ;; (display "!*prec-generic-infix-parser : 0 : terms=") (display terms) (newline)
  ;; (newline)

  (when (null? terms) ; special case ?
	;;(display "!*prec-generic-infix-parser returning early 0 : null terms") (newline)
	(return terms))

  ;; only for debug info
  ;; (when (not (list? terms)) 
  ;;   (display "!*prec-generic-infix-parser : WARNING , terms is not a list, perheaps expander is not psyntax (Portable Syntax)") (newline)
  ;;   (display "!*prec-generic-infix-parser : WARNING , terms=") (display terms) (newline))

  
  (when (atom? terms)
    ;;(display "!*prec-generic-infix-parser returning early") (newline)
    (return terms))

 
  ;; parse superscript number **  and successive operands *  and after for + - (precedence rule for exponential versus signs)
  (define parsed-superscript (superscript-operator-loop terms))
  ;;(display "!*prec-generic-infix-parser : parsed-superscript=") (display parsed-superscript) (newline)

  (define parsed+- parsed-superscript) ; by default

  (when (and (not (null? parsed-superscript))
	     (not (null? (cdr parsed-superscript)))) ;; forbid to treat (something) ,example : (a_syntax)
    ;; unless that it would return a_syntax , loosing the ( ) because begin-operators+- return sometimes the car when there is only one element in the return list
	     ;;(infix? parsed-superscript)) ; we already know we are in infix
	(set! parsed+- (begin-operators+- parsed-superscript '())))
  
  ;;(display "!*prec-generic-infix-parser : parsed+-=") (display parsed+-) (newline)
  
  (define deep-terms parsed+-) ;; parsed for + - and superscript number ** 
  ;;(display "!*prec-generic-infix-parser : deep-terms=") (display deep-terms) (newline)


  (when (atom? deep-terms) ; no need to go further
	;;(display "!*prec-generic-infix-parser returning early 1") (newline)
	;;(display "!*prec-generic-infix-parser returning early 1 : return deep-terms : ") (display deep-terms) (newline) (newline)
    (return deep-terms)) 


  ;; infix parser do not know how to deal with only 2 terms

  ;; strange case (2 elements) can not remember how we arrive here... anyway we recall-infix on all (both) elements.
  ;; {- - 2}
  ;;($nfx$ - - 2)
  ;;2
  ;;#<eof>
  (when (and (list? deep-terms)
	     (= 2 (length deep-terms))) ; example : syntax something of (- (- 2))
    ;;(display "!*prec-generic-infix-parser : length = 2") (newline)
    (return ; before infix parsing as it is already infix
       (map (lambda (x) ; mapping in infix the deep terms
	     (!*prec-generic-infix-parser-rec x creator)) 
	   deep-terms)))

  ;;(display "!*prec-generic-infix-parser 1 before in/equalities : deep-terms=") (display deep-terms) (newline)
  ;; (newline)

  ;; (when (not (list? deep-terms))
  ;;   (display "!*prec-generic-infix-parser deep-terms is NOT a list") (newline))

  ;;(display "!*prec-generic-infix-parser (length deep-terms) :") (display (length deep-terms)) (newline)


  ;; deal with possible in/equalities
  (when (and (list? deep-terms)
	     (>= (length deep-terms) 5)) ; length of a < b < c
    ;;(display "!*prec-generic-infix-parser launching in/equalities-state-1") (newline)
    (set! deep-terms (in/equalities-state-1 deep-terms '() '() '() '())))

  ;;(display "!*prec-generic-infix-parser after in/equalities : deep-terms=") (display deep-terms) (newline)
  ;; (newline)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; general case of mapping in infix the deep terms
  ;; if there is (define ... we must not compute deep-terms with !*prec-generic-infix-parser but simply copy the terms in deep-terms
  ;; because we do not want to evaluate any ( ... ) as infix but as prefix

  (when (and (list? deep-terms)
	     (not (datum=? 'define ; define is preserved this way (no infix recursive in define)
			   (car deep-terms))))
    ;;(display "!*prec-generic-infix-parser : recalling !*prec-generic-infix-parser via map") (newline) (newline)
    (set! deep-terms (map (lambda (x) (!*prec-generic-infix-parser-rec x creator))
			  deep-terms)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;(display "!*prec-generic-infix-parser 2 : deep-terms=") (display deep-terms) (newline)
  ;;(newline)

  #;(if (prefix? deep-terms) ; could be prefix  , for debug
      (begin
	(display "!*prec-generic-infix-parser PREFIX DETECTED")
	(newline))
      (begin
	(display "!*prec-generic-infix-parser PREFIX NOT DETECTED")
	(newline)))

  
  ;; commented it as we must be infix : if we already have prefix (and already recall on the deep terms) then why continuing?
  #;(when (prefix? deep-terms) ; could be prefix
    (display "!*prec-generic-infix-parser returning early 2 PREFIX DETECTED EARLY") (newline)
    (return deep-terms))
 

  ;;(display "!*prec-generic-infix-parser : rv : deep-terms:") (display deep-terms) (newline)
  ;; test for simple-infix (no operator precedence)
  (when (or (= (length deep-terms) 3) ; we are infix because { } , we have 3 terms this should be simple infix
	    ;; this allows some things like that without strict SRFI 105 mode:
	    ;; {(cinque) - {(cinque) (minus) 3}}
	    (simple-infix-list-syntax? deep-terms))
    ;;(display "!*prec-generic-infix-parser : deep-terms is a simple infix list") (newline)
    ;;(display "!*prec-generic-infix-parser : deep-terms=") (display deep-terms) (newline)
    (return 
     (cons (cadr deep-terms) ; cadr is op in arg1 op arg2 op ....
	   (alternating-parameters deep-terms)))) 
  
  ;; (display "!*prec-generic-infix-parser : deep-terms is not a simple infix list") (newline)
  ;; (display "!*prec-generic-infix-parser : deep-terms=") (display deep-terms) (newline)
  ;; (newline)
	
  ;; we can check for expressions like 2<3<=3 here
  ;; can not remember why we should deal twice with in/equalities (also above in the code) seems parsing is in 2 stage
  (when (multiple-in-equalities? deep-terms)
    (return (infix->prefix-in-equality deep-terms)))

  
  (define rv (pre-check-!*-generic-infix-parser deep-terms creator ))

  
  ;; (display "!*prec-generic-infix-parser : rv=") (display rv) (newline)

  ;; (newline)
  ;; (newline)

  (define expr (car rv))
  ;;(display "!*prec-generic-infix-parser : (car rv) = expr =") (display expr) (newline)

  ;; TODO pass to n-arity also arithmetic expressions (+ , * , ...) note: fail with n-arity
  ;; note: some overloaded arithmetic operator could not have implemented the n-arity
  ;; perheaps write this in another module ,sort of !*post-generic-infix-parser
  (if ;;(not (isEXPONENTIAL? expr))
   (or (isDEFINE? expr)
       (isASSIGNMENT? expr))
   
   ;;  make n-arity for <- and <+ only (because could be false with ** , but not implemented in n-arity for now)
   ;; (begin
   ;; 	 (display "!*prec-generic-infix-parser : calling n-arity on expr :") (display expr) (newline) 
   (n-arity ;; this avoids : '{x <- y <- z <- t <- u <- 3 * 4 + 1}
    ;; SRFI-105.scm : !0 result = (<- (<- (<- (<- (<- x y) z) t) u) (+ (* 3 4) 1)) ;; fail set! ...
    ;; transform in : '(<- x y z t u (+ (* 3 4) 1))
            expr) ;) ; end begin
   
   expr)
  
  ) ; end def





(def (!*prec-generic-infix-parser-rec terms creator )   ;; precursor of !*-generic-infix-parser

  ;;(display "!*prec-generic-infix-parser-rec : start terms=") (display terms) (newline)
  ;; (newline)

  (var-syntax2list terms)

  ;; (newline)
  ;; (display "!*prec-generic-infix-parser-rec : 0 : terms=") (display terms) (newline)
  ;; (newline)

  (when (null? terms) ; special case ?
	;;(display "!*prec-generic-infix-parser-rec returning early 0 : null terms") (newline)
	(return terms))

  ;; only for debug info
  ;; (when (not (list? terms)) 
  ;;   (display "!*prec-generic-infix-parser-rec : WARNING , terms is not a list, perheaps expander is not psyntax (Portable Syntax)") (newline)
  ;;   (display "!*prec-generic-infix-parser-rec : WARNING , terms=") (display terms) (newline))

  
  (when (atom? terms)
    ;;(display "!*prec-generic-infix-parser-rec returning early") (newline)
    (return terms))

 
  ;; parse superscript number **  and successive operands *  and after for + - (precedence rule for exponential versus signs)
  (define parsed-superscript (superscript-operator-loop terms))
  ;;(display "!*prec-generic-infix-parser-rec : parsed-superscript=") (display parsed-superscript) (newline)

  (define parsed+- parsed-superscript) ; by default

  
  (when (and (not (null? parsed-superscript))
	     (not (null? (cdr parsed-superscript))) ;; forbid to treat (something) ,example : (a_syntax)
	     ;; unless that it would return a_syntax , loosing the ( ) because begin-operators+- return sometimes the car when there is only one element in the return list
	     (infix? parsed-superscript)) 
	(set! parsed+- (begin-operators+- parsed-superscript '())))
  
  ;;(display "!*prec-generic-infix-parser-rec : parsed+-=") (display parsed+-) (newline)
  
  (define deep-terms parsed+-) ;; parsed for + - and superscript number ** 
  ;;(display "!*prec-generic-infix-parser-rec : deep-terms=") (display deep-terms) (newline)


  (when (atom? deep-terms) ; no need to go further
	;;(display "!*prec-generic-infix-parser-rec returning early 1") (newline)
	;;(display "!*prec-generic-infix-parser-rec returning early 1 : return deep-terms : ") (display deep-terms) (newline) (newline)
    (return deep-terms)) 


  ;; infix parser do not know how to deal with only 2 terms

  ;; strange case (2 elements) can not remember how we arrive here... anyway we recall-infix on all (both) elements.
  ;; {- - 2}
  ;;($nfx$ - - 2)
  ;;2
  ;;#<eof>
  (when (and (list? deep-terms)
	     (= 2 (length deep-terms))) ; example : syntax something of (- (- 2))
    ;;(display "!*prec-generic-infix-parser-rec : length = 2") (newline)
    (return ; before infix parsing as it is already infix
       (map (lambda (x) ; mapping in infix the deep terms
	     (!*prec-generic-infix-parser-rec x creator)) 
	   deep-terms)))

  ;;(display "!*prec-generic-infix-parser-rec 1 before in/equalities : deep-terms=") (display deep-terms) (newline)
  ;; (newline)

  ;; (when (not (list? deep-terms))
  ;;   (display "!*prec-generic-infix-parser-rec deep-terms is NOT a list") (newline))

  ;;(display "!*prec-generic-infix-parser-rec (length deep-terms) :") (display (length deep-terms)) (newline)


  ;; deal with in/equalities
  (when (and (list? deep-terms)
	     (>= (length deep-terms) 5)) ; length of a < b < c
    ;;(display "!*prec-generic-infix-parser-rec launching in/equalities-state-1") (newline)
    (set! deep-terms (in/equalities-state-1 deep-terms '() '() '() '())))

  ;;(display "!*prec-generic-infix-parser-rec after in/equalities : deep-terms=") (display deep-terms) (newline)
  ;; (newline)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; general case of mapping in infix the deep terms
  ;; if there is (define ... we must not compute deep-terms with !*prec-generic-infix-parser-rec but simply copy the terms in deep-terms
  ;; because we do not want to evaluate any ( ... ) as infix but as prefix

  (when (and (list? deep-terms)
	     (not (datum=? 'define ; define is preserved this way (no infix recursive in define)
			   (car deep-terms))))
    ;;(display "!*prec-generic-infix-parser-rec : recalling !*prec-generic-infix-parser-rec via map") (newline) (newline)
    (set! deep-terms (map (lambda (x) (!*prec-generic-infix-parser-rec x creator))
			  deep-terms)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;(display "!*prec-generic-infix-parser-rec 2 : deep-terms=") (display deep-terms) (newline)
  ;;(newline)

  #;(if (prefix? deep-terms) ; could be prefix  , for debug
      (begin
	(display "!*prec-generic-infix-parser-rec PREFIX DETECTED")
	(newline))
      (begin
	(display "!*prec-generic-infix-parser-rec PREFIX NOT DETECTED")
	(newline)))

  
  ;; if we already have prefix (and already recall on the deep terms) then why continuing?
  (when (prefix? deep-terms) ; could be prefix
    ;;(display "!*prec-generic-infix-parser-rec returning early 2 PREFIX DETECTED EARLY") (newline)
    (return deep-terms))
 

  ;; we can check for expressions like 2<3<=3 here or later in the 'else of next 'if
  
  ;;(display "!*prec-generic-infix-parser-rec : rv : deep-terms:") (display deep-terms) (newline)
  ;; test for simple-infix (no operator precedence)
  (when (simple-infix-list-syntax? deep-terms)
    ;;(display "!*prec-generic-infix-parser-rec : deep-terms is a simple infix list") (newline)
    ;;(display "!*prec-generic-infix-parser-rec : deep-terms=") (display deep-terms) (newline)
    (return
     (cons (cadr deep-terms) ; cadr is op in arg1 op arg2 op ....
	   (alternating-parameters deep-terms)))) 
  
  ;; (display "!*prec-generic-infix-parser-rec : deep-terms is not a simple infix list") (newline)
  ;; (display "!*prec-generic-infix-parser-rec : deep-terms=") (display deep-terms) (newline)
  ;; (newline)
	
  ;; we can check for expressions like 2<3<=3 here
  ;; twice: same comment as for non recursiver version
  (when (multiple-in-equalities? deep-terms)
      (return (infix->prefix-in-equality deep-terms)))

  
  (define rv (pre-check-!*-generic-infix-parser deep-terms creator ))

  
  ;; (display "!*prec-generic-infix-parser-rec : rv=") (display rv) (newline)

  ;; (newline)
  ;; (newline)

  (define expr (car rv))
  ;;(display "!*prec-generic-infix-parser-rec : (car rv) = expr =") (display expr) (newline)

  ;; TODO pass to n-arity also arithmetic expressions (+ , * , ...) note: fail with n-arity
  ;; note: some overloaded arithmetic operator could not have implemented the n-arity
  ;; perheaps write this in another module ,sort of !*post-generic-infix-parser
  (if ;;(not (isEXPONENTIAL? expr))
   (or (isDEFINE? expr)
       (isASSIGNMENT? expr))
   
   ;;  make n-arity for <- and <+ only (because could be false with ** , but not implemented in n-arity for now)
   ;; (begin
   ;; 	 (display "!*prec-generic-infix-parser-rec : calling n-arity on expr :") (display expr) (newline) 
   (n-arity ;; this avoids : '{x <- y <- z <- t <- u <- 3 * 4 + 1}
    ;; SRFI-105.scm : !0 result = (<- (<- (<- (<- (<- x y) z) t) u) (+ (* 3 4) 1)) ;; fail set! ...
    ;; transform in : '(<- x y z t u (+ (* 3 4) 1))
            expr) ;) ; end begin
   
   expr)
  
  )





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; version called from SRFI-105 (will prepare possible runtime parsing)
;; precursor generic infix parser
;; (this is generally one of the main entry routines of this module)
;; note: i removed all syntax stuff as this routine should be only call from external parser (SRFI-105), not by the Scheme+ syntax transformers
(def (!*prec-generic-infix-parser-prepare-runtime terms creator )   ;; precursor of !*-generic-infix-parser

  ;; (display "!*prec-generic-infix-parser-prepare-runtime : start terms=") (display terms) (newline)
  ;; (newline)

  (when (null? terms) ; special case ?
	;;(display "!*prec-generic-infix-parser-prepare-runtime returning early 0 : null terms") (newline)
	(return terms))


  (when (atom? terms)
    ;;(display "!*prec-generic-infix-parser-prepare-runtime returning early") (newline)
    (return terms))

  ;; parse superscript number **  and successive operands *  and after for + - (precedence rule for exponential versus signs)
  (define parsed-superscript (superscript-operator-loop terms))
  ;;(display "!*prec-generic-infix-parser-prepare-runtime : parsed-superscript=") (display parsed-superscript) (newline)

  
  ;; this detect some need of evaluation at runtime before parsing with operator precedence
  (when (two-symbols-or-list-following? parsed-superscript)
    ;;(error "two-symbols-or-list-following? : runtime needed" parsed-superscript) 
    ;; we need to parse the terms because they will be evaluated at runtime and they must be in prefix notation then
    (define parsed-deep-terms (map (lambda (x) ; mapping in infix the deep terms
				     (!*prec-generic-infix-parser-rec-prepare x creator)) 
				   parsed-superscript))

    (return `(!*prec-generic-infix-parser-runtime ,(cons 'list parsed-deep-terms) ; ah ah ! not only terms
						  (lambda (op a b) (op a b))))) ; end when

  
  
  (define parsed+- parsed-superscript) ; by default

  (when (and (not (null? parsed-superscript))
	     (not (null? (cdr parsed-superscript)))) ;; forbid to treat (something) ,example : (a_syntax)
    ;; unless that it would return a_syntax , loosing the ( ) because begin-operators+- return sometimes the car when there is only one element in the return list
             ;;(infix? parsed-superscript)) ; we already know we are in infix
	(set! parsed+- (begin-operators+- parsed-superscript '())))
  
  ;;(display "!*prec-generic-infix-parser-prepare-runtime : parsed+-=") (display parsed+-) (newline)
  
  (define deep-terms parsed+-) ;; parsed for + - and superscript number ** 
  ;;(display "!*prec-generic-infix-parser-prepare-runtime : deep-terms=") (display deep-terms) (newline)


  (when (atom? deep-terms) ; no need to go further
	;;(display "!*prec-generic-infix-parser-prepare-runtime returning early 1") (newline)
	;;(display "!*prec-generic-infix-parser-prepare-runtime returning early 1 : return deep-terms : ") (display deep-terms) (newline) (newline)
    (return deep-terms)) 


  ;; infix parser do not know how to deal with only 2 terms

  ;; strange case (2 elements) can not remember how we arrive here... anyway we recall parsing on all (both) elements.
  ;; {- - 2}
  ;;($nfx$ - - 2)
  ;;2
  ;;#<eof>
  (when (and (list? deep-terms)
	     (= 2 (length deep-terms))) ; example : syntax something of (- (- 2))
    ;;(display "!*prec-generic-infix-parser-prepare-runtime : length = 2") (newline)
    (return ; before infix parsing as it is already infix
       (map (lambda (x) ; mapping in infix the deep terms
	      (!*prec-generic-infix-parser-rec-prepare x creator)) 
	    deep-terms)))


  ;; deal with possible in/equalities
  (when (and (list? deep-terms)
	     (>= (length deep-terms) 5)) ; length of a < b < c
    ;;(display "!*prec-generic-infix-parser-prepare-runtime launching in/equalities-state-1") (newline)
    (set! deep-terms (in/equalities-state-1 deep-terms '() '() '() '())))

  ;;(display "!*prec-generic-infix-parser-prepare-runtime after in/equalities : deep-terms=") (display deep-terms) (newline)
  ;; (newline)


  ;; TODO : test this commented
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; general case of mapping in infix the deep terms
  ;;
  ;; if there is (define ... we must not compute deep-terms with !*prec-generic-infix-parser-prepare-runtime but simply copy the terms in deep-terms
  ;; because we do not want to evaluate any ( ... ) as infix but as prefix

  ;; (when (and (list? deep-terms)
  ;; 	     (not (datum=? 'define ; define is preserved this way (no infix recursive in define)
  ;; 			   (car deep-terms))))
    ;;(display "!*prec-generic-infix-parser-prepare-runtime : recalling !*prec-generic-infix-parser-rec-prepare via map") (newline) (newline)
    (set! deep-terms (map (lambda (x)
			    (!*prec-generic-infix-parser-rec-prepare x creator))
			  deep-terms))
   ; )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;(display "!*prec-generic-infix-parser-prepare-runtime 2 : deep-terms=") (display deep-terms) (newline)
  ;;(newline)

  ;; test for simple-infix (no operator precedence)
  (when (or (= (length deep-terms) 3) ; we are infix because { } , we have 3 terms this should be simple infix
	    ;; this allows some things like that without strict SRFI 105 mode:
	    ;; {(cinque) - {(cinque) (minus) 3}}
	    (simple-infix-list-syntax? deep-terms))
    ;;(display "!*prec-generic-infix-parser-prepare-runtime : deep-terms is a simple infix list") (newline)
    ;;(display "!*prec-generic-infix-parser-prepare-runtime : deep-terms=") (display deep-terms) (newline)
    (return 
     (cons (cadr deep-terms) ; cadr is op in arg1 op arg2 op ....
	   (alternating-parameters deep-terms)))) 
  
  ;; (display "!*prec-generic-infix-parser-prepare-runtime : deep-terms is not a simple infix list") (newline)
  ;; (display "!*prec-generic-infix-parser-prepare-runtime : deep-terms=") (display deep-terms) (newline)
  ;; (newline)
	
  ;; we can check for expressions like 2<3<=3 here
  ;; can not remember why we should deal twice with in/equalities (also above in the code) seems parsing is in 2 stage
  (when (multiple-in-equalities? deep-terms)
    (return (infix->prefix-in-equality deep-terms)))

  ;; general case (when no other 'return' has happened on simplier things
  (define rv (pre-check-!*-generic-infix-parser deep-terms creator ))

  
  ;; (display "!*prec-generic-infix-parser-prepare-runtime : rv=") (display rv) (newline)

  ;; (newline)
  ;; (newline)

  (define expr (car rv))
  ;;(display "!*prec-generic-infix-parser-prepare-runtime : (car rv) = expr =") (display expr) (newline)

  ;; TODO pass to n-arity also arithmetic expressions (+ , * , ...) note: fail with n-arity
  ;; note: some overloaded arithmetic operator could not have implemented the n-arity
  ;; perheaps write this in another module ,sort of !*post-generic-infix-parser
  (if ;;(not (isEXPONENTIAL? expr))
   (or (isDEFINE? expr)
       (isASSIGNMENT? expr))
   
   ;;  make n-arity for <- and <+ only (because could be false with ** , but not implemented in n-arity for now)
   ;; (begin
   ;; 	 (display "!*prec-generic-infix-parser-prepare-runtime : calling n-arity on expr :") (display expr) (newline) 
   (n-arity ;; this avoids : '{x <- y <- z <- t <- u <- 3 * 4 + 1}
    ;; SRFI-105.scm : !0 result = (<- (<- (<- (<- (<- x y) z) t) u) (+ (* 3 4) 1)) ;; fail set! ...
    ;; transform in : '(<- x y z t u (+ (* 3 4) 1))
            expr) ;) ; end begin
   
   expr)
  
  ) ; end def




;; runtime version

(def (!*prec-generic-infix-parser-runtime parsed-superscript creator )   ;; precursor of !*-generic-infix-parser

  ;; (display "!*prec-generic-infix-parser-runtime : start parsed-superscript=") (display parsed-superscript) (newline)
  ;; (newline)

  (define parsed+- parsed-superscript) ; by default

  (when (and (not (null? parsed-superscript))
	     (not (null? (cdr parsed-superscript)))) ;; forbid to treat (something) ,example : (a_syntax)
    ;; unless that it would return a_syntax , loosing the ( ) because begin-operators+- return sometimes the car when there is only one element in the return list
             ;;(infix? parsed-superscript)) ; we already know we are in infix
	(set! parsed+- (begin-operators+- parsed-superscript '())))
  
  ;;(display "!*prec-generic-infix-parser-runtime : parsed+-=") (display parsed+-) (newline)
  
  (define deep-terms (map (lambda (x) (if (equal? x '-)  ; replace '- by procdure -
					  -
					  x))
			  parsed+-)) ;; parsed for + - and superscript number **
  
  ;;(display "!*prec-generic-infix-parser-runtime : deep-terms=") (display deep-terms) (newline)


  (when (atom? deep-terms) ; no need to go further
	;;(display "!*prec-generic-infix-parser-runtime returning early 1") (newline)
	;;(display "!*prec-generic-infix-parser-runtime returning early 1 : return deep-terms : ") (display deep-terms) (newline) (newline)
    (return deep-terms)) 

  ;;(display "!*prec-generic-infix-parser-runtime 0 : (procedure? (car deep-terms)) = ") (display (procedure? (car deep-terms))) (newline)
   
  
  ;; infix parser do not know how to deal with only 2 terms

  ;; strange case (2 elements) can not remember how we arrive here... anyway we recall parsing on all (both) elements.
  ;; {- - 2}
  ;;($nfx$ - - 2)
  ;;2
  ;;#<eof>
  (when (and (list? deep-terms)
	     (= 2 (length deep-terms))) ; example : syntax something of (- (- 2))
    ;;(display "!*prec-generic-infix-parser-runtime : length = 2") (newline)
    ;;(display "!*prec-generic-infix-parser-runtime : (car deep-terms) = ") (display (car deep-terms)) (newline)
    ;;(display "!*prec-generic-infix-parser-runtime : (procedure? (car deep-terms)) = ") (display (procedure? (car deep-terms))) (newline)
    (return (apply (car deep-terms)
		   (cdr deep-terms))))  ; return before infix parsing as it is already infix but evaluate deep-terms = (proc arg)


  ;; deal with possible in/equalities
  (when (and (list? deep-terms)
	     (>= (length deep-terms) 5)) ; length of a < b < c
    ;;(display "!*prec-generic-infix-parser-runtime launching in/equalities-state-1") (newline)
    (set! deep-terms (in/equalities-state-1 deep-terms '() '() '() '())))

  ;; (display "!*prec-generic-infix-parser-runtime after in/equalities : deep-terms=") (display deep-terms) (newline)
  ;; (newline)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; no more mapping of deep terms here, it has been already done in the external parser stage
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; test for simple-infix (no operator precedence)
  (when (or (= (length deep-terms) 3) ; we are infix because { } , we have 3 terms this should be simple infix
	    ;; this allows some things like that without strict SRFI 105 mode:
	    ;; {(cinque) - {(cinque) (minus) 3}}
	    (simple-infix-list-syntax? deep-terms))
    ;;(display "!*prec-generic-infix-parser-runtime : deep-terms is a simple infix list") (newline)
    ;;(display "!*prec-generic-infix-parser-runtime : deep-terms=") (display deep-terms) (newline)
    (return 
     (apply (cadr deep-terms) ; cadr is op in arg1 op arg2 op ....
	    (alternating-parameters deep-terms)))) 
  
  ;; (display "!*prec-generic-infix-parser-runtime : deep-terms is not a simple infix list") (newline)
  ;; (display "!*prec-generic-infix-parser-runtime : deep-terms=") (display deep-terms) (newline)
  ;; (newline)
	
  ;; we can check for expressions like 2<3<=3 here
  ;; can not remember why we should deal twice with in/equalities (also above in the code) seems parsing is in 2 stage
  (when (multiple-in-equalities? deep-terms)
    ;;(display "!*prec-generic-infix-parser-runtime : multiple in/equalities detected") (newline)
    (define in-eq-terms (infix->prefix-in-equality-runtime deep-terms))
    ;;(display "!*prec-generic-infix-parser-runtime : in-eq-terms=") (display in-eq-terms) (newline)
    (return (recursive-apply in-eq-terms))) ; should be &&
			     

  ;; general case (when no other 'return' has happened on simplier things
  (define rv (pre-check-!*-generic-infix-parser deep-terms creator)) ; creator replaced by evaluator

  
  ;; (display "!*prec-generic-infix-parser-runtime : rv=") (display rv) (newline)

  ;; (newline)
  ;; (newline)

  (define expr (car rv))
  ;;(display "!*prec-generic-infix-parser-runtime : (car rv) = expr =") (display expr) (newline)
   
  expr
  
  ) ; end def






(def (!*prec-generic-infix-parser-rec-prepare terms creator )   ;; precursor of !*-generic-infix-parser

  ;;(display "!*prec-generic-infix-parser-rec-prepare : start terms=") (display terms) (newline)
  ;; (newline)

  (when (null? terms) ; special case ?
     ;;(display "!*prec-generic-infix-parser-rec-prepare returning early 0 : null terms") (newline)
     (return terms))
  
  (when (atom? terms)
    ;;(display "!*prec-generic-infix-parser-rec-prepare returning early") (newline)
    (return terms))

 
  ;; parse superscript number **  and successive operands *  and after for + - (precedence rule for exponential versus signs)
  (define parsed-superscript (superscript-operator-loop terms))
  ;;(display "!*prec-generic-infix-parser-rec-prepare : parsed-superscript=") (display parsed-superscript) (newline)

  (define parsed+- parsed-superscript) ; by default

  
  (when (and (not (null? parsed-superscript))
	     (not (null? (cdr parsed-superscript))) ;; forbid to treat (something) ,example : (a_syntax)
	     ;; unless that it would return a_syntax , loosing the ( ) because begin-operators+- return sometimes the car when there is only one element in the return list
	     (infix? parsed-superscript)) 
	(set! parsed+- (begin-operators+- parsed-superscript '())))
  
  ;;(display "!*prec-generic-infix-parser-rec-prepare : parsed+-=") (display parsed+-) (newline)
  
  (define deep-terms parsed+-) ;; parsed for + - and superscript number ** 
  ;;(display "!*prec-generic-infix-parser-rec-prepare : deep-terms=") (display deep-terms) (newline)


  (when (atom? deep-terms) ; no need to go further
	;;(display "!*prec-generic-infix-parser-rec-prepare returning early 1") (newline)
	;;(display "!*prec-generic-infix-parser-rec-prepare returning early 1 : return deep-terms : ") (display deep-terms) (newline) (newline)
    (return deep-terms)) 


  ;; infix parser do not know how to deal with only 2 terms

  ;; strange case (2 elements) can not remember how we arrive here... anyway we recall-infix on all (both) elements.
  ;; {- - 2}
  ;;($nfx$ - - 2)
  ;;2
  ;;#<eof>
  (when (and (list? deep-terms)
	     (= 2 (length deep-terms))) ; example : syntax something of (- (- 2))
    ;;(display "!*prec-generic-infix-parser-rec-prepare : length = 2") (newline)
    (return ; before infix parsing as it is already infix
       (map (lambda (x) ; mapping in infix the deep terms
	     (!*prec-generic-infix-parser-rec-prepare x creator)) 
	   deep-terms)))

  ;;(display "!*prec-generic-infix-parser-rec-prepare 1 before in/equalities : deep-terms=") (display deep-terms) (newline)
  ;; (newline)

  ;; deal with in/equalities
  (when (and (list? deep-terms)
	     (>= (length deep-terms) 5)) ; length of a < b < c
    (set! deep-terms (in/equalities-state-1 deep-terms '() '() '() '())))

  ;;(display "!*prec-generic-infix-parser-rec-prepare after in/equalities : deep-terms=") (display deep-terms) (newline)
  ;; (newline)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; general case of mapping in infix the deep terms
  ;; if there is (define ... we must not compute deep-terms with !*prec-generic-infix-parser-rec-prepare but simply copy the terms in deep-terms
  ;; because we do not want to evaluate any ( ... ) as infix but as prefix

  (when (and (list? deep-terms)
	     (not (datum=? 'define ; define is preserved this way (no infix recursive in define)
			   (car deep-terms))))
    ;;(display "!*prec-generic-infix-parser-rec-prepare : recalling !*prec-generic-infix-parser-rec-prepare via map") (newline) (newline)
    (set! deep-terms (map (lambda (x)
			    (!*prec-generic-infix-parser-rec-prepare x creator))
			  deep-terms)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;;(display "!*prec-generic-infix-parser-rec-prepare 2 : deep-terms=") (display deep-terms) (newline)
  ;;(newline)
  
  ;; if we already have prefix (and already recall on the deep terms) then why continuing?
  (when (prefix? deep-terms) ; could be prefix
    ;;(display "!*prec-generic-infix-parser-rec-prepare returning early 2 PREFIX DETECTED EARLY") (newline)
    (return deep-terms))
 

  ;; we can check for expressions like 2<3<=3 here or later in the 'else of next 'if
  
  ;;(display "!*prec-generic-infix-parser-rec-prepare : rv : deep-terms:") (display deep-terms) (newline)
  ;; test for simple-infix (no operator precedence)
  (when (simple-infix-list-syntax? deep-terms)
    ;;(display "!*prec-generic-infix-parser-rec-prepare : deep-terms is a simple infix list") (newline)
    ;;(display "!*prec-generic-infix-parser-rec-prepare : deep-terms=") (display deep-terms) (newline)
    (return
     (cons (cadr deep-terms) ; cadr is op in arg1 op arg2 op ....
	   (alternating-parameters deep-terms)))) 
  
  ;; (display "!*prec-generic-infix-parser-rec-prepare : deep-terms is not a simple infix list") (newline)
  ;; (display "!*prec-generic-infix-parser-rec-prepare : deep-terms=") (display deep-terms) (newline)
  ;; (newline)
	
  ;; we can check for expressions like 2<3<=3 here
  ;; twice: same comment as for non recursiver version
  (when (multiple-in-equalities? deep-terms)
      (return (infix->prefix-in-equality deep-terms)))

  
  (define rv (pre-check-!*-generic-infix-parser deep-terms creator ))

  
  ;; (display "!*prec-generic-infix-parser-rec-prepare : rv=") (display rv) (newline)

  ;; (newline)
  ;; (newline)

  (define expr (car rv))
  ;;(display "!*prec-generic-infix-parser-rec-prepare : (car rv) = expr =") (display expr) (newline)

  ;; TODO pass to n-arity also arithmetic expressions (+ , * , ...) note: fail with n-arity
  ;; note: some overloaded arithmetic operator could not have implemented the n-arity
  ;; perheaps write this in another module ,sort of !*post-generic-infix-parser
  (if ;;(not (isEXPONENTIAL? expr))
   (or (isDEFINE? expr)
       (isASSIGNMENT? expr))
   
   ;;  make n-arity for <- and <+ only (because could be false with ** , but not implemented in n-arity for now)
   ;; (begin
   ;; 	 (display "!*prec-generic-infix-parser-rec-prepare : calling n-arity on expr :") (display expr) (newline) 
   (n-arity ;; this avoids : '{x <- y <- z <- t <- u <- 3 * 4 + 1}
    ;; SRFI-105.scm : !0 result = (<- (<- (<- (<- (<- x y) z) t) u) (+ (* 3 4) 1)) ;; fail set! ...
    ;; transform in : '(<- x y z t u (+ (* 3 4) 1))
            expr) ;) ; end begin
   
   expr)
  
  )



) ; end module





;; Welcome to DrRacket, version 8.13 [cs].
;; Language: r6rs, with debugging; memory limit: 8192 MB.
;; (!*prec-generic-infix-parser '(x <- 10.0 - 3.0 - 4.0 + 1 - 5.0 * 2.0 ** 3.0 / 7.0 ** 3.0)   infix-operators-lst-for-parser (lambda (op a b) (list op a b)))
;; ((<- x (- (+ (- (- 10.0 3.0) 4.0) 1) (/ (* 5.0 (** 2.0 3.0)) (** 7.0 3.0)))))
;; (- (+ (- (- 10.0 3.0) 4.0) 1) (/ (* 5.0 (** 2.0 3.0)) (** 7.0 3.0)))
;; (define ** expt)
;; (- (+ (- (- 10.0 3.0) 4.0) 1) (/ (* 5.0 (** 2.0 3.0)) (** 7.0 3.0)))
;; 3.883381924198251

;; Python:
;; 10.0 - 3.0 - 4.0 + 1 - 5.0 * 2.0 ** 3.0 / 7.0 ** 3.0
;; 3.883381924198251


;; (!*prec-generic-infix-parser '(a ** b ** c)  infix-operators-lst-for-parser (lambda (op a b) (list op a b)))
;; ((** a (** b c)))

;;  (!*prec-generic-infix-parser '(a - b - c) infix-operators-lst-for-parser (lambda (op a b) (list op a b)))
;; ((- (- a b) c))


;;  {(3 * 5 + {2 * (sin .5)}) - 4 * 5}

;; ($nfx$ (3 * 5 + ($nfx$ 2 * (sin 0.5))) - 4 * 5)
;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:65:69 ((3 * 5 + ($nfx$ 2 * (sin 0.5...>
;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax (3 * 5 + ($nfx$ 2 * (sin 0.5)))> .#<syntax -> .#<syntax 4> .#<syntax *> .#<syntax 5>)
;; $nfx$ : parsed-args=.#<syntax (- (+ (* 3 5) ($nfx$ 2 * (sin...>
;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:65:69 (2 * (sin 0.5))>
;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax 2> .#<syntax *> .#<syntax (sin 0.5)>)
;; $nfx$ : parsed-args=.#<syntax (* 2 (sin 0.5))>
;; -4.041148922791594




;;  {(3 * 5 + (2 * (sin .5))) - 4 * 5}

;; ($nfx$ (3 * 5 + (2 * (sin 0.5))) - 4 * 5)
;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:65:69 ((3 * 5 + (2 * (sin 0.5))) - ...>
;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax (3 * 5 + (2 * (sin 0.5)))> .#<syntax -> .#<syntax 4> .#<syntax *> .#<syntax 5>)
;; $nfx$ : parsed-args=.#<syntax (- (+ (* 3 5) (* 2 (sin 0.5))...>
;; -4.041148922791594

;; {(3 + 1) * (2 * (2 + 1) - 1) + (2 * 5 - 5)}


;; ($nfx$ (3 + 1) * (2 * (2 + 1) - 1) + (2 * 5 - 5))
;; $nfx$: #'(e1 op1 e2 op2 e3 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:63:76 ((3 + 1) * (2 * (2 + 1) - 1) ...>
;; $nfx$: (syntax->list #'(e1 op1 e2 op2 e3 op ...))=(.#<syntax (3 + 1)> .#<syntax *> .#<syntax (2 * (2 + 1) - 1)> .#<syntax +> .#<syntax (2 * 5 - 5)>)
;; $nfx$ : parsed-args=.#<syntax (+ (* (+ 3 1) (- (* 2 (+ 2 1)...>
;; 25

;; {x <- #(1 2 3)[1] + 1}


;; ($nfx$ x <- ($bracket-apply$ #(1 2 3) 1) + 1)
;; $nfx$: #'(e1 op1 e2 op2 e3 op ...)=.#<syntax:Users/mattei/Library/CloudStorage/Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:63:76 (x <- ($bracket-apply$ #(1 2 ...>
;; $nfx$: (syntax->list #'(e1 op1 e2 op2 e3 op ...))=(.#<syntax x> .#<syntax <-> .#<syntax ($bracket-apply$ #(1 2 3) 1)> .#<syntax +> .#<syntax 1>)
;; $nfx$ : parsed-args=.#<syntax (<- x (+ ($bracket-apply$ #(1...>

;; bracket-apply : #'parsed-args=.#<syntax (list 1)>



;; x
;; x
;; 3


;; {- - 3}
;; 3

;; {3 · 5 + 2}
;; 17

;; {3 · 5 + 2 ³}
;; !*-generic-infix-parser : terms = ((.#<syntax +> (.#<syntax ·> .#<syntax 3> .#<syntax 5>) (** .#<syntax 2> 3)))
;; $nfx$ : parsed-args=.#<syntax (+ (· 3 5) (** 2 3))>
;; 23

;; {3 ² + 2 · 3 · 5 + 5 ²}
;; 64

;; {3 ⁻²}
;; 1/9

;; {2 ¹⁰}
;; 1024

;; {5 -  - 2}


;; ($nfx$ 5 - - 2)
;; !*prec-generic-infix-parser : terms=(.#<syntax 5> .#<syntax -> .#<syntax -> .#<syntax 2>)
;; #<procedure:->
;; !*prec-generic-infix-parser : deep-terms=(.#<syntax 5> .#<syntax -> (- .#<syntax 2>))
;; !*prec-generic-infix-parser 2 : deep-terms=(.#<syntax 5> .#<syntax -> (- .#<syntax 2>))
;; !*prec-generic-infix-parser : rv : deep-terms:(.#<syntax 5> .#<syntax -> (- .#<syntax 2>))
;; 7

;;  {3 * 5 -  - 2}
;; 17

;; {5 - - - - + - - 2}
;; 7



;;(define a 7)
;;(define b 3)

;; {a * - b}
;; -21



;; {- 2 · 3}
;; -6

;; {3 · (2 ³ - 1)}
;; 21

;; {(2 ³) ⁴}
;; 4096

;; {10.0 - 3.0 - 4.0 + 1 - 5.0 * 2.0 ** 3.0 / 7.0 ** 3.0}
;; 3.883381924198251


;; {1 + 2 ** 3 ** 4}
;; 2417851639229258349412353


;; (define (foo x y) {-4 · sin(x) + x · y ² - 5 · x / y})
;; (foo 1.23 3.4)
;; 8.640021262861444

;; in Python
;; def foo(x,y):
;;     return -4 * sin(x) + x * y ** 2 - 5 * x / y
;;
;; from math import *
;; foo(1.23,3.4)
;; 8.640021262861444

;; {2 ³}
;; 8

;; {2 ³ ⁴}  ; strange syntax :-) for 2 ** 3 ** 4 = 2 ** (3 ** 4)
;; 2417851639229258349412352

;; {3 ** (2 + 1)}
;; 27

;; note: under Linux and on a PC (french) keyboard superscript characters
;; can be generated with the keystroke sequence: ^ n where n is  a number or sign
;; under MacOS the keystroke sequence is : Command Shift + n (you really need to type + key for superscript)

;; {(for/sum ([k (in-range 5)]) k)}
;; 10

;; (define n 7)
;; {(- n 4)}
;; 3


;; {2 * (- n 4)}
;; 6

;; {(1 + 2 + 3)}
;; 6

;; {(3 + 1) * (2 * (+ 2 1) - 1) + (2 * 5 - 5)}
;; 25

;; {(3 + 1) * (2 * (+ 2 1) - 1) + ((* 2 5) - 5)}
;; 25




;; {(3 + 1) * (2 * (+ 2 1) - (sin 0.3)) + ((* 2 5) - 5)}

;;(+
;; (*
;;   (+ 3 1)
;;   (- (* 2 (+ 2 1)) (sin 0.3)))
;;  (- (* 2 5) 5))


;; (+ (* (+ 3 1) (- (* 2 (+ 2 1)) (sin 0.3))) (- (* 2 5) 5))
;; 27.817919173354642


;; {(3 + 1) * (2 * (+ 2 1) - sin(0.3)) + ((* 2 5) - 5)}
;; 27.817919173354642


;; {3 * (+ 2 4) - 1}
;; 17

;; {(- 7 (3 * (+ 2 4) - 1))}
;; -10

;; {(- 7 (3 * (+ 2 4) - 1)) + 3}
;; -7

;; {(+ 2 3) * 4}
;; 20


;; (define x 2)
;; (define y 4)
;; (define z 4)
;; {x < 3 < y <= z}
;; #t

;; (define y 1)
;; {x < 3 < y <= z}
;; #f


;; (define add +)
;; #<eof>
;; > {1 add 2 add 3}
;; (add 1 2 3)
;; 6



;; (define (add) +)
;; #<eof>
;; > {1 (add) 2 (add) 3}
;; ((add) 1 2 3)
;; 6




;; runtime tests

;; > (define (inferior) <)


;; (define (inferior) <)


;; #<eof>
;; > (define (three) 3)


;; (define (three) 3)


;; #<eof>
;; > {2 (inferior) (three) <= 4}


;; (!*prec-generic-infix-parser-runtime
;;  (list 2 (inferior) (three) <= 4)
;;  (lambda (op a b) (op a b)))
;; !*prec-generic-infix-parser-runtime launching in/equalities-state-1
;; !*prec-generic-infix-parser-runtime after in/equalities : deep-terms=(2 #<procedure:<> 3 #<procedure:<=> 4)

;; !*prec-generic-infix-parser-runtime : multiple in/equalities detected
;; !*prec-generic-infix-parser-runtime : in-eq-terms=(#<procedure:&&> (#<procedure:<> 2 3) (#<procedure:<=> 3 4))
;; #t


;; #<eof>
;; > (define (inferior-and-equal) (equal))


;; (define (inferior-and-equal) (equal))


;; #<eof>
;; > (define (inferior-strict-or-equal) (inferior-or-equal))


;; (define (inferior-strict-or-equal) (inferior-or-equal))


;; #<eof>
;; > (define (inferior-or-equal) <=)


;; (define (inferior-or-equal) <=)


;; #<eof>
;; > {2 (inferior) (three) (inferior-strict-or-equal) 4}


;; (!*prec-generic-infix-parser-runtime
;;  (list 2 (inferior) (three) (inferior-strict-or-equal) 4)
;;  (lambda (op a b) (op a b)))
;; !*prec-generic-infix-parser-runtime launching in/equalities-state-1
;; !*prec-generic-infix-parser-runtime after in/equalities : deep-terms=(2 #<procedure:<> 3 #<procedure:<=> 4)

;; !*prec-generic-infix-parser-runtime : multiple in/equalities detected
;; !*prec-generic-infix-parser-runtime : in-eq-terms=(#<procedure:&&> (#<procedure:<> 2 3) (#<procedure:<=> 3 4))
;; #t


;; #<eof>
;; >


;; > {2 (inferior) (three) (inferior-strict-or-equal) 4 = 2}


;; (!*prec-generic-infix-parser-runtime
;;  (list 2 (inferior) (three) (inferior-strict-or-equal) 4 = 2)
;;  (lambda (op a b) (op a b)))
;; !*prec-generic-infix-parser-runtime launching in/equalities-state-1
;; !*prec-generic-infix-parser-runtime after in/equalities : deep-terms=(2 #<procedure:<> 3 #<procedure:<=> 4 #<procedure:=> 2)

;; !*prec-generic-infix-parser-runtime : multiple in/equalities detected
;; !*prec-generic-infix-parser-runtime : in-eq-terms=(#<procedure:&&> (#<procedure:<> 2 3) (#<procedure:<=> 3 4) (#<procedure:=> 4 2))
;; #f

;; #<eof>




;;> (define (cinque) 5)
;; (define (cinque) 5)

;; #<eof>
;; > (define (mult) *)

;; (define (mult) *)

;; #<eof>
;; > (define add +)

;; (define add +)

;; #<eof>
;; > {3 (mult) (cinque) add 2}

;; (!*prec-generic-infix-parser-runtime
;;  (list 3 (mult) (cinque) add 2)
;;  (lambda (op a b) (op a b)))
;; 17

;; #<eof>
;; > 



;; (define str (string-append "abcdef"))
;; (define (tre) 3)
;; (define (due) 2)
;; (define (multiply) *)
;; (define (minus) -)
;; (define (quattro) 4)
;; (define (cinque) 5)
;; {str[{(tre) (multiply) (due) (minus) (quattro)}]}
;; #\c
