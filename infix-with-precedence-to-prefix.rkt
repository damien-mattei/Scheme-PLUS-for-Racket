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
	   ;;recall-infix-parser
	   superscript-operator-loop
	   begin-operators+-)

  (require ;(only-in srfi/1 any)
	   Scheme+/syntax
	   Scheme+/prefix
	   Scheme+/operators-list
	   Scheme+/operators
	   Scheme+/infix
	   Scheme+/def
	   SRFI-105/SRFI-105-curly-infix ; for alternating-parameters
	   Scheme+/superscript-parser
	   Scheme+/block
	   Scheme+/infix-prefix
	   Scheme+/plus-minus-parser
	   Scheme+/atom) 
	  

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
  (if (or (null? operator-groups) ; done evaluating all operators
	  (null? (cdr terms)))    ; only one term left
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





;; deal with simple infix with same operator n-arity
;; check we really have infix expression before
;; wrap a null test
(define (pre-check-!*-generic-infix-parser terms creator)

  ;;(display "pre-check-!*-generic-infix-parser : terms = ") (display terms) (newline)

  ;; todo: terms = (.#<syntax n> .#<syntax -> .#<syntax 1> .#<syntax :n> .#<syntax +> .#<syntax 1>) n'a pas été detecté 'pas infix' mais trouve plus où est l'exemple fautif

  ;; pre-check we have an infix expression because parser can not do it and then will return erroneous result
  ;; as we pre parsed the expressions infix? is enought checking, we check the whole expression (not just testing the begin)
  (when (not (infix? #;infix-simple? terms))
	(newline)
	(display "pre-check-!*-generic-infix-parser :  arguments do not form an infix expression :terms: ") (display terms) (newline)
	(newline)
	(error "pre-check-!*-generic-infix-parser  : arguments do not form an infix expression :terms:"
	       terms))
  

  (if (null? terms) ;; never for infix as there is e1 op1 e2 op2 e3 at least
	terms
	(!*-generic-infix-parser terms ; (reverse terms) ; will now reverse only later when expo
				 infix-operators-lst-for-parser-syntax
				 creator)))




;; DEPRECATED
;; use in a map to recurse deeply
;; why so long and complex as we should only recall !*prec-generic-infix-parser ???
;; (define (recall-infix-parser-bak expr creator)

;;   (display "recall-infix-parser : expr =") (display expr) (newline)

;;   (var-syntax2list expr)
  
;;   ;; (display "recall-infix-parser (after syntax test) : expr= ") (display expr) (newline)
;;   ;; (display "recall-infix-parser : (list? expr)= ") (display (list? expr)) (newline)

;;   ;; (display "recall-infix-parser : before cond ") (newline)
;;   (display "recall-infix-parser 2 : expr =") (display expr) (newline)

  
;;   (cond ((not (list? expr)) ; atom
;; 	 (display "recall-infix-parser : expr not list.") (newline)
;; 	 expr)
	
;; 	((null? expr) ; ()
;; 	 (display "recall-infix-parser : null expr.") (newline)
;; 	 expr)

;; 	((null? (cdr expr)) ; (a)  what todo with a? a could be of form a=(b) 
;; 	 (display "recall-infix-parser : null cdr expr.") (newline)
;; 	 expr)

;; 	;; at least 2 elements in list

;; 	;; i suppose here we do  nothing else than returning expr=($nfx$ ...) because, latter $nfx$ will do the job
;; 	((datum=? '$nfx$ (car expr)) ; test {e1 op1 e2 ...}
;; 	 (display "recall-infix-parser : $nfx$ detected.") (newline)
;; 	 expr)

;; 	;; already in prefix !
;; 	((prefix? #;prefix-limited? expr) ; test (proc1 arg0 arg1 ...)
;; 	 (display "recall-infix-parser : prefix expr detected.") (newline)
;; 	 (cons (car expr) ; todo : should we recall on first element too? example ((f ° g) x) i think yes !
;; 	       (map (lambda (x) (recall-infix-parser x creator)) ; but recall on arguments that could be infix
;; 		    (cdr expr))))
;; 	;;expr)

;; 	(else ; infix , we transform it in prefix...
;; 	 (display "recall-infix-parser : infix parsing....") (newline)
;; 	 (define expr-d
;; 	   ($+>
;; 	    ;; DO it only when infix
;; 	    ;; parse superscript number **  and successive operands *  and after for + - (precedence rule for exponential versus signs
;; 	    ;; to did: deal with syntax with var-syntax2list
;; 	    ;; done: why doing all those premice as they are in the call of !*prec-generic-infix-parser ???
;; 	    (define parsed-superscript (superscript-operator-loop expr))
;; 	    (display "!*prec-generic-infix-parser : parsed-superscript=") (display parsed-superscript) (newline)
;; 	    (define parsed+- (begin-operators+- parsed-superscript '()))
;; 	    (display "recall-infix-parser : parsed+-=") (display parsed+-) (newline)
;; 	    (set! expr parsed+-)
;; 	    (newline)
;; 	    (display "recall-infix-parser : calling !*prec-generic-infix-parser") (newline)
;; 	    (newline)
;; 	    (!*prec-generic-infix-parser expr creator))   ; recursive call to the caller
;; 	   ) ;  end define
;; 	 (display "recall-infix-parser : expr-d=")(display expr-d)(newline)
;; 	 ;;(car expr-d);  probably because the result will be encapsuled in a list !
;; 	 expr-d
;; 	 ) ; end else
;; 	) ; end cond
;;   )
					; end recall...

;; DEPRECATED
;; (define (recall-infix-parser expr creator)

;;   (display "recall-infix-parser : expr =") (display expr) (newline)

;;   (!*prec-generic-infix-parser expr creator))   ; recursive call to the caller




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main entry routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; precursor generic infix parser
;; (this is generally the main entry routine of this module)
(def (!*prec-generic-infix-parser terms creator)   ;; precursor of !*-generic-infix-parser

  ;; (display "!*prec-generic-infix-parser : start terms=") (display terms) (newline)
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

  ;; if we already have prefix (and already recall on the deep terms) then why continuing?
  ;; (when (prefix? terms) 
  ;; 	(display "!*prec-generic-infix-parser returning early 0") (newline)
  ;; 	(return terms))
 
 
 
 
  ;; parse superscript number **  and successive operands *  and after for + - (precedence rule for exponential versus signs)
  (define parsed-superscript (superscript-operator-loop terms))
  ;;(display "!*prec-generic-infix-parser : parsed-superscript=") (display parsed-superscript) (newline)

  (define parsed+- parsed-superscript) ; by default

  ;; forbid to treat (something) ,example : (a_syntax)
  ;; unless that it would return a_syntax , loosing the ( ) because begin-operators+- return sometimes the car when there is only one element in the return list
  (when (and (not (null? parsed-superscript))
	     (not (null? (cdr parsed-superscript)))
	     (infix? parsed-superscript)) 
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
       (map (lambda (x)
	     (!*prec-generic-infix-parser x creator)) 
	   deep-terms)))

  ;; (display "!*prec-generic-infix-parser 1 : deep-terms=") (display deep-terms) (newline)
  ;; (newline)


  ;; general case of mapping in infix deep terms
  ;; if there is (define ... we must not compute deep-terms with !*prec-generic-infix-parser but simply copy the terms in deep-terms
  ;; because we do not want to evaluate any ( ... ) as infix but as prefix

  (when (and (list? deep-terms)
	     (not (datum=? 'define ; define is preserved this way (no infix recursive in define)
			   (car deep-terms))))
	;;(display "!*prec-generic-infix-parser : recalling !*prec-generic-infix-parser via map") (newline) (newline)
	(set! deep-terms (map (lambda (x)
				(!*prec-generic-infix-parser x creator))
			      deep-terms)))
  
  ;; (display "!*prec-generic-infix-parser 2 : deep-terms=") (display deep-terms) (newline)
  ;; (newline)

  ;; if we already have prefix (and already recall on the deep terms) then why continuing?
  (when (prefix? deep-terms) 
  	;;(display "!*prec-generic-infix-parser returning early 2") (newline)
  	(return deep-terms))
 

  
  (define rv

    (begin
      ;;(display "!*prec-generic-infix-parser : rv : deep-terms:") (display deep-terms) (newline)
      ;; test for simple-infix (no operator precedence)
      (if (simple-infix-list-syntax? deep-terms)
	  (begin
	    ;;(display "!*prec-generic-infix-parser : deep-terms is a simple infix list") (newline)
	    ;;(display "!*prec-generic-infix-parser : deep-terms=") (display deep-terms) (newline)
	    (list ; cadr is op in arg1 op arg2 op ....
	     (cons (cadr deep-terms) (alternating-parameters deep-terms)))) ; we put it in a list because rv2 take the car...

	  (begin
	    ;; (display "!*prec-generic-infix-parser : deep-terms is not a simple infix list") (newline)
	    ;; (newline)
            (pre-check-!*-generic-infix-parser  deep-terms
						creator)))))

  ;;(display "!*prec-generic-infix-parser : rv=") (display rv) (newline)

  ;; (newline)
  ;; (newline)

  (define rv2 (car rv))
  ;;(display "!*prec-generic-infix-parser : (car rv) = rv2 =") (display rv2) (newline)
  
  rv2)




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

;; > {(3 + 1) * (2 * (2 + 1) - 1) + (2 * 5 - 5)}


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



;; > x
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

;; > {5 -  - 2}


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

