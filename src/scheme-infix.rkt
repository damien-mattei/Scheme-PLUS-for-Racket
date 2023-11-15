
;; infix evaluator with operator precedence


;;(require "./infix-operators.rkt")

;; this file must now be included in your main project file like this:
;; at the beginning of your main file add
;; for infix operator precedence:
;; (define-namespace-anchor ankh)
;; (define bsns (namespace-anchor->namespace ankh))
;; (current-namespace bsns)

;; to be compatible with overloading of operator precedence at the end of your main file add:
;; (include "../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/included-files/scheme-infix.rkt")
;; or something like that


;; > {5 * 3 + 2}
;; 17

;; { #f or #f and (begin (display "BAD") (newline) #t)}
;; and: bad syntax in: and  (stopped working)
;; #f 

;; > { #t and #f and (begin (display "BAD") (newline) #t)}
;; #f




;; > (define x 3)
;; > (define y -5)
;; > (!0 3 * x + (* 5 y) - 7)
;; -23

;;  (define x 3)
;; > (!0 x * 5 >= 10)
;; #t
;; ($nfx x * 5 >= 10)
;; #t
;; ($nfx$ 3 * x + (* 5 y) - 7)
;; -23




;; can you believe they made && and || special forms??? yes :-) but with advantage of being short-circuited,but i admit it has been a headlock for an infix solution 
;; note: difference between bitwise and logic operator


;; a list of lists of operators. lists are evaluated in order, so this also
;; determines operator precedence
;;  added bitwise operator with the associated precedences and modulo too
;; (define infix-operators-lst
  
;;   ;;'()
  
;;   (list
   
;;    (list expt **)
;;    (list * / %)
;;    (list + -)
   
;;    (list << >>)

;;    (list & ∣ )

;;   					; now this is interesting: because scheme is dynamically typed, we aren't
;;   					; limited to any one type of function
   
;;    (list < > = <> ≠ <= >=)
    
;;    ;;(list 'dummy) ;; can keep the good order in case of non left-right assocciative operators.(odd? reverse them) 
   
;;    )

;;   )



;; calls order: !prec -> !0 -> !*
;; !*prec -> !*


;; > (!0 #f && (begin (display "BAD") (newline) #t))
;; BAD
;; #f
;;; evaluates `terms` as a basic infix expression
;;(define (!0 . terms)
;;(define (!0 infix-operators . terms)
(define (!0 infix-operators terms)
  ;;(display "! : infix-operators=") (display infix-operators) (newline)
  ;;(display "! : terms=") (display terms) (newline)
  (if (null? terms) ;; i added this null case but with correct input this should not be necessary
      terms
      (car (!* terms
	       infix-operators
	       #f))))

(define (!prec . terms) ;; precursor of !0
  ;;(display "!prec : infix-operators-lst=") (display infix-operators-lst) (newline)(newline)
  (!0 (cdr infix-operators-lst) terms)) ; we skip the version number




;; > (define b #f)
;; > ($nfx$ #t and b)
;; #f

;; > ($nfx$ #f and (begin (display "BAD") (newline) #t))
;; #f
;;  { 4 + 3 * 2 - 19 < 0 - 4}
;; #t
(define-syntax $nfx$
  (syntax-rules ()

    ((_ term) term) ;; this infix case is not different than prefix or postfix , useless for infix with precedence

    ((_ term1 op term2) (op term1 term2)) ;; no precedence used here , useless for infix with precedence

    ((_ ident opspecial term1 op term2) (cond ((or (equal? (quote opspecial) (quote <-)) (equal? (quote opspecial) (quote ←)))
						   ;;(equal? (quote opspecial) (quote <+)) (equal? (quote opspecial) (quote ⥆)))
					       ;;(begin
						 ;;(display "$nfx$ case 1") (newline)
						 (opspecial  ident (op term1 term2))) ;; {ident <- term1 op term2}
					      
					      ((or (equal? (quote op) (quote ->)) (equal? (quote op) (quote →)))
					       (op term2 (opspecial ident term1))) ;; Warning: argument names of macro do not represent the values contained in this case
					      
					      
					      (else
					       (!prec
						;; here we need to (eval o quote) the <- or -> to avoid a bad syntax error with those macros at expansion stage but this case will never be evaluated at evaluation stage with <- or -> !
						ident (eval (quote opspecial) (current-namespace)) term1 op term2))))
    
						   
    ((_ ident opspecial term1 op term2 ...) (if (or (equal? (quote opspecial) (quote <-)) (equal? (quote opspecial) (quote ←)))
						;;  (equal? (quote opspecial) (quote <+)) (equal? (quote opspecial) (quote ⥆)))
						
						;; there is no 'cond here because there is no way to deal with the 2nd case of 'cond above with multiple values unfortunately
						(opspecial ident ($nfx$ term1 op term2 ...))
						
						(!prec ident (eval (quote opspecial) (current-namespace))  term1 op term2 ...))))) ;; this is in fact a general case ($nfx$ term0 ops term1 op term2 ...)



    ;; now quoting all the macros,function, symbols ... so we are no more annoyed with macro 'bad syntax' error and also this should (?) keep the 'and and 'or short-circuited functionalities.



;; DEPRECATED (4 procedures below)
;;  (andy (#t #t #t))
;; #t
;; test if short circuited:
;;  (andy (#f (begin (display "BAD") (newline) #t)))
;; #f
;; (define-syntax andy ;; Warhol :-)
;;   (syntax-rules ()
    
;;     ((_ (term ...)) (and (eval term (interaction-environment)) ...)))) ;; as 'and and 'or has been quoted previously but as macro can not be evaluated we need procedures like the macro


;; (define-syntax ory ;; Oryx ? :-) ,no just call 'or on terms

;;    (syntax-rules ()
    
;;     ((_ (term ...)) (or (eval term (interaction-environment)) ...)))) ;; as 'and and 'or has been quoted previously but as macro can not be evaluated we need procedures like the macro
  
  			   

;; (define-syntax andy2
;;   (syntax-rules ()
    
;;     ((_ term1 term2) (if odd?
;; 			   (and (eval term1 (interaction-environment)) (eval term2 (interaction-environment)))
;; 			   (and (eval term2 (interaction-environment)) (eval term1 (interaction-environment)))))))


;; ;; > (define b #t)
;; ;; > {#f or #f or b}
;; ;; #t
;; (define-syntax ory2

;;   (syntax-rules ()
    
;;     ((_ term1 term2) (if odd?
;; 			   (or (eval term1 (interaction-environment)) (eval term2 (interaction-environment)))
;; 			   (or (eval term2 (interaction-environment)) (eval term1 (interaction-environment)))))))




;; evaluate one group of operators in the list of terms
(define (!** terms stack operators odd?)


  ;; (display "!** : terms = ") (display terms) (newline)
  ;; (display "!** : operators = ") (display operators) (newline)
  ;; (display "!** : stack = ") (display stack) (newline)
  ;; (display "!** : odd? = ") (display odd?) (newline)

					; why `odd?`? because scheme's list-iteration is forwards-only and
					; list-construction is prepend-only, every other group of operators is
					; actually evaluated backwards which, for operators like / and -, can be a
					; big deal! therefore, we keep this flipped `odd?` counter to track if we
					; should flip our arguments or not


  ;; sometimes quoted args must be evaluated, depending of odd? args order must be swaped,sometimes both !
  ;; (define (calc-proc op a b) ;; keep in mind that now the op-erator and args are quoted !
 
  ;;   (define proc (eval op (interaction-environment)))  ;; this only works with procedure, not special form !
  ;;   ;;(display "before eval arg")
  ;;   (define val-a (eval a (interaction-environment)))
  ;;   (define val-b (eval b (interaction-environment)))
  ;;   ;;(display "after eval arg")
  ;;   (if odd? (proc val-a val-b) (proc val-b val-a)))


  (define (calc op a b)
    (if odd? (op a b) (op b a))) ;; evaluation is here 

  
  ;; (define (calc op a b) ;; keep in mind that now the op-erator and args are quoted !
  ;;   ;; (display "calc : op = ") (display op) (newline)
  ;;   ;; (display "calc : a = ") (display a) (newline)
  ;;   ;; (display "calc : b = ") (display b) (newline)
  ;;   ;; (display "calc : odd? = ") (display odd?) (newline)
    
  ;;   ;; special forms cases else procedure
  ;;   ;; (cond ((eq? op 'and) (andy2 a b))
  ;;   ;; 	  ((eq? op 'or) (ory2 a b))
  ;;   ;; 	  (else (calc-proc op a b)))) ;; procedure case
  ;;   (if odd? 
  ;; 	(eval (list op a b) (interaction-environment))
  ;; 	(eval (list op b a) (interaction-environment))))


  ;; for memory: (!** terms stack operators odd?)
  ;; stack is the result accumulator 
  
  (cond ((null? terms) stack) ; end case, we return the accumulated result 
	;; operator we can evaluate -- pop operator and operand, then recurse
	((and (> (length stack) 1) ;; (begin
				   ;;   (display "operators=") (display operators) (newline)
				   ;;   (let* ((op (car stack))
				   ;; 	    (mres (member op operators))) ;; (memq op operators)))
				   ;;     (display "op=") (display op) (newline)
				   ;;     (display "mres=") (display mres) (newline) (newline)
				   ;;     mres)))
	      (member (car stack) operators))
	      
	 (let ((op (car stack))
	       (fst (car terms)) ;; a
	       (snd (cadr stack))) ;; b
	   ;;(display "op=") (display op) (newline)
	   (!** (cdr terms)
		(cons (calc op fst snd) (cddr stack))
		operators
		(not odd?))))
	
	;; otherwise just keep building the stack  (if operator not found we do not evaluate expression! bad!)
	(else (!** (cdr terms)
		   (cons (car terms) stack)
		   operators
		   (not odd?)))))



;; evaluate a list of groups of operators in the list of terms
(define (!* terms operator-groups odd?)
  ;; (display "!* : terms = ") (display terms) (newline)
  ;; (display "!* : operator-groups = ") (display operator-groups) (newline)
  (if (or (null? operator-groups) ; done evaluating all operators
	  (null? (cdr terms)))    ; only one term left
      terms ; finished processing operator groups
      ;; evaluate another group -- separating operators into groups allows
      ;; operator precedence
      (!* (!** terms '() (car operator-groups) odd?)
	  (cdr operator-groups)
	  (not odd?))))



;; {5 * 3 + 2}

;; ; also works for inequalities!

;; { 4 + 3 * 2 - 19 < 0 - 4}

;; { #f and (begin (display "BAD") (newline) #t)}

;; scheme@(guile-user)> (define c 300000)
;; scheme@(guile-user)> (define v 299990)
;; scheme@(guile-user)> (define t 30)
;; scheme@(guile-user)> (define x 120)
;; scheme@(guile-user)> (declare xp)
;; scheme@(guile-user)> '{xp <- {x - v * t} / (sqrt {1 - v ** 2 / c ** 2})} 
;; ($nfx$ xp <- ($nfx$ x - v * t) / (sqrt ($nfx$ 1 - v ** 2 / c ** 2)))
;; scheme@(guile-user)> { xp <- {x - v * t} / (sqrt {1 - v ** 2 / c ** 2}) } 
;; -1102228130.2405226
;; scheme@(guile-user)> xp
;; -1102228130.2405226


(define (!*prec terms)   ;; precursor of !*
  ;;(display "!*prec : infix-operators-lst=") (display infix-operators-lst) (newline)
  (if (null? terms) 
      terms
      (!* terms (cdr infix-operators-lst) #f))) ; we skip the version number

