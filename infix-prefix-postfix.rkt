;; This file is part of Scheme+

;; Copyright 2025 Damien MATTEI

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

;; > (require Scheme+/infix-prefix-postfix)
;; REPL Curly Infix:

;; (require Scheme+/infix-prefix-postfix)

;; Parsed annotations. :
;; (require Scheme+/infix-prefix-postfix)

;; > (infix-prefix-postfix-test? '(3 +))
;; REPL Curly Infix:

;; (infix-prefix-postfix-test? '(3 +))

;; Parsed annotations. :
;; (infix-prefix-postfix-test? '(3 +))

;; 'postfix

;; (require Scheme+/infix-prefix-postfix)

;; (infix-prefix-postfix-test? '(x * y))
;; 'infix

;; (infix-prefix-postfix-test? '(cons + L))
;; 'prefix

;; (infix-prefix-postfix-test? '(sin ∘ sqr))
;; 'infix

;; (infix-prefix-postfix-test? '(x ²))
;; infix-prefix-postfix State 0
;; infix-prefix-postfix State 4
;; 'infix

;; (infix-prefix-postfix-test? '(+ - x * 3))
;; 'infix

;; (infix-prefix-postfix-test? '(-4 · (sin x) + x · y ² - 5 · x / y))
;; 'infix

;; (infix-prefix-postfix-test? '(x ² + y ² + z ²))
;; infix-prefix-postfix State 0
;; infix-prefix-postfix State 4
;; 'infix

;; (infix-prefix-postfix-test? '(- 2 · a · b + b ²))
;; infix-prefix-postfix State 0
;; infix-prefix-postfix State 1
;; infix-prefix-postfix State 2
;; 'infix

;; (infix-prefix-postfix-test? '(+ - x * 3))
;; infix-prefix-postfix State 0
;; infix-prefix-postfix State 1
;; 'infix

;; (infix-prefix-postfix-test? '(- x · y))
;; infix-prefix-postfix State 0
;; infix-prefix-postfix State 1
;; infix-prefix-postfix State 2
;; 'infix

;; (infix-prefix-postfix-test? '(- x y))
;; infix-prefix-postfix State 0
;; infix-prefix-postfix State 1
;; infix-prefix-postfix State 2
;; 'prefix

;; (infix-prefix-postfix-test? '((x or y) or (a and b)))
;; (infix-prefix-postfix-test? '((x or y) or (a and b)))
;; infix-prefix-postfix State 0
;; L=((x or y) or (a and b))
;; f=(x or y)
;; fate=#(struct:exn:fail:syntax or: bad syntax #<continuation-mark-set> (.#<syntax or>))
;; generic-known-program
;; infix-prefix-postfix State 4
;; 'infix


(module infix-prefix-postfix racket/base

	(provide infix-prefix-postfix-test?
		 infix?
		 prefix?
                 postfix?
		 simple-infix-list-syntax?
		 infix-canonical?
		 two-symbols-or-list-following?
                 not-infix-and-known-procedure-at-end-then-postfix?)

	(require Scheme+/operators
		 Scheme+/superscript
		 Scheme+/condx
		 Scheme+/def-function
		 Scheme+/program-type
		 Scheme+/syntax
                 (only-in srfi/1 first)
                 (only-in racket/list rest)
                 Scheme+/operators-list)


	;; State 0
	(def (infix-prefix-postfix-test? L)
	  ;;(display "infix-prefix-postfix State 0") (newline)
	  ;;(display "L=") (display L) (newline)

	  (var-syntax2list L)
	  
	  
	  (when (superscript? L)
		(error "infix-prefix-postfix : infix-prefix-postfix-test? : SYNTAX ERROR superscript not allowed at the beginning of an expression: " L))

          ;; atoms or empty list
	  (when (or (not (list? L))
		    (null? L))
	    (display "infix-prefix-postfix-test? : state 0: infix/prefix detected") (newline)	
	    (return 'infix-prefix))
	  
	  ;; at this point list is not empty
	  (define f (car L)) 
	  ;;(display "f=") (display f) (newline)

	  (var-syntax2list f) ; detect syntax
	  ;;(display "after var-syntax2list f=") (display f) (newline)
	  
	  (when (superscript-only? f) ; allow * which is both super and normal script, do not cause error on * (which anyway can not be at beginning of a superscripted _infix_ expression)
	    (error "infix-prefix-postfix : infix-prefix-postfix-test? : SYNTAX ERROR superscript not allowed at the beginning of an expression: " L))

          (when (number? f) ; TODO check this
            (state-4
             (cdr L) f))
          
	  (when (SIGN-op? f) ; + , - 
		(return (state-1 (cdr L))))
	  ;; debug
	  ;; (when (operator-symbol-or-syntax? f)
	  ;; 	(display "operator-symbol-or-syntax") (newline))

	  ;; (when (generic-known-program? f)
	  ;; 	(display "generic-known-program") (newline))

	  (when (list? f) ;; example: '((x or y) or (a and b))
		(return (state-4
			 (cdr L) f)))
	  
	  (when (or (operator-symbol-or-syntax? f) ; operators
		    (generic-known-program? f)) ; procedures,macros
		(return (state-3 (cdr L))))
	  
	  ;; other cases (including for example: (- 2) · a · b + b ² which should not at this stage but could be detected at runtime) ???
          ;; numbers ?
	  (state-4
	   (cdr L) f))
	  


	
	(def (state-1 L)
	  ;;(display "infix-prefix-postfix State 1") (newline)

	  (var-syntax2list L)
	     
	  (condx ((null? L) 'prefix)
		 (exec (define f (car L))
		       (var-syntax2list f))
		 ((or (superscript? f)
		      (and (operator-symbol-or-syntax? f)
			   (not+-? f)
			   (not (COMPOSITION-op? f))))
		  (error "infix-prefix-postfix : state-1 : SYNTAX ERROR in parsing the remaining list: " L))
		 ((or (SIGN-op? f)
		      (COMPOSITION-op? f)) 'infix) ; + - ∘
		 ;; at this point this could not be an operator
		 (else (state-2 (cdr L)))))

	
	

	(def (state-2 L)
	  ;;(display "infix-prefix-postfix State 2") (newline)
	  (var-syntax2list L)
	  
	  (when (null? L)
		(return 'prefix))
	  
	  (define f (car L))
	  (var-syntax2list f)
	  
	  (cond ((NO-OP? f) 'prefix)
		((or (arithmetic-operator? f) 
		     (superscript? f)) 'infix)
		(else (error "infix-prefix-postfix : state-2 : unknow case in parsing the remaining list: " L))))


	

	(def (state-3 L)
	  ;;(display "infix-prefix-postfix State 3") (newline)

	  (var-syntax2list L)
	     
	  (when (null? L)
	     (return 'prefix))
	  
	  (define f (car L))
	  (var-syntax2list f)
	  
	  (cond ((COMPOSITION-op? f) 'infix)
		;;((SINGLE-VARIABLE-ASSIGNMENT-op? f) 'infix)
		((ASSIGNMENT-op? f) 'infix)
		((DEFINE-op? f) 'infix)
		((superscript? f) (error "infix-prefix-postfix : state-3 : SYNTAX ERROR ,find superscript in parsing the remaining list: " L))
		(else 'prefix)))
	

	
	(def (state-4 L p) ; p: previous element
	  ;;(display "infix-prefix-postfix State 4") (newline)
	  ;;(display "L=") (display L) (newline)

	  (var-syntax2list L)
	  
	  (when (null? L)
	    (return 'prefix))

	  (define f (car L))
	  (var-syntax2list f)

          (define r (rest L))
	  
	  (condx ((and (not (null? r))
                       (or (superscript? f) ; superscript exponentiation (power)
                           (COMPOSITION-op? f) ; function composition
                           (operator-symbol-or-syntax? f)))
		  'infix)
                 
		 ; a case where it is postfix when no op,no known procedure,no superscript,no composition in middle
		 ; and known procedure at end
		 (exec (define rev-L (reverse L))
                       (define rest-rev-L-datum (map secure-syntax->datum (cdr rev-L)))
		       (define last-elem (first rev-L))
                       (define last-elem-datum (secure-syntax->datum last-elem))
                       (define p-datum (secure-syntax->datum p))
		       ;(define middle-rev-L (rest rev-L))
                       ;(display "state-4 : middle-rev-L:") (display middle-rev-L) (newline)
                       #;(define (no-op/superscript/proc/compo s)
                         (not (or (operator-symbol-or-syntax? s)
                                  (superscript? s)
                                  (generic-known-program? s)
                                  (COMPOSITION-op? s)))))
		 ((or (member-in-postfix-lst last-elem-datum)
                      (and (generic-known-program? last-elem) ; test only in parser, not runtime! catastrophic time penalty on prefix/postfix test
                           (number? p-datum)
                           (andmap number? rest-rev-L-datum)))
                       
                  ;(display "infix-prefix-postfix State 4 postfix :") (display L) (newline)
                  ;(display "last element : ") (display last-elem) (newline)
                  ;(display "(generic-known-program? last-elem) : ") (display (generic-known-program? last-elem)) (newline)
                  'postfix)
		 
		 (else 'prefix)))

	
	
	(define (infix? L)
	  (define rs (infix-prefix-postfix-test? L))
	  (if (or (eq? rs
		       'infix)
		  (eq? rs
		       'infix-prefix))
	      #t
	      #f))

	(define (prefix? L)
	  ;;(display "infix-prefix-postfix.rkt : prefix? : L = ") (display L) (newline)
	  (if (eq? (infix-prefix-postfix-test? L)
		   'prefix)
	      #t
	      #f))


        (define (postfix? L)
          (if (eq? (infix-prefix-postfix-test? L)
                   'postfix)
              #t
              #f))

        ; test for not infix and known procedure at end, then should be postfix
        (def (not-infix-and-known-procedure-at-end-then-postfix? L)

          (var-syntax2list L) ; by precaution

          (when (infix? L)
            (return #f))

          (when (null? L)
            (return #f))
          
          (define f (car (reverse L)))
          
          (generic-known-program? f))


	;; this procedure check that we have a canonical infix expression
	;; i call 'canonical' an expression such as 3 * 5 + 2
	;; in contrary an equivalent expression such as this one: - - 3 * 5 + 2 is not 'canonical',etc
	;; conditions to be 'canonical' will be to have :
	;; * at least 3 terms in expression
	;; * odd number of terms
	;; * operators between terms

	;;{2 * 3 4}
	;;($nfx$ 2 * 3 4)
	;;. . ../infix-with-precedence-to-prefix.rkt:452:14: infix-with-precedence-to-prefix.rkt : !*prec-generic-infix-parser : not a canonical infix expression:  '(#<syntax 2> #<syntax *> #<syntax 3> #<syntax 4>)
	(def (infix-canonical? L)
	  ;;(display "infix-canonical? : L =")(display L)(newline)	  
	  (define lgt (length L))
	  ;;(display "infix-canonical? : lgt =")(display lgt)(newline)
	  (when (or (< lgt 3)
		    (not (odd? lgt)))
	    (return #f))
	  (def (check-operators? L2)
	    ;;(display "check-operators? : L2=")(display L2)(newline)   
	    (when (null? L2)
	      (return #t))
	    (if (not (operator-syntax? (car L2)))
		#f
		(check-operators? (cddr L2))))
	  (check-operators? (cdr L)))
	

	;; procedures adapted from SRFI 105 for syntax

; check that we have a simple infix list, i.e an infix list with always the same operator (no precedence needed to parse)

  ; Return true if lyst has an even # of parameters, and the (alternating)
  ; first parameters are "op".  Used to determine if a longer lyst is infix.
  ; If passed empty list, returns true (so recursion works correctly).
  
  (define (even-and-op-prefix-syntax? op lyst)
    (cond
      ((null? lyst) #t)
      ((not (pair? lyst)) #f)
      ((not (datum=? op (car lyst))) #f) ; fail - operators not the same
      ((not (pair? (cdr lyst)))  #f) ; Wrong # of parameters or improper
      (#t   (even-and-op-prefix-syntax? op (cddr lyst))))) ; recurse.

  ; Return true if the lyst is in simple infix format
  ; (and thus should be reordered at read time).
  
  (define (simple-infix-list-syntax? lyst)
    (and
      (pair? lyst)           ; Must have list;  '() doesn't count.
      (pair? (cdr lyst))     ; Must have a second argument.
      (pair? (cddr lyst))    ; Must have a third argument (we check it
                             ; this way for performance)
      (even-and-op-prefix-syntax? (cadr lyst) (cdr lyst)))) ; true if rest is simple

 
  ;; check if there is at least one operator in 2 following tokens
  ;; if not then the infix expression will require evaluation before
  ;; operator precedence application
  (define (two-symbols-or-list-following? L)
    
    (define (rec-2? L p) ; p : previous find of not a known operator
      ;;(display "rec-2? : L=") (display L) (newline)
      ;;(display "rec-2? : p=") (display p) (newline)
      (cond ((null? L) #f)
	    ((operator? (car L))
	     (rec-2? (cdr L) #f))
	    (p #t)
	    (else
	     (rec-2? (cdr L) #t))))

    (rec-2? L #f))
	      


	
) ; end module
