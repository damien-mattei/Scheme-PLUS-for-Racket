;; check that expression is infix


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


(module infix racket/base

	(provide infix-simple?
		 simple-infix-list-syntax?)

	(require Scheme+/syntax
		 Scheme+/operators-list)

  

  
;; modified for syntax too
	

;; > (infix-simple? '(2 + 3 -))
;; #f
;; > (infix-simple? '(2 + 3))
;; #t
;; > (infix-simple? '(+ 2 3))
;; #f
;; > (infix-simple? '(+ 2 3 4))
;; #f
;; > (infix-simple? '(3))
;; #f
;; > (infix-simple? '3)
;; #t

;; check full expression for infix (but only at top level)
;; Warning: check only infix at the top level of an expression (do not dive in subexpressions)
(define (infix-simple? expr)

  (define oper-lst operators-lst-syntax)
  ;;(display "infix-simple? : expr=") (display expr) (newline)
   
  ;; check we have (operator expression ...) recursively
  (define (infix-rec? expr) ; (op1 e1 op2 e2 ...)

    ;;(display "infix-rec? : expr=") (display expr) (newline)

    (if (null? expr)
	#t
	;; (begin
	;;   ;; some debug info
	;;   (when (not (null? (cdr expr)))
	;;     (display "infix-rec? forbids: op1 without e1 : (not (null? (cdr expr))) :") (display (cdr expr)) (newline))
	;;   (display "infix-rec? check (op1 e1 ...) : (member-generic (car expr) oper-lst) : ") (display (member-generic (car expr) oper-lst)) (newline)
	;;   (display "infix-rec? : (car expr)=") (display (car expr)) (newline)
	;;   ;;(display "infix-rec? : oper-lst=") (display oper-lst) (newline)
	;;   (display "infix-rec? check not (op1 op2 ...) : (not (member-generic (cadr expr) oper-lst)) :") (display (not (member-generic (cadr expr) oper-lst))) (newline)


    	(and (not (null? (cdr expr))) ; forbids: op1 without e1
	     (member-generic (car expr) oper-lst) ;; check (op1 e1 ...)   test both syntax or symbol operators
	     (not (member-generic (cadr expr) oper-lst)) ; check not (op1 op2 ...)   warning: does not allow  + - + - - etc
	     (infix-rec? (cddr expr)))
	
	;;) ;  end begin

	)) ; continue with (op2 e2 ...) 




  

  
  (define rv
    
    (cond ((not (list? expr)) ;; sort of atom
	   ;; (begin
	   ;;   (display "infix-simple? : not a list") (newline)
	   ;;   (display "infix-simple? : check not an operator: (not (member-generic expr oper-lst)):") (display (not (member-generic expr oper-lst))) (newline)
	   (not (member-generic expr oper-lst)) ;; TODO: pourquoi + ne serait pas une expression infix-simple???
	  ; ) ;  end begin
	   ) ; ex: 3 , not an operator !
	  
	  ((null? expr) ;; (begin
			;;   (display "infix-simple? : null expr") (newline)
	   #t
	   ;; ) ; end begin
	   ) ; by definition

	  
	  ;; > (infix-simple? `((,(syntax cos) (,(syntax *) ,(syntax 2) ,(syntax 0.3)))) operators-lst-syntax)
	  ;; (infix-simple? `((,#'cos (,#'* ,#'2 ,#'0.3))) operators-lst-syntax)
	  ;; #t

	  ;; (a) allowed or not as infix
	  ((null? (cdr expr)) ;; (begin
			      ;; 	(display "infix-simple? : (a)  allowed or not  as infix ,check code") (newline)
	   #t ;#f
	   ;;  ) ; end begin
	   ) ; (a) allowed or not as infix

	  
	  (else
	   
	   (and ;; (begin
		;;   (display "infix-simple? : check not start with an operator:")
		;;   (display (not (member-generic (car expr) oper-lst)))
		;;   (newline)
	    (not (member-generic (car expr) oper-lst)) ; not start with an operator !  warning: does not allow  + - + - - etc
		;;  ) ; end begin 
					
	    (infix-rec? (cdr expr)))))) ; sublist : (op arg ...) match infix-rec
  
  ;;(display "infix-simple? : rv=") (display rv) (newline)

  rv ; return rv
  
  ) ; end define infix-simple?



;; procedures shared with SRFI 105

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

 


) ; end library

