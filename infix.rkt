;; check that expression is infix


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


(module infix racket/base

	(provide infix?
		 simple-infix-list-syntax?)

  (require Scheme+/syntax)

  

  
;; modified for syntax too


;; Welcome to DrRacket, version 8.13 [cs].
;; Language: r6rs, with debugging; memory limit: 8192 MB.
;; > (infix? '(2 + 3 -) operators-lst)
;; #f
;; > (infix? '(2 + 3) operators-lst)
;; #t
;; > (infix? '(+ 2 3) operators-lst)
;; #f
;; > (infix? '(+ 2 3 4) operators-lst)
;; #f
;; > (infix? '(3) operators-lst)
;; #f
;; > (infix? '3 operators-lst)
;; #t


;; Warning: check only infix at the top level of an expression (do not dive in subexpressions)
(define (infix? expr oper-lst)

  ;; (display "infix? : expr=") (display expr) (newline)
  ;; (display "infix? : oper-lst=") (display oper-lst) (newline)

  
  ;; check we have (operator expression ...) recursively
  (define (infix-rec? expr) ; (op1 e1 op2 e2 ...)

    ;;(display "infix-rec? : expr=") (display expr) (newline)

    (if (null? expr)
	#t
	;; (begin
	;;   ;; some debug info
	;;   (when (not (null? (cdr expr)))
	;;     (display "infix? forbids: op1 without e1 : (not (null? (cdr expr))) :") (display (cdr expr)) (newline))
	;;   (display "infix? check (op1 e1 ...) : (member-syntax (car expr) oper-lst) : ") (display (member-syntax (car expr) oper-lst)) (newline)
	;;   (display "infix? check not (op1 op2 ...) : (not (member-syntax (cadr expr) oper-lst)) :") (display (not (member-syntax (cadr expr) oper-lst))) (newline)
	  
    	  (and (not (null? (cdr expr))) ; forbids: op1 without e1
	       (member-syntax (car expr) oper-lst) ;; check (op1 e1 ...) 
	       (not (member-syntax (cadr expr) oper-lst)) ; check not (op1 op2 ...)
	       (infix-rec? (cddr expr)))

	; ) ;  end begin

	)) ; continue with (op2 e2 ...) 




  

  
  (define rv
    
    (cond ((not (list? expr))
	   ;; (begin
	   ;;   (display "infix? : not a list") (newline)
	   ;;   (display "infix? : check not an operator: (not (member-syntax expr oper-lst)):") (display (not (member-syntax expr oper-lst))) (newline)
	   (not (member-syntax expr oper-lst)) ;; TODO: pourquoi + ne serait pas une expression infix???
	  ; ) ;  end begin
	   ) ; ex: 3 , not an operator !
	  
	  ((null? expr) ;; (begin
			;;   (display "infix? : null expr") (newline)
	   #t
	   ;; ) ; end begin
	   ) ; by definition

	  
	  ;; > (infix? `((,(syntax cos) (,(syntax *) ,(syntax 2) ,(syntax 0.3)))) operators-lst-syntax)
	  ;; (infix? `((,#'cos (,#'* ,#'2 ,#'0.3))) operators-lst-syntax)
	  ;; #t

	  ((null? (cdr expr)) ;; (begin
			      ;; 	(display "infix? : (a)  allowed or not  as infix ,check code") (newline)
	   #t ;#f
	   ;;  ) ; end begin
	   ) ; (a) allowed or not as infix

	  
	  (else
	   
	   (and ;; (begin
		;;   (display "infix? : check not start with an operator:")
		;;   (display (not (member-syntax (car expr) oper-lst)))
		;;   (newline)
	    (not (member-syntax (car expr) oper-lst)); not start with an operator !
	    ;; ) ; end begin 
					
	    (infix-rec? (cdr expr)))))) ; sublist : (op arg ...) match infix-rec
  
  ;;(display "infix? : rv=") (display rv) (newline)


  
  rv ; return rv
  
  ) ; end define infix?




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

