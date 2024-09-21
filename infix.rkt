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


(module infix racket

  (provide infix?)

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

(define (infix? expr oper-lst)

  ;;(display "infix? : expr=") (display expr) (newline)
  ;;(display "infix? : oper-lst=") (display oper-lst) (newline)
  
  (define (infix-rec? expr) ; (op1 e1 op2 e2 ...)
    ;;(display "infix-rec? : expr=") (display expr) (newline)
    (if (null? expr)
	#t
    	(and (not (null? (cdr expr))) ; forbids: op1 without e1
	     (member-syntax (car expr) oper-lst) ;; check (op1 e1 ...) 
	     (not (member-syntax (cadr expr) oper-lst)) ; check not (op1 op2 ...)
	     (infix-rec? (cddr expr))))) ; continue with (op2 e2 ...) 


  (define rv
    (cond ((not (list? expr)) (not (member-syntax expr oper-lst))) ; ex: 3 , not an operator ! 
	  ((null? expr) #t) ; definition
	  ((null? (cdr expr)) #f) ; (a) not allowed as infix
	  (else
	   (and (not (member-syntax (car expr) oper-lst)) ; not start with an operator !
		(infix-rec? (cdr expr)))))) ; sublist : (op arg ...) match infix-rec
  
  ;;(display "infix? : rv=") (display rv) (newline)

  rv
  
  )) ; end library

