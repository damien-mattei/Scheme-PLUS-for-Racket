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


(module nfx racket


  (provide $nfx$)

  (require (for-syntax Scheme+/n-arity)
	   (for-syntax Scheme+/infix-with-precedence-to-prefix)
	   (for-syntax Scheme+/operators-list)
	   (for-syntax Scheme+/operators)
	   (for-syntax Scheme+/infix))

  
  
;; Welcome to DrRacket, version 8.13 [cs].
;; Language: r6rs, with debugging; memory limit: 8192 MB.
;; > ($nfx$ 3 * 5 + 2)
;; $nfx$ : parsed-args={.#<syntax:15-interactions from an unsaved editor:3:15 +> {.#<syntax:15-interactions from an unsaved editor:3:11 *> .#<syntax:15-interactions from an unsaved editor:3:9 3> .#<syntax:15-interactions from an unsaved editor:3:13 5>} .#<syntax:15-interactions from an unsaved editor:3:17 2>}
;; 17

(define-syntax $nfx$

  (lambda (stx)
    
    (syntax-case stx ()

      (($nfx$ expr) #'expr)
      (($nfx$ op1 e1) #'(op1 e1))
      ;; (($nfx$ e1 op1 e2) #'(op1 e1 e2))
      ;; (($nfx$ e1 op1 e2 op2)

      ;;  (error "$nfx$ : called with 4 args (should be 3 or 5 or more) :" #'e1 #'op1 #'e2 #'op2))

      ;; note that ,in the normal case,to have $nfx$ called you need at minimum to have 2 different operator causing an operator precedence question
      ;; and then at least those 2 operators must be between operands each, so there is a need for 3 operand
      ;; the syntax then looks like this : e1 op1 e2 op2 e3 ..., example : 3 * 4 + 2
      ;;(($nfx$ e1 op1 e2 op2 e3 op ...) ; note: i add op because in scheme op ... could be non existent
      (($nfx$ e1 op1 e2 op ...)
       
	 (with-syntax ;; let
			 
	     ((parsed-args

	       (begin

		 ;; (display "$nfx$: #'(e1 op1 e2 op2 e3 op ...)=") (display #'(e1 op1 e2 op2 e3 op ...)) (newline)
		 ;; (display "$nfx$: (syntax->list #'(e1 op1 e2 op2 e3 op ...))=") (display (syntax->list #'(e1 op1 e2 op2 e3 op ...))) (newline)
		 (display "$nfx$: #'(e1 op1 e2 op ...)=") (display #'(e1 op1 e2 op ...)) (newline)
		 (display "$nfx$: (syntax->list #'(e1 op1 e2 op ...))=") (display (syntax->list #'(e1 op1 e2 op ...))) (newline)
		 		
		 ;; ;; pre-check we have an infix expression because parser can not do it
		 ;; (when (not (infix? ;;(list #'e1 #'op1 #'e2 #'op2 #'e3 #'op ...) ; ellipsis impossible
		 ;; 		    ;;#'(e1 op1 e2 op2 e3 op ...)
		 ;; 	            ;;(syntax->list #'(e1 op1 e2 op2 e3 op ...))
		 ;; 	     (syntax->list #'(e1 op1 e2 op ...))
		 ;; 	     operators-lst-syntax))
		   
		 ;;       ;; (error "$nfx$ : arguments do not form an infix expression : here is #'(e1 op1 e2 op2 e3 op ...) and (syntax->list #'(e1 op1 e2 op2 e3 op ...)) for debug:"
		 ;;       (error "$nfx$ : arguments do not form an infix expression : here is #'(e1 op1 e2 op ...) and (syntax->list #'(e1 op1 e2 op ...)) for debug:"
		 ;; 	  ;;#'(e1 op1 e2 op2 e3 op ...)
		 ;; 	  #'(e1 op1 e2 op ...)
		 ;; 	  ;;(syntax->list #'(e1 op1 e2 op2 e3 op ...))))
		 ;; 	  (syntax->list #'(e1 op1 e2 op ...))))

		 (let ((expr (car ;  probably because the result will be encapsuled in a list !
			      ;;(!*prec-generic (syntax->list #'(e1 op1 e2 op2 e3 op ...)) ; apply operator precedence rules
			      (!*prec-generic (syntax->list #'(e1 op1 e2 op ...))
					      infix-operators-lst-for-parser-syntax
					      (lambda (op a b) (list op a b))))))

		   ;; TODO pass back in n-arity also arithmetic operators (+ , * , ...) note: fail with n-arity
		   (if ;;(not (isEXPONENTIAL? expr))
		       (or (isDEFINE? expr)
		       	   (isASSIGNMENT? expr))
		       ;;  make n-arity for <- and <+ only (because could be false with ** , but not implemented in n-arity for now)
		       ;; (begin
		       ;; 	 (display "$nfx$ : calling n-arity on expr :") (display expr) (newline) 
			 (n-arity ;; this avoids : '{x <- y <- z <- t <- u <- 3 * 4 + 1}
			  ;; SRFI-105.scm : !0 result = (<- (<- (<- (<- (<- x y) z) t) u) (+ (* 3 4) 1)) ;; fail set! ...
			  ;; transform in : '(<- x y z t u (+ (* 3 4) 1))
			  expr) ;) ; end begin
		       expr)))))
	      
	   (display "$nfx$ : parsed-args=") (display #'parsed-args) (newline)
	   #'parsed-args)))))

) ; end module

