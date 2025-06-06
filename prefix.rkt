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


(module prefix racket/base


  (provide prefix-limited?)

  (require Scheme+/operators
	   Scheme+/superscript)

  ;; (prefix-limited? (syntax->list #'(2 * (2 + 1) - 1)))
  ;; prefix-limited? : expr =(.#<syntax 2> .#<syntax *> .#<syntax (2 + 1)> .#<syntax -> .#<syntax 1>)
  ;; prefix-limited? : rv=#f
  ;; #f

  ;; (prefix-limited? (syntax->list #'($bracket-apply$ #(1 2 3) 1)))
  ;; prefix-limited? : expr =(.#<syntax $bracket-apply$> .#<syntax #(1 2 3)> .#<syntax 1>)
  ;; prefix-limited? : rv=#t
  ;; #t

  ;; (prefix-limited? (syntax->list #'(+ 1 2 3)))
  ;; #t


  ;; detect prefix only! based on the beginning of list expression, do not check all the expression even at top level !
  ;; Warning: detect only prefix at the top level of an expression (do not dive in subexpressions)
  (define (prefix-limited? expr)

    (display "prefix-limited? : expr =")(display expr)(newline)

    (define rv
      (cond ((not (list? expr))
	     (display "prefix-limited? : not list")(newline)
	     #t)
	    
	    ((null? expr)
	     (display "prefix-limited? : null expr")(newline)
	     #t)

	    ;; at this point we have at least one element in the list
	    
	    ((null? (cdr expr))
	     (display "prefix-limited? : only one argument")(newline)
	     #t) ; example: (fct)    prefix  (and infix ? ambiguous)

	    
	    ;; syntax version                                                                     ;  syntax and symbol versions
	    ((and (syntax? (car expr)) ; ex (#'+)  prefix     even if this would lead to error    ;
		  (operator-syntax? (car expr)))                                                  ;
	     (display "prefix-limited? : testing first argument syntax and operator")(newline)    ;
	     #t)                                                                                  ;
                                                                                                  ;      
	    ;; symbol version                                                                     ;
	    ((and (symbol? (car expr))                                                            ;
		  (operator? (car expr))) ; (+)    prefix                                         ;
	     (display "prefix-limited? : testing first argument symbol and operator")(newline)    ;             
	     #t)                                                                                  ;

	   
	    ;; at this point we have at least two elements in the list

	    ;; superscript exposants in syntax on symbol denotes infix notation only
	    ((superscript? (cadr expr))  ; (x ²)
	     (display "prefix-limited? : superscript is infix only")(newline)
	     #f)
	    
	    ;; syntax version                                                                                                                            ;
	    ;; ((syntax? (cadr expr)) ; ex: (1 #'+ 2 #'+ 3)  not prefix but infix (even if (cons + lst) is prefix !)        syntax and symbol versions   ;
	    ;;  (display "prefix-limited? : testing second argument syntax and not operator")(newline)    ; (#'sin #'3) prefix                           ;
	    ;;  (not (operator-syntax? (cadr expr)))) ; forbid operator in prefix expression at this position argument (second position)                    ;
	    ;;  ;; it is not true, to pass operator you will have to hide them in a variable                                                                ;
            ;;                                                                                                                                              ;
	    ;; ;; symbol version                                                                                                                            ;
	    ;; ((symbol? (cadr expr)) ; ex: (1 + 2) not prefix but infix                                                                                    ;
	    ;;  (display "prefix-limited? : testing second argument symbol and not operator")(newline)      ; (sin 3)   prefix                              ;
	    ;;  ;; it is not true, to pass operator you will have to hide them in a variable                                                                ;
	    ;;  (not (operator? (cadr expr))))   ; forbid operator in prefix expression at this position argument (second position)                         ;                                                       
            
	    ((or (syntax? (cadr expr)) ; ex: (1 #'+ 2 #'+ 3)  not prefix but infix (even if (cons + lst) is prefix !)    syntax and symbol versions   ;
		 (symbol? (cadr expr))) ; ex: (1 + 2) not prefix but infix    
	     (display "prefix-limited? : testing second argument syntax or symbol and not operator")(newline)    ; (#'sin #'3) prefix                    ;
	     (not (operator-symbol-or-syntax? (cadr expr)))) ; forbid operator in prefix expression at this position argument (second position)          ;
	     ;; it is not true, to pass operator you will have to hide them in a variable                                                                ;
                                                                                            
	    (else
	     (display "prefix-limited? : else : unknow case")(newline)
	     (error "prefix-limited? : else : do not know how to handle the expression:" expr))))

    (display "prefix-limited? : rv=")(display rv)(newline)
    rv)



  ) ; end module


;; note:

;; (+ 1 2) is prefix
;; (1 + 2) is infix
;; but (cons + 1) seems infix but is prefix, the problem is it can not be detected at the syntax level
;; because we need to know that (procedure? cons) is true but (procedure? #'cons) is not true.
;; and consider (f ° g) where ° compose f,g being procedure f,g and ° being all procedures....
