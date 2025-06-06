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

;; try to return the type of a callable object (procedure,macro..)

(module program-type racket/base


	(provide known-program?
		 syntax-known-program?
		 quote-known-program?
		 generic-known-program?)

	(require try-catch
		 ;; Scheme+
		 ;; (for-syntax Scheme+)
		 )


	;; (quote-known-program? '+)
	;; #t

	;; (quote-known-program? 'foo)
	;; (exn:fail:contract:variable "foo: undefined;\n cannot reference an identifier before its definition" #<continuation-mark-set> 'foo)


	;; macro versions are not usefull

	;; (define-syntax quote-known-program?
	;;   (syntax-rules ()
	;;     ((_ kwot-var1)
	;;      ;; (defatalize (program? (eval kwot-var1
	;;      ;; 				   (make-base-namespace) ;(current-namespace)
	;;      ;; 				   )))
	;;      (known-program? kwot-var1))))


	(define (quote-known-program? kwot-var1)
	  (known-program? kwot-var1))
	
	
	;; (define-syntax known-program?
	;;   (syntax-rules ()
	;;     ((_ var1)

	;;      (let ((fate (defatalize (procedure? (eval var1 
	;; 					       #;(current-namespace) (make-base-namespace))))))
	;;        (display "fate=") (display fate) (newline)
	;;        (if (struct? fate)
	;; 	   (cond ((exn:fail:syntax? fate) 'macro)
	;; 		 ((exn:fail:contract:variable? fate) #f)
	;; 		 (else (error "known-program? : exception:" fate)))
	;; 	   fate)))))
   
	(define (known-program? var1)

	     (let ((fate (defatalize (procedure? (eval var1 (make-base-namespace))))))
	       ;;(display "fate=") (display fate) (newline)
	       (if (struct? fate)
		   (cond ((exn:fail:syntax? fate) 'macro)
			 ((exn:fail:contract:variable? fate) #f)
			 ((exn:fail:contract? fate) #f)
			 (else
			  (error "known-program? : exception: fate var1=" fate var1)))
		   fate)))

	;; (define-syntax syntax-known-program?
	;;   (syntax-rules ()
	;;     ((_ stx-var1)
	;;      (begin
	;;        (display "syntax-known-program?") (newline)
	;;        (known-program? (syntax->datum stx-var1))))))

	(define (syntax-known-program? stx-var1)
	  ;;(display "syntax-known-program?") (newline)
	  (known-program? (syntax->datum stx-var1)))

	;; warning: (list not allowed)
	;; (generic-known-program? '(x + y))
	;; fate=#(struct:exn:fail:contract:variable x: undefined;
	;;  cannot reference an identifier before its definition #<continuation-mark-set> x)
	;; #f

	;;(generic-known-program? '(x or y))
	;;fate=#(struct:exn:fail:syntax or: bad syntax #<continuation-mark-set> (.#<syntax or>))
	;;'macro
	
	;; this generic will choose which one from the other macro to call
	(define-syntax generic-known-program?
	  (syntax-rules ()
	    ((_ (kwot-or-syntax var1))
	     (cond ((equal? (quote quote) (syntax->datum #'kwot-or-syntax))
		    (quote-known-program? (kwot-or-syntax var1)))
		   
		   ((equal? (quote syntax) (syntax->datum #'kwot-or-syntax))
		    ;;(display "generic-known-program? : syntax detected.") (newline)
		    (syntax-known-program? (kwot-or-syntax var1)))
		   
		   (else
		    (error "generic-known-program? : find not syntax ,nor quote but:" (quote kwot-or-syntax) ))))

	    ((_ var1)
	     (known-program? var1))))

	
	
	) ; end module



;; (define v (vector 1 2 3))

;; (vector-syntax? (syntax v) (current-namespace)) ; 1

;; (define (foo) 7)

;; (syntax-known-program? (syntax foo)) ; 2

;; (syntax-known-program? (syntax +)) ; 3

;; (known-program? +)
;; #t

;; (vector-syntax? (syntax v) (make-base-namespace))  ; 5



;; #lang reader SRFI-105
;; (module repl racket

;;   (provide (all-defined-out)) 
;;   (require Scheme+)
;;   (require Scheme+/program-type)
  
;;   ;; put your code here or simply use the REPL

;;   (define (foo) 7)
;;   (generic-known-program? foo)

;;   )
