
;; This file is part of Scheme+

;; Copyright 2021-2025 Damien MATTEI

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



;; for internal use only , see def for the extern use

(module defun racket/base


	(provide def #;def+ return return-rec)
	
	(require srfi/31 ;; for 'rec in def*
		 Scheme+/return
		 racket/stxparam
		 (for-syntax racket/base))
	  




;; Welcome to DrRacket, version 8.14 [cs].
;; Language: racket, with debugging; memory limit: 8192 MB.
;; > (require Scheme+/defun)
;; > (def (foo) (display "hello") (newline) (return) (display "world") (newline))
;; > (foo)
;; hello
;; > (return)
;; return: can only be used inside def


;; scheme@(guile-user)> (def (foo) (when #t (return "hello") "bye"))
;; scheme@(guile-user)> (foo)
;;  "hello"

;; (def x)

;;(define return '()) ;; for debug of Typed Racket


;;(def (foo) (return 1 2 3))

;; (foo)
;; 1
;; 2
;; 3

(define-syntax def

  (lambda (stx)
    
      (syntax-case stx ()

	;; multiple definitions without values assigned
	;; (def (x y z))
	;; TODO: remove? redundant with (declare x y z)
	((_ (var1 ...)) #`(begin (define var1 '()) ...))
	
	;;  (def (foo) (when #t (return "hello") "bye"))
        ;; ((_ (<name> <arg> ...) <body> <body>* ...)
        ;;  (let ((ret-id (datum->syntax stx 'return)))
        ;;    #`(define (<name> <arg> ...)
        ;;        (call/cc (lambda (#,ret-id) <body> <body>* ...)))))


	;; ((_ (<name> <arg> ...) <body> <body>* ...)
	 
        ;;  (with-syntax ((ret-id (datum->syntax stx 'return))
	;; 	       (ret-rec-id (datum->syntax stx 'return-rec)))

	;; 	      (display "def.scm : def : ret-id = ") (display #'ret-id) (newline)
	;; 	      (display "def.scm : def : ret-rec-id = ") (display #'ret-rec-id) (newline)

	;; 	      #'(define (<name> <arg> ...)

	;; 		  (call/cc (lambda (ret-rec-id) ;(#,ret-rec-id)
				     
	;; 			     (apply (rec <name> (lambda (<arg> ...)
							  
	;; 						  (call/cc
	;; 						   (lambda (ret-id) ;(#,ret-id)
	;; 							     <body> <body>* ...))))
					    
	;; 				    (list <arg> ...)))))))
  

	((_ (<name> <arg> ...) <body> <body>* ...)
	 
         
	 #'(define (<name> <arg> ...)

	     (call/cc

	      (lambda (ret-rec-id) ;(#,ret-rec-id)
		;; In the body we adjust the 'return-rec' keyword so that calls
		;; to 'return-rec' are replaced with calls to the escape
		;; continuation.

		(syntax-parameterize
		 ([return-rec (syntax-rules ()
				[(return-rec vals (... ...))
				 (ret-rec-id vals (... ...))])])
		 
		 (apply (rec <name> (lambda (<arg> ...)
				      
				      (call/cc

				       (lambda (ret-id) ;(#,ret-id)
					 ;; In the body we adjust the 'return' keyword so that calls
					 ;; to 'return' are replaced with calls to the escape
					 ;; continuation.
					 (syntax-parameterize
					  ([return (syntax-rules ()
						     [(return vals (... ...))
						      (ret-id vals (... ...))])])
					  <body> <body>* ...)))))
			
			(list <arg> ...)))))))

	

	;; variadic arguments in list
	;; (def (foo a . L) (when #t (return (cons a L))))
	;; (foo 1 2 3)
	;; '(1 2 3)
	((_ (<name> <arg> . L) <body> <body>* ...)
	 
         
	 #'(define (<name> <arg> . L)

	     (call/cc

	      (lambda (ret-rec-id) ;(#,ret-rec-id)
		;; In the body we adjust the 'return-rec' keyword so that calls
		;; to 'return-rec' are replaced with calls to the escape
		;; continuation.

		(syntax-parameterize
		 ([return-rec (syntax-rules ()
				[(return-rec vals (... ...))
				 (ret-rec-id vals (... ...))])])
		 
		 (apply (rec <name> (lambda (<arg> . L)
				      
				      (call/cc

				       (lambda (ret-id) ;(#,ret-id)
					 ;; In the body we adjust the 'return' keyword so that calls
					 ;; to 'return' are replaced with calls to the escape
					 ;; continuation.
					 (syntax-parameterize
					  ([return (syntax-rules ()
						     [(return vals (... ...))
						      (ret-id vals (... ...))])])
					  <body> <body>* ...)))))
			
			(cons <arg> L)))))))


	

	;; single definition without a value assigned
	;; (def x)
	((_ var) #`(define var '()))

	;; (def x 7)
	((_ var expr) #`(define var expr))

	((_ err ...) #`(syntax-error "Bad def form"))

	)))




) ; end library
