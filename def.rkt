
;; This file is part of Scheme+

;; Copyright 2021-2024 Damien MATTEI

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





(module def racket/base


	(provide def return return-rec
		 <+ +>
		 ⥆ ⥅
		 :+ +:
		 if-defined)

	(require srfi/31 ;; for 'rec in def.scm
		 racket/stxparam
		 (for-syntax racket/base))
	  

; Tests
;; (define x 3)
;; (if (defined? x) 'defined 'not-defined) ; -> defined

;; (let ([y 4])
;;    (if (defined? y) 'defined 'not-defined)) ; -> defined

;; (if (defined? z) 'defined 'not-defined) ; -> not-defined
;; (define-syntax (defined? stx)
;;   (syntax-case stx ()
;;     [(_ id)
;;      (with-syntax ([v (identifier-binding #'id)]) ; Racket feature , not RnRS
;;        #''v)]))

; Tests
;; (if-defined z (list z) 'not-defined) ; -> not-defined

;; (if-defined t (void) (define t 5))
;; t ; -> 5

;; (define x 3)
;; (if-defined x (void) (define x 6))
;; x ; -> 3

;;(define-syntax (if-defined stx)
(define-syntax if-defined
    (lambda (stx)
      (syntax-case stx ()
	[(_ id iftrue iffalse)
	 (let ([exist-id (identifier-binding #'id)])
	   ;;(display "id=") (display #'id) (newline)
	   ;;(display "if-defined : exist-id=") (display exist-id) (newline) (newline)
	   (if exist-id #'iftrue #'iffalse))])))


(define-syntax-parameter return-rec
  (lambda (stx)
    (raise-syntax-error 'return-rec "can only be used inside def")))

(define-syntax-parameter return
  (lambda (stx)
    (raise-syntax-error 'return "can only be used inside def")))
  

;; Welcome to DrRacket, version 8.14 [cs].
;; Language: racket, with debugging; memory limit: 8192 MB.
;; > (require Scheme+/def)
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

	      

	;; single definition without a value assigned
	;; (def x)
	((_ var) #`(define var '()))

	;; (def x 7)
	((_ var expr) #`(define var expr))

	((_ err ...) #`(syntax-error "Bad def form"))

	)))



;; definition and assignment
;; { x <+ 7 } is equivalent to : (<- x 7) or (define x 7)

;; > {(a b c) <+ (values 7 8 9)}
;; 7
;; 8
;; 9
;; > (list a b c)
;; '(7 8 9)

;; > { y <+ z <+ 7 } 
;; > z
;; 7
;; > y
;; 7
;; > { x <+ y <+ z <+ 7 } 
;; > (list x y z)
;; '(7 7 7)

;; > {(x y z) <+ (u v w) <+ (a b c)  <+ (values 2 4 5)}
;; 2
;; 4
;; 5
;; > (list x y z u v w a b c)
;; '(2 4 5 2 4 5 2 4 5)

;; to define vector based on another vector slice:
;; {T0 <+ #(1 2 3 4 5 6 7 8 9)}
;; '#(1 2 3 4 5 6 7 8 9)
;; > '{T <+ {T0[3 / 5]}}
;; '(<+ T ($bracket-apply$ T0 3 / 5))
;; > {T <+ {T0[3 / 5]}}
;; '#(4 5)
;; > T0
;; '#(1 2 3 4 5 6 7 8 9)
;; > T
;; '#(4 5)

(define-syntax <+
  (syntax-rules ()
    
    ((_ (var1 ...) expr) ;;(begin
			   (define-values (var1 ...) expr)
			   )
    ;;)
			   ;;(values var1 ...)))
    ;; (begin
    ;;   (define var1 '())
    ;;   ...
    ;;   ;;(display "<+ multiple") (newline)
    ;;   (set!-values (var1 ...) expr)))

    ;; > {(x y z) <+ (u v w) <+ (a b c)  <+ (values 2 4 5)}
    ;; 2
    ;; 4
    ;; 5
    ;; > (list x y z u v w a b c)
    ;; '(2 4 5 2 4 5 2 4 5)
    ((_ (var10 ...) (var11 ...) ... expr) (begin  ;; i do not do what the syntax says (assignation not in the good order) but it gives the same result 
    					    (define-values (var10 ...) expr)
    					    (define-values (var11 ...) (values var10 ...))
    					    ...
					    ))
    					    ;;(values var10 ...)))
    
   			    

    
    ((_ var expr)
     ;; (begin
     ;;   (define var expr)
     ;;   var))
     (define var expr))
    
     ;; > { y <+ z <+ 7 }
     ;; 7
     ;; > z
     ;; 7
     ;; > y
     ;; 7
     ;; > { x <+ y <+ z <+ 7 }
     ;; 7
     ;; > (list x y z)
     ;; '(7 7 7)
     ((_ var var1 ... expr) (begin ;; i do not do what the syntax says (assignation not in the good order) but it gives the same result 
			      (define var expr)
			      (define var1 var)
			      ...
			      ;;var))
			      ))
    
     ))






(define-syntax ⥆
  (syntax-rules ()

     ((_ var ...) (<+ var ...))))



(define-syntax :+
  (syntax-rules ()

    ((_ var ...) (<+ var ...))))



;; > {(values 2 4 5) +> (x y z) +> (u v w) +> (a b c)} 
;; 2
;; 4
;; 5
(define-syntax +>
  (syntax-rules ()

    ((_ exp var ...) (<+ var ... exp)))) 


(define-syntax +:
  (syntax-rules ()

    ((_ exp var ...) (<+ var ... exp)))) 

   
;; > {(values 2 4 5) ⥅ (x y z) ⥅ (u v w) ⥅ (a b c)} 
;; 2
;; 4
;; 5
;; > (list x y z u v w a b c)
;; '(2 4 5 2 4 5 2 4 5)
;; (define-syntax ⥅
;;   (syntax-rules ()

;;      ((_ expr ...) (+> expr ...))))

(define-syntax ⥅
  (syntax-rules ()

    ((_ exp var ...) (<+ var ... exp)))) 


) ; end library
