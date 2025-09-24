(module defvar racket/base


	(provide <+ +>
		 ⥆ ⥅
		 :+ +:
		 if-defined)

	(require racket/stxparam
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


)
