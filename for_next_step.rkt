;; Copyright 2022-2024 Damien MATTEI

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






(module for_next_step racket

  (provide for continue break
	   for-basic
	   for-next
	   for-basic/break
	   for-basic/break-cont
	   for/break-cont
	   for-each-in)

  (require (only-in racket/base [for for-racket]) ;; backup original Racket 'for'
	   (for-syntax r6rs/private/base-for-syntax) ; unless :  racket-8.14/share/pkgs/errortrace-lib/errortrace/stacktrace.rkt:709:4: identifier-syntax: undefined; cannot reference an identifier before its definition
	   Scheme+/increment
	   (only-in racket/stxparam define-syntax-parameter syntax-parameterize))



;; > (for-basic ((k 5)) (display k) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5


;; > (for-basic (k 0 10) (display k) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; 6
;; 7
;; 8
;; 9
;; 10
;; '()

;; > (for-basic (k 0 10 3) (display k) (newline) (newline) (for-basic (j 0 3) (display j) (newline)) (newline))
;; 0

;; 0
;; 1
;; 2
;; 3

;; 3

;; 0
;; 1
;; 2
;; 3

;; 6

;; 0
;; 1
;; 2
;; 3

;; 9

;; 0
;; 1
;; 2
;; 3

;; '()
;;

;; DEPRECATED
(define-syntax for-basic
  
  (syntax-rules ()

    ;; ((_ ((i to)) b1 ...) ;; for old compatibility
     
    ;;  (let loop ((i 0))
    ;;    (when (< i to)
    ;; 	     b1 ...
    ;; 	     (loop (incf i)))))
    
    ((_ (i to) b1 ...)
     
     (let loop ((i 0))
       (when (< i to)
	     b1 ...
	     (loop (incf i)))))

    
    
    ((_ (i from to) b1 ...)

     (let loop ((i from))
       (when (<= i to)
	     (let () ;; why only this one have let ?
	       b1 ...)
	     (loop (incf i)))))
    
    ((_ (i from to step) b1 ...)
     
     (let loop ((i from))
       (when (<= i to)
	     b1 ...
	     (loop (+ i step)))))))


;; (for-next k = 0 to 10  (display k) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; 6
;; 7
;; 8
;; 9
;; 10
;; > (for-next k = 0 to 5 step 2 (display k) (newline))
;; 0
;; 2
;; 4
;; > (for-next (k = 0 to 5 step 2) (display k) (newline))
;; 0
;; 2
;; 4
;; > (for-next (k = 0 to 5) (display k) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; > 
;; DEPRECATED
(define-syntax for-next
  
  (syntax-rules (= to step)


    ;; The patterns are matched top down (the one with step must be before others patterns)
    ((for-next i = start to finish step inc b1 ...)
     
     (let loop ((i start))
       (when (<= i finish)
	     b1 ...
	     (loop (+ i inc)))))

    
    ((for-next i = start to finish b1 ...)

     (let loop ((i start))
       (when (<= i finish)
	     b1 ...
	     (loop (incf i)))))
    
   
    
    ((for-next (i = start to finish) b1 ...)

     (let loop ((i start))
       (when (<= i finish)
	     b1 ...
	     (loop (incf i)))))
    
    ((for-next (i = start to finish step inc) b1 ...)
     
     (let loop ((i start))
       (when (<= i finish)
	     b1 ...
	     (loop (+ i inc)))))))

;; deprecated
;; (define-syntax for-next-step
;;   (syntax-rules (= to step)
    
;;     ((for-next-step i = start to finish step inc b1 ...)
     
;;      (let loop ((i start))
;;        (when (<= i finish)
;; 	     b1 ...
;; 	     (loop (+ i inc)))))))


;; > (for-basic/break breaky (i 1 4) (for-basic/break breakable (j 1 2) (display-nl i)))
;; 1
;; 1
;; 2
;; 2
;; 3
;; 3
;; 4
;; 4
;; > (for-basic/break breaky (i 1 4) (for-basic/break breakable (j 1 2) (display-nl i) (when (= i 2) (breakable))))
;; 1
;; 1
;; 2
;; 3
;; 3
;; 4
;; 4
;; > (for-basic/break breaky (i 1 4) (for-basic/break breakable (j 1 2) (display-nl i) (when (= i 2) (breaky))))
;; 1
;; 1
;; 2
;; >
;; > (define x 0)
;; > (for-basic/break breaky (i 1 3) (set! x i) )
;; > x
;; 3
;;
;;  (for-basic/break breaky (i 1 3) (if (= i 3)  (breaky i) '()) )
;; 3

;; (for-basic/break breaky (i 1 3) (display i) (newline) (if (= i 2)  (begin (breaky i)) '()) )
;; 1
;; 2
;; $3 = 2
;; DEPRECATED
(define-syntax for-basic/break
  (syntax-rules ()
    ((_ <break-id> (i from to) b1 ...)
     (call/cc (lambda (<break-id>)
		(let loop ((i from))
        	  (when (<= i to)
			;;(begin b1 ...
			b1 ...
			(loop (incf i)))))))));)

;; scheme@(guile-user)>  (for-basic/break-cont break continue (i 1 3) (display i) (newline) (if (= i 2)  (begin (continue) (break)) '()) )
;; 1
;; 2
;; 3
;; DEPRECATED
(define-syntax for-basic/break-cont
  (syntax-rules ()
    ((_ <break-id> <continue-id> (i from to) b1 ...)
     (call/cc (lambda (<break-id>)
		(let loop ((i from))
        	  (when (<= i to)
			(call/cc (lambda (<continue-id>) b1 ...))
			(loop (incf i)))))))))


;; scheme@(guile-user)> (for ({i <+ 0} {i < 5} (incf i)) (display i) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4

;; scheme@(guile-user)> (for ({i <+ 0} {i < 5} {i <- {i + 1}}) (display i) (newline))
;; 0
;; 1
;; 2
;; 3
;; 4

;;  (for ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (display x) (newline))
;; 7
;; 7
;; 7
;; 7
;; 7
;; (define-syntax for
  
;;   (syntax-rules ()
    
;;     ((_ (init test incrmt) b1 ...)

;;        (let ()
;; 	 init
;; 	 (let loop ()
;; 	   (when test
;; 		 ;;(let ()
;; 		   b1 ...
;; 		   incrmt
;; 		   (loop)))))));)


;; scheme@(guile-user)> (for/break-cont break continue ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (display x) (newline))
;; 7
;; 7
;; 7
;; 7
;; 7

;; scheme@(guile-user)> (for/break-cont break continue ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (display x) (newline) (break))
;; 7

;; scheme@(guile-user)> (for/break-cont break continue ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (continue) (display x) (newline) (break))
;; perheaps not usefull DEPRECATED? but compatible with R6RS and R5RS
(define-syntax for/break-cont
  
  (syntax-rules ()
    
    ((_ <break-id> <continue-id> (init test incrmt) b1 ...)

     (call/cc (lambda (<break-id>)
		(let ()
		  init
		  (let loop ()
		    (when test
			  (call/cc (lambda (<continue-id>) b1 ...))
			  incrmt
			  (loop)))))))))



;; scheme@(guile-user)> (for ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (display x) (newline) (break))
;; 7
;; scheme@(guile-user)> (for ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (continue) (display x) (newline) (break))
;; scheme@(guile-user)>

;; (for ({k <+ 0} {k < 3} {k <- {k + 1}})
;;      (display k)
;;      (newline)
;;      (for ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7}
;; 	  (display x)
;; 	  (newline)
;; 	  (break))
;;      (newline))

;; 0
;; 7

;; 1
;; 7

;; 2
;; 7

;; does not works as expected (break is from srfi-1)
;; (for ({k <+ 0} {k < 3} {k <- {k + 1}})
;;      (display k)
;;      (break even? '(3 1 4 1 5 9))
;;      (newline)
;;      (continue)
;;      (for ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7}
;; 	  (display x)
;; 	  (newline)
;; 	  (break))
;;      (newline))

;; 0
;; 1
;; 2

;; (define-syntax for
  
;;   (lambda (stx)
;;     (syntax-case stx ()
;;       ((kwd (init test incrmt) body ...)
	  
;;        (with-syntax
;; 	((BREAK (datum->syntax #'kwd 'break))
;; 	 (CONTINUE (datum->syntax #'kwd 'continue)))

;; 	#`(call/cc
;; 	   (lambda (escape)
;; 	     (let-syntax
;; 		 ((BREAK (identifier-syntax (escape))))
;; 	       init
;; 	       (let loop ()
;; 		 (when test

;; 		       #,#'(call/cc
;; 			    (lambda (next)
;; 			      (let-syntax
;; 				  ((CONTINUE (identifier-syntax (next))))
;; 				body ...)))
			  
;; 		       incrmt
;; 		       (loop)))))))))))


;; note: may require the specific 'when (that allows inner definitions)

;; below syntax is incompatible with SRFI-105 implementation for Racket: not sure why ?!
;; (define-syntax for
;;   (lambda (stx)
;;     (syntax-case stx ()
;;       ((kwd (init test incrmt) body ...)
;;        (with-syntax ((BREAK (datum->syntax #'kwd 'break))
;; 		     (CONTINUE (datum->syntax #'kwd 'continue)))
;; 		    #'(call/cc
;; 		       (lambda (escape)
;; 			 (let-syntax ((BREAK (identifier-syntax (escape))))
;; 			   init
;; 			   (let loop ()
;; 			     (when test
;; 				   (call/cc
;; 				    (lambda (next)
;; 				      (let-syntax ((CONTINUE (identifier-syntax (next))))
;; 					body ...)))
;; 				   incrmt
;; 				   (loop)))))))))))




;; insert in header in Racket:
;; (require (for-syntax r6rs/private/base-for-syntax))

;; this version is used:
;; (for ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7} (display i) (newline) )
;; 0
;; 1
;; 2
;; 3
;; 4

;; (for ((define i 0) (< i 5) (set! i (+ i 1))) (define x 7) (display i) (newline) )
;; 0
;; 1
;; 2
;; 3
;; 4

;; > (for ((define i 0) (< i 5) (set! i (+ i 1))) (define x 7) (display i) (continue) (newline) )
;;012340

;; > (for ((define i 0) (< i 5) (set! i (+ i 1))) (define x 7) (display i) (newline) (break))
;;0

;; version that work but which dit useless job with res not returning something interesting !
;; (define-syntax for
;;    (lambda (stx)
;;      (syntax-case stx ()
;;        ((kwd (init test incrmt) body ...)
;;         (with-syntax ((BREAK (datum->syntax (syntax kwd) 'break))
;;                       (CONTINUE (datum->syntax (syntax kwd) 'continue)))
;; 		     (syntax
;; 		      (call/cc
;; 		       (lambda (escape)
;; 			 (let-syntax ((BREAK (identifier-syntax (escape))))
;; 			   init
;; 			   (let loop ((res 0)) ;; now we will return a result at the end if no break but if we continue? what happens?
;; 			     (if test
;; 				 (begin
;; 				   (call/cc
;; 				    (lambda (next)
;; 				      (set! res (let-syntax ((CONTINUE (identifier-syntax (next))))
;; 						  (let () ;; allow definitions
;; 						    body ...)))))
;; 				   incrmt
;; 				   (loop res)) ; end begin
;; 				 res) ; little problem: res = 0 is returned ,seems not modified ?!
;; 			     ))))
;; 		      ) ;; close syntax
		     
;; 		     )))))




;; Welcome to DrRacket, version 8.14 [cs].
;; Language: racket, with debugging; memory limit: 8192 MB.
;; > (require Scheme+/for_next_step)
;; > (for ((define i 0) (< i 5) (set! i (+ i 1))) (define x 7) (display i) (newline) (break i))
;; 0
;; > (for ((define i 0) (< i 5) (set! i (+ i 1))) (define x 7) (display i) (newline) (break))
;; 0
;; > (for ((define i 0) (< i 5) (set! i (+ i 1))) (define x 7) (display i) (continue) (newline) )
;; 01234
;; > (for ((define i 0) (< i 5) (set! i (+ i 1))) (define x 7) (display i) (newline) )
;; 0
;; 1
;; 2
;; 3
;; 4

;; this version no more try to return silly things !
;; (define-syntax for
;;    (lambda (stx)
;;      (syntax-case stx ()
;;        ((kwd (init test incrmt) body ...)
	
;;         (with-syntax ((BREAK (datum->syntax (syntax kwd) 'break))
;;                       (CONTINUE (datum->syntax (syntax kwd) 'continue)))
	  
;; 		     (syntax
;; 		      (call/cc
;; 		       (lambda (escape)
;; 			 (let-syntax ((BREAK (identifier-syntax (escape))))
;; 			   init
;; 			   (let loop ()
;; 			     (when test
;; 			       (call/cc
;; 				(lambda (next)
;; 				  (let-syntax ((CONTINUE (identifier-syntax (next))))
;; 				    (let () ;; allow definitions
;; 				      body ...)))) ; end call/cc
;; 			       incrmt
;; 			       (loop))) ; end let loop
;; 			   ))))) ;; close with-syntax
;; 	))))




;; Welcome to DrRacket, version 8.14 [cs].
;; Language: racket, with debugging; memory limit: 8192 MB.
;; > (require Scheme+/for_next_step)
;; > (define res (for ((define i 0) (< i 5) (set! i (+ i 1))) (define x 7) (display i) (newline) (when (= i 2) (break i))))
;; 0
;; 1
;; 2
;; > (for ((define i 0) (< i 5) (set! i (+ i 1))) (define x 7) (display i) (newline) )
;; 0
;; 1
;; 2
;; 3
;; 4
;; > (for ((define i 0) (< i 5) (set! i (+ i 1))) (define x 7) (display i) (continue) (newline) )
;; 01234
;; > res
;; 2
;; > 
(define-syntax for
  
  (lambda (stx)
    
    (syntax-case stx ()
      
      ((_ (init test incrmt) body ...)
       
       (with-syntax ((BREAK (datum->syntax stx 'break))
                     (CONTINUE (datum->syntax stx 'continue)))
     (syntax
      (call/cc
       (lambda (escape)
         (let ((BREAK escape))
           init
           (let loop ()
         (when test
           (call/cc
            (lambda (next)
              (let ((CONTINUE next))
            (let () ;; allow definitions
              body ...)))) ; end call/cc
           incrmt
           (loop))) ; end let loop
           ))))) ;; close with-syntax
       ))))



(define-syntax-parameter break
  (lambda (stx)
    (raise-syntax-error 'break "can only be used inside for")))

(define-syntax-parameter continue
  (lambda (stx)
    (raise-syntax-error 'continue "can only be used inside for")))
  

;; cause bad numerical result in computaion of neural sine
;; (define-syntax for
  
;;   (lambda (stx)
    
;;     (syntax-case stx ()
      
;;       ((_ (init test incrmt) body ...)
       
;;    	 (syntax
	  
;; 	  (call/cc
;; 	   (lambda (escape)
;; 	     ;; In the body we adjust the 'break' keyword so that calls
;; 	     ;; to 'break' are replaced with calls to the escape
;; 	     ;; continuation.
;; 	     (syntax-parameterize
;; 	      ([break (syntax-rules ()
;; 			     [(break vals (... ...))
;; 			      (escape vals (... ...))])])
		 
;;                init
;;                (let loop ()
;; 		 (when test
;; 		   (call/cc
;; 		    (lambda (next)
;; 		      ;; In the body we adjust the 'continue' keyword so that calls
;; 		      ;; to 'continue' are replaced with calls to the escape
;; 		      ;; continuation.
;; 		      (syntax-parameterize
;; 		       ([continue (syntax-rules ()
;; 				    [(continue vals (... ...))
;; 				     (next vals (... ...))])])
		 
;; 		       (let () ;; allow definitions
;; 			 body ...)))) ; end call/cc
;; 		   incrmt
;; 		   (loop)))))))))))



(define-syntax for-each-in
  
  (syntax-rules ()
    
    ((_ (i seq) stmt0 stmt1 ...) (for-each (lambda (i) stmt0 stmt1 ...)
					   seq))))


;; (for/bc ({k <+ 0} {k < 3} {k <- {k + 1}})
;;      (display k)
;;      (break even? '(3 1 4 1 5 9))
;;      (newline)
;;      (continue)
;;      (for/bc ({i <+ 0} {i < 5} {i <- {i + 1}}) {x <+ 7}
;; 	  (display x)
;; 	  (newline)
;; 	  (break))
;;      (newline))

;; (use-modules (ice-9 control))


;; (define-syntax-parameter break
;;    (lambda (sintax)
;;      (syntax-violation 'break "break outside of for/bc" sintax)))

;; (define-syntax-parameter continue
;;    (lambda (sintax)
;;      (syntax-violation 'continue "continue outside of for/bc" sintax)))

;; (define-syntax-rule (for/bc (init test increment) body body* ...)
;;    (let () ;; (begin 
;;      init
;;      (let/ec escape
;;        (syntax-parameterize ((break (identifier-syntax (escape))))
;;          (let loop ()
;;            (when test
;;              (let/ec next
;;                (syntax-parameterize ((continue (identifier-syntax (next))))
;;                  body body* ...))
;;              increment
;;              (loop)))))))



;; (let ((i #f))
;;    (for/bc ((set! i 0) (< i 10) (set! i (1+ i)))
;;      (when (< i 5)
;;        continue)
;;      (when (> i 9)
;;        break)
;;      (display i)
;;      (newline)))






;; (define-syntax for/bc
;;    (lambda (stx)
;;      (syntax-case stx ()
;;        ((kwd (init test incrmt) body ...)
;;         #`(call/cc
;;            (lambda (escape)
;;              (let-syntax ((#,(datum->syntax #'kwd 'break)
;;                            (identifier-syntax (escape))))
;;                init
;;                (let loop ()
;;                  (when test
;;                    (call/cc
;;                     (lambda (next)
;;                       (let-syntax ((#,(datum->syntax #'kwd 'continue)
;;                                     (identifier-syntax (next))))
;;                         body ...)))
;;                    incrmt
;;                    (loop))))))))))


) ; end library




