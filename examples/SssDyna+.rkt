#lang reader SRFI-105


;; Sub Set Sum problem
;; Dynamic solution
;; Racket version

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


(module sssdyna racket



(require Scheme+
	 "best-solution+.rkt"
	 memo)
	
(require srfi/25) ;; Multi-dimensional Array Primitives

;;(provide (all-defined-out)) ;; export all bindings


(define (one? n)
  (= n 1))

(declare L-init t-init ls dyna cpt)

{L-init <- '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027)}

{t-init <- 35267}
;;{t-init <- 21}

{ls <- (length L-init)}
{dyna <- (make-array (shape 0 (ls + 1)
			    0 (t-init + 1))
		     0)}

(define (tf->12 b)
  (if b 1 2))

{cpt <- 0}



;; scheme@(guile-user)> (ssigma-dyna L-init t-init)
;; $2 = #t
;; scheme@(guile-user)> cpt
;; $3 = 147801
;; (define (ssigma-dyna L t)

;;   {cpt <- {cpt + 1}}
  
;;   ;;(display L) (display " ") (display t) (newline)
  
;;   (let*  [(ls (length L))
;; 	  (dyn (array-ref dyna ls t))]
    
;;     ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
;;     (one?
;;      (if (not (zero? dyn))
	
;; 	dyn
	
;; 	(array-ref-set! dyna ;; set the array but return the variable
;; 			(tf->12
;; 			 (if (null? L)
;; 			     #f
;; 			     (let [ (c (first L))
;; 				    (R (rest L)) ]
;; 			       (cond [ {c = t} #t ] ;; c is the solution
;; 				     [ {c > t} (ssigma-dyna R t) ] ;; c is to big to be a solution but can be an approximation
;; 				     ;; c < t at this point
;; 				     ;; c is part of the solution or his approximation
;; 				     ;; or c is not part of solution or his approximation
;; 				     [ else {(ssigma-dyna R {t - c}) or (ssigma-dyna R t)} ] ))))
;; 			ls
;; 			t  )))))


;; scheme@(guile-user)> (ssigma-dyna-local L-init t-init)
;; $1 = #t
;; scheme@(guile-user)> cpt
;; $2 = 147801
;; (define (ssigma-dyna-local L t)

;;   {cpt ← {cpt + 1}} ;; cpt is defined at toplevel
  
;;   ;;(display L) (display " ") (display t) (newline)
  
;;   (local  [ ls (length L)
;; 	    dyn {dyna[ls t]} ]
    
;;     ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
;;     (one?
;;      (if (not (zero? dyn))
	
;; 	dyn

;; 	;; set the array but return the variable
;; 	{ dyna[ls t] ← (tf->12
;; 			  (if (null? L)
;; 			      #f
;; 			      (local [ c (first L)
;; 				       R (rest L) ]
;; 				     (cond [ {c = t} #t ] ;; c is the solution
;; 					   [ {c > t} (ssigma-dyna-local R t) ] ;; c is to big to be a solution but can be an approximation
;; 					   ;; c < t at this point
;; 					   ;; c is part of the solution or his approximation
;; 					   ;; or c is not part of solution or his approximation
;; 					   [ else {(ssigma-dyna-local R {t - c}) or (ssigma-dyna-local R t)} ] )))) } ))))

;; scheme@(guile-user)> (ssigma-dyna-define-anywhere L-init t-init)
;; $1 = #t
;; scheme@(guile-user)> cpt
;; $2 = 147801
(define (ssigma-dyna-define-anywhere L t)
  
  {cpt <- cpt + 1} ;; cpt is defined at toplevel
  
  ;;(display L) (display " ") (display t) (newline)

  ;;(def ls (length L))
  ;;(def dyn {dyna[ls t]})

  (def (ls dyn)) ;; declare multiple variables WARNING: ls is global !!! so i force it a local one here

  {ls <- (length L)}

  ;;(display "ls=") (display ls) (display " ") (display "t=") (display t) (newline)
  
  {dyn <- dyna[ls t]}
  
  (def c)
  (def R)

  ;;(display "ls=") (display ls) (display " ") (display "dyn=") (display dyn) (newline)
  
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  (one?
   
    (if (not (zero? dyn))
	
	dyn
	
	;; set the array but return the variable
	($>
	 { dyna[ls t] <- (tf->12
			  (if (null? L)
			      #f
			      ($> ;;(display "assignment") (newline)
				{c <- (first L)}
				{R <- (rest L)}
				(cond [ {c = t} #t ] ;; c is the solution
				      [ {c > t} (ssigma-dyna-define-anywhere R t) ] ;; c is to big to be a solution but can be an approximation
				      ;; c < t at this point
				      ;; c is part of the solution or his approximation
				      ;; or c is not part of solution or his approximation
				      [ else {(ssigma-dyna-define-anywhere R (t - c)) or (ssigma-dyna-define-anywhere R t)} ] )))) }
	 ;; (display "one? ls=") (display ls) (display " ") (display "t=") (display t) (newline)
	 ;; (display "dyna[ls t] =") (display {dyna[ls t]}) (newline)
	 { dyna[ls t] }))))



;; scheme@(guile-user)> (ssigma-dyna-def L-init t-init)
;; $1 = #t
;; scheme@(guile-user)> cpt
;; $2 = 147801
;; (define (ssigma-dyna-def L t)

;;   ;;(display L) (display " ") (display t) (newline)
  
;;   ;; (display L) (newline)
;;   ;; (display t) (newline)
;;   ;; (newline)
 
  
;;   {cpt <- cpt + 1} ;; cpt is defined at toplevel
  
;;   (def (ls dyn)) ;; declare multiple variables 
  
;;   {ls <- (length L)}

;;   ;; (display "ls=") (display ls) (display " ") (display "t=") (display t) (newline)
;;   {dyn <- dyna[ls t]}

;;   ;; declare one variable at a time
;;   (def c)
;;   (def R)

;;   ;;(display "ls=") (display ls) (display " ") (display "dyn=") (display dyn) (newline)

;;   ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
;;   (one? ; 1: solution found ?
  
;;    (if (not (zero? dyn)) ; we already know the solution
       
;;        dyn ; 1: solution found, 2: no solution

;;        ;; 0: unknown solution
;;        ;; set the array but return the variable
;;        ($>
;; 	{ dyna[ls t] <- (tf->12
;; 			 (if (null? L)
;; 			    #f
;; 			    ($> ;;(display "assignment") (newline)
;; 			     {c <- (first L)}
;; 			     {R <- (rest L)}
;; 			     (cond [ {c = t} #t ] ;; c is the solution
;; 				   [ {c > t} (ssigma-dyna-def R t) ] ;; c is to big to be a solution but can be an approximation
;; 				   ;; c < t at this point
;; 				   ;; c is part of the solution or his approximation
;; 				   ;; or c is not part of solution or his approximation
;; 				   [ else {(ssigma-dyna-def R {t - c}) or (ssigma-dyna-def R t)} ] )))) }
;; 	;; (display "one? ls=") (display ls) (display " ") (display "t=") (display t) (newline)
;; 	;; (display "dyna[ls t] =") (display {dyna[ls t]}) (newline)
;; 	{ dyna[ls t] }))))







;; scheme@(guile-user)> (ssigma-proto L-init t-init)
;;  = #t
;; scheme@(guile-user)> cpt
;; $2 = 147801
(define (ssigma-proto L t)

  (set! cpt {cpt + 1})
 
  (define ls (length L))
  (define dyn (array-ref dyna ls t))
    
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution

  (cond [(not (zero? dyn)) (one? dyn)]
	[(null? L) (array-set! dyna 2 ls t) #f] ;; return #f
	
	[else (let [(c (first L))]
		
		(if {c = t} ;; c is the solution
		      
		    (begin
		      (array-set! dyna 1 ls t)
		      #t)  ;; return #t

		    ;; else
		    (let [(R (rest L))]
		      
		      (if {c > t}   ;; c is too big to be a solution or part of it -> continue searching a solution in the rest
			    
			  (let [(s (ssigma-proto R t))]
			    (array-set! dyna
					(tf->12 s)
					ls t)
			      
			    s) ;; return s
			
			  ;; else
			  ;; c < t at this point
			  ;; c is part of the solution (or his approximation)
			  ;; or c is not part of solution
			  (let [(s {(ssigma-proto R {t - c}) or (ssigma-proto R t)})]
			    (array-set! dyna (tf->12 s)
					ls t)
			    s)))))
	      ] ))

;; (ssigma-proto-condx  L-init t-init)
;; ... 
;;  = #t
(define (ssigma-proto-condx L t)

  (set! cpt {cpt + 1})

  (define ls (length L))
  (define dyn (array-ref dyna ls t))


  ;; (display L) (newline)
  ;; (display t) (newline)
 
    
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  
  (condx [(not (zero? dyn)) (one? dyn)]
	 [(null? L) (array-set! dyna 2 ls t) #f] ;; return #f
	 
	 [exec (define c (first L))]
	 
	 ;; c is the solution
	 [{c = t} (array-set! dyna 1 ls t) #t]  ;; return #t
	 
	 [exec (define R (rest L))]
	 
	 ;; continue searching a solution in the rest
	 [{c > t} (define s (ssigma-proto-condx R t))
	  (array-set! dyna
		      (tf->12 s)
		      ls t)
	  s] ;; return s
			
	 ;; else :
	 ;; c < t at this point
	 ;; c is part of the solution or his approximation
	 ;; or c is not part of solution
	 [else (define s {(ssigma-proto-condx R {t - c}) or (ssigma-proto-condx R t)})
	       (array-set! dyna (tf->12 s)
			   ls t)
	       s]))





;; (subset-sum-dyna  L-init t-init)
;; #t ;; there exists a solution

(def (subset-sum-dyna L t)

  (declare ls) ;; declare a ls local as it exists too in global

  {ls <- (length L)}
  {dyn <- dyna[ls t]}

  ;; dyna[ls t] means : 0: unknown solution, 1: solution found, 2: no solution

  (when {dyn <> 0} ;; IF or WHEN : it is the same thing here (only one statement) but not the same for Racket !!!
	(return (one? dyn)))

  (when (null? L)
    {dyna[ls t] <- 2}
    (return #f))

  {c <- (first L)}

  (when {c = t}  ;; c is the solution
    {dyna[ls t] <- 1}
    (return #t))

  {R <- (rest L)} ;; continue searching a solution in the rest

  (declare s)
  (if {c > t}  ;; c is to big to be a solution
    {s <- (subset-sum-dyna R t)}                              ; those calls can modify ls (which exist global and must exist local)
    ;; c is part of the solution or c is not part of solution
    {s <- (subset-sum-dyna R (t - c)) or (subset-sum-dyna R t)})

  {dyna[ls t] <- (tf->12 s)}
  s) ;; return boolean value


;; scheme@(guile-user)> (subset-sum-dynamic L-init t-init)
;; $1 = #t
(def (subset-sum-dynamic L t)

  (declare ls dyn c R s) ;; declare multiple variables

  {ls <- (length L)}
  {dyn <- dyna[ls t]} ;; dyna is a toplevel defined array

  ;; dyna[ls t] means : 0: unknown solution, 1: solution found, 2: no solution

  (when {dyn <> 0} ;; IF or WHEN : it is the same thing here (only one statement)  but not the same for Racket !!!
	(return (one? dyn)))

  (when (null? L)
    {dyna[ls t] <- 2}
    (return #f))

  {c <- (first L)}

  (when {c = t}  ;; c is the solution
    {dyna[ls t] <- 1}
    (return #t))

  {R <- (rest L)} ;; continue searching a solution in the rest

  (if {c > t}  ;; c is to big to be a solution
    {s <- (subset-sum-dynamic R t)}
    ;; c is part of the solution or c is not part of solution
    {s <- (subset-sum-dynamic R (t - c)) or (subset-sum-dynamic R t)})

  {dyna[ls t] <- (tf->12 s)}
  s) ;; return boolean value
  

;;(subset-sum-condx L-init t-init)
;;$1 = #t

(define (subset-sum-condx L t)

  (declare ls) ;;  ls is also GLOBAL,when i declare it here the procedure will modify its own ls variable, not the global one. 

  {ls <- (length L)}
  {dyn <- dyna[ls t]}
    
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  
  (condx [{dyn <> 0} (one? dyn)]
	 [(null? L) {dyna[ls t] <- 2}  #f] ;; return #f
	 
	 [exec {c <- (first L)}]	 
	 ;; c is the solution
	 [{c = t} {dyna[ls t] <- 1}  #t]  ;; return #t
	 
	 [exec {R <- (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} {s <- (subset-sum-condx R t)}
	          {dyna[ls t] <- (tf->12 s)}
		  s] ;; return boolean value
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 [else {s <- (subset-sum-condx R (t - c)) or (subset-sum-condx R t)}
	       {dyna[ls t] <- (tf->12 s)}
	       s])) ;; return boolean value




;; scheme@(guile-user)> (subset-sum-dec L-init t-init)
;;$1 = #t
(define (subset-sum-dec L t) ; declaration on top

  (declare ls dyn c R s)
  
  {ls <- (length L)}
  {dyn <- dyna[ls t]}

  {cpt <- cpt + 1} ;; cpt has been already defined at toplevel
  
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  
  (condx [{dyn <> 0} (one? dyn)]
	 [(null? L) {dyna[ls t] <- 2}  #f] ;; return #f
	 
	 [exec {c <- (first L)}]	 
	 ;; c is the solution
	 [{c = t} {dyna[ls t] <- 1}  #t]  ;; return #t
	 
	 [exec {R <- (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} {s <- (subset-sum-dec R t)}
	          {dyna[ls t] <- (tf->12 s)}
		  s] ;; return boolean value
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 [else {s <- (subset-sum-dec R {t - c}) or (subset-sum-dec R t)}
	       {dyna[ls t] <- (tf->12 s)}
	       s])) ;; return boolean value



;; find the solution like with backtracking

;; > (subset-sum-value L-init t-init)
;; '(1 3 4 16 17 24 45 64 197 256 275 323 540 889 915 1040 1041 1093 1099 1111 1344 1520 2027 2500 2734 3267 3610 4285 5027)
;; > t-init
;; 35267
;; > (+ 1 3 4 16 17 24 45 64 197 256 275 323 540 889 915 1040 1041 1093 1099 1111 1344 1520 2027 2500 2734 3267 3610 4285 5027)
;; 35267

;; > (subset-sum-value  '(17 24 45 64 197 256 323 540 723 889 915 1040 1041 1093 1111 1284 1344 1520 2027 2500 2734 3000 3267 4285 5027) t-init)
;; #f

;; > (subset-sum-value L-init t-init)
;; '(1 3 4 16 17 24 45 64 197 256 275 323 540 889 915 1040 1041 1093 1099 1111 1344 1520 2027 2500 2734 3267 3610 4285 5027)
;; > t-init
;; 35267
;; > (+ 1 3 4 16 17 24 45 64 197 256 275 323 540 889 915 1040 1041 1093 1099 1111 1344 1520 2027 2500 2734 3267 3610 4285 5027)
;; 35267


(define (subset-sum-value L t)

  ;; declaration on top
  (declare ls dyn c R s) ; c: current/first number of list, R: rest of list,s: solution if found
  
  {ls <- (length L)}
  {dyn <- dyna[ls t]}

  {cpt <- cpt + 1} ;; cpt has been already defined at toplevel
  
  ;; dyna[ls t] means 0: unknown solution, true (example: list): solution found, false: #f: no solution
  
  (condx [(not (equal? dyn 0)) dyn] ; the solution if found (already computed)
	 [(null? L) {dyna[ls t] <- #f}  #f] ;; return #f
	 
	 [exec {c <- (first L)}]	 
	 ;; c is the solution
	 [{c = t} {s <- dyna[ls t] <- (list c)} s]  ;; return a true value ( the list containing the solution)
	 
	 [exec {R <- (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} {s <- (subset-sum-value R t)}
	          {dyna[ls t] <- s}
		  s] ;; return boolean value which is the solution when true/found.
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 
	 [exec {s <- (subset-sum-value R {t - c})}]
	 [s {s <- dyna[ls t] <- (cons c s)} s] ; case c is part of a solution
	       
	 [exec {s <- (subset-sum-value R t)}] ; at this point c is not part of a solution
	 [else {dyna[ls t] <- s} s])) ;; return boolean value which is the solution when true/found.




;; (ssigma-approx-solution-list-memo L-init t-init)
;; exact solution found
;; (275 723 915 1040 1041 1093 1099 1111 1520 2027 2500 2734 3000 3267 3610 4285 5027)
;; note: ultra fast !!!

(define (ssigma-approx-solution-list-memo L t)

  ;; intermediate memoization function
  ;; because we have not yet lambda-return+ and rec+ 
  (define/memoize (ssigma-solution-approx-memo L t s)
    (ssigma-solution-approx L t s))
        
  ;; warning this function returns 2 values : a boolean and the solution
  (def (ssigma-solution-approx L t s) ; s : searched solution

       {cpt := cpt + 1}
       ;;(display L) (display " ") (display t) (newline)
       
       (when (null? L) 
	 
 	 ;; (display "null L")
	 ;; (newline)
	 ;; (newline)
	 
	 (return (list #f s)))
       
       {c := (first L)}
       {R := (rest L)}

       ;; c = t
       (when {c = t} (return (list #t
			     (cons c s)))) ;; c is the solution and should be the shortest solution as we insert only one element

       ;; otherwise we test for approximative or exact solution with or without c
       {rv0 := (ssigma-solution-approx-memo R (t - c) s)} ; with c
       {c-sol := (car rv0)}
       {sol-c-sol := (cadr rv0)}
       
       {rv1 := (ssigma-solution-approx-memo R t s)} ; without c
       {c-not-sol := (car rv1)}
       {sol-c-not-sol := (cadr rv1)}
       
       (when c-sol
	 {sol-c-sol := (cons c sol-c-sol)}) ; we add c to the solution of t - c to get the solution c + (t - c) = t

       (when {c-sol and c-not-sol} ; both are exact solutions
	 (if {(length sol-c-sol) < (length sol-c-not-sol)} ; get the shortest exact solution
	     (return (list #t
			   (append sol-c-sol
				   s)))
	     (return (list #t
			   (append sol-c-not-sol
				   s)))))
       
       ;; is this one an exact solution?
       (when c-not-sol ; without c in solution
	 (return (list #t
		       (append sol-c-not-sol
			       s))))

       ;; is this one an exact solution?
       (when c-sol ; with c in solution
	 (return (list #t
		       (append sol-c-sol
			       s))))
       
       ;; no exact solution , we take the best approximation, first in distance, and if distance equal in length of list
       {best-approx := (best-sol t
				 sol-c-not-sol
				 sol-c-sol)}
       
       (return (list #f
		     (append best-approx
			     s))))
  
       ;; end internal definition
       

  {rv2 := (ssigma-solution-approx-memo L t '())}
  {sf := (car rv2)}
  {sol := (cadr rv2)}
  
  (if sf 
      (display "exact solution found")
      (display "no exact solution found"))

  (newline)
  (display sol)
  (newline))





;; WARNING :functions below are not good

;; (define (start-ssigma-sol-approx-dyna L t)
;;   ;; (display "start-ssigma-sol-approx")
;;   ;; (newline)
;;   ;; (display "L=") (display L)
;;   ;; (newline)
;;   ;; (display "t=") (display t)
;;   ;; (newline)
;;   ;; (newline)
  


;;   (let [(ls (length L))
;;   	(dyn (array-ref dyna ls t))]
;;     (if (not (null? dyn))
;;   	dyn
;; 	(ssigma-sol-approx-dyna L t '() t '()))))

;; ;; TODO try to get out function constant parameters (if there are)
;; (define (ssigma-sol-approx-dyna L t S t-init AS) ;; AS:approximative solution

;;   ;; (display "L=") (display L)
;;   ;; (newline)
;;   ;; (display "S=") (display S)
;;   ;; (newline)
;;   ;; (display "AS=") (display AS)
;;   ;; (newline)
;;   ;; (newline)
  
;;   (if (null? L)
      
;;       (begin
;; 	;; (display "null L")
;; 	;; (newline)
;; 	;; (display "S=") (display S)
;; 	;; (newline) 
;; 	;; (display "AS=") (display AS)
;; 	;; (newline)
;; 	;; (display "return best-sol")
;; 	;; (newline)
;; 	(best-sol t-init AS S)) ;; must return S or AS and save it in dyna
      
;;       (let [ (c (first L))
;; 	     (R (rest L)) ]
;; 	(cond [ {c = t} (best-sol t-init AS (cons c S)) ] ;; c is the solution and save the best in dyna
;; 	      [ {c > t} (ssigma-sol-approx-dyna R t S t-init (best-sol t-init
;; 								  AS
;; 								  (list c))) ] ;; c is to big to be a solution but can be an approximation 
;; 	      ;; c < t at this point
;; 	      ;; c is part of the solution or his approximation
;; 	      ;; or c is not part of solution or his approximation
;; 	      [ else (best-sol3 t-init AS
				
;; 				       (begin
;; 					 ;;(display "append c=") (display c) (newline)

;; 					 (append (cons c S)
;; 						 (start-ssigma-sol-approx-dyna R {t - c}))) ;; we have to find a solution for t-c now

;; 				       (ssigma-sol-approx-dyna R t S t-init AS))])))) ;;  we must save the best in dyna (TODO : where? verify)



) ; end module

