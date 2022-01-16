#lang reader "../SRFI/SRFI-105.rkt"

(require "../Scheme+.rkt")
;;(require Scheme-PLUS-for-Racket/Scheme+)

(require srfi/25) ;; Multi-dimensional Array Primitives

;; Sub Set Sum problem
;; Dynamic solution
;; Guile compatible

;; Copyright 2021 Damien MATTEI

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


;; files below can be retrieved here: https://github.com/damien-mattei/library-FunctProg
(include "../library/pair.scm") ;; useless,but definitions reused in some not used functions in number.scm 
(include "../library/number.scm")

;; {L-init <+ '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027)}
;; {t-init <+ 35267}
;; {ls <+ (length L-init)}
;; {dyna <+ (make-array 0 {ls + 1} {t-init + 1})} ;; Guile compatible !
;; {cpt <+ 0} ;;(define cpt 0)

(declare L-init t-init ls dyna cpt)

{L-init <- '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027)}
{t-init <- 35267}
;;{t-init <- 21}

{ls <- (length L-init)}
{dyna <- (make-array (shape 0 {ls + 1}
			    0 {t-init + 1})
		     0)}

(define (one-two b)
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
;; 			(one-two
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
;; 	{ dyna[ls t] ← (one-two
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
  
  {cpt <- {cpt + 1}} ;; cpt is defined at toplevel
  
  ;;(display L) (display " ") (display t) (newline)

  ;;(def ls (length L))
  ;;(def dyn {dyna[ls t]})

  {ls <+ (length L)}

  ;;(display "ls=") (display ls) (display " ") (display "t=") (display t) (newline)
  
  {dyn <+ {dyna[ls t]}}

  ;;(display "dyn=") (display dyn) (newline)
  
  (def c)
  (def R)

 
  
  ;; TODO: write this code simplier
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  (one?
    (if (not (zero? dyn))
	
	dyn
	
	;; set the array but return the variable
	($
	 ;;(display "/ ls=") (display ls) (display " ") (display "t=") (display t) (newline)
	 { dyna[ls t] <- (one-two
			  (if (null? L)
			      #f
			      ($
			       ;;(display "assignment") (newline)
				{c <- (first L)}
				{R <- (rest L)}
				(cond [ {c = t} #t ] ;; c is the solution
				      [ {c > t} (ssigma-dyna-define-anywhere R t) ] ;; c is to big to be a solution but can be an approximation
				      ;; c < t at this point
				      ;; c is part of the solution or his approximation
				      ;; or c is not part of solution or his approximation
				      [ else {(ssigma-dyna-define-anywhere R {t - c}) or (ssigma-dyna-define-anywhere R t)} ] )))) }) )))



;; scheme@(guile-user)> (ssigma-dyna-def L-init t-init)
;; $1 = #t
;; scheme@(guile-user)> cpt
;; $2 = 147801
(define (ssigma-dyna-def L t)
  
  {cpt <- {cpt + 1}} ;; cpt is defined at toplevel
  
  (def (ls dyn)) ;; declare multiple variables 
  
  {ls <- (length L)}
  {dyn <- {dyna[ls t]}}

  ;; declare one variable at a time
  (def c)
  (def R)

  ;; TODO: write this code simplier
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  (one?
   (if (not (zero? dyn))
       
       dyn
       
       ;; set the array but return the variable
       { dyna[ls t] <- (one-two
			(if (null? L)
			    #f
			    ;; TODO: rename $ which is already used by SRFI-9 record utiliser  § ou | (option shift L sur mac)
			    ($ ;;(display "assignment") (newline)
			     {c <- (first L)}
			     {R <- (rest L)}
			     (cond [ {c = t} #t ] ;; c is the solution
				   [ {c > t} (ssigma-dyna-def R t) ] ;; c is to big to be a solution but can be an approximation
				   ;; c < t at this point
				   ;; c is part of the solution or his approximation
				   ;; or c is not part of solution or his approximation
				   [ else {(ssigma-dyna-def R {t - c}) or (ssigma-dyna-def R t)} ] )))) } )))







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
	[(null? L) (array-set! dyna ls t 2) #f] ;; return #f
	
	[else (let [(c (first L))]
		
		(if {c = t} ;; c is the solution
		      
		    (begin
		      (array-set! dyna 1 ls t)
		      #t)  ;; return #t

		    ;; else
		    (let [(R (rest L))]
		      
		      (if {c > t}   ;; continue searching a solution in the rest
			    
			  (let [(s (ssigma-proto R t))]
			    (array-set! dyna
					ls t
					(one-two s))
			      
			    s) ;; return s
			
			  ;; else
			  ;; c < t at this point
			  ;; c is part of the solution or his approximation
			  ;; or c is not part of solution
			  (let [(s {(ssigma-proto R {t - c}) or (ssigma-proto R t)})]
			    (array-set! dyna 
					ls t
					(one-two s))
			    s)))))
	      ] ))


(define (ssigma-proto-condx L t)

  (set! cpt {cpt + 1})

  (define ls (length L))
  (define dyn (array-ref dyna ls t))


  (display L) (newline)
  (display t) (newline)
 
    
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
		      (one-two s)
		      ls t)
	  s] ;; return s
			
	 ;; else :
	 ;; c < t at this point
	 ;; c is part of the solution or his approximation
	 ;; or c is not part of solution
	 [else (define s {(ssigma-proto-condx R {t - c}) or (ssigma-proto-condx R t)})
	       (array-set! dyna (one-two s)
			   ls t)
	       s]))





;; (subset-sum-dyna  L-init t-init)
;; #t ;; there exist a solution

(def (subset-sum-dyna L t)

  (declare ls dyn) ;; declare multiple variables

  {ls <- (length L)}
  {dyn <- dyna[ls t]}

  ;; dyna[ls t] means : 0: unknown solution, 1: solution found, 2: no solution

  ;;(if {dyn <> 0} ;; IF or WHEN : it is the same thing here (only one statement) Warning: Racket incompatibility
  (when {dyn <> 0}
    (return (one? dyn)))

  (when (null? L)
    {dyna[ls t] <- 2}
    (return #f))

  {c <+ (first L)}

  (when {c = t}  ;; c is the solution
    {dyna[ls t] <- 1}
    (return #t))

  {R <+ (rest L)} ;; continue searching a solution in the rest

  (declare s)
  (if {c > t}  ;; c is to big to be a solution
    {s <- (subset-sum-dyna R t)}
    ;; c is part of the solution or c is not part of solution
    {s <- {(subset-sum-dyna R {t - c}) or (subset-sum-dyna R t)}})

  {dyna[ls t] <- (one-two s)}
  s) ;; return boolean value


;; scheme@(guile-user)> (subset-sum-dynamic L-init t-init)
;; $1 = #t
(def (subset-sum-dynamic L t)

  (declare ls dyn c R s) ;; declare multiple variables

  {ls <- (length L)}
  {dyn <- dyna[ls t]} ;; dyna is a toplevel defined array

  ;; dyna[ls t] means : 0: unknown solution, 1: solution found, 2: no solution

  ;;(if {dyn <> 0} ;; IF or WHEN : it is the same thing here (only one statement)  Warning: Racket incompatibility
  (when {dyn <> 0}
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
    {s <- {(subset-sum-dynamic R {t - c}) or (subset-sum-dynamic R t)}})

  {dyna[ls t] <- (one-two s)}
  s) ;; return boolean value
  


(define (subset-sum-condx L t)

  (declare ls dyn) ;; declare multiple variables or use <+ instead of <- below

  {ls <- (length L)}
  {dyn <- dyna[ls t]}
    
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  
  (condx [{dyn <> 0} (one? dyn)]
	 [(null? L) {dyna[ls t] <- 2}  #f] ;; return #f
	 
	 [exec {c <+ (first L)}]	 
	 ;; c is the solution
	 [{c = t} {dyna[ls t] <- 1}  #t]  ;; return #t
	 
	 [exec {R <+ (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} {s <+ (subset-sum-condx R t)}
	          {dyna[ls t] <- (one-two s)}
		  s] ;; return boolean value
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 [else {s <+ {(subset-sum-condx R {t - c}) or (subset-sum-condx R t)}}
	       {dyna[ls t] <- (one-two s)}
	       s])) ;; return boolean value



(define (subset-sum-guile L t)

  {ls <+ (length L)}
  {dyn <+ dyna[ls t]}

  {cpt <- {cpt + 1}} ;; cpt has been already defined at toplevel
  
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  
  (condx [{dyn <> 0} (one? dyn)]
	 [(null? L) {dyna[ls t] <- 2}  #f] ;; return #f
	 
	 [exec {c <+ (first L)}]	 
	 ;; c is the solution
	 [{c = t} {dyna[ls t] <- 1}  #t]  ;; return #t
	 
	 [exec {R <+ (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} {s <+ (subset-sum-guile R t)}
	          {dyna[ls t] <- (one-two s)}
		  s] ;; return boolean value
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 [else {s <+ {(subset-sum-guile R {t - c}) or (subset-sum-guile R t)}}
	       {dyna[ls t] <- (one-two s)}
	       s])) ;; return boolean value


(define (subset-sum-guile-dec L t)

  (declare ls dyn c R s)
  
  {ls <- (length L)}
  {dyn <- dyna[ls t]}

  {cpt <- {cpt + 1}} ;; cpt has been already defined at toplevel
  
  ;; dyna[ls t] means 0: unknown solution, 1: solution found, 2: no solution
  
  (condx [{dyn <> 0} (one? dyn)]
	 [(null? L) {dyna[ls t] <- 2}  #f] ;; return #f
	 
	 [exec {c <- (first L)}]	 
	 ;; c is the solution
	 [{c = t} {dyna[ls t] <- 1}  #t]  ;; return #t
	 
	 [exec {R <- (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} {s <- (subset-sum-guile-dec R t)}
	          {dyna[ls t] <- (one-two s)}
		  s] ;; return boolean value
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 [else {s <- {(subset-sum-guile-dec R {t - c}) or (subset-sum-guile-dec R t)}}
	       {dyna[ls t] <- (one-two s)}
	       s])) ;; return boolean value


