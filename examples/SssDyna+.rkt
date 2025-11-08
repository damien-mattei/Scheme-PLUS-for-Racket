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

;; This files contains a lot of Subset sum procedures, just consider the ones at the
;; end of file as the best ones and they are more easy to understand than the previous ones


(module sssdyna racket



(require Scheme+
	 "best-solution+.rkt"
	 memo
         plot
         math/base)
	
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

;; > (ssigma-approx-solution-list-memo L-init {t-init + 100000})
;; (ssigma-approx-solution-list-memo L-init (+ t-init 100000))
;; no exact solution found
;; (1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027)

;; {L-init <- '(1 3 4 16 17 24)}
;; {t-init <- 11}
;; (ssigma-approx-solution-list-memo L-init t-init)
;; no exact solution found
;; (1 3 4)
(define (ssigma-approx-solution-list-memo L t)

  ;; we have a '(solution-flag solution-list) list as return value
  ;; below are accessors
  (define sol-flag car)
  (define sol-list cadr)

  ;; intermediate memoization function
  ;; because we have not yet lambda+ and rec+ 
  (define/memoize (ssigma-solution-approx-memo L t)
    (ssigma-solution-approx L t))
        
  ;; warning this function returns 2 values in a list : a boolean and the solution
  (def (ssigma-solution-approx L t)

       {cpt := cpt + 1} ; just for statistic
       ;;(display L) (display " ") (display t) (newline)
       
       (when (null? L) 
	 
 	 ;; (display "null L")
	 ;; (newline)
	 ;; (newline)
	 
	 (return (list #f '())))
       
       {c := (first L)}
       {R := (rest L)}

       ;; c = t
       (when {c = t} (return (list #t
				   (list c)))) ;; c is the solution and should be the shortest solution as we insert only one element

       (when (null? R) ; we reached the null list !!! search for an approximate solution !
	 ;;(display "c=") (display c) (newline)
	 ;;(display "t=") (display t) (newline)
	 (if {c < 2 * t} ; 0 ...... t ...... 2t
	     (return (list #f
			   (list c))) ; c is the best approximation
	     (return (list #f
			   '())))) ; 0 is the best approximation and even shorter as we do not include it in list !
	 
       
       ;; otherwise we test for approximative or exact solution with or without c
       {rv := (ssigma-solution-approx-memo R (t - c))} ; with c
       {c-sol := (sol-flag rv)}
       {solu-c-sol := (sol-list rv)}

       ;; anyway c could be a solution or part of an approximation
       {solu-c-sol := (cons c solu-c-sol)} ; we add c to the solution of t - c to get the solution c + (t - c) = t

       
       {rv := (ssigma-solution-approx-memo R t)} ; without c
       {c-not-sol := (sol-flag rv)}
       {solu-c-not-sol := (sol-list rv)}
       
     
       (when {c-sol and c-not-sol} ; both are exact solutions
	 (if {(length solu-c-sol) < (length solu-c-not-sol)} ; get the shortest exact solution
	     (return (list #t
			   solu-c-sol))
	     (return (list #t
			   solu-c-not-sol))))
       
       ;; is this one an exact solution? (return this one first because without c will be shortest than with)
       (when c-not-sol ; without c in solution
	 (return (list #t
		       solu-c-not-sol)))

       ;; is this one an exact solution?
       (when c-sol ; with c in solution
	 (return (list #t
		       solu-c-sol)))


       ;; approximate solution continue here !!!
       
       ;; no exact solution , we take the best approximation, first in distance, and if distance equal in length of list
       {best-approx := (best-sol t
				 solu-c-not-sol
				 solu-c-sol)}
       
       (return (list #f
		     best-approx)))
  
       ;; end internal definition
       

  {rval := (ssigma-solution-approx-memo L t)}
  {sf := (sol-flag rval)}
  {sol := (sol-list rval)}
  
  (if sf 
      (display "exact solution found")
      (display "no exact solution found"))

  (newline)
  (display sol)
  (newline)

  rval)



;; {L-init := '(1 3 4 16 17 24)}
;; {t-init := 11}
;; (ssigma-approx-solution-list-memo-condx L-init t-init)
;; no exact solution found
;; (1 3 4)
;; '(#f (1 3 4))

;; (ssigma-approx-solution-list-memo-condx (cons 150000 L-init) 145000)
;; no exact solution found
;; (150000)
;; '(#f (150000))
(define (ssigma-approx-solution-list-memo-condx L t)

  ;; we have a '(solution-flag solution-list) list as return value
  ;; below are accessors
  (define sol-flag car)
  (define sol-list cadr)

  ;; internal definition
  ;; memoization function
  (define/memoize (ssigma-solution-approx-memo-condx L t)
          
    ;; warning this function returns 2 values in a list : a boolean and the solution
    
    (condx [(null? L) (list #f '())]

           [exec {c := (first L)}
                 {R := (rest L)}]

           ;; c = t
           [{c = t} (list #t
                          (list c))] ; c is the solution and should be the shortest solution as we insert only one element

           ;; we reached the null list !!! search for an approximate solution !
           [(null? R) (if {c < 2 * t} ; 0 ...... t ...... 2t : search from 0 or c which one is the nearest of t
                          ; it is c, unless c is greater than 2t
                          (list #f
                                (list c)) ; c is the best approximation
                          ; 0 is nearer from t than c 
                          (list #f
                                '()))] ; 0 is the best approximation and even shorter as we do not include it in list !
           
           
           ;; otherwise we test for approximative or exact solution with or without c
           [exec {rv := (ssigma-solution-approx-memo-condx R (t - c))} ; with c
                 {c-sol := (sol-flag rv)}
                 {solu-c-sol := (sol-list rv)}

                 ;; anyway c could be a solution or part of an approximation
                 {solu-c-sol := (cons c solu-c-sol)} ; we add c to the solution of t - c to get the solution c + (t - c) = t

                 
                 {rv := (ssigma-solution-approx-memo-condx R t)} ; without c
                 {c-not-sol := (sol-flag rv)}
                 {solu-c-not-sol := (sol-list rv)}]
           
           ; both are exact solutions
           [{c-sol and c-not-sol} (if {(length solu-c-sol) < (length solu-c-not-sol)} ; get the shortest exact solution
                                      (list #t
                                            solu-c-sol)
                                      (list #t
                                            solu-c-not-sol))]
           
           ;; is this one an exact solution? (return this one first because without c will be shortest than with)
           ; without c in solution
           [c-not-sol (list #t
                            solu-c-not-sol)]

           ;; is this one an exact solution?
           ; with c in solution
           [c-sol (list #t
                        solu-c-sol)]

           ;; approximate solution continue here !!!
           ;; no exact solution , we take the best approximation, first in distance, and if distance equal in length of list
           [else {best-approx := (best-sol t
                                           solu-c-not-sol
                                           solu-c-sol)}
                 (list #f
                       best-approx)]))
  
  ;; end internal definition
  

  {rval := (ssigma-solution-approx-memo-condx L t)}
  {sf := (sol-flag rval)}
  {sol := (sol-list rval)}
  
  (if sf 
      (display "exact solution found")
      (display "no exact solution found"))
  
  (newline)
  (display sol)
  (newline)
  
  rval)




;; now without internal definition ,calling directly the memoized function
;; like that if we recall at REPL a computation it uses the data in storage

;; we have a '(solution-flag solution-list) list as return value
;; below are accessors
(define sol-flag car)
(define sol-list cadr)



;; > (time (ssigma-solution-approx-memo-condx (cons 150000 L-init) 145000))

;; (time (ssigma-solution-approx-memo-condx (cons 150000 L-init) 145000))
;; cpu time: 4776 real time: 4778 gc time: 2786
;; '(#f (150000))


;; > (time (ssigma-solution-approx-memo-condx (cons 150000 L-init) 145000))

;; (time (ssigma-solution-approx-memo-condx (cons 150000 L-init) 145000))
;; cpu time: 0 real time: 0 gc time: 0
;; '(#f (150000))

(define/memoize (ssigma-solution-approx-memo-condx L t)
          
    ;; warning this function returns 2 values in a list : a boolean and the solution
    
    (condx [(null? L) (list #f '())]

           [exec {c := (first L)}
                 {R := (rest L)}]

           ;; c = t
           [{c = t} (list #t
                          (list c))] ; c is the solution and should be the shortest solution as we insert only one element

           ;; we reached the null list !!! search for an approximate solution !
           [(null? R) (if {c < 2 * t} ; 0 ...... t ...... 2t : search from 0 or c which one is the nearest of t
                          ; it is c, unless c is greater than 2t
                          (list #f
                                (list c)) ; c is the best approximation
                          ; 0 is nearer from t than c 
                          (list #f
                                '()))] ; 0 is the best approximation and even shorter as we do not include it in list !
           
           
           ;; otherwise we test for approximative or exact solution with or without c
           [exec {rv := (ssigma-solution-approx-memo-condx R (t - c))} ; with c
                 {c-sol := (sol-flag rv)}
                 {solu-c-sol := (sol-list rv)}

                 ;; anyway c could be a solution or part of an approximation
                 {solu-c-sol := (cons c solu-c-sol)} ; we add c to the solution of t - c to get the solution c + (t - c) = t

                 
                 {rv := (ssigma-solution-approx-memo-condx R t)} ; without c
                 {c-not-sol := (sol-flag rv)}
                 {solu-c-not-sol := (sol-list rv)}]
           
           ; both are exact solutions
           [{c-sol and c-not-sol} (if {(length solu-c-sol) < (length solu-c-not-sol)} ; get the shortest exact solution
                                      (list #t
                                            solu-c-sol)
                                      (list #t
                                            solu-c-not-sol))]
           
           ;; is this one an exact solution? (return this one first because without c will be shortest than with)
           ; without c in solution
           [c-not-sol (list #t
                            solu-c-not-sol)]

           ;; is this one an exact solution?
           ; with c in solution
           [c-sol (list #t
                        solu-c-sol)]

           ;; approximate solution continue here !!!
           ;; no exact solution , we take the best approximation, first in distance, and if distance equal in length of list
           [else {best-approx := (best-sol t
                                           solu-c-not-sol
                                           solu-c-sol)}
                 (list #f
                       best-approx)]))




;; make statistic for finding time complexity of the previous procedures

;; * make variable sum to find for a given subset
;; * make variable subset size and sum (general case) but keep sum at same proportion than subset sum elements

;; warning: i define here the variable of the size of subset sum problem having in mind it will serve to solve by reduction the vertex cover problem and other NP problems (3SAT ,etc...)
{n := 6} ; arbitrary max length of number in modified base 4 because it is related to variables , digit,vertex,boolean variable in other NP problems...

{rg := 2 * n} ; number of elements in subset for sum, this is the double of n ,because in vertex cover problem we have a square incidence matrix of size n and for solving vertex cover with subset sum problem we must put below the square matrix,which is another diagonal square matrix of same size (filled of 1 on diagonal but not the identity matrix!), this double the vertical size of the global matrix ,making it rectangular of size n in columns and 2n in rows.

{g := 4 ⁿ⁺¹} ; gap where elements of subset are chosen (⁺¹ is for the vertex count in vertex cover problem, but this is of no importance in our Subset sum problem) the most signifiant bit count for presence or not of the vertex,it is set to 1 so each add give the total number of required vertex,again no importance for subset sum problem. 4 is the base 4 modified.

(display "g=") (display g) (newline)

;; list of elements of subset
{L-in := (map (λ (x) (random-natural g))
              (range rg))}

{L-in := (append (range 1 10) L-in)}

(display "L-in=") (display L-in) (newline)

{som := (apply + L-in)} ; sum of all elements of subset

(display "som=") (display som) (newline)

{p-start := 0.01}
{p-stop := 0.7}

{gap := (range (int (som * p-start))
               (int (som * p-stop))
               (int (som * (p-stop - p-start) / 100)))} ; gap where to compute statistics


(display "gap=") (display gap) (newline)


(define (ssigma-approx-solution-list-memo-condx-reset L t) ; will reset the memo hash table after computation

  ;; we have a '(solution-flag solution-list) list as return value
  ;; below are accessors
  (define sol-flag car)
  (define sol-list cadr)

  ;; internal definition
  ;; memoization function
  (define/memoize (ssigma-solution-approx-memo-condx L t)
          
    ;; warning this function returns 2 values in a list : a boolean and the solution
    
    (condx [(null? L) (list #f '())]

           [exec {c := (first L)}
                 {R := (rest L)}]

           ;; c = t
           [{c = t} (list #t
                          (list c))] ; c is the solution and should be the shortest solution as we insert only one element

           ;; we reached the null list !!! search for an approximate solution !
           [(null? R) (if {c < 2 * t} ; 0 ...... t ...... 2t : search from 0 or c which one is the nearest of t
                          ; it is c, unless c is greater than 2t
                          (list #f
                                (list c)) ; c is the best approximation
                          ; 0 is nearer from t than c 
                          (list #f
                                '()))] ; 0 is the best approximation and even shorter as we do not include it in list !
           
           
           ;; otherwise we test for approximative or exact solution with or without c
           [exec {rv := (ssigma-solution-approx-memo-condx R (t - c))} ; with c
                 {c-sol := (sol-flag rv)}
                 {solu-c-sol := (sol-list rv)}

                 ;; anyway c could be a solution or part of an approximation
                 {solu-c-sol := (cons c solu-c-sol)} ; we add c to the solution of t - c to get the solution c + (t - c) = t

                 
                 {rv := (ssigma-solution-approx-memo-condx R t)} ; without c
                 {c-not-sol := (sol-flag rv)}
                 {solu-c-not-sol := (sol-list rv)}]
           
           ; both are exact solutions
           [{c-sol and c-not-sol} (if {(length solu-c-sol) < (length solu-c-not-sol)} ; get the shortest exact solution
                                      (list #t
                                            solu-c-sol)
                                      (list #t
                                            solu-c-not-sol))]
           
           ;; is this one an exact solution? (return this one first because without c will be shortest than with)
           ; without c in solution
           [c-not-sol (list #t
                            solu-c-not-sol)]

           ;; is this one an exact solution?
           ; with c in solution
           [c-sol (list #t
                        solu-c-sol)]

           ;; approximate solution continue here !!!
           ;; no exact solution , we take the best approximation, first in distance, and if distance equal in length of list
           [else {best-approx := (best-sol t
                                           solu-c-not-sol
                                           solu-c-sol)}
                 (list #f
                       best-approx)]))
  
  ;; end internal definition
  

  {rval := (ssigma-solution-approx-memo-condx L t)}
  {sf := (sol-flag rval)}
  {sol := (sol-list rval)}
  
  (if sf 
      (display "exact solution found")
      (display "no exact solution found"))
  
  (newline)
  (display sol)
  (newline)

  ;; this should not be usefull as we have an internal definition called from an external procedure each time we make statistics
  (set-box! (ssigma-solution-approx-memo-condx) (hasheq)) ; amnesia 
  
  rval)



;;(subset-sum-result L-init t-init)
;;'(1 3 4 16 17 24 45 64 197 256 275 323 540 889 915 1040 1041 1093 1099 1111 1344 1520 2027 2500 2734 3267 3610 4285 5027)



;; (subset-sum-result
;; (list
;;   518533
;;   1037066
;;   2074132
;;   1648264
;;   796528
;;   1593056
;;   686112
;;   1372224
;;   244448
;;   488896
;;   977792
;;   1955584
;;   1411168
;;   322336
;;   644672
;;   1289344
;;   78688
;;   157376
;;   314752
;;   629504
;;   1259008)
;;  2463098)
;;
;; '(1037066 796528 629504)

(define/memoize (subset-sum-result  L t)

  ;(display "L=") (display L) (newline)
  ;(display "t=") (display t) (newline)

  ;; declaration on top
  ;(declare c R s) ; c: current/first number of list, R: rest of list,s: solution if found
  
 
  ;; result  means :
  ; true (example: list): solution found,
  ; false: #f: no solution
  
  (condx [(null? L) #f] ;; return #f
	 
	 [exec {c <- (first L)}]	 
	 ;; c is the solution
	 [{c = t} (list c)]  ;; return a true value ( the list containing the solution)
	 
	 [exec {R <- (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} (subset-sum-result R t)] ;; return boolean value which is the solution when true/found.
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 
	 [exec {s <- (subset-sum-result R (t - c))}]
	 [s (cons c s)] ; case c is part of a solution
	       
	 ; at this point c is not part of a solution
	 [else (subset-sum-result R t)])) ;; return boolean value which is the solution when true/found.


;; will clean the memo
(define (subset-sum-result-amnesia  L t)
  {rv := (subset-sum-result  L t)}
  (set-box! (subset-sum-result) (hasheq))  ; amnesia
  rv) 
  

(define (chrono-subsetsum-approx-minim t)
  (display "chrono-subsetsum-approx-minim : t=") (display t) (newline)
  {(rv cpu rel gc) := (time-apply ssigma-approx-solution-list-memo-condx-reset (list L-in t))}
  (display "rv=") (display rv) (newline)
  (list (vector t rel) (car (sol-flag rv))))


(define (chrono-subsetsum-amnesia t)
  (display "chrono-subsetsum-amnesia : t=") (display t) (newline)
  {(rv cpu rel gc) := (time-apply subset-sum-result-amnesia (list L-in t))}
  (display "rv=") (display rv) (newline)
  (list (vector t rel) (car rv)))

(define (chrono-subsetsum t)
  (display "chrono-subsetsum : t=") (display t) (newline)
  {(rv cpu rel gc) := (time-apply subset-sum-result (list L-in t))}
  (display "rv=") (display rv) (newline)
  (list (vector t rel) (car rv)))

;; (start-computation-and-plot chrono-subsetsum-amnesia)
;; (start-computation-and-plot chrono-subsetsum-approx-minim)
(define (start-computation-and-plot chrono)
  {L-SubSetSum := (map chrono gap)}
  {L-SubSetSum-exact := (filter (λ (l) (cadr l))
                                L-SubSetSum)}
  {L-SubSetSum-approx := (filter (λ (l) (not (cadr l)))
                                 L-SubSetSum)}
  {Lplot-SubSetSum-exact := (map car L-SubSetSum-exact)}
  {Lplot-SubSetSum-approx := (map car L-SubSetSum-approx)}
                                           

  (plot (list (points Lplot-SubSetSum-approx  #:sym 'fullcircle1
                                              #:color "blue"
                                              #:label "no solution or approximation")
              (points Lplot-SubSetSum-exact  #:sym 'circle1
                                             #:color "red"
                                             #:label "exact solution"))))


;; (subset-sum-rec  '(17 24 45 64 197 256 323 540 723 889 915 1040 1041 1093 1111 1284 1344 1520 2027 2500 2734 3000 3267 4285 5027) t-init)
;; #f
(define (subset-sum-rec  L t)

  ;(display "L=") (display L) (newline)
  ;(display "t=") (display t) (newline)
  ;(newline)
  
  ;; declaration on top
  ;; (declare c R s) ; c: current/first number of list, R: rest of list,s: solution if found
  
 
  ;; result  means :
  ; true (example: list): solution found,
  ; false: #f: no solution
  
  (condx [(null? L) #f] ;; return #f
	 
	 [exec {c <- (first L)}]	 
	 ;; c is the solution
	 [{c = t} (list c)]  ;; return a true value ( the list containing the solution)
	 
	 [exec {R <- (rest L)}]	 
	 ;; continue searching a solution in the rest
	 [{c > t} (subset-sum-rec R t)] ;; return boolean value which is the solution when true/found.
			
	 ;; else : c < t at this point
	 ;; c is part of a solution OR not part of a solution
	 
	 [exec {s <- (subset-sum-rec R (t - c))}]
	 [s (cons c s)] ; case c is part of a solution
	       
	 ; at this point c is not part of a solution
	 [else (subset-sum-rec R t)])) ;; return boolean value which is the solution when true/found.


) ; end module

