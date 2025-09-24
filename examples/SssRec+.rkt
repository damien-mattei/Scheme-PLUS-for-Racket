#lang reader SRFI-105


;; Sub Set Sum problem
;; Recursive version
;; Racket version

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


;; if necessary some files can be retrieved here: https://github.com/damien-mattei/library-FunctProg

;; note := and <- are strictly equivalent

(module sssrec racket

(provide L-init
	 t-init
	 ssigma
	 ssigma-exact-solution)

(require Scheme+
	 "best-solution+.rkt")	

;; (apply + L-init)
;; 40274
{L-init := '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027)}
{t-init := 35267}

{cpt := 0}

;;> (ssigma L-init t-init)
;;#t
(def (ssigma L t)

  {cpt := cpt + 1}
  ;;(display L) (display " ") (display t) (newline)
  
  (when (null? L) 
    
    ;; (display "null L")
    ;; (newline)
    ;; (newline)
    (return #f))
  
  {c := (first L)}
  {R := (rest L)}
  (cond [ {c = t} #t ] ;; c is the solution
	[ {c > t} (ssigma R t) ] ;; c is to big to be a solution
	;; c < t at this point
	;; c is part of the solution
	;; or c is not part of solution
	[ else {(ssigma R (t - c)) or (ssigma R t)} ] ))



;; > (ssigma-exact-solution L-init t-init)


;; (ssigma-exact-solution L-init t-init)
;; solution found is:(1 3 4 16 17 24 45 64 197 256 275 323 540 889 915 1040 1041 1093 1099 1111 1344 1520 2027 2500 2734 3267 3610 4285 5027)


;; #<eof>
;; > {t-init = (apply + '(1 3 4 16 17 24 45 64 197 256 275 323 540 889 915 1040 1041 1093 1099 1111 1344 1520 2027 2500 2734 3267 3610 4285 5027))}


;; (=
;;  t-init
;;  (apply
;;   +
;;   '(1
;;     3
;;     4
;;     16
;;     17
;;     24
;;     45
;;     64
;;     197
;;     256
;;     275
;;     323
;;     540
;;     889
;;     915
;;     1040
;;     1041
;;     1093
;;     1099
;;     1111
;;     1344
;;     1520
;;     2027
;;     2500
;;     2734
;;     3267
;;     3610
;;     4285
;;     5027)))
;; #t


;; #<eof>
(define (ssigma-exact-solution L t)
  
  ;; warning this function returns 2 values : a boolean and the solution
  (def (ssigma-solution-exact L t s) ; s : searched solution

       {cpt <- cpt + 1}
       ;;(display L) (display " ") (display t) (newline)
       
       (when (null? L) 
	 
 	 ;; (display "null L")
	 ;; (newline)
	 ;; (newline)
	 
	 (return #f s))
       
       {c <- (first L)}
       {R <- (rest L)}
       (condx [ {c = t} (return #t
				(cons c s)) ] ;; c is the solution
	      
	      [ {c > t} (ssigma-solution-exact R t s) ] ;; c is to big to be a solution
	      
	      ;; c < t at this point
	      ;; c is part of the solution	 
	      [ exec
		{(c-sol sol) <- (ssigma-solution-exact R (t - c) s)} ]
	      
	      [ c-sol (return c-sol
			      (append (cons c s) ; c is in the solution for t, we insert it in the previous partial solution
				      sol)) ] ; we append with sol to get also the solutions of t - c,resulting in solutions of c + (t - c) = t
	      ;; or c is not part of solution
	      ;; or the two ! perheaps there exists 2 solutions using or not using c ! (in this case we got only the first found!)
	      [ else
		(ssigma-solution-exact R t s) ] ))

  {(sf sol) <- (ssigma-solution-exact L t '())}
  
  (if sf then
      (display "solution found is:")
      (display sol)
      (newline)
    else
      (display "no solution found")))





;; (ssigma-approx-solution L-init t-init)
;; exact solution found
;; (275 723 915 1040 1041 1093 1099 1111 1520 2027 2500 2734 3000 3267 3610 4285 5027)

;; #<eof>
;; > (apply + '(275 723 915 1040 1041 1093 1099 1111 1520 2027 2500 2734 3000 3267 3610 4285 5027))


;; (apply
;;  +
;;  '(275
;;    723
;;    915
;;    1040
;;    1041
;;    1093
;;    1099
;;    1111
;;    1520
;;    2027
;;    2500
;;    2734
;;    3000
;;    3267
;;    3610
;;    4285
;;    5027))
;; 35267


;; #<eof>
;; > t-init


;; t-init
;; 35267


;; slow solution (search for the shortest one by exploring all the branches of tree)
(define (ssigma-approx-solution L t)
  
  ;; warning this function returns 2 values : a boolean and the solution
  (def (ssigma-solution-approx L t s) ; s : searched solution

       {cpt := cpt + 1}
       ;;(display L) (display " ") (display t) (newline)
       
       (when (null? L) 
	 
 	 ;; (display "null L")
	 ;; (newline)
	 ;; (newline)
	 
	 (return #f s))
       
       {c := (first L)}
       {R := (rest L)}

       ;; c = t
       (when {c = t} (return #t
			     (cons c s))) ;; c is the solution and should be the shortest solution as we insert only one element

       ;; otherwise we test for approximative or exact solution with or without c
       {(c-sol sol-c-sol) := (ssigma-solution-approx R (t - c) s)} ; with c
       {(c-not-sol sol-c-not-sol) := (ssigma-solution-approx R t s)} ; without c
       
       (when c-sol
	 {sol-c-sol := (cons c sol-c-sol)}) ; we add c to the solution of t - c to get the solution c + (t - c) = t

       (when {c-sol and c-not-sol} ; both are exact solutions
	 (if {(length sol-c-sol) < (length sol-c-not-sol)} ; get the shortest exact solution
	     (return #t
		     (append sol-c-sol
			     s))
	     (return #t
		     (append sol-c-not-sol
			     s))))
       
       ;; is this one an exact solution?
       (when c-not-sol ; without c in solution
	 (return #t
		 (append sol-c-not-sol
			 s)))

       ;; is this one an exact solution?
       (when c-sol ; with c in solution
	 (return #t
		 (append sol-c-sol
			 s)))
       

       ;; no exact solution , we take the best approximation, first in distance, and if distance equal in length of list
       {best-approx := (best-sol t
				 sol-c-not-sol
				 sol-c-sol)}
       
       (return #f
	       (append best-approx
		       s)))
       
       

  {(sf sol) := (ssigma-solution-approx L t '())}
  
  (if sf 
      (display "exact solution found")
      (display "no exact solution found"))

  (newline)
  (display sol)
  (newline))


;; (ssigma-fast-approx-solution L-init t-init)
;; exact solution found
;; (1 3 4 16 17 24 45 64 197 256 275 323 540 889 915 1040 1041 1093 1099 1111 1344 1520 2027 2500 2734 3267 3610 4285 5027)

;; fast solution (will not search for the shortest one)
(define (ssigma-fast-approx-solution L t)
  
  ;; warning this function returns 2 values : a boolean and the solution
  (def (ssigma-fast-solution-approx L t s) ; s : searched solution

       {cpt := cpt + 1}
       ;;(display L) (display " ") (display t) (newline)
       
       (when (null? L) 
	 
 	 ;; (display "null L")
	 ;; (newline)
	 ;; (newline)
	 
	 (return #f s))
       
       {c := (first L)}
       {R := (rest L)}

       ;; c = t
       (when {c = t} (return #t
			     (cons c s))) ;; c is the solution and should be the shortest solution as we insert only one element

       ;; otherwise we test for approximative or exact solution with or without c
       {(c-sol sol-c-sol) := (ssigma-fast-solution-approx R (t - c) s)} ; with c

       ;; is this one an exact solution?
       (when c-sol ; with c in solution
	 {sol-c-sol := (cons c sol-c-sol)} ; we add c to the solution of t - c to get the solution c + (t - c) = t
	 (return #t
		 (append sol-c-sol
			 s)))

       {(c-not-sol sol-c-not-sol) := (ssigma-fast-solution-approx R t s)} ; without c
       
       ;; is this one an exact solution?
       (when c-not-sol ; without c in solution
	 (return #t
		 (append sol-c-not-sol
			 s)))
       

       ;; no exact solution , we take the best approximation, first in distance, and if distance equal in length of list
       {best-approx := (best-sol t
				 sol-c-not-sol
				 sol-c-sol)}
       
       (return #f
	       (append best-approx
		       s)))
       
       

  {(sf sol) := (ssigma-fast-solution-approx L t '())}
  
  (if sf 
      (display "exact solution found")
      (display "no exact solution found"))

  (newline)
  (display sol)
  (newline))
 
) ; end module

