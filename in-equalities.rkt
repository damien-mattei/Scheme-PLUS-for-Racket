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

(module in-equalities racket/base


	(provide in-equality?
		 multiple-in-equalities?
		 infix->prefix-in-equality
		 infix->prefix-in-equality-runtime
		 in/equalities-state-1)

	(require Scheme+/alternating-parameters
	         ;;SRFI-105/SRFI-105-curly-infix ; for alternating-parameters
		 Scheme+/operators
		 Scheme+/insert
		 Scheme+/defun
		 Scheme+/list
		 Scheme+/conjunction)


	;; this module implements a FSM (Finite State Machine) described in the file in-equality (.odg)


  ;; predicate to test if an infix expression is an in/equality, ex: (x < 3 < y <= z)
  ;; (in-equality? '(2 < 3))
  ;; #t
  ;; (in-equality? '(x < 3 < y <= z))
  ;; #t
  (define (in-equality? s)
    (define l (length s))
    (and (>= l 3) ; an infix expression has at least 3 terms ,example , (a <= b)
	 (odd? l) ; an infix expression has an odd number of terms
	 (andmap IN-EQUALITY-op? (alternating-parameters (cdr s))))) ; (cdr s) = (< 3 < y <= z)

  
  ;; (multiple-in-equalities? '(x < 3 < y <= z))
  ;; #t

  ;; (multiple-in-equalities? '(y <= z))
  ;; #f
  (define (multiple-in-equalities? s)
    (and (in-equality? s)
	 (> (length s) 3)))


  (define (get-in-equalities x)
      (if (null? (cdr x))
	  '()
	  (cons (list (cadr x)
		      (car x)
		      (caddr x))
		(get-in-equalities (cddr x)))))
  
  
  ;; (infix->prefix-in-equality '(x < 3 < y <= z))
  ;; '(and (< x 3) (< 3 y) (<= y z))
  (define (infix->prefix-in-equality x) ; multiple in/equalities in infix expression, ex: (x < 3 < y <= z)

    (cons 'and (get-in-equalities x)))

  (define (infix->prefix-in-equality-runtime x) ; multiple in/equalities in infix expression, ex: (x < 3 < y <= z)

    (cons && (get-in-equalities x)))

  

  ;; we have to deal with something like that
  ;; (1 + d < x - xw < 7 and 0 < y - yw < 5)

  ;; ((1 + d) < x - xw < 7 and 0 < y - yw < 5)

  ;; ((1 + d) < (x - xw) < 7 and 0 < y - yw < 5)

  ;; (((1 + d) < (x - xw) < 7) and 0 < y - yw < 5)


  ;; (a + 1 < b * 3 and 1 + d < x - xw < 7 and 0 < y - yw < 5)
  ;; (a + 1)

 
  ;; start with (in/equalities-state-1 s '() '() '() '())
  (def (in/equalities-state-1 s inner-lst outer-lst result-lst possible-result-lst) ; inner-lst : expression with parenthesis, outer-lst: expression with inner list and operators result-lst : expression without parenthesis
       ;; possible result list is in case no N-arity in/equalities are found

    ;; (display "in/equalities-state-1 s =") (display s)  (newline)
    ;; (display "in/equalities-state-1 inner-lst =") (display inner-lst)  (newline)
    ;; (display "in/equalities-state-1 outer-lst =") (display outer-lst)  (newline)
    ;; (display "in/equalities-state-1 result-lst =") (display result-lst)  (newline)
    ;; (display "in/equalities-state-1 possible-result-lst =") (display possible-result-lst)  (newline)
    ;; (newline)   
   
       
    (when (null? s) ;  empty list case
      (return (append result-lst possible-result-lst))) ; fall back to possible result list as the outer list is no an in/equality
       
    (define f (car s))
    (define r (cdr s))
    ;;(display "in/equalities-state-1 f =") (display f)  (newline)
    
    (cond ((in/equalities-operator? f) ; < , > , <= , >= , =
	   ;;(display "in/equalities-state-1 in/equalities-operator?") (newline)
	   (define inner-expr inner-lst)
	   (when (singleton-list? inner-expr) ; check we have not a singleton in inner list before putting it in outer list
	     (set! inner-expr (car inner-expr))) ; get only the element of the singleton
	   
	   (binary-in/equalities-state-2 r
					 '() ; future inner list for next state		 
					 (insert-tail (insert-tail outer-lst inner-expr) ; put the inner expression inside outer list
						      f) ; put the in/equal operator at tail : ex: ((a + b) < ... to be continued in state 2 ...) , outer list
					 result-lst
					 (insert-tail possible-result-lst f)))

	  ;; and / or
	  ((LOGIC-N-ARY-op? f) (not-N-ary-in/equality r
						      '() ; inner list , drop the results because it is not in/equalities
						      '() ; outer list
						      result-lst
						      (insert-tail possible-result-lst f)))
	  
	  (else ; something to add in the inner list
	   (in/equalities-state-1 r
				  (insert-tail inner-lst f)
				  outer-lst
				  result-lst
				  (insert-tail possible-result-lst f)))))
  
  ;; Not N-ary in/equality is an intermediate state that could have been coded in state 1 and 2
  ;; it is better to create an intermediate state to save code redundancy
  (define (not-N-ary-in/equality s inner-lst outer-lst result-lst possible-result-lst)

    ;; (display "not-N-ary-in/equality s =") (display s) (newline)
    ;; (display "not-N-ary-in/equality inner-lst =") (display inner-lst)  (newline)
    ;; (display "not-N-ary-in/equality outer-lst =") (display outer-lst)  (newline)
    ;; (display "not-N-ary-in/equality result-lst =") (display result-lst)  (newline)
    ;; (display "not-N-ary-in/equality possible-result-lst =") (display possible-result-lst)  (newline)
  
    ;; (newline)

    (in/equalities-state-1 s
			   '() ; inner list
			   '() ; outer list
			   (append result-lst possible-result-lst) ; use possible result list because we did not found an in/equality expression
			   '())) ; continue with an empty possible result list


  
  ;; state 2 binary in/equalities
  (def (binary-in/equalities-state-2 s inner-lst outer-lst result-lst possible-result-lst)

    ;; (display "binary-in/equalities-state-2 s =") (display s)  (newline)
    ;; (display "binary-in/equalities-state-2 inner-lst =") (display inner-lst)  (newline)
    ;; (display "binary-in/equalities-state-2 outer-lst =") (display outer-lst)  (newline)
    ;; (display "binary-in/equalities-state-2 result-lst =") (display result-lst)  (newline)
    ;; (display "binary-in/equalities-state-2 possible-result-lst =") (display possible-result-lst)  (newline)
  
    ;; (newline)
   

    (when (null? s)
      (return (append result-lst possible-result-lst)))
   
    (define f (car s))
    (define r (cdr s))
    
    (cond ((in/equalities-operator? f) ; < , > , <= , >= , =
	   (define inner-expr inner-lst)
	   (when (singleton-list? inner-expr) ; check we have not a singleton in inner list before putting it in outer list
	     (set! inner-expr (car inner-expr))) ; get only the element of the singleton
	   (n-ary-in/equality-state-3 r
				      '() ; inner list
				      (insert-tail (insert-tail outer-lst inner-expr) ; put the inner expression inside outer list
						   f) ; put the in/equal operator at tail : ex: ((a + b) < ... to be continued in state 2 ...) , outer list
				      result-lst
				      (insert-tail possible-result-lst f)))
	  
	  ;; and / or
	  ((LOGIC-N-ARY-op? f) (not-N-ary-in/equality r
						      '() ; inner list , drop the results because it is not in/equalities
						      '() ; outer list
						      result-lst
						      (insert-tail possible-result-lst f)))
	  
	  (else ; something to add in the inner list
	   (binary-in/equalities-state-2 r
					 (insert-tail inner-lst f)
					 outer-lst
					 result-lst
					 (insert-tail possible-result-lst f)))))



  
  ;; state 3 N ary in/equalities
  (def (n-ary-in/equality-state-3 s inner-lst outer-lst result-lst  possible-result-lst)

    ;; (display "n-ary-in/equality-state-3 s =") (display s)  (newline)
    ;; (display "n-ary-in/equality-state-3 inner-lst =") (display inner-lst)  (newline)   
    ;; (display "n-ary-in/equality-state-3 outer-lst =") (display outer-lst)  (newline)
    ;; (newline)
    
    (when (null? s)
      (define inner-expr inner-lst)
      (when (singleton-list? inner-expr) ; check we have not a singleton in inner list before putting it in outer list
	(set! inner-expr (car inner-expr))) ; get only the element of the singleton
      (insert-tail-set! outer-lst inner-expr) ; put the inner expression inside outer list
      ;;(display "n-ary-in/equality-state-3 null? outer-lst =") (display outer-lst)  (newline)

      (define outer-expr outer-lst)
      (when (singleton-list? outer-expr) ; check we have not a singleton in the list before putting it in the list of upper level
	(set! outer-expr (car outer-expr))) ; get only the element of the singleton

      ;;(display "n-ary-in/equality-state-3 null? outer-expr =") (display outer-expr)  (newline)

      (when (null? result-lst) ; avoid having nested lists : '((0 ≤ 4 ≤ 4))
	(return outer-expr))

      (insert-tail-set! result-lst outer-expr) ; put the outer expression inside result list
      (return result-lst))

    (define f (car s))
    (define r (cdr s))
    
    (cond ((in/equalities-operator? f) ; < , > , <= , >= , =
	   (define inner-expr inner-lst)
	   (when (singleton-list? inner-expr) ; check we have not a singleton in inner list before putting it in outer list
	     (set! inner-expr (car inner-expr))) ; get only the element of the singleton
	   (n-ary-in/equality-state-3 r
				      '() ; inner list
				      (insert-tail (insert-tail outer-lst inner-expr) ; put the inner expression inside outer list
						   f) ; put the in/equal operator at tail : ex: ((a + b) < ... to be continued in state 2 ...) , outer list
				      result-lst
				      (insert-tail possible-result-lst f)))

	  ;; and / or
	  ((LOGIC-N-ARY-op? f)
	   
	   ;; things to do with expressions:
	   ;;(display "state-3 outer-lst =") (display outer-lst)  (newline)
	   (define inner-expr inner-lst)
	   (when (singleton-list? inner-expr) ; check we have not a singleton in inner list before putting it in outer list
	     (set! inner-expr (car inner-expr))) ; get only the element of the singleton
	   (insert-tail-set! outer-lst inner-expr) ; put the inner expression inside outer list
   
	   (in/equalities-state-1 r
				  '()
				  '()
				  (insert-tail (insert-tail result-lst outer-lst) ; append  (a < b < c) and ...
					       f) ; put the symbolic logic operator and/or at tail, ex: (a < b < c) and ...
				  '())) ; fixed argument to NIL
	  
	  (else ; something to add in the inner list
	   (n-ary-in/equality-state-3 r
				      (insert-tail inner-lst f)
				      outer-lst
				      result-lst
				      (insert-tail possible-result-lst f)))))

  
  ) ; end module


;; (in/equalities-state-1 '(x + 2 < 3 and a + 1 < 3 * b <= c - 1 < d + 2)  '() '() '()  '())
;; '(x + 2 < 3 and ((a + 1) < (3 * b) <= (c - 1) < (d + 2)))

;; (in/equalities-state-1 '(x + 2 < 3 and a + 1 < 3 * b <= c - 1 < d + 2 or y)  '() '() '()  '())
;; '(x + 2 < 3 and ((a + 1) < (3 * b) <= (c - 1) < (d + 2)) or y)

;; (in/equalities-state-1 '(x + 2 < 3 and a + 1 < 3 * b <= c - 1 < d + 2 or y * 3 > 4)  '() '() '()  '())
;; '(x + 2 < 3 and ((a + 1) < (3 * b) <= (c - 1) < (d + 2)) or y * 3 > 4)

;; (in/equalities-state-1 '(5 < 6 < 7 or y > 4 or 1 < 2 < 3)  '() '() '() '())
;; '((5 < 6 < 7) or y > 4 or (1 < 2 < 3))

;; (in/equalities-state-1 '(x + 2 < 3 and a + 1 < 3 * b <= c - 1 < d + 2 or y * 3 > 4 and f and 1 < 2 < 3)  '() '() '()  '())
;; '(x + 2 < 3 and ((a + 1) < (3 * b) <= (c - 1) < (d + 2)) or y * 3 > 4 and f and (1 < 2 < 3))


;; (in/equalities-state-1 '(f x)  '() '() '() '())
;; '(f x)

;; (in/equalities-state-1 '(f)  '() '() '() '())
;; '(f)

;; (in/equalities-state-1 '(0 ≤ 4 ≤ 4)  '() '() '() '())
;; '(0 ≤ 4 ≤ 4)


;; {0 ≤ 1 ≤ 2 and #f}

;; ($nfx$ 0 ≤ 1 ≤ 2 and #f)
;; #f

;; {#t and (#f or 1 < 2 <= 2)}
;; #t

;; {#t and (#f or 1 < 2 < 2)}
;; #f

;; Warning in/equalities get priority over or/and:

;; > {(or 1 < 2 < 2)}


;; ($nfx$ (or 1 < 2 < 2))
;; #f


;; #<eof>
;; > {(or 1 < 2 <= 2)}


;; ($nfx$ (or 1 < 2 <= 2))
;; #t

;; for normal binary in/equalities this is still good:
;; > {(or 1 < 2)}


;; ($nfx$ (or 1 < 2))
;; 1


;; #<eof>
;; > {(or 1 > 2)}


;; ($nfx$ (or 1 > 2))
;; 1
