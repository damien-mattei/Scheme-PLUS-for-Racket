;; This file is part of Scheme+

;; Copyright 2024-2025 Damien MATTEI

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


;; code from Scheme+R6RS

  
(module operators-list racket/base

	(provide definition-operator
		 assignment-operator
                 
		 exponentiation-operator		 
		 infix-operators-lst-for-parser
		 
		 definition-operator-syntax
		 ;;single-variable-assignment-operator-syntax
		 assignment-operator-syntax
                 
		 exponentiation-operator-syntax
		 arithmetic-operator-syntax	 
		 infix-operators-lst-for-parser-syntax
		 
		 operators-lst
		 operators-lst-syntax
		 arithmetic-operator-lst-syntax
		 strict-precedence-over-minus
		 in/equalities-operator-syntax)

	(require srfi/1 ; take-while
		 Scheme+/syntax
		 Scheme+/map-nested)

	

;;  from syntax we create the quoted operators symbols with syntax->datum, but we could have done the reverse (with datum->syntax)
	

	

(define definition-operator-syntax (list #'<+ #'+>
					 #'⥆ #'⥅
					 #':+ #'+:
					 ))



(define definition-operator (map syntax->datum definition-operator-syntax))

;; ???????? a quoi ça devait servir ?
;;(define single-variable-assignment-operator-syntax (list #'<- #'->))


(define assignment-operator-syntax ;;(append single-variable-assignment-operator-syntax
					   (list #'<- #'->
					         #'← #'→
					         #':= #'=: 
					         #'<v #'v>
					         #'⇜ #'⇝));)
  
(define assignment-operator (map syntax->datum assignment-operator-syntax))


(define exponentiation-operator-syntax (list #'expt #'**))


(define exponentiation-operator (map syntax->datum exponentiation-operator-syntax))





;; precedence lists


(define calculus-operator-syntax (list (list #'* #'/ #'% #'·)
				       ;;(list #'·) ; symbolic logic And
				       (list #'⊕) ; symbolic Xor
				       (list #'- #'+)
				       (list #'<< #'>>)
				       (list #'&)
				       (list #'^) ; bitwise xor
				       (list #'∣) ; it is not the keyboard character because reserved in Racket !
				       ))

(define in/equalities-operator-syntax (list #'< #'> #'= #'≠ #'<= #'>= #'<> #'≤ #'≥ #'equal?))

(define arithmetic-operator-syntax (append (list exponentiation-operator-syntax)
					   calculus-operator-syntax
					   (list in/equalities-operator-syntax)
					   (list (list #'and))
					   (list (list #'or))))


(define infix-operators-lst-for-parser-syntax

  `(,(list #'∘)
    ,@arithmetic-operator-syntax
    ,(list #'~>) ; for Qi
    ,assignment-operator-syntax
    ,definition-operator-syntax))


(define infix-operators-lst-for-parser (map-nested syntax->datum infix-operators-lst-for-parser-syntax))



;; liste à plate des operateurs

(define arithmetic-operator-lst-syntax
  (apply append arithmetic-operator-syntax))


(define operators-lst
  (apply append infix-operators-lst-for-parser))




(define operators-lst-syntax
  (apply append infix-operators-lst-for-parser-syntax))

(define strict-precedence-over-minus (take-while (lambda (x) (not (datum=? x #'-)))
						 operators-lst-syntax))


;; (define infix-operators-lst-for-parser

;;   (list

;;    (list '∘) ; composition operator (for functions) Option + k  on MacOS
    
;;    exponentiation-operator
   
;;    (list '* '/ '% '·) ;; for:a ² + 2 · a · b + b ²      AltGr + Maj + 1 --> · on french keyboard ,Option + h on MacOS

;;    ;;(list '·) ; symbolic logic And   

;;    (list '⊕) ; symbolic logic Xor
 
;;    (list '- '+) ; note + could be used as symbolic logic Or, for this reason and operator precedence i put And and Xor above
   
;;    (list '<< '>>)
   
;;    (list '&)
;;    (list '^)
;;    (list '∣)
   
;;    (list '< '> '= '≠ '<= '>= '<> 'equal?)

;;    (list 'and)
   
;;    (list 'or)
    
;;    (append assignment-operator 
;; 	   definition-operator)
     
;;    )
  
;;   )

) ; end module



;; > {1 & 1 ^ 0}


;; (^ (& 1 1) 0)
;; 1


;; > {1 & 1 ^ 1}


;; (^ (& 1 1) 1)
;; 0


;; > {1 & 1 ^ 1 ∣ 1}


;; (∣ (^ (& 1 1) 1) 1)
;; 1

