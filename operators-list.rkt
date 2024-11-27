;; This file is part of Scheme+

;; Copyright 2024 Damien MATTEI

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
		 exponential-operator
		 
		 infix-operators-lst-for-parser
		 
		 definition-operator-syntax
		 assignment-operator-syntax
		 exponential-operator-syntax
		 
		 infix-operators-lst-for-parser-syntax
		 
		 operators-lst
		 operators-lst-syntax)



(define definition-operator (list '<+ '+>
				  '⥆ '⥅
				  ':+ '+:
				  ))

(define assignment-operator (list '<- '->
				  '← '→
				  ':=  '=:
				  '<v 'v>
				  '⇜ '⇝))

(define exponential-operator (list 'expt '**))


(define infix-operators-lst-for-parser

  (list
    
   exponential-operator
   
   (list '* '/ '%)

   (list '·) ; symbolic logic And

   (list '⊕) ; symbolic logic Xor
 
   (list '+ '-) ; note + could be used as symbolic logic Or, for this reason and operator precedence i put And and Xor above
   
   (list '<< '>>)
   
   (list '&)
   (list '^)
   (list '∣)
   
   (list '< '> '= '≠ '<= '>= '<> 'equal?)

   (list 'and)
   
   (list 'or)
    
   (append assignment-operator 
	   definition-operator)
     
   )
  
  )



  
(define definition-operator-syntax (list #'<+ #'+>
					 #'⥆ #'⥅
					 #':+ #'+:
					 ))

(define assignment-operator-syntax (list #'<- #'->
					 #'← #'→
					 #':= '=:
					 #'<v #'v>
					 #'⇜ #'⇝))


(define exponential-operator-syntax (list #'expt #'**))



(define infix-operators-lst-for-parser-syntax

  (list
    exponential-operator-syntax
    (list #'* #'/ #'%)
    (list #'·) ; symbolic logic And
    (list #'⊕) ; symbolic logic Xor
    (list #'+ #'-)
	
    (list #'<< #'>>)

    (list #'& #'∣)

    (list #'< #'> #'= #'≠ #'<= #'>= #'<> #'equal?)

    (list #'and)

    (list #'or)

    assignment-operator-syntax
    definition-operator-syntax 
    )

  )



;; liste à plate des operateurs
(define operators-lst
  (apply append infix-operators-lst-for-parser))

(define operators-lst-syntax
  (apply append infix-operators-lst-for-parser-syntax))



) ; end module
