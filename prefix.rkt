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


(module prefix racket/base


  (provide prefix?)

  (require Scheme+/operators)

  ;; (prefix? (syntax->list #'(2 * (2 + 1) - 1)))
  ;; prefix? : expr =(.#<syntax 2> .#<syntax *> .#<syntax (2 + 1)> .#<syntax -> .#<syntax 1>)
  ;; prefix? : rv=#f
  ;; #f

  ;; (prefix? (syntax->list #'($bracket-apply$ #(1 2 3) 1)))
  ;; prefix? : expr =(.#<syntax $bracket-apply$> .#<syntax #(1 2 3)> .#<syntax 1>)
  ;; prefix? : rv=#t
  ;; #t
  (define (prefix? expr)

    ;;(display "prefix? : expr =")(display expr)(newline)

    (define rv
      (cond ((not (list? expr))
	     #t)
	    
	    ((null? expr)
	     #t)
	    
	    ((and (syntax? (car expr)) ; ex (+ 1 2 3)
		  (operator-syntax? (car expr)))
	     #t)

	    ((and (syntax? (cadr expr)) ; ex: (1 + 2 + 3)
		  (operator-syntax? (cadr expr))) ; forbid operator in prefix expression at this position argument
	     ;; it is not true, to pass operator you will have to hide them in a variable
	     #f)

	    (else (or (operator? (car expr))
		      (not (operator? (cadr expr)))))))

    ;;(display "prefix? : rv=")(display rv)(newline)
    rv)



  ) ; end module


   
