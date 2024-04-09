;;#lang racket

;; Scheme+.rkt 

;; version 8.2

;; author: Damien MATTEI

;; location: France

;; Racket Scheme version

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


;; use :

;; this file must now be included in your main project file like this:
;; at the beginning of your main file add
;; for infix operator precedence:
;; (define-namespace-anchor ankh)
;; (define bsns (namespace-anchor->namespace ankh))
;; (current-namespace bsns)

;; (require "main.rkt")
;; previous name was: (require "Scheme+.rkt")




(module Scheme+ racket
	
	(module test racket/base) ;; dummy


	(provide def
		 ;;$bracket-apply$
		 $bracket-apply$next
		 : ;;$
		 for
		 for-racket
		 for-basic
		 for-basic/break
		 reversed
		 <- ← :=
		 -> ;; can conflict with typed racket
		 →  ;; can conflict with typed racket
		 <+ ⥆ :+
		 +> ⥅
		 declare
		 $>
		 $+>
		 condx
		 <> ≠
		 **
		 <v v>
		 ⇜ ⇝
		 if repeat do when unless while
		
		 %
		 << >>
		 & ∣

		 ;;$nfx$ !*prec
		 
		 ;$ovrld-ht$
		 
		 define-overload-procedure
		 overload-procedure
		 
		 define-overload-existing-procedure
		 overload-existing-procedure
		 
		 define-overload-operator
		 overload-operator
		 
		 define-overload-existing-operator
		 overload-existing-operator
		 
		 define-overload-n-arity-operator
		 overload-n-arity-operator
		 
		 define-overload-existing-n-arity-operator
		 overload-existing-n-arity-operator

		 
		 ;$ovrld-square-brackets-lst$
		 
		 overload-square-brackets
		 ;;find-getter-and-setter-for-overloaded-square-brackets
		 find-getter-for-overloaded-square-brackets
		 find-setter-for-overloaded-square-brackets

		 ;;infix-operators-lst
		 ;;set-infix-operators-lst!
		 ;;replace-operator!

		 ;; for debug:
		 ;;assignment-argument-0
		 ;;return ;; should not work, for debug of Typed Racket
		 )


	;;  U+2223 ∣ DIVIDES (&mid;, &shortmid;, &smid;, &VerticalBar;) see: https://en.wikipedia.org/wiki/Vertical_bar because vertical line is reserved in Racket
	;; do not type the ∣ with keyboard it is not the same and will give an 'undefined symbol' error ,also vertical_line disturb highlight syntaxing in emacs

	;; conflict solving with -> and some Racket syntax:
	;; https://docs.racket-lang.org/reference/function-contracts.html#%28form._%28%28lib._racket%2Fcontract%2Fbase..rkt%29._-~3e%29%29

	
	(require srfi/31) ;; for 'rec in def.scm
	(require srfi/69) ;; Basic hash tables

	(require (rename-in flomat
			    (repeat repeat-flomat)
			    (shape shape-flomat)
			    (transpose transpose-flomat)))

	(require srfi/25) ;; Multi-dimensional Array Primitives
	(require srfi/8) ;; Values , receive

	(require (only-in racket/base [for for-racket] ;; backup original Racket 'for'
			              [do do-scheme])) ;; backup original Scheme 'do'

	(require (for-syntax r6rs/private/base-for-syntax)) ;; for macro syntax (for ... : stxparam.rkt identifier-syntax: undefined

	;(require "infix-operators.rkt")

	(require "overload.rkt")
	(require "array.rkt")

	(include "def.scm")

	(include "declare.scm")
	(include "condx.scm")
	(include "block.scm")
	(include "not-equal.scm")
	(include "exponential.scm")

	(include "when-unless.rkt")
	(include "while-do.scm")
	
	(include "repeat-until.scm")
	(include "bitwise.rkt")
	(include "modulo.scm")

	(include "slice.scm")

        (include "set-values-plus.scm") ;; in racket set!-values already exist but i add another one to deal with other types using <- instead of set!

	(include "increment.scm")
	(include "for_next_step.scm")

	;;(include "scheme-infix.rkt")

	(include "assignment.rkt")
	(include "apply-square-brackets.rkt")

)
