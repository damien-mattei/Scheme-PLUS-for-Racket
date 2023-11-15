;;#lang racket

;; Scheme+.rkt 

;; version 7.2

;; author: Damien MATTEI

;; location: France

;; Racket Scheme version

;; Copyright 2021-2023 Damien MATTEI

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

;; (require "Scheme+.rkt")




(module Scheme+ racket
	
	(module test racket/base) ;; dummy


	(provide def
		 $bracket-apply$
		 $
		 for
		 for-racket
		 for-basic
		 reversed
		 <- ←
		 -> →
		 <+ ⥆
		 +> ⥅
		 declare
		 $>
		 $+>
		 condx
		 <> ≠
		 **
		 <v v>
		 ⇜ ⇝
		 if repeat do when unless
		 %
		 << >>
		 & ∣
		 $nfx$ !*prec
		 
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

		 infix-operators-lst
		 set-infix-operators-lst!
		 replace-operator!

		 )


	;;  U+2223 ∣ DIVIDES (&mid;, &shortmid;, &smid;, &VerticalBar;) see: https://en.wikipedia.org/wiki/Vertical_bar because vertical line is reserved in Racket
	;; do not type the ∣ with keyboard it is not the same and will give an 'undefined symbol' error ,also vertical_line disturb highlight syntaxing in emacs

	;; conflict solving with -> and some Racket syntax:
	;; https://docs.racket-lang.org/reference/function-contracts.html#%28form._%28%28lib._racket%2Fcontract%2Fbase..rkt%29._-~3e%29%29

	
	(require srfi/31) ;; for 'rec in def.scm
	(require srfi/69) ;; Basic hash tables

	(require (rename-in flomat (repeat repeat-flomat)
			    (shape shape-flomat)
			    (transpose transpose-flomat)))

	(require srfi/25) ;; Multi-dimensional Array Primitives
	(require srfi/8) ;; Values , receive

	(require (only-in racket/base [for for-racket])) ;; backup original Racket 'for'

	(require (for-syntax r6rs/private/base-for-syntax)) ;; for macro syntax (for ... : stxparam.rkt identifier-syntax: undefined

	(require "src/infix-operators.rkt")

	(require "src/overload.rkt")
	(require "src/array.rkt")

	(include "src/def.scm")

	(include "src/declare.scm")
	(include "src/condx.scm")
	(include "src/block.scm")
	(include "src/not-equal.scm")
	(include "src/exponential.scm")
	
	(include "src/while-do-when-unless.scm")
	(include "src/repeat-until.scm")
	(include "src/bitwise.rkt")
	(include "src/modulo.scm")

	(include "src/slice.scm")

	;;(include "set-values-plus.scm") ;; useless in racket i suppose because set!-values already exist

	(include "src/increment.scm")
	(include "src/for_next_step.scm")

	(include "src/scheme-infix.rkt")

	(include "src/assignment.rkt")
	(include "src/apply-square-brackets.rkt")

)
