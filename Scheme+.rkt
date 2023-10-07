;;#lang racket

;; Scheme+.rkt 

;; version 7.1

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

;; (include "Scheme+.rkt")


;; (module Scheme+ racket
	
;; 	(module test racket/base) ;; dummy

;; 	(provide define-overload-existing-operator define-overload-n-arity-operator define-overload-operator define-overload-existing-procedure define-overload-procedure $ovrld-ht$ overload def $bracket-apply$ <- ← -> → <+ ⥆ +> ⥅ declare $> $+> condx <> ≠ ** <v v> ⇜ ⇝ if repeat do when unless   % << >> % & $ ∣ ) ;;$nfx$ !*prec set-infix-operators! update-operators infix-operators-lst)


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


(include "def.scm")

(include "array.scm")

(include "declare.scm")
(include "condx.scm")
(include "block.scm")
(include "not-equal.scm")
(include "exponential.scm")
	
(include "while-do-when-unless.scm")
(include "repeat-until.scm")
(include "bitwise.rkt")
(include "modulo.scm")

(include "slice.scm")

;;(include "set-values-plus.scm") ;; useless in racket i suppose because set!-values already exist

(include "increment.scm")
(include "for_next_step.scm")




;; include from your main file afer overloadind declarations when any with :

;;(include "scheme-infix.rkt")

;;(include "assignment.rkt")
;;(include "apply-square-brackets.rkt")

