;;#lang racket

;; Scheme+.rkt

;; version 1.2

;; author: Damien MATTEI

;; location: France

;; date: 2022

;; Racket Scheme version

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


;; use :
;; (require Scheme-PLUS-for-Racket/Scheme+)
;; or :
;; (require "Scheme+.rkt")


(module Scheme+ racket
	
	(module test racket/base) ;; dummy

	(provide def $bracket-apply$ <- ← -> → <+ ⥆ +> ⥅ declare $ & condx <> ≠ ** <v v>)


	(include "included-files/def.scm")
	(include "included-files/array.scm")

	(require "required-files/apply-square-brackets.rkt")
	(require "required-files/assignment.rkt")

	(include "included-files/declare.scm")
	(include "included-files/condx.scm")
	(include "included-files/block.scm")
	(include "included-files/not-equal.scm")
	(include "library/exponential.scm")

	) ;; end module


