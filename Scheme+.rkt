#lang racket

;; Scheme+.rkt

;; version 1.0

;; author: Damien MATTEI

;; location: France

;; date: 2022

;; Guile Scheme version

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
;; (require "Scheme+.rkt")


(provide def $bracket-apply$ <- ← -> → <+ ⥆ +> ⥅ declare $ & condx <> ≠)


(include "def.scm")
(include "array.scm")
(require "apply-square-brackets.rkt")
(require "assignment.rkt")
(include "declare.scm")
(include "condx.scm")
(include "block.scm")
(include "not-equal.scm")

