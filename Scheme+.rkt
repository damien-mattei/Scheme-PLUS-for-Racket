;; This file is part of Scheme+

;; Copyright 2023-2025 Damien MATTEI

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

;; version 10

;; use: (require Scheme+)

;; note : seems (require Scheme+/def-nfx) should be required separately in REPL



(module Scheme+ racket/base

  (provide

   
   ;; definition and block

   def def+ return return-rec
   define
   define+
   
   <+ +>
   ⥆ ⥅
   :+ +:
   declare
   $> ; begin
   $+> ; (let () ...
   begin-def


   
   ;; infix notation,indexing,slicing and assignment

   : ; slice symbol
   $nfx$ ; infix
   $bracket-apply$ ; [ ]
   <- ->
   ← →
   :=  =:
   ;; multiple values assignment
   <v v>
   ⇜ ⇝

   ;; control: if then else
   if

   ;; iteration
   for continue break
   for-basic
   for-next
   for-basic/break
   for-basic/break-cont
   for/break-cont
   for-each-in

   ;; sequence
   ;; DEPRECATED for Racket as already exists 'range'  and 'reversed' is simply 'reverse'
   ;; also 'in-range' in Racket already exists for streams, etc
   ;;in-range
   ;;reversed

   
   ;; control
   condx
   repeat
   do
   unless
   while


   
   ;; operators
   <> ≠
   ≤ ≥
   **
   %
   << >>
   &
   ∣ ; warning: this pipe could be a special character (different from keyboard stroke)
   · ; AltGr + Maj (CAPS) + 1 --> · on french keyboard ,Option + h on MacOS

   ;; functions
   √ ∛
   && ; procedure and

   ;; overloading
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
   
   overload-square-brackets

   find-getter-for-overloaded-square-brackets
   find-setter-for-overloaded-square-brackets

   ;;!*-generic-infix-parser
   !*prec-generic-infix-parser-runtime

   infix-operators-lst-for-parser
   
   
   )


  

  (require

   (only-in racket/base [define define-scheme])
   
   ;; definition and block
   Scheme+/def
   Scheme+/def+
   Scheme+/def-nfx ; infix define
   Scheme+/declare
   Scheme+/block
   Scheme+/return

   
   ;; infix notation,indexing,slicing and assignment
   Scheme+/slice
   Scheme+/bracket-apply
   Scheme+/assignment
   Scheme+/nfx

   ;; control: if then else
   Scheme+/if-then-else

   ;; iteration
   Scheme+/for_next_step
   ;;Scheme+/range

   ;; control
   Scheme+/condx
   Scheme+/when-unless
   Scheme+/while-do
   Scheme+/repeat-until

   ;; operators
   Scheme+/not-equal
   Scheme+/exponential
   Scheme+/bitwise
   Scheme+/modulo
   Scheme+/multiply
   Scheme+/little-greater-or-equal
   Scheme+/conjonction

   ;; functions
   Scheme+/square-and-cubic-root

   ;; overloading
   Scheme+/overload
   
   Scheme+/infix-with-precedence-to-prefix
   Scheme+/operators-list

   setup/getinfo ; for parsing info.rkt


   ) ; end import



  (define info-getter (get-info '("Scheme+")))
  (define version (cond ((procedure? info-getter) (info-getter 'version))
			(else info-getter)))
  
  (newline)
  (display (string-append "Scheme+ v" version " by Damien Mattei")) (newline)
  (newline)


  ) ; end library


;; note: under Linux and on a PC (french) keyboard superscript characters
;; can be generated with the keystroke sequence: ^ n where n is  a number or sign
;; under MacOS the keystroke sequence is : Command Shift + n (you really need to type + key for superscript)

;; Using the text editor vim, one can produce subscripted and superscripted numbers by using the digraphs control-k-ns for subscription and control-k-nS for superscription, where n is an Arabic numeral.
