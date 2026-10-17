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

;; version 18

;; use: (require Scheme+)

;; in case of rare bug (only when modifying the language itself, so not for user but developers of Scheme+) like 'undefined something'
;; which is defined and provided remove all the 'compiled' directories from Scheme+ and restart Racket
;; this is not a Scheme+ bug but a Racket bug !


(module Scheme+ racket/base

  (provide

   
   ;; definition and block

   def def+ return return-rec
   define
   define+
   lambda+
   defun
   rec+
   
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
   $bracket-apply-vector$
   $bracket-apply-string$
   $bracket-apply-hash-table$
   $bracket-apply-array$
   $bracket-apply-flomat$
   $bracket-apply-overloaded$ ; written by the type annotation pass for square brackets overloaded with overload-square-brackets
   <- ->
   <-vector-vector*
   <-string-string*
   ← →
   :=  =:
   :=> ; the walrus operator : assigns and returns the value
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
   ;;unless
   while


   
   ;; operators
   <> ≠ fx≠ fl≠
   ≤ ≥
   **
   %
   << >>
   &
   ∣ ; warning: this pipe could be a special character (different from keyboard stroke)
   ^ ; bitwise xor
   · ; AltGr + Maj (CAPS) + 1 --> · on french keyboard ,Option + h on MacOS
   × ; alt + ;  on PC french keyboard
   ~ ; bitwise not
   
   ;; functions
   √
   ∛
   && ; procedure and
   ∣∣ ; procedure or
   int

   ;; postfix operators
   !
   
   
   ;; overloading
   ;define-overload-procedure ; replaced by overload-procedure
   ;define-overload-procedure-annot
   overload-procedure
   require-overload-procedure
   
   ;define-overload-existing-procedure ; replaced by overload-procedure
   ;define-overload-existing-procedure-annot
   ;overload-existing-procedure
   
   define-overload-operator
   ;define-overload-operator-annot
   overload-operator
   
   define-overload-existing-operator
   define-overload-existing-operator-annot
   require-overload-operator
   overload-unary-operator
   require-overload-unary-operator
   define-overloaded-operator
   overload-existing-operator
   
   define-overload-n-ary-associative-operator
   ;define-overload-n-ary-associative-operator-annot
   overload-n-ary-operator
   
   define-overload-existing-n-ary-associative-operator
   define-overload-existing-n-ary-associative-operator-annot
   overload-existing-n-ary-operator
   
   overload-square-brackets
   require-overload-square-brackets ; for the type annotation pass only

   find-getter-for-overloaded-square-brackets
   find-setter-for-overloaded-square-brackets

   ;;!*-generic-infix-parser
   !*prec-generic-infix-parser-runtime

   infix-operators-lst-for-parser

   ;; annotation
   declare-postfix-operator
   declare-prefix-operator

   ;; type annotation
   type ; this is in fact a dummy macro in case some type annotation has been forgotten by the type annotation parser, so that the code can still be compiled and run, but with a warning message

   ; backup operators
   orig-*
   orig--
   orig-·
   orig-×
   orig-/
   orig-**
   orig-expt
   orig-+
   orig-%
   orig-<
   orig->
   orig-<=
   orig->=
   orig-=
   orig-≠
   orig-<>

   ; flonum operators
   flabs ; the numeric procedures specialized by the type annotation
   flmin
   flmax
   flsin
   flcos
   fltan
   flatan
   flexp
   flfloor
   flceiling
   flround
   fltruncate
   fl+
   fl-
   fl*
   fl/
   fl<
   fl>
   fl<=
   fl>=
   fl=

   ; fixnum operators
   fx+
   fx-
   fx*
   fxquotient
   fxmodulo
   fxnot ; ~ of a fixnum (type annotation)
   fxremainder ; the numeric procedures specialized by the type annotation
   fxabs
   fxmin
   fxmax
   fx->fl
   fx<
   fx>
   fx<=
   fx>=
   fx=
   
   $ovrld-ht$ ; for debug only
   $ovrld-ht-n-ary$ ; for debug only
   $ovrld-ht-proc$ ; for debug only

   ANNOTATION-ON
	 ANNOTATION-OFF
   :print-type) ; the type known by the annotation , at the REPL only


  

  (require

   ;;(only-in racket/base [define define-scheme])
   
   ;; definition and block
   Scheme+/def
   Scheme+/defvar
   Scheme+/def+
   Scheme+/def-nfx ; infix define
   Scheme+/declare
   Scheme+/block
   Scheme+/return
   Scheme+/lambda+
   Scheme+/defun
   Scheme+/rec+
   
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
   ;;Scheme+/when-unless
   Scheme+/while-do
   Scheme+/repeat-until

   ;; operators WARNING : see also backup operators
   Scheme+/not-equal
   Scheme+/exponentiation
   Scheme+/bitwise
   Scheme+/modulo
   Scheme+/multiply
   Scheme+/little-greater-or-equal
   Scheme+/conjunction
   Scheme+/disjunction

   ;; functions
   Scheme+/square-and-cubic-root
   Scheme+/int

   ;; overloading
   Scheme+/overload
   
   Scheme+/infix-with-precedence-to-prefix
   Scheme+/operators-list

   ;(for-template Scheme+/operators-list)

   ;; postfix operators
   Scheme+/factorial
   (only-in Scheme+/parser-declarations declare-postfix-operator declare-prefix-operator) ; declare-executable and declare-data are internal (written by the reader)

   ;; type annotation
   Scheme+/type

   setup/getinfo ; for parsing info.rkt

   ; backup operators
   (only-in Scheme+/multiply (· orig-·)
	                     (× orig-×)) ; TODO in annot+

   (only-in Scheme+/exponentiation (** orig-**))

   (only-in Scheme+/not-equal (≠ orig-≠)
	                      (<> orig-<>))

   (only-in racket/base (* orig-*)
            (- orig--)
	    (+ orig-+)
	    (/ orig-/)
            (expt orig-expt)
	    (< orig-<)
	    (> orig->)
	    (<= orig-<=)
	    (>= orig->=)
	    (= orig-=))

   racket/flonum ; real
   racket/fixnum ; integer

   ) ; end import



  (define info-getter (get-info '("Scheme+")))
  (define version (cond ((procedure? info-getter) (info-getter 'version))
			                  (else info-getter)))

  (define verbose #f) ; otherwise make garbage in curly-infix2prefix4racket
  
  (when verbose
    (newline)
    (display (string-append "Scheme+ v" version " by Damien Mattei.")) (newline)
    (newline))


  ) ; end library


;; note: under Linux and on a PC (french) keyboard superscript characters
;; can be generated with the keystroke sequence: ^ n where n is  a number or sign
;; under MacOS the keystroke sequence is : Command Shift + n (you really need to type + key for superscript)

;; Using the text editor vim, one can produce subscripted and superscripted numbers by using the digraphs control-k-ns for subscription and control-k-nS for superscription, where n is an Arabic numeral.


