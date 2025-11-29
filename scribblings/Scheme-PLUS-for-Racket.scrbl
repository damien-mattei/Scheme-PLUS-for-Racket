#lang scribble/manual
@(require scribble/core)
@(require scribble-code-examples)
@require[scriblib/footnote]

@centerline{@image[#:scale 1.0 "Scheme+.png"]}


	   
@title[#:style '(toc)]{Scheme+ for Racket}

@author[(author+email "Damien MATTEI" "Damien.MATTEI@univ-cotedazur.fr")]



@defmodule[SRFI-105 #:reader]{
This reader package provides the SRFI-105 Curly Infix reader/parser.

Source code: @url["https://github.com/damien-mattei/SRFI-105-for-Racket"]

Package: @url["https://pkgs.racket-lang.org/package/SRFI-105-for-Racket"]

@hyperlink["https://srfi.schemers.org/srfi-105/srfi-105.html"]{Scheme Request For Implementations 105}

}



@defmodule[Scheme+]{
This package provides the Scheme+ language definitions.

Source code: @url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket"]

Package: @url["https://pkgs.racket-lang.org/package/Scheme-PLUS-for-Racket"]
}

@defmodule[SRFI-110 #:reader]{
This other reader package provides also the Curly Infix reader/parser as it is also part of SRFI-110 specifications and implementation.

Source code: @url["https://github.com/damien-mattei/SRFI-110-for-Racket"]

Package: @url["https://pkgs.racket-lang.org/package/SRFI-110-for-Racket"]

@hyperlink["https://srfi.schemers.org/srfi-110/srfi-110.html"]{Scheme Request For Implementations 110}

}

Scheme+ is an extension of the syntax of the Scheme language.

Scheme+ adds to Scheme a way to use also infix notation with a compatibility near 100% and not as a sub-system of Lisp syntax as it is often done but with a complete integration in the Scheme reader/parser system and also for Racket at REPL (Read Eval Print Loop).

Scheme+ is to Scheme what a concept-car is to automobile.Scheme+ is a concept-language.It is ideally what should be a modern Scheme.
As Scheme+ syntax is compatible with Scheme syntax you can mix the two syntaxes in the same program. 

Scheme+ makes it easy the assignment of Scheme objects in infix (works also in prefix) notation with a few new operators <- (or: ← , :=),⥆ (or <+) for definitions.

Scheme+ makes it easy the access and assignment for arrays,strings,hash tables,etc by allowing the classic square brackets [ ] of other languages.

What Scheme+ is not: Scheme+ is not a DSL (Domain Specific Language) a contrario Scheme+ tends to be the more universal possible and to be applied in a lot of domains.

@table-of-contents[]

@section[#:tag "both"]{Both prefix and infix expressions in the same world}

You can mix infix sub-expressions and prefix sub-expressions in the same expression:


@code-examples[#:lang "reader SRFI-105" #:context #'here]|{
{3 * (- 9 4) + 2}
}|



In Scheme+, in an expression surrounded by @racket[{ }] curly brackets,the sub-expressions using @racket[( )] round brackets can contains prefix or infix expressions.The parser will automatically detect the infix or prefix and parse it accordingly.

@code-examples[#:lang "reader SRFI-105" #:context #'here]|{
{(3 + 1) * (2 * (+ 2 1) - (sin 0.3)) + ((* 2 5) - 5)}
}|

The above expression is automatically parsed and converted in a classic scheme prefix expression before evaluation:
@codeblock|{
(+
 (*
   (+ 3 1)
   (- (* 2 (+ 2 1)) (sin 0.3)))
  (- (* 2 5) 5))
}|

Other examples:


@codeblock|{
(define (fib n)
  (if {n < 2}
      n
      {(fib (n - 1)) + (fib (n - 2))} ))

(fib 7)
13
}|



@code-examples[#:lang "reader SRFI-105" #:context #'here]|{
{(- 7 (3 * (+ 2 4) - 1)) + 3}
}|

The parsing is the result of sometimes up to 3 stages of parsing:
@itemlist[#:style'ordered
          @item{SRFI 105 Curly Infix is an external parser necessary for the curly brackets { } syntax which is not in the base of scheme language.}
          @item{Syntax transformers are sometimes used at a "compile" stage before the code run.}
	  @item{Parsing at runtime is rarely done but can be necessary when the parsed expression or some of her subexpressions remain ambiguous at prior parsing stages about being infix or prefix expressions.}]

Here the parsing process is activated by the encountering of curly parenthesis { } in the expressions (there exist also other ways to force the parsing):

@codeblock|{
(define (line-length x0 y0 x1 y1)
  (√ {(x1 - x0) ² + (y1 - y0) ²}))

{(ksy / (√ 2)) * ((- x) + y)}

(define (norm x y)
  {x ² + y ²})
}|

@section[#:tag "installation"]{Scheme+ Installation and Depandencies}

Scheme+ can be installed via the Racket Package system or downloaded from Github.See the links at the top of this page.
Scheme+ is designed to be used with the package SRFI-105 for Racket which is a curly infix reader also available in the same way described above.
As an alternative to SRFI 105 you can also use SRFI 110.


@section[#:tag "REPL"]{Scheme+ and Curly Infix SRFI-105 REPL (Read Eval Print Loop)}

As Scheme+ is designed to be used with the Curly infix syntax of SRFI-105, the latter one requires an external reader/parser and a specific REPL (Read Eval Print Loop).
The REPL must be loaded in Racket GUI or invoked in the shell if you work in command line mode.
The REPL file can be found in the source code of Scheme+ or SRFI-105 in the @bold{src/} sub-directory, his name is @bold{REPL-Scheme-PLUS.rkt}.

Here is the source code of @bold{REPL-Scheme-PLUS.rkt} which is also a typical example of how to write a simple Scheme+ module or program:
@codeblock|{
#! /usr/bin/env -S racket --load REPL-Scheme-PLUS.rkt --repl
;; the line above is not mandatory,only for launching a script in command line
;; in CLI you must (require Scheme+) manually
;; but if you want to have syntax color in CLI start racket yourself and do:
;; (load "REPL-Scheme-PLUS.rkt")

#lang reader SRFI-105 ; SRFI-105 Curly-infix-expressions

(module repl racket

  (provide (all-defined-out)) 
  (require Scheme+)
  
  ;; put your code here or simply use the REPL

  )
}|


@section[#:tag "hello"]{A simple Hello World application in Scheme+ and Curly Infix}

@codeblock|{
#reader SRFI-105 ; SRFI-105 Curly-infix-expressions
(require Scheme+)
(display "Hello world") (newline)
}|

Another example that really use Curly infix and Scheme+ :

@codeblock|{
#reader SRFI-105
(require Scheme+)
(define-overload-existing-operator +)
(overload-existing-operator + string-append (string? string?))
{"Hello" + " " + "world !"}
}|

Result:
@codeblock|{
"Hello world !"
}|

You can also use SRFI-110 which implements too the curly infix parser:

@codeblock|{
#reader SRFI-110 ; implements too the curly infix parser
(display "Hello world") (newline)
}|


A module example:

@codeblock|{
#lang reader SRFI-105 ; SRFI-105 Curly-infix-expressions

(module hello-world racket

  (provide (all-defined-out)) 
  (require Scheme+)
  
  (display "Hello world") (newline)

  )
}|

note it seems in the latest version of Racket we can replace the first line simply by :
@codeblock|{
#reader SRFI-105
(provide (all-defined-out)) 
(require Scheme+)
(display "Hello world") (newline)
}|

@section[#:tag "syntax"]{Scheme+ Syntax and Conventions}

@subsection[#:tag "curly"]{Curly Infix notation with { }}

In general Scheme+ use the same convention for infix expression than SRFI-105 Curly Infix that is an infix expression is between curly parenthesis { }.
But infix sub-expressions are allowed to be between normal parenthesis ( ) like it is in mathematic notation. Infix or prefix is then autodetected.In case of ambiguities { } force infix mode. Inside curly infix expression surrounded by { } parenthesis associativity and operator precedence are applied.

@codeblock|{
#lang reader SRFI-105
(require Scheme+)
{3 * 5 + 2}
(+ (* 3 5) 2) ; generated code displayed by REPL/parser
17 ; result
}|


@code-examples[#:lang "reader SRFI-105" #:context #'here]|{
{3 * 7 + 2 - 12 / 3 + -2}
}|

In the examples the display of the REPL/parser in prefix will generally be removed from examples for clarity, also the prompt > and the displayed #<eof> (end of file), if any,printed by some ancient versions of the SRFI-105 parser.



@codeblock|{
#lang reader SRFI-105
(require Scheme+)
{3 · 5 + 2 ³}
23
}|


Note that in the above example and in Scheme+ the operator @racket[·] is @racket[*], also note that superscript chars are exposants.

In the following sections i will omit to rewrite the directives about lang or reader and the requirement in each example to keep the code and the web page compact.

An example of inner round brackets inside curly brackets in a full infix expression:

@codeblock|{
(define x {3 * (2 + 4) - 1})
(display x)
17
}|

Example of a mixed infix and prefix expression autodetected:

@code-examples[#:lang "reader SRFI-105" #:context #'here]|{
{3 * (+ 2 4) - 1}
}|

Other examples:

@codeblock|{
(define (fib n)
  (if {n < 2}
      n
      {(fib (n - 1)) + (fib (n - 2))} ))

(fib 7)
13
}|

Curly infix can also be used in some macro definitions:

@codeblock|{
(define-syntax +=
    (syntax-rules ()
      ({var1 _ var2} {var1 := var1 + var2})))

(define-syntax += (syntax-rules () ((_ var1 var2) (:= var1 (+ var1 var2))))) ; parsed result

{x := 3}
{x += 7}
x

10
}|


@codeblock|{
(define-syntax plus
    (syntax-rules ()
      ({var1 _ ...} {var1 + ...})))

(define-syntax plus (syntax-rules () ((_ var1 ...) (+ var1 ...)))) ; parsed result

{2 plus 3}
(plus 2 3) ; parsed result
5

{2 plus 3 plus 4 plus 5 plus 6}
(plus 2 3 4 5 6)  ; parsed result
20
}|



@codeblock|{
{3 ² + 2 · 3 · 5 + 5 ²}
64
}|


@subsection[#:tag "square"]{Square Bracket notation with [ ]}

The square bracket notation can be used with any object that can be indexed or retrieved by a key: it works with vectors,strings of characters, arrays, hash tables,etc.


@defform[#:id \[\] {T[k]}]{
Return value of vector or in general object T indexed by k.
}

@codeblock|{
{#(1 2 3 4)[2]} 
($bracket-apply$ #(1 2 3 4) 2) ; code generated by the parser
3

}|

I will not always display generated code by parser and end of file symbol as only the result is important for the programmer.

@codeblock|{
{#(1 2 3 4)[1]}
2
}|

@codeblock|{
(define T (vector 1 2 3 4 5))
{T[3]}
4
}|

@codeblock|{
{"hello"[4]}
#\o
}|


Negative index, that start from the end of object, can be used as in the Python language:
@codeblock|{
{"hello"[-4]}
#\e
}|


A slice notation with @bold{:} is available.It allows the same features as the slicing of Python language and sometimes more.


@defform[#:id \: {T[begin : end]}]{
Slicing of vectors or strings.
}

@codeblock|{
{"elephant"[2 : 5]}
"eph"
}|

@codeblock|{
{#(1 2 3 4 5)[2 :]}
'#(3 4 5)	
}|

@codeblock|{
{#(1 2 3 4 5 6 7)[2 * 5 - 8 : 3 * 5 - 10]}
'#(3 4 5)
}|

@codeblock|{
(define a 0)
(define b 5)
{"hello world"[a : b]}
"hello"
}|


@defform[#:link-target? #f  #:id \: {T[begin : end : step]}]{
@racket[begin], @racket[end], @racket[step]  are optional, see the Python language documentation for informations and use.
}

@codeblock|{
{#(1 2 3 4 5 6 7 8 9)[3 : 8 : 2]}
'#(4 6 8)
}|

@codeblock|{
{#(1 2 3 4 5 6 7 8 9)[-1 : 5 : -1]}
'#(9 8 7)
}|

@codeblock|{
{#(1 2 3 4 5 6 7 8)[: : 3]}
'#(1 4 7)
}|

@codeblock|{
{#(1 2 3 4 5 6 7 8)[: : -2]}
'#(8 6 4 2)
}|

@codeblock|{
{"abcdefghijklmno"[3 : : 2]}
"dfhjln"
}|

The square bracket notation [ ] works also with multiple dimensions vectors:

@codeblock|{
{M <- #(#(1 2 3)
        #(4 5 6))}
{M[1][2]}
6
}|

You can use also this [ ] notation variant, like matrix notation, and compatible with arrays in case of you use them:

@codeblock|{
{M <- #(#(1 2 3)
        #(4 5 6))}
{M[1 2]}
6
}|

Examples:

@codeblock|{
{ᐁ[i][k] <- y[k] - z[i][k]}
}|

@subsection[#:tag "file_naming"]{File naming conventions}
Generally a Scheme+ program file is named with a + symbol before the normal scheme extension, examples: SssDyna+.scm or SssDyna+.rkt.The Makefile (see section more below) i provide will automatically parse files named with this convention.

@section[#:tag "reference"]{Scheme+ Reference}

This sections describe the macros,operators and procedures that are the core of Scheme+.

From subsections 1 to 6 what are described are almost every time macros.

@subsection[#:tag "declarations"]{Declarations}

@defform[(declare name1 name2 ...)]{
Declare new variables named @racket[name1],@racket[name2],....
}

@codeblock|{
(declare x)
(when (identifier-binding #'x)
    "variable x exists")
"variable x exists"
}|

You can not both declare and define a variable in a program (module):

@codeblock|{
#reader SRFI-105
(provide (all-defined-out)) 
(require Scheme+)
(declare x)
(define x 3)
}|

@elem[#:style (style #f (list (color-property "red")))]{module: identifier already defined in: x}

Note that @racket[declare] is rarely used,instead assignment macros like @racket[<-] are used, but it could be usefull in case a variable used in a block must be used outside the scope of the block too like in this example:

@codeblock|{
(declare x)
{t <- 3}
(when {t > 1}
  {x <- 7})
{x <- x + 1}
x

8 ; result
}|



By default the declared variables are set to @bold{NIL}, that is @bold{'()}

@codeblock|{
(declare x)
x
'()
}|

@subsection[#:tag "definitions"]{Definitions}


@defform[#:link-target? #f #:id <+ {name <+ value}]{
Define new variable @racket[name] and assign value @racket[value].
Mnemonic of <+ is to @italic{add} a new variable in the environment.
}

@codeblock|{
{x <+ 7}
x
7
}|

Note: this is almost never used in Racket,because Scheme+ for Racket has a special feature of @bold{autodetecting} if a variable needs to be defined before assignment and in this case it will @racket[define] it for the programmer in the current block.

There is some alias of @racket[<+] :

@defform[#:id ⥆ {name ⥆ value}]{}

Note: a lot of operators of Scheme+ that works left to right exist in the opposite sense, i will not document them as it is obvious,example:

@defform[#:link-target? #f #:id ⥅ {value ⥅ name}]{}

Also a lot of Scheme+ operator have infix n-arity capability , i will not completely document them at the beginning of this document but in other parts later.

@codeblock|{
{name ⥆ name2 ⥆ 7}
(list name name2)
'(7 7)
}|

@subsection[#:tag "assignment"]{Assignments}


@defform[#:link-target? #f #:id <- {name <- value}]{
Assign to the variable @racket[name] the value @racket[value].
}

@codeblock|{
{x <- 7}
x
7
}|

If the variable was not existing in the current environment it will be automatically declared,this is a special feature of the Scheme+ version for Racket that can rarely be reproduced in other scheme implementations.

There is some alias of @racket[<-] :

@defform[#:link-target? #f #:id := {name := value}]{}

@codeblock|{
{index := index + 1}
}|


@defform[#:id ← {name ← value}]{}

@codeblock|{
{z ← 1.13+1.765i}
}|

@defform[#:id << {(name1 name2 ...) <- values}]{
You can also use the assignment or definition operators for creating tuples (like in Python) of values.
}

@codeblock|{
{(x y z) <- (values 1 2 3)}
(list x y z)
'(1 2 3)
}|



The assignment operator is a macro that works in taking in account the square bracket [ ] notation in the RHS (right hand side) of any expression:

@codeblock|{
(define T (make-vector 5))
{T[3] <- 7}
{T[3]}
7
}|

Remember @racket[:=],for Pascal language fans, is exactly the same as @racket[<-].

@codeblock|{
(require srfi/25)
{a := (make-array (shape 0 5 0 3) 0)}
{a[1 2] := 7}
{a[1 2]}
7
}|

Other example:

@codeblock|{
{M_i_o[j (i + 1)]  <-  M_i_o[j (i + 1)] + η · z_input[i] · მzⳆმz̃(z_output[j] z̃_output[j]) · ᐁ_i_o[j]}
}|

Note in the example above the special neoteric expression available by SRFI-105 Curly Infix: @bold{მzⳆმz̃(z_output[j] z̃_output[j])} which is automatically parsed and transformed in the prefix expression : @racket[(მzⳆმz̃ z_output[j] z̃_output[j])], note also that @racket[მzⳆმz̃] is only a symbol.

Assignment and slicing works as in Python language or even better,allowing more possibilities.

Examples:

@codeblock|{
{v <+ (vector 1 2 3 4 5 6 7 8 9)}
'#(1 2 3 4 5 6 7 8 9)
{v[: : 2] <- (vector -1 -2 -3 -4 -5)}
v
'#(-1 2 -2 4 -3 6 -4 8 -5)
}|

Note: remember @racket[<+] is for new definitions but in Racket i could have directly use @racket[<-].

@codeblock|{
{v <+ (vector 1 2 3 4 5 6 7 8 9)}
{v[: : -2] := (vector -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13)[: : 2]}
v
'#(-9 2 -7 4 -5 6 -3 8 -1)
}|

Remember @racket[:=] is exactly the same as @racket[<-].

@codeblock|{
{v <+ (vector 1 2 3 4 5 6 7 8 9)}
{v[: : -2] <- "abcdefghijklmnop"[: : 2]}
v
'#(#\i 2 #\g 4 #\e 6 #\c 8 #\a)
}|

@codeblock|{
{v <+ (vector 1 2 3 4)}
{v[1 : 3] <- "abcdef"[2 : 4]}
v
'#(1 #\c #\d 4)
}|

@codeblock|{
{v <+ (vector 1 2 3 4 5 6 7 8)}
{v[3 : : 2] := (vector -1 -2 -3)}
v
'#(1 2 3 -1 5 -2 7 -3)
}|

@codeblock|{
{v <+ (vector 1 2 3 4 5 6 7 8)}
{v[5 : : -2] <- (vector -1 -2 -3)}
v
'#(1 -3 3 -2 5 -1 7 8)
}|

@codeblock|{
{v <- (vector 1 2 3 4 5 6 7 8 9)}
{v[7 : 2 : -2] <- (vector -1 -2 -3)}
v
'#(1 2 3 -3 5 -2 7 -1 9)
}|



@subsection[#:tag "define"]{Procedure definitions}

@defform[(def (name args ...) body ...+)]{
Define a procedure (also named function).
This does the same as the classic @racket[define] but allows the program to @racket[return] from the body of definitions at any moment.
This works the same as in other languages such as C or Python languages.
}

@defform[(return value ...)]{
Return immediately from the executing procedure defined with @racket[def] and return the values: @racket[value ...] . 
As the ellipsis indicate it, you can return multiple values.
}

Example:

@codeblock|{
(def (foo L)
   (when (null? L)
      (return "empty list"))
   (cons "hello list :" L))

(foo '())
"empty list"
}|

Note: @racket[return] can only be used inside specific macros :

	 
@codeblock|{
(return 7)
}|
@elem[#:style (style #f (list (color-property "red")))]{return: can only be used inside def,def+ and lambda+}

Another example:

@codeblock|{
;; this procedure check that we have a canonical infix expression
;; i call 'canonical' an expression such as 3 * 5 + 2
;; in contrary an equivalent expression such as this one: - - 3 * 5 + 2 is not 'canonical',etc
;; conditions to be 'canonical' will be to have :
;; * at least 3 terms in expression
;; * odd number of terms
;; * operators between terms
(def (infix-canonical? L)

	  (define lgt (length L))
	  (when (or (< lgt 3)
		    (not (odd? lgt)))
	    (return #f))
	    
	  (def (check-operators? L2)
	    (when (null? L2)
	      (return #t))
	    (if (not (operator-syntax? (car L2)))
		#f
		(check-operators? (cddr L2))))
		
	  (check-operators? (cdr L)))

(infix-canonical? '(2 * 3 4))
#f

(infix-canonical? '(2 * 3 + 4))
#t
}|

@defform[(return-rec value ...)]{
@racket[return-rec] will return from all the previous recursive calls of the executing procedure (if it was a recursive function).
}


@defform[#:link-target? #f (def name args args-optional ...)]{Define @racket[name] with @racket[(args args-optional ...)] forming an infix expression.}

@codeblock|{
(def t  3 * (+ 2 4) - 1)
t
17

(def z  (3 + 1) * (2 * (+ 2 1) - (sin 0.3)) + ((* 2 5) - 5))
z
27.817919173354642
}|

@defform[#:link-target? #f (def name)]{Like @racket[declare].}

@defform[(def+ (name args ...) body ...+)]{Same as @racket[def] but all @racket[body ...] will be parsed as possibly infix or still prefix.}

Examples:

@codeblock|{
(def+ (g x y)
  (abs (sin (√ (x ² + y ²)))))
}|

@codeblock|{
;; draw a vector point
(def+ (draw-vect-point dc z-vect point-brush)
  (send dc set-pen no-pen)
  (send dc set-brush point-brush) ; blue-brush)
  ;; size of the ellipse / point in pixels
  {ga ← 3} ; grand axe / big axis
  {pa ← 3} ; petit axe / little axis
  {x ← z-vect[0]}
  {y ← z-vect[1]}
  ;;(display "before (z ← x + i * y)") (newline)
  (z ← x + i * y) ; complex number
  ;;(display "after (z ← x + i * y)") (newline)
  ((x y) ← (to-screen-multi-values z))
  {x ← x - (quotient ga 2)}
  (y ← y - (quotient pa 2))
  (send dc draw-ellipse x y ga pa))


;; convert in screen coordinates
(def+ (to-screen-multi-values z0) ; z0 is a complex number
  {re ← (real-part z0)}
  {im ← (imag-part z0)}
  {xs ← re * unit-axis-in-pixel}
  {ys ← im * unit-axis-in-pixel}
  (values (round (xo + xs))
	  (round (yo - ys))))
}|



@codeblock|{
;; get a normalized scalar between [0,1] and return the values of red, green and blue of the color in the long rainbow
(def+ (scalar-to-long-rainbow-rgb s)
  {a := (1 - s) / 0.2} ; invert and group
  {x := (inexact->exact (floor a))} ; this is the integer part
  {y := (inexact->exact (floor (255 * (a - x))))} ; fractional part from 0 to 255
  (case x
    ((0) (values 255 y 0))
    ((1) (values (255 - y) 255 0))
    ((2) (values 0 255 y))
    ((3) (values 0 (255 - y) 255))
    ((4) (values y 0 255))
    ((5) (values 255 0 255))
    (else
     (display "s=") (display s) (newline)
     (display "a=") (display a) (newline)
     (display "x=") (display x) (newline)
     (error "else in scalar-to-long-rainbow-rgb"))))
}|


@codeblock|{
(def+ (chaos p q d x0 y0)
  
  (def a   2 * (cos (2 * pi * p / q)))
  (def+ ksx  (√ ((2 + a) / 2)) )
  {ksy := (√ ((2 - a) / 2))} 
  
  (stream-map (lambda (z)
                (match-let (((vector x y) z))
                  (vector ((ksx / (√ 2)) * (x + y))
			  {(ksy / (√ 2)) * ((- x) + y)}))) ; here { } could be replaced by ( )
                  (stream-iterate (lambda (z)
                                    (match-let (((vector x y) z))
                                      (vector
				       ((a * x) + y + (d * x) / (add1 (x ²))) ; infix left to right evaluation avoid extra parenthesis but is hard for humans
				       (- x))))
				  (vector x0 y0))))
}|

@racket[define], which has been extended, and @racket[define+] respectively work as @racket[def] and @racket[def+] but without the @racket[return] and @racket[return-rec] features. They could be usefull as their implementation result in faster runtime execution.


@defform*[((lambda+ (args ...) body ...+)
(lambda+ (args . L) body ...+)
(lambda+ L body ...+))]{Same as @racket[lambda] but all @racket[body ...] will be parsed as possibly infix or still prefix.Only @racket[return] is allowed in @racket[lambda+].}

@codeblock|{
(define x 3)
(define foo (rec bar (lambda+ ()
                     (when (< x 5)
                       (set! x (+ x 1))
                       (display "super!")(newline)
                       (bar))
                     (display "returning") (newline)
                     (return 17)
                     'not_good)))

(foo)
super!
super!
returning
returning
returning
17
}|

@defform[(rec+ name expr)]{@racket[rec] allowing infix expression and using internally @racket[lambda+]}.
@codeblock|{
(define f (rec+ foo (lambda () (2 + 3))))
(f)
5
}|

@subsection[#:tag "control"]{Control flow : Conditionals and Loops}

@defform[#:literals (then else)
         (if test [then] then-body ... [else] else-body ...)]{
Evalutes @racket[test] and, if @racket[test] is @racket[True], evaluates
@racket[then-body ...], otherwise it evaluates @racket[else-body ...]. Note that multiple
expressions can be placed in each block in the syntax; you can remove the @racket[then] or/and @racket[else] block delimiters as long as there is no ambiguities with the syntax you expressed,so you can fall back to the classic scheme @racket[if] form and it is backward compatible with the classic scheme syntax.
}

@codeblock|{
(if #t then "True")
"True"
}|

@codeblock|{
(if #f then
      "You"
      "should"
      "not"
      "be"
      "there."
    else
      "I"
      "am"
      "here")
"here"
}|


Example from real code:

@codeblock|{
(if (singleton-set? sos) then

      ;; singleton
      (reverse acc)

   else
      
         ;; at least 2 elements in set of sets
         {mt-set1 <- (car sos)} ;; minterm set 1
	 {mt-set2 <- (cadr sos)} ;; minterm set 2
	 {mt-set2-to-mt-setn <- (cdr sos)} ;; minterm sets 2 to n
	 {weight-mt-set1 <- (floor-bin-minterm-weight (car mt-set1))} ;; in a set all minterms have same weight
	 {weight-mt-set2 <- (floor-bin-minterm-weight (car mt-set2))}
	 {delta-weight <- {weight-mt-set2 - weight-mt-set1}}

	 (if {delta-weight = 1} then ;; if minterms set are neighbours

	     ;; unify neighbours minterms sets
	     {unified-mt-set1-and-mt-set2 <- (funct-unify-minterms-set-1-unit-threads mt-set1 mt-set2)} 		
	     (if (null? unified-mt-set1-and-mt-set2)
		 (funct-unify-minterms-set-of-sets-rec-tail mt-set2-to-mt-setn acc) ;; the result will be the continuation with sets from 2 to n
		 (funct-unify-minterms-set-of-sets-rec-tail mt-set2-to-mt-setn (insert unified-mt-set1-and-mt-set2 acc)))
		 
	  else
	     
	     (funct-unify-minterms-set-of-sets-rec-tail mt-set2-to-mt-setn acc))) ;; continue with sets from 2 to n
}|

@defform*[ #:link-target? #f #:id if  #:literals (else)
({statement if test [else statement2]}
 (statement if test [else statement2]))]{

An infix @racket[if] inspired from Python language.}

Examples:

@codeblock|{
{"Hello world" if #t}

"Hello world"
}|

@codeblock|{
{"test positif" if ((#f or #f) and #t) else "test negatif"}

(if (and (or #f #f) #t) "test positif" "test negatif") ; result of parsing

"test negatif"
}|

@codeblock|{
(def+ (foo)
    ("test positif" if ((#f or #f) and #t) else "test negatif"))

(foo)
"test negatif"
}|



@defform[(when test body ...)]{}
@defform[(unless test body ...)]{
Scheme+ implementation provides both @racket[when] and @racket[unless] forms that allow declarations and statements in their body. The @racket[when] and @racket[unless] forms already exist in Racket implementation. Refer to the Racket documentation about them.
}


@defform[#:literals (exec) (condx (test body ...)
		    	   	  (exec body ...)
			   	  ...
				  (else body ...))]{
A @racket[cond] that allow execution of code or even definitions between the test clauses.
}

@codeblock|{
(define x 1)
(condx ((= x 7) 'never)
        (exec
          (define y 3)
          (set! x 7))
        ((= y 1) 'definitely_not)
        (exec
          (set! y 10)
          (define z 2))
        ((= x 7) (+ x y z))
        (else 'you_should_not_be_here))
 19
}|

Note that scheme @racket[cond] and scheme+ @racket[condx] are a style of programming, you can use other style with same result, i think of using @racket[def] and @racket[return] with @racket[when] and @racket[if] that can replace them.


Example from real code of the subset sum problem from the file SssRec+.rkt in examples directory :

@codeblock|{
{c <- (first L)}
{R <- (rest L)}
(condx [ {c = t} (return #t
	       		 (cons c s)) ] ;; c is the solution
	      
       [ {c > t} (ssigma-solution-exact R t s) ] ;; c is to big to be a solution
	      
       ;; c < t at this point
       ;; c is part of the solution	 
       [ exec {(c-sol sol) <- (ssigma-solution-exact R (t - c) s)} ]
	      
       [ c-sol (return c-sol
		       (append (cons c s) ; c is in the solution for t, we insert it in the previous partial solution
			       sol)) ] ; we append with sol to get also the solutions of t - c,resulting in solutions of c + (t - c) = t

       ;; or c is not part of solution
       ;; or the two ! perheaps there exists 2 solutions using or not using c ! (in this case we got only the first found!)
       [ else
	  (ssigma-solution-exact R t s) ] )
}|

Note about the example above which mix @racket[condx] and @racket[return] because it is part of a procedure not displayed here, i encourage the reader to see the full source code SssRec+.rkt to understand it better.

@defform[#:literals (until) (repeat body ... until test)]{Like in Pascal or Lua: loop over @racket[body ...] until @racket[test] is true.}

@codeblock|{
{i := 5}
(repeat
     (display i)
     (newline)
     {i := i - 1}
   until {i < 0})

5
4
3
2
1
0
}|

Note: above i used @racket[:=] which is exactly the same as @racket[<-] or @racket[←].

@defform[(while test  body ...)]{Initially the syntax should be as @racket[while] ... @racket[do] from Pascal but not possible as @racket[do] already exist in scheme.}

@defform[(for (initialization condition updation) body ...)]{Syntax of @racket[for] ... as it exists in C/C++,Javascript,Java language.}

@codeblock|{
(for ({i := 0} {i < 3} {i := i + 1})
    (display i)
    (newline))
0
1
2
}|

Other examples:
@codeblock|{
(for ({i <- 0} {i < n - 2} {i <- i + 1}) 
    ;; create an array with 1 in front for the bias coefficient
    {z_1 <- #(1) + z[i]} ; + operator has been overloaded to append scheme vectors
    {z̃[i + 1] <- M[i] · z_1} ; z̃ = matrix * vector , return a vector
    {z[i + 1] <- vector-map(activation_function_hidden_layer z̃[i + 1])})
}|

Example from matrix multiplication, compute element i,j of result:

@codeblock|{
(for ({k <- 0} {k < p1} {k <- k + 1})
    {sum <- sum + M1[i][k] * M2[k][j]})
}|

Note about Scheme+ for Racket: the @racket[for] described above replace the original @racket[for] of Racket, you should back up it as below before using that. Also Scheme+ define @racket[in-range] and @racket[reversed] as in Python language but they already exist in Racket Scheme.

@codeblock|{
(require (only-in racket/base [for for-racket])) ;; backup original Racket 'for'
(for-racket ([i (reverse (range 1 i_output_layer))])
				{nc <- vector-length(z[i])}
				{ns <- vector-length(z[i + 1])}
				(for-racket ([j (range nc)])
					{ᐁ[i][j] <- (for/sum ([k (range ns)])
							     (მzⳆმz̃(z[i + 1][k] z̃[i + 1][k]) · M[i][k (j + 1)] · ᐁ[i + 1][k]))})
				; modification des poids de la matrice de transition de la couche i-1 à i
         			{modification_des_poids(M[i - 1] ηₛ  z[i - 1] z[i] z̃[i] ᐁ[i] მzⳆმz̃)})
}|

@defform[(in-range start stop step)]{As @racket[range] in Racket or Python}
@defform[(reversed in-range-expression)]{As @racket[reverse] in Scheme or @racket[reversed] in Python}

@subsection[#:tag "blocks"]{Blocks}

@defform[($> body ...)]{@racket[$>] is as @racket[begin].
@codeblock|{
(if (compare-minterm-and-implicant {iepi[lin 0]}
				   {iepi[0 col]})
		;; then
		($>
		  (incf cpt-mt)
		  (when (= 1 cpt-mt)
			{lin-pos-epi ← lin}) ;; position of essential prime implicant
		  {iepi[lin col] ← 1})

		;; else
		{iepi[lin col] ← 3})
}|
}

@defform[($+> body ...)]{@racket[$+>] will allow also definitions in the @racket[body], the @racket[+] means you can add definitions in the environment.It is defined this way:
@codeblock|{
(define-syntax $+>
  (syntax-rules ()
    ((_ ev)  (let () ev)) ;;  there can be a <+ in it expanding with a 'define'
    ((_ ev ...) (let () ev ...))))
}|

Example:
@codeblock|{
;; PHASE 0 : eliminate equivalence
;; a <=> b ----> (a => b) and (b => a)
(define (elim-equivalence expr)
  (cond
   ((symbol? expr) expr)
   ((boolean? expr) expr)
   ((isNOT? expr) `(not ,(elim-equivalence (arg expr))))
   ((isIMPLIC? expr) `(=> ,(elim-equivalence (arg1 expr)) ,(elim-equivalence (arg2 expr))))
   ((isEQUIV? expr) ($+> ;; a <=> b ----> (a => b) and (b => a)
		      {a <+ (arg1 expr)} ; definitions, as : (define a (arg1 expr))
		      {b <- (arg2 expr)} ; note: in Racket i could use also <- which auto-define the variable if necessary :
		      {ae <- (elim-equivalence a)}
		      {be <- (elim-equivalence b)}
		      `(and (=> ,ae ,be) (=> ,be ,ae))))
   (else `(,(operator expr) ,(elim-equivalence (arg1 expr)) ,(elim-equivalence (arg2 expr))))))
}|
}

@subsection[#:tag "procedures"]{Operators and Procedures}

@defform[#:id \@ {n \@ m}]{Modulo.Same as @racket[(modulo n m)].}

@defform[#:id ** {n ** m}]{Exponentiation.Same as @racket[expt].}

Note: exponentiation can also be written with superscript numbers and expressions,see after in this document.

@defform[(√ n)]{Square root.Same as @racket[sqrt].}

@codeblock|{
(√ 2)
1.4142135623730951
}|

@defform[(∛ n)]{Cubic root.}

@defform[#:id << {n << m}]{Left shift.Same as @racket[(arithmetic-shift n m)].}

@defform[#:id >> {n >> m}]{Right shift.Same as @racket[(arithmetic-shift n (- m))].}

@defform[#:id & {n & m}]{Bitwise And}

@defform[#:id ∣ {n ∣ m}]{Bitwise Or.Note: this is U+2223  because vertical line is reserved in Racket.}

@defform[#:id ^ {n ^ m}]{Bitwise Exclusive Or}

@defform[(~ n)]{Bitwise Not}

@defform[#:id && {expr1 && expr2}]{This is a procedural @racket[and] intended to be used both internally,inside Scheme+ source code,and externally in a Scheme+ program when parsing is required at runtime where we can not use the macro @racket[and] and thus we will use the @racket[&&] procedure.Unless your expression will be parsed at runtime do not use it and prefer it @racket[and].}

@defform[#:id ∣∣ {expr1 ∣∣ expr2}]{This is a procedural @racket[or].See explanations above.See also note about vertical line reserved in Racket above.}

As being procedurals @racket[&&] and @racket[∣∣] are not short-circuited.

@defform[#:id ≤ {n ≤ m}]{
@codeblock|{
{3 ≤ 4 < 5}
#t
}|

}

@defform[#:id ≥ {n ≥ m}]{
@codeblock|{
;; we plot only in the window
(when {x0 ≥ 0 and x0 ≤ xws  and x1 ≥ 0 and x1 ≤ xws and
       y0 ≥ 0 and y0 ≤ ywsp and y1 ≥ 0 and y1 ≤ ywsp}
    (send dc draw-line
	  x0 y0
	  x1 y1))
}|
}

@defform[#:id ≠ {n ≠ m}]{The same is defined @racket[<>].}

@defform[#:id · {n · m}]{Same as @racket[*]}

Note: some Qi operators not documented here (see Qi documentation for them) are recognized, for example @racket[~>] should immediately work in infix:

@codeblock|{
;; displayTrajectory3D : parse the Bepi Colombo spacecraft trajectory file text and display it in 3D
#lang reader SRFI-105

;; (displayTrajectory3D  "/opt/homebrew/var/www/drive/BepiColombo-Mio_MSO-orbit_1min_short.txt" "/opt/homebrew/var/www/drive/BepiColombo-Mio_MSO-FlyBy_1min_short.txt")

(module displayTrajectory3D racket
	
	(require Scheme+)	
	(require xml
		 (except-in 2htdp/batch-io xexpr?)) ; for: read-lines
	(require plot)
	(require racket/pretty) ; pretty print
	(require qi qi/list) ; Qi flow language

	(provide displayTrajectory3D)

	(define (displayTrajectory3D src src1)

	  ;; check we have a .txt file
	  {ext <- src[-4 :]} ; try to get the .txt extension

	  (when (not (equal? ext ".txt"))
		(error "Not a text file."))

	  ;; read all lines
	  {input-lines <- (read-lines src)}
	  {vl <- (list->vector input-lines)}

	  {input-lines1 <- (read-lines src1)}
	  {vl1 <- (list->vector input-lines1)}

	  ;; find the basename
	  {basename <- src[: -4]} ; skip the 4 char of extension

	  ;; find the spatial_unit if exist
	  {spatial_unit <- (regexp-match #rx"km" vl[4])}
	  (when (not spatial_unit)
		{spatial_unit <- (regexp-match #rx"Rm" vl[4])})

	  (when (not spatial_unit)
		(error "No spatial unit (km or Rm) found."))

	  {spatial_unit <- (first spatial_unit)}

	  {tdl <- vl[5 :]} ; TABLEDATA lines, skip the header to go to table data lines
	  {tdl1 <- vl1[5 :]}
	  
	  ;; for points3d we must have a list of vectors of x,y,z
	  (define Lplot1  (for/list ([tr tdl1])
				    (list->vector (map string->number (rest (string-split tr))))))

	  (define-qi-foreign-syntaxes $bracket-apply$)

	  {Lplot := (src) ~> read-lines ~> list->vector ~> _[5 :] ~> vector->list ~> (△ (string-split ~> rest ~> (△ string->number) ~> vector)) ~> ▽}
	  
	  (plot3d (list (points3d Lplot
				  #:sym 'dot
				  #:color "blue")

			(points3d Lplot1			  
				  #:sym 'dot
				  #:color "red")

			(polar3d (λ (θ ϕ) 1) #:color "gray"))

		  #:title "BepiColombo FlyBy (red) over Mercury planet and Injection (blue)"
		  #:x-min -6	 	 	 	 
		  #:x-max 6	 	 	 	 
		  #:y-min -6	 	 	 	 
		  #:y-max 6	 	 	 	 
		  #:z-min -6	 	 	 	 
		  #:z-max 6))) ; end module
}|

@centerline{@image[#:scale 1.1 "BepiColombo-Mercury-Planet.png"]}


@defform[(int x)]{Return the integer part of a number.

@codeblock|{
(int 2.5)
2

(int -3.7)
-3
}|

}


@subsection[#:tag "superscript"]{Superscript}
Superscript characters can be used to form numbers, variable or expressions forming an exponent,thus defining an exponentiation without the need of @racket[**] operator:
@codeblock|{
{2 ³}
(** 2 3) ; parsed expression displayed
8
}|

@codeblock|{
(define n 3)
{3 ⁻²·⁽ⁿ⁻⁴⁾}
9
}|

Note that alphabetic superscript characters and numeric superscript characters can display at different level of altitude in the browser font system, this is not the case in Emacs editor.
Note also that not all alphabetic characters currently exists in superscript, here is the source code definitions:

@codeblock|{
(define superscript-string "⁻⁺⁰¹²³⁴⁵⁶⁷⁸⁹") ; super script numbers and signs
(define superscript-only-string (string-append superscript-string ; numbers and signs
					  "ᴬᵃᴮᵇᶜᴰᵈᴱᵉᶠᴳᵍᴴʰᴵᶦᴶʲᴷᵏᴸˡᴹᵐᴺⁿᴼᵒᴾᵖᴿʳˢᵀᵗᵁᵘⱽᵛᵂʷˣʸᶻ" ; note all letters are not available (ex:C,Q,q...)
					  "⁽⁾"
					  "⸱" ; will be used as decimal separator
					  ;; "*" ; multiplication is ambiguous (both super and normal script)
					  ;; "·" ; another multiplication symbol (but ambiguous too)
					  "⸍"; division
					  ))
}|

Note under Linux and on a PC (french) keyboard superscript characters can be generated with the keystroke sequence: ^ n where n is  a number or sign.(but this not works for alpha char as it only put some accent on the alpha char)

Superscript also works to write complex numbers exponents:

@codeblock|{
{3 ⁻²⁺³ᶦ}
(** 3 (+ (- 2) 0+3i)) ; code generated by parser
-0.10979199190468787-0.01707036982454185i
}|

Note that unfortunately for superscript complex number the lower case superscript @racket[i] does not exist and @racket[I] is not really a mathematical convention for complex number but you can use the true electrical convention that use the symbol @racket[j]:

@codeblock|{
{2 ⁻⁵⁺⁷ʲ}
(** 2 (+ (- 5) 0+7i)) ; code generated by parser
0.004349621840375144-0.03094581215359732i
}|

A last example:
@codeblock|{
{2-3i ⁻⁵⁺⁷ʲ}
(** 2-3i (+ (- 5) 0+7i)) ; code generated by parser
0.38835198150723493+1.5475077026949722i
}|

Not all fonts support superscript characters, i use Monospace Bold with Emacs.

@subsection[#:tag "overload"]{Overloading procedures and operators}

Overloading must be set at a module toplevel, it will not works in a procedure,and there is no need of that.
You can @racket[provide] an overloaded procedure or operator from the module where it is defined and overloaded to other modules.

Different macros and procedures are provided for overloading because of the various type of operator or procedure, operator can be only binary or be n-arity,operators have associativity properties that do not have procedures.Also operators or procedure can preexist in Scheme or you can overload one of your new procedure or operator.All that requires different macros and procedures for overloading that can not be fusioned in a generic macro.

Overloading of operator or procedure is made in two steps:
@itemlist[#:style'ordered
          @item{first we define that a procedure or operator will be overloaded, this will result in creating a new procedure or operator that will be specialized in the next step.}
	  @item{second we overload a procedure or operator by providing a specialized function and some predicates that will be used to test the arguments at runtime.}]

With this schema we can have a same function (procedure or operator) that will works on different arguments.

Note that we can also extend square bracket [ ] to use different type of object, the same way it is done in the Python language for the angle bracket.This is done by overloading the underlaying procedure that do the bracket apply and the assignment macros,we will see that at the end of this section.

@defform[(define-overload-procedure name)]{Define an overloaded procedure named @racket[name].}

@codeblock|{
(define-overload-procedure area)
}|

@defform[(overload-procedure name special (predicate ...))]{Overload a procedure named @racket[name] with a specialized procedure named @racket[special] which use parameters that pass the @racket[predicates] test.}

@codeblock|{
(define (area-square x) (* x x))
(overload-procedure area area-square (number?))

(define (area-rect x y) (* x y))
(overload-procedure area area-rect (number? number?))

(area 3)
9

(area 2 3)
6
}|

Another example:

@codeblock|{
(define-overload-procedure uniform)
;; return a number in ]-1,1[
;; the dummy parameter is needed by a flomat procedure
(define (uniform-dummy dummy) {-1 + (random) * 2})
; return a random number between [inf, sup]
(define (uniform-interval inf sup)
  {gap <- sup - inf}
  {inf + gap * (random)})
(overload-procedure uniform uniform-dummy (number?))
(overload-procedure uniform uniform-interval (number? number?))
}|

@defform[(define-overload-existing-procedure name)]{Define an overloaded existing procedure named @racket[name].}
@codeblock|{
(define-overload-existing-procedure length)
}|


@defform[(overload-existing-procedure name special (predicate ...))]{Overload an existing procedure named @racket[name] with a specialized procedure named @racket[special] which use parameters that pass the @racket[predicates] test.}


@codeblock|{
(overload-existing-procedure length vector-length (vector?))
(overload-existing-procedure length string-length (string?))

(length #(1 2 3 4))
4

(length '(1 2 3))
3

(length "abcde")
5
}|

Note that when no defined predicates matches the arguments, in the above example nor @racket[vector?] nor @racket[string?] will return true for a @racket[list] then the overloading schema fall back to the original procedure definition that is here @racket[length] that works with @racket[list].



@defform[(define-overload-operator name)]{Define an overloaded operator named @racket[name].}

@defform[(overload-operator name special (predicate ...))]{Overload an operator named @racket[name] with a specialized operator named @racket[special] which use parameters that pass the @racket[predicates] test.}

@defform[(define-overload-existing-operator name)]{Overload an already existing operator named @racket[name] with a specialized operator named @racket[special] which use parameters that pass the @racket[predicates] test.}

@codeblock|{
(define-overload-existing-operator +)
}|

@defform[(overload-existing-operator name special (predicate ...))]{Overload an already existing operator named @racket[name] with a specialized operator named @racket[special] which use parameters that pass the @racket[predicates] test.}

@codeblock|{
(overload-existing-operator + vector-append (vector? vector?))
}|

Now the @racket[+] append vectors like in Python language:

@codeblock|{
{#(1 2 3) + #(4 5 6 7)}
'#(1 2 3 4 5 6 7)

{(vector 1 2 3) + (vector 4 5 6 7)}
'#(1 2 3 4 5 6 7)

;; create an array with 1 in front for the bias coefficient
{z_1 <- #(1) + z[i]} ; + operator has been overloaded to append scheme vectors
}|


@defform[(define-overload-n-arity-operator name)]{Define an overloaded n-arity operator named @racket[name].}

@defform[(overload-n-arity-operator name special (predicate ...))]{Overload an n-arity operator named @racket[name] with a specialized operator named @racket[special] which use parameters that pass the @racket[predicates] test.}

@defform[(define-overload-existing-n-arity-operator name)]{Define an overloaded already existing n-arity operator named @racket[name].}

@codeblock|{
(define-overload-existing-n-arity-operator +)
}|

@defform[(overload-existing-n-arity-operator name special (predicate ...))]{Overload an already existing n-arity operator named @racket[name] with a specialized operator named @racket[special] which use parameters that pass the @racket[predicates] test.}

@codeblock|{
(define (add-n-lists . vn-lst)
  {map-args <- (cons + vn-lst)}
  (apply map map-args))

(overload-existing-n-arity-operator + add-n-lists (list? list?))

{'(1 2 3) + '(4 5 6) + '(7 8 9)}
'(12 15 18)

(define-overload-existing-operator *)
(define (mult-num-list k v) (map (λ (x) (* k x)) v))
(overload-existing-operator * mult-num-list (number? list?))
{3 * '(1 2 3) + '(4 5 6) + '(7 8 9)}
'(14 19 24)
}|

@defform[(overload-square-brackets getter setter! (pred-obj pred-coord ...))]{Overload square bracket [ ] for @racket[object] that return true with predicate @racket[pred-obj] with @racket[getter] for read and @racket[setter!] for write at coordinates of type validated by predicates @racket[pred-coord ...].}

Example for accessing and modifying a line of a matrix:

@codeblock|{
(struct matrix-vect (v)) ;; matrix based on vector of vectors

(define (matrix-vect-line-ref M lin)
  {v <- (matrix-vect-v M)}
  {v[lin]})


(define (matrix-vect-line-set! M lin vect-line)
  {v <- (matrix-vect-v M)}
  {v[lin] <- vect-line})
  
(overload-square-brackets matrix-vect-line-ref matrix-vect-line-set! (matrix-vect? number?))

(define Mv (matrix-vect #(#(1 2 3) #(4 5 6))))
{Mv[1]}
'#(4 5 6)

; define getter,setter
(define (matrix-vect-ref M lin col)
  {v <- (matrix-vect-v M)}
  {v[lin][col]})

(define (matrix-vect-set! M lin col x)
  {v <- (matrix-vect-v M)}
  {v[lin][col] <- x})

(overload-square-brackets matrix-vect-ref matrix-vect-set!  (matrix-vect? number? number?))

{Mv[1][0]}
4
}|

See the full example at @url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket/blob/main/examples/racket/matrix-by-vectors+.rkt"]


@section[#:tag "makefile"]{Pragmas}

There is currently no need of pragmas with Scheme+. The very few pragmas that can be used are the few ones of SRFI 105 Curly infix reader described in the doc: @other-doc['(lib "SRFI-105/scribblings/SRFI-105.scrbl")]


@section[#:tag "pragmas"]{Using Makefile and debugging Scheme+ program}

The Curly Infix (SRFI 105) reader parser can now display the line, column, and character offset in the source file if a parsing error arise at this stage.
The Racket GUI can not display the line of error in a Scheme+ program.The reason is because the Scheme+ program is pre-parsed by the SRFI 105 reader and the Racket compiler has not access to the original lines of code and even do not display the error line relative to the resulting parsed line of code.

If you have to debug your source code you must generate a Scheme file
from your Scheme+ file with @bold{curly-infix2prefix4racket} in SRFI 105 package and load the
result file in Racket GUI. Then you will have all the debug
information of Racket on the scheme file.

Another solution is to simply copy/paste the pure Racket
code generated by the SRFI 105 Curly Infix parser and run it like a normal Racket program.
Again you will have all the information about errors with the true line number displayed.

Another method is to use the Makefile provided:

@url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket/blob/main/examples/racket/Makefile"]

Put the Makefile in the same place of your Scheme+ program and type in the terminal @bold{make}.
The Makefile will parse all Scheme+ program in the same directory and generate the scheme version in @bold{MODULE_DIRECTORY=parsed_files_directory}.You can then just load and run the parsed files in Racket GUI to use all the Racket features and tools (macro stepper,etc) for debugging.

Example:

@verbatim|{
mattei@acer:~/Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/examples/chaos$ ls -la
total 28K
drwxrwxr-x 3 mattei mattei 4,0K sept. 21 17:42 .
drwxrwxr-x 7 mattei mattei 4,0K sept. 26 22:06 ..
-rw-r--r-- 1 mattei mattei  12K sept. 21 17:42 chaos+.rkt
-rw-rw-r-- 1 mattei mattei 1,5K nov.  23  2024 Makefile
drwxrwxr-x 2 mattei mattei 4,0K déc.  16  2024 parsed_files_directory
mattei@acer:~/Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/examples/chaos$ make
PARSING chaos+.rkt :
../../../../../SRFI-105-for-Racket/src/curly-infix2prefix4racket.rkt chaos+.rkt > parsed_files_directory/chaos+.rkt

mattei@acer:~/Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/examples/chaos$ ls -la parsed_files_directory/
total 16
drwxrwxr-x 2 mattei mattei 4096 déc.  16  2024 .
drwxrwxr-x 3 mattei mattei 4096 sept. 21 17:42 ..
-rw-r--r-- 1 mattei mattei 6032 sept. 26 22:23 chaos+.rkt
}|

@section[#:tag "example"]{Place to find Scheme+ programs and examples}

More examples can be find in this directory and subdirectories:

@url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket/blob/main/examples/"]

@section[#:tag "emacs"]{Emacs configuration for syntax highligting of Scheme+}

Add the line below in your .emacs configuration file:

@codeblock|{
(font-lock-add-keywords 'scheme-mode
  '(("\\<\\(define\\|define+\\|def\\|def+\\|return\\|return-rec\\|:=\\|<-\\|condx\\|then\\|else\\)\\>" . font-lock-keyword-face)))
}|

If you use @hyperlink["https://www.racket-mode.com/"]{Emacs Racket mode} you can add the same as above too in your .emacs file but then replace 'scheme-mode with 'racket-mode.@hyperlink["https://www.racket-mode.com/"]{Emacs Racket mode} is great for closing the corresponding curly parenthesis.

Not all fonts support superscript characters, Monospace (Bold) with Emacs do it.

@section[#:tag "thoughts"]{Thoughts}

@define-footnote[my-note make-my-note]

I hope this documentation is not a write-only@my-note{ « APL is a write-only language. I can write programs in APL, but I can’t read any of them ». Roy Keir } one and will be usefull for any reader.


@make-my-note[]
