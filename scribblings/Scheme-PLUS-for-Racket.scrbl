#lang scribble/manual
@(require scribble/core)
@;(require scribble-code-examples) ; replaced by curly-example below (no evaluation while building the doc)

@; a Scheme+ expression and its result, displayed as an interaction of the REPL (the result is written, not computed)
@(define-syntax-rule (curly-example expr result)
   (nested #:style 'code-inset
           (tabular (list (list (racketinput0 expr))
                          (list (racketresultblock0 result))))))
@(require scriblib/footnote)
@(require racket/runtime-path)
@(require scribble/html-properties racket/file)

@; the superscripts and subscripts of the code in one font (Fira-Mono has no subscript letters : Bₖ₋₁) , the CSS is
@; included in the page
@(define-runtime-path scripts-font-css "scripts-font.css")

@(define-runtime-path Scheme+ "Scheme+.png")
@centerline{@image[#:scale 1.0 Scheme+]}


	   
@title[#:style (make-style #f (list 'toc (css-addition (file->bytes scripts-font-css))))]{Scheme+ for Racket}

@author[(author+email "Damien MATTEI" "Damien. MATTEI@univ-cotedazur.fr")]

@; the lisp logo at the right of the two packages
@(define-runtime-path lisplogo "lisplogo_warning_128.png")
@; a float that takes no width (margin-right = - its width - margin-left) : it goes into the right margin
@para[#:style (make-style #f (list (attributes '((style . "float: right; margin: 2px -72px 0 20px;")))))]{@image[#:scale 0.63 lisplogo]}

@; the two essential packages at the top (as most packages are presented) , the complete descriptions are below
@defmodule[Scheme+ #:no-declare #:link-target? #f]
@defmodule[SRFI-105 #:reader #:no-declare #:link-target? #f]

Scheme+ is Scheme with, in addition, infix expressions (with the precedence of the operators), postfix expressions, the square brackets @litchar{[ ]} of the other languages to access arrays, strings and hash tables, operators of assignment and definition, and an optional type annotation with type inference, which specializes the operators at read time. They can be mixed with the usual Scheme syntax:

@codeblock|{
(define (norm x0 y0 x1 y1)
  (√ {(x1 - x0) ² + (y1 - y0) ²}))  ; prefix and infix mixed , superscript exponents
(norm 1 2 4 6)
5

{v := (make-vector 3 0)}
{v[2] := 3 * (- 9 4) + 2}         ; square brackets , assignment , a prefix expression inside an infix one
v
'#(0 0 17)

{5 !}                             ; postfix
120

(define (σ z̃)                     ; the sigmoid of a neural network
  (type ℝ z̃)                ; type annotation (in a region ANNOTATION-ON) : ℝ is exactly flonum? predicate
  {1.0 / (1.0 + (exp (- z̃)))})    ; rewritten as (fl/ 1.0 (fl+ 1.0 (exp (fl- z̃)))) : the operators of the floats
(σ 2.0)
0.8807970779778823
}|

The type annotation and inference are described in @secref["annotation"].

Scheme+ is an extension of the syntax of the Scheme language.

Scheme+ adds to Scheme a way to use also infix and even postfix notation with a compatibility near 100% and not as a sub-system of Lisp syntax as it is often done but with a complete integration in the Scheme reader/parser system and also for Racket at REPL (Read Eval Print Loop).

Scheme+ is to Scheme what a concept-car is to automobile. Scheme+ is a concept-language. It is ideally what should be a modern Scheme.
As Scheme+ syntax is compatible with Scheme syntax you can mix the two syntaxes in the same program. 

Scheme+ makes the assignment of Scheme objects easy in infix (also in prefix) notation with a few new operators := (or: <- , ←), ⥆ (or <+ , :+) for definitions.

Scheme+ makes the access and the assignment easy for arrays, strings, hash tables, etc by allowing the classic square brackets [ ] of other languages.

What Scheme+ is not: Scheme+ is not a DSL (Domain Specific Language) a contrario Scheme+ tends to be the more universal possible and to be applied in a lot of domains.

Scheme+ is available for different Scheme implementations: Racket, Guile, Kawa... the Racket implementation is for now (2026) the most advanced.

Scheme+ is used with a Curly Infix reader, provided by the following packages:




@defmodule[Scheme+]{
This package provides the Scheme+ language definitions.

Source code: @url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket"]

Package: @url["https://pkgs.racket-lang.org/package/Scheme-PLUS-for-Racket"]
}


@defmodule[SRFI-105 #:reader #:no-declare #:link-target? #f]{
This reader package provides the SRFI-105 Curly Infix reader/parser used with Scheme+.

Source code: @url["https://github.com/damien-mattei/SRFI-105-for-Racket"]

Package: @url["https://pkgs.racket-lang.org/package/SRFI-105-for-Racket"]

@hyperlink["https://srfi.schemers.org/srfi-105/srfi-105.html"]{Scheme Request For Implementations 105}

}

Optional but recommended:

@defmodule[curly-infix #:reader #:no-declare #:link-target? #f]{
curly-infix is an alias more friendly to read and say than the enigmatic "SRFI-105" (that sounds like R2-D2).

Source code: @url["https://github.com/damien-mattei/curly-infix"]

Package: @url["https://pkgs.racket-lang.org/package/curly-infix"]

}

Not required:

@defmodule[SRFI-110 #:reader #:no-declare #:link-target? #f]{
This other reader package provides also the Curly Infix reader/parser as it is also part of SRFI-110 specifications and implementation.

Source code: @url["https://github.com/damien-mattei/SRFI-110-for-Racket"]

Package: @url["https://pkgs.racket-lang.org/package/SRFI-110-for-Racket"]

@hyperlink["https://srfi.schemers.org/srfi-110/srfi-110.html"]{Scheme Request For Implementations 110}

}

@defmodule[infix-sweet #:reader #:no-declare #:link-target? #f]{
infix-sweet is an alias of SRFI-110, as curly-infix is an alias of SRFI-105: @litchar{#lang reader infix-sweet} reads the curly infix expressions of SRFI 105 and the neoteric and sweet expressions of SRFI 110 (see @secref["sweet-expressions"]).

Source code: @url["https://github.com/damien-mattei/infix-sweet"]

Package: @url["https://pkgs.racket-lang.org/package/infix-sweet"]

}


@; the table of contents is announced by its name , to separate it from the presentation and the packages above
@larger{@larger{@bold{Table of Contents}}}

@table-of-contents[]

@section[#:tag "both"]{Prefix, infix and postfix expressions in the same world}

You can mix infix, prefix and postfix sub-expressions in the same expression:


@curly-example[{3 * (- 9 4) + 2} 17]



In Scheme+, in an expression surrounded by @racket[{ }] curly brackets, the sub-expressions using @racket[( )] round brackets can contain prefix, infix or postfix expressions. The parser will automatically detect the infix, prefix or postfix and parse it accordingly.

@curly-example[{(3 + 1) * (2 * (+ 2 1) - (sin 0.3)) + ((* 2 5) - 5)} 27.817919173354642]

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


@curly-example[{(- 7 (3 * (+ 2 4) - 1)) + 3} -7]

The detection is done when the code is read. Besides the procedures of @racket[racket/base], the parser knows the procedures and macros of your program: those defined in the file, those declared with a type of procedure (see @racket[type]) and the overloaded ones. Between @racket[{ }] the definition or the declaration must be written before its use. A @racket[( )] sub-expression inside @racket[{ }] that has the shape of an infix expression, a procedure unknown by the parser (for example imported from a module) followed by an operator and a single argument, is read as infix:

@codeblock|{
(define v (vector 1 2 3))

{w := (vector-map - v)} ; read as (:= w (- vector-map v)) : vector-map is taken as an operand

(type procedure? vector-map)
{w := (vector-map - v)} ; read as (:= w (vector-map - v)) : a call of vector-map
}|

You can also write such a call outside the curly brackets: @racket[(vector-map - v)] alone is a prefix call. @racket[(list - 1)] inside @racket[{ }] stays prefix because @racket[list] is a procedure of Racket, and @racket[(vector-map - v w)] too because it is not an infix expression. See also @secref["scheme-forms"].

Postfix:

@curly-example[{1 2 3 4 +} 10]

A quoted or quasiquoted list is data: it is not parsed, even inside @racket[{ }]. In a quote or a quasiquote only the @racket[{ }] curly brackets are infix, as in SRFI 105: a quoted @racket[{ }] expression is converted in prefix at its top level only, its @racket[( )] sub-lists stay as written whatever the number of operators (@litchar{'{(x + y) * z + 1}} is @racket['(+ (* (x + y) z) 1)]), and an unquoted part written in infix must use curly brackets too, @litchar{,{…}}. For the symbolic computation, the pragma @racket[CARE-OF-QUOTES-OFF] opens a region where the quoted lists are infix expressions at all levels, see @secref["pragmas"].

@codeblock|{
(define y 2)

{d := '(x + y)}            ; a list of 3 symbols
d
'(x + y)

{q := `(x + y ,{y * 3})}
q
'(x + y 6)

{e := '{x + y}}
e
'(+ x y)

{f := '{{B1 · B0} ⊕ {C1 · {B1 ⊕ B0}}}}
f
'(⊕ (· B1 B0) (· C1 (⊕ B1 B0)))
}|

The parsing is the result of sometimes up to 3 stages of parsing:
@itemlist[#:style'ordered
          @item{The modified SRFI 105 Curly Infix reader is an external parser necessary for the curly brackets { } syntax which is not in the base of scheme language and for the indexing [ ] square brackets.}
          @item{Syntax transformers are sometimes used at a "compile" stage before the code run.}
	  @item{Parsing at runtime is rarely done but can be necessary when the parsed expression or some of its subexpressions remain ambiguous at prior parsing stages about being infix or prefix expressions (for performance and optimisation and because of their rarity postfix expressions are never parsed at runtime).}]

Here the parsing process is activated by the encountering of curly parenthesis { } in the expressions (there exist also other ways to force the parsing):

@codeblock|{
(define (line-length x0 y0 x1 y1)
  (√ {(x1 - x0) ² + (y1 - y0) ²}))

{(ksy / (√ 2)) * (- x + y)}

(define c 299792458) ; the speed of light in m/s

; special relativity : a duration Δt of a clock moving at the speed v lasts longer for an observer at rest
(define (dilated-time Δt v)
  {Δt / (√ (1 - v ² / c ²))})
}|

@section[#:tag "installation"]{Scheme+ Installation and Dependencies}

Scheme+ can be installed via the Racket Package system or downloaded from Github. See the links at the top of this page.
Scheme+ is designed to be used with the package curly-infix or with SRFI-105 for Racket which is a curly infix reader also available in the same way described above.
As an alternative to SRFI 105 you can also use SRFI 110, which reads also the sweet-expressions: Scheme+ can then be written with indentation instead of parenthesis, see the documentation of SRFI 110.

@bold{It is better to disable Background Expansion in the Preferences menu of Racket because it could warn for false error if not disabled.}But this should not block the compilation of code.


@section[#:tag "REPL"]{Scheme+ and Curly Infix SRFI-105 REPL (Read Eval Print Loop)}

As Scheme+ is designed to be used with the Curly infix syntax of SRFI-105 or SRFI-110, the latter one requires an external reader/parser and a specific REPL (Read Eval Print Loop).
The REPL must be loaded in Racket GUI or invoked in the shell if you work in command line mode.
The REPL file can be found in the source code of Scheme+ or SRFI-105 in the @bold{src/} sub-directory, its name is @bold{REPL-Scheme-PLUS.rkt}.

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

If you installed Scheme+ with the Package Manager of the DrRacket the place of the above file @bold{REPL-Scheme-PLUS.rkt} depends on your system and scope of installation of @bold{Scheme+}.

For example under Linux and for a user installation of @bold{Scheme+}, @bold{REPL-Scheme-PLUS.rkt} should be relative to your home directory in the @bold{.local/share/racket} subdirectory (@bold{.racket} with the older versions of Racket) followed by a path depending on the version of Racket and finally in @bold{pkgs/Scheme-PLUS-for-Racket/src}.

Example under Linux with a user (mattei) installation of @bold{Scheme+} and the 9.3 version of Racket:
@bold{/home/mattei/.local/share/racket/9.3/pkgs/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt}

You could simply open the above file in @bold{DrRacket} to work with @bold{Scheme+}.

At the REPL, @racket[:print-type] displays the type known by the type annotation for an expression (see @secref["type-form"]).

@section[#:tag "hello"]{A simple Hello World application in Scheme+ and Curly Infix}

@codeblock|{
#reader SRFI-105 ; SRFI-105 Curly-infix-expressions
(require Scheme+)
(display "Hello world") (newline)
}|

Another example that really uses Curly infix syntax with Scheme+ :

@codeblock|{
#reader curly-infix ; alias of SRFI-105
(require Scheme+)
(overload-operator + string-append (string?))
{"Hello" + " " + "world !"}
}|

Result:
@codeblock|{
"Hello world !"
}|

You can also use SRFI-110, which also implements the curly infix parser (in sweet-expressions, write one expression per line):

@codeblock|{
#reader SRFI-110 ; also implements the curly infix parser
(display "Hello world")
(newline)
}|

or, with the directive @litchar{#!curly-infix} of SRFI 110, the rest of the file is read as with SRFI 105 (curly infix), and two forms can be written on the same line:

@codeblock|{
#reader SRFI-110 ; also implements the curly infix parser
#!curly-infix
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
#reader curly-infix
(provide (all-defined-out)) 
(require Scheme+)
(display "Hello world") (newline)
}|

@section[#:tag "syntax"]{Scheme+ Syntax and Conventions}

@subsection[#:tag "curly"]{Curly Infix notation with { }}

In general Scheme+ uses the same convention for infix expressions as SRFI-105 Curly Infix, that is an infix expression is between curly brackets { }.
But infix or postfix sub-expressions are allowed to be between normal parenthesis ( ) as in the mathematical notation. Infix, postfix or prefix is then autodetected. In case of ambiguity { } force infix mode and possibly postfix mode, while ( ) remain prefix mode. Inside curly infix expression surrounded by { } parenthesis associativity and operator precedence are applied.

@codeblock|{
#lang reader SRFI-105
(require Scheme+)
{3 * 5 + 2}
(+ (* 3 5) 2) ; generated code displayed by REPL/parser
17 ; result
}|


@curly-example[{3 * 7 + 2 - 12 / 3 + -2} 17]

In the examples the display of the REPL/parser in prefix will generally be removed from examples for clarity, also the prompt > and the displayed #<eof> (end of file), if any, printed by some ancient versions of the SRFI-105 parser.



@codeblock|{
#lang reader curly-infix
(require Scheme+)
{3 · 5 + 2 ³}
23
}|


Note that in the above example and in Scheme+ the operator @racket[·] is @racket[*], also note that superscript characters are exponents.

In the following sections i will omit to rewrite the directives about @racket[lang] or @racket[reader] and the requirement in each example to keep the code and the web page compact.

An example of inner round brackets inside curly brackets in a full infix expression:

@codeblock|{
(define x {3 * (2 + 4) - 1})
(display x)
17
}|

Example of a mixed infix and prefix expression autodetected:

@curly-example[{3 * (+ 2 4) - 1} 17]

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

The square bracket notation can be used with any object that can be indexed or retrieved by a key: it works with vectors, strings of characters, arrays, hash tables, etc.


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


Negative index, that starts from the end of object, can be used as in the Python language:
@codeblock|{
{"hello"[-4]}
#\e
}|


A slice notation with @bold{:} is available. It allows the same features as the slicing of Python language and sometimes more.


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
@racket[begin], @racket[end], @racket[step]  are optional, see the Python language documentation for information and use.
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
{M := #(#(1 2 3)
        #(4 5 6))}
{M[1][2]}
6
}|

You can use also this [ ] notation variant, like matrix notation, and compatible with arrays in case you use them:

@codeblock|{
{M := #(#(1 2 3)
        #(4 5 6))}
{M[1 2]}
6
}|

Examples:

@codeblock|{
{ᐁ[i][k] := y[k] - z[i][k]}
}|

@subsection[#:tag "file_naming"]{File naming conventions}
Generally a Scheme+ program file is named with a + symbol before the normal scheme extension, examples: SssDyna+.scm or SssDyna+.rkt. The Makefile (see section more below) i provide will automatically parse files named with this convention.

@section[#:tag "reference"]{Scheme+ Reference}

This section describes the macros, operators and procedures that are the core of Scheme+.

What is described in the subsections 1 to 7 is almost always macros.

@subsection[#:tag "declarations"]{Declarations}

@defform[(declare name1 name2 ...)]{
Declare new variables named @racket[name1], @racket[name2],....
}

@codeblock|{
(declare x)
(when (identifier-binding #'x)
    "variable x exists")
"variable x exists"
}|

You can not both declare and define a variable in a program (module):

@codeblock|{
#reader curly-infix
(provide (all-defined-out)) 
(require Scheme+)
(declare x)
(define x 3)
}|

@elem[#:style (style #f (list (color-property "red")))]{module: identifier already defined in: x}

Note that @racket[declare] is rarely used, instead assignment macros like @racket[:=] are used, but it could be useful in case a variable used in a block must be used outside the scope of the block too like in this example:

@codeblock|{
(declare x)
{t := 3}
(when {t > 1}
  {x := 7})
{x := x + 1}
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

Note: this is almost never used in Racket, because Scheme+ for Racket has a special feature of @bold{autodetecting} if a variable needs to be defined before assignment and in this case it will @racket[define] it for the programmer in the current block.

There are some aliases of @racket[<+] :

@defform[#:id ⥆ {name ⥆ value}]{}

@defform[#:id :+ {name :+ value}]{
@racket[:+] begins with a colon like @racket[:=] : @racket[:+] adds (defines) a new variable, @racket[:=] assigns a value to a variable.
}

@codeblock|{
{y :+ 3}         ; definition
{y := y + 1}     ; assignment
y
4
}|

Note: a lot of operators of Scheme+ that work left to right exist in the opposite sense, i will not document them as it is obvious, example:

@defform[#:link-target? #f #:id ⥅ {value ⥅ name}]{}

(or @racket[+>] , @racket[+:])

Also a lot of Scheme+ operators have infix n-arity capability , i will not completely document them at the beginning of this document but in other parts later.

@codeblock|{
{name ⥆ name2 ⥆ 7}
(list name name2)
'(7 7)
}|

@subsection[#:tag "assignment"]{Assignments}


@defform[#:link-target? #f #:id := {name := value}]{
Assign to the variable @racket[name] the value @racket[value].
}

@codeblock|{
{x := 7}
x
7
}|

If the variable did not exist in the current environment it will be automatically declared, this is a special feature of the Scheme+ version for Racket that can rarely be reproduced in other scheme implementations.

In a body, @racket[{x := …}] becomes a @racket[set!] if @racket[x] is defined later in an enclosing body (Racket binds the definitions of a body in the whole body), and the error is then @tt{x: assignment disallowed; cannot assign before initialization}: put the definitions at the beginning of the body, as R7RS requires.

There are some aliases of @racket[:=] :

@defform[#:link-target? #f #:id <- {name <- value}]{}

@codeblock|{
{index <- index + 1}
}|


@defform[#:id ← {name ← value}]{}

@codeblock|{
{z ← 1.13+1.765i}
}|

To assign a variable and return the value inside an expression, as the @tt{:=} of Python, see @secref["walrus"].

@defform[#:id << {(name1 name2 ...) := values}]{
You can also use the assignment or definition operators for creating tuples (like in Python) of values.
}

@codeblock|{
{(x y z) := (values 1 2 3)}
(list x y z)
'(1 2 3)
}|

The values of the right hand side are computed before the assignment, so they can use the old values of the variables, as in this swap:

@codeblock|{
{(a b) := (values 1 2)}
{(a b) := (values b a)}
(list a b)
'(2 1)
}|



The assignment operator is a macro that takes into account the square bracket [ ] notation in the LHS (left hand side) of the assignment:

@codeblock|{
(define T (make-vector 5))
{T[3] := 7}
{T[3]}
7
}|

Remember @racket[:=], for Pascal language fans, is exactly the same as @racket[<-].

@codeblock|{
(require srfi/25)
{a :+ (make-array (shape 0 5 0 3) 0)}
{a[1 2] := 7}
{a[1 2]}
7
}|

Other example:

@codeblock|{
{M_i_o[j (i + 1)]  :=  M_i_o[j (i + 1)] + η · z_input[i] · მzⳆმz̃(z_output[j] z̃_output[j]) · ᐁ_i_o[j]}
}|

Note in the example above the special neoteric expression available by SRFI-105 Curly Infix:@codeblock|{მzⳆმz̃(z_output[j] z̃_output[j])}| which is automatically parsed and transformed in the prefix expression : @codeblock|{(მzⳆმz̃ z_output[j] z̃_output[j])}|, note also that @racket[მzⳆმz̃] is only a symbol.

Assignment and slicing work as in the Python language or even better, allowing more possibilities.

Examples:

@codeblock|{
{v ⥆ (vector 1 2 3 4 5 6 7 8 9)}
'#(1 2 3 4 5 6 7 8 9)
{v[: : 2] := (vector -1 -2 -3 -4 -5)}
v
'#(-1 2 -2 4 -3 6 -4 8 -5)
}|

Note: remember @racket[⥆] (or @racket[<+] , @racket[:+]) is for new definitions but in Racket i could have directly used @racket[:=].

@codeblock|{
{v ⥆ (vector 1 2 3 4 5 6 7 8 9)}
{v[: : -2] := (vector -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13)[: : 2]}
v
'#(-9 2 -7 4 -5 6 -3 8 -1)
}|

@codeblock|{
{v ⥆ (vector 1 2 3 4 5 6 7 8 9)}
{v[: : -2] := "abcdefghijklmnop"[: : 2]}
v
'#(#\i 2 #\g 4 #\e 6 #\c 8 #\a)
}|

@codeblock|{
{v ⥆ (vector 1 2 3 4)}
{v[1 : 3] := "abcdef"[2 : 4]}
v
'#(1 #\c #\d 4)
}|

@codeblock|{
{v ⥆ (vector 1 2 3 4 5 6 7 8)}
{v[3 : : 2] := (vector -1 -2 -3)}
v
'#(1 2 3 -1 5 -2 7 -3)
}|

@codeblock|{
{v ⥆ (vector 1 2 3 4 5 6 7 8)}
{v[5 : : -2] := (vector -1 -2 -3)}
v
'#(1 -3 3 -2 5 -1 7 8)
}|

@codeblock|{
{v :+ (vector 1 2 3 4 5 6 7 8 9)}
{v[7 : 2 : -2] := (vector -1 -2 -3)}
v
'#(1 2 3 -3 5 -2 7 -1 9)
}|



@subsection[#:tag "walrus"]{The walrus operator}

@defform[#:id :=> {name :=> value}]{
The walrus operator: assign to the variable @racket[name] the value @racket[value] and return this value, as the assignment expression @tt{:=} of Python (@tt{if (n := len(a)) > 10:}). The assignment @racket[:=] returns nothing (@tt{#<void>}), @racket[:=>] returns the value assigned, so it can be used inside an expression. The value is computed only once.
}

@codeblock|{
(declare x y)
{y := {x :=> 5} + 1}
(list x y)
'(5 6)

(define (long-list? l)
  (declare n)
  (if {(n :=> (length l)) > 10}
      n
      #f))
}|

The variable must exist before: defined, declared with @racket[declare], or a parameter of the procedure. A new name is refused at expansion time (@tt{:=>: the variable must be declared before}): @racket[:=] would define it, but inside the expression the definition would be local and lost, whereas Python creates the variable in the enclosing scope. As @racket[:=], @racket[:=>] accepts the square brackets and the chained assignments:

@codeblock|{
{T[i] :=> 7}
7
{a :=> b :=> 3}
3
}|

With the type annotation, @tt{{name :=> value}} has the type of @racket[value]: @tt{{y := {x :=> a * 2.0} * 3.0}} with a flonum @racket[a] is read as @racket[(:= y (fl* (:=> x (fl* a 2.0)) 3.0))].



@subsection[#:tag "define"]{Procedure definitions}

@defform[(def (name args ...) body ...+)]{
Define a procedure (also named function).
This does the same as the classic @racket[define] but allows the program to @racket[return] from the body of definitions at any moment.
This works the same as in other languages such as C or Python languages.
}

@defform[(return value ...)]{
Return immediately from the executing procedure defined with @racket[def] or @racket[lambda+] and return the values: @racket[value ...] . 
As the ellipsis indicates, you can return multiple values.
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
@elem[#:style (style #f (list (color-property "red")))]{return: can only be used inside def or lambda+}

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
@racket[return-rec] will return from all the previous recursive calls of the executing procedure (if it was a recursive function). It can be used inside @racket[def].
}

@defform*[((lambda+ (args ...) body ...+)
           (lambda+ (args ... . L) body ...+)
           (lambda+ L body ...+))]{
Like @racket[lambda], but @racket[return] can be used in the @racket[body] (not @racket[return-rec]).
}

@codeblock|{
(define first-negative
  (lambda+ (L)
    (for-each (λ (x) (when {x < 0} (return x))) L)
    #f))

(first-negative '(3 -2 5 -7))
-2
}|

@racket[def] and @racket[lambda+] take a continuation (@racket[call/cc]) only when @racket[return] or @racket[return-rec] is written in their body: otherwise they are exactly @racket[define] and @racket[lambda], at the same speed. With @racket[return], each call costs about 8 to 17 times the call of a procedure, which matters only for a procedure called very often.


@;{ deprecated : (def name args args-optional ...) , infix value converted at the expansion of the macro
@defform[#:link-target? #f (def name args args-optional ...)]{Define @racket[name] with @racket[(args args-optional ...)] forming an infix expression.}

@codeblock|{
(def t  3 * (+ 2 4) - 1)
t
17

(def z  (3 + 1) * (2 * (+ 2 1) - (sin 0.3)) + ((* 2 5) - 5))
z
27.817919173354642
}|
}

@defform[#:link-target? #f (def name)]{Like @racket[declare].}

@;{ deprecated : def+ , define+ , the extended define , lambda-x (lambda+ before) , rec+ : the body (or the value) is converted at the expansion of the macro
@defform[(def+ (name args ...) body ...+)]{Same as @racket[def] but all @racket[body ...] will be parsed as possibly infix, postfix or still prefix.}

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

@racket[define], which has been extended, and @racket[define+] respectively work as @racket[def] and @racket[def+] but without the @racket[return] and @racket[return-rec] features. They could be useful as their implementation result in faster runtime execution.


@defform*[((lambda-x (args ...) body ...+)
(lambda-x (args . L) body ...+)
(lambda-x L body ...+))]{Same as @racket[lambda] but all @racket[body ...] will be parsed as possibly infix, postfix or still prefix. Only @racket[return] is allowed in @racket[lambda-x].}

@codeblock|{
(define x 3)
(define foo (rec bar (lambda-x ()
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

@defform[(rec+ name expr)]{@racket[rec] allowing infix, postfix or prefix expressions and using internally @racket[lambda-x]}.
@codeblock|{
(define f (rec+ foo (lambda () (2 + 3))))
(f)
5
}|
}

@subsection[#:tag "control"]{Control flow : Conditionals and Loops}

@defform[#:literals (then else)
         (if test [then] then-body ... [else] else-body ...)]{
Evaluates @racket[test] and, if @racket[test] is true (not @racket[#f]), evaluates
@racket[then-body ...], otherwise it evaluates @racket[else-body ...]. Note that multiple
expressions can be placed in each block in the syntax; you can remove the @racket[then] or/and @racket[else] block delimiters as long as there is no ambiguity with the syntax you expressed, so you can fall back to the classic scheme @racket[if] form and it is backward compatible with the classic scheme syntax.
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

The @racket[then] and @racket[else] blocks can also be written as sub-lists, @racket[(then then-body ...)] and @racket[(else else-body ...)], and the two writings can be mixed:

@codeblock|{
(if #f
    (then "You" "should" "not" "be" "there.")
    (else "I" "am" "here"))
"here"
}|

This is what the indentation gives with the sweet-expressions of SRFI 110, where the blocks can then be indented under their keyword:

@codeblock|{
if {x < 0}
  then
    'negative
  else
    'positive
}|


Example from real code:

@codeblock|{
(if (singleton-set? sos) then

      ;; singleton
      (reverse acc)

   else
      
         ;; at least 2 elements in set of sets
         {mt-set1 := (car sos)} ;; minterm set 1
	 {mt-set2 := (cadr sos)} ;; minterm set 2
	 {mt-set2-to-mt-setn := (cdr sos)} ;; minterm sets 2 to n
	 {weight-mt-set1 := (floor-bin-minterm-weight (car mt-set1))} ;; in a set all minterms have same weight
	 {weight-mt-set2 := (floor-bin-minterm-weight (car mt-set2))}
	 {delta-weight := {weight-mt-set2 - weight-mt-set1}}

	 (if {delta-weight = 1} then ;; if minterms set are neighbours

	     ;; unify neighbours minterms sets
	     {unified-mt-set1-and-mt-set2 := (funct-unify-minterms-set-1-unit-threads mt-set1 mt-set2)} 		
	     (if (null? unified-mt-set1-and-mt-set2)
		 (funct-unify-minterms-set-of-sets-rec-tail mt-set2-to-mt-setn acc) ;; the result will be the continuation with sets from 2 to n
		 (funct-unify-minterms-set-of-sets-rec-tail mt-set2-to-mt-setn (insert unified-mt-set1-and-mt-set2 acc)))
		 
	  else
	     
	     (funct-unify-minterms-set-of-sets-rec-tail mt-set2-to-mt-setn acc))) ;; continue with sets from 2 to n
}|

@defform*[ #:link-target? #f #:id if  #:literals (else)
({statement if test [else statement2]}
 (statement if test [else statement2]))]{

An infix @racket[if] inspired from Python language, also called ternary operator (or conditional operator).}

Examples:

@codeblock|{
{"Hello world" if #t}

"Hello world"
}|

@codeblock|{
(declare interpolation-code)
{{interpolation-code := "cut_1D.py"} if ipic3D else {interpolation-code := "cut_1D_menura.py"}}
}|

@codeblock|{
{"test positif" if ((#f or #f) and #t) else "test negatif"}

(if (and (or #f #f) #t) "test positif" "test negatif") ; result of parsing

"test negatif"
}|

@codeblock|{
(define (foo)
    {"test positif" if ((#f or #f) and #t) else "test negatif"})

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
A @racket[cond] that allows the execution of code or even definitions between the test clauses.
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
{c :+ (first L)}
{R :+ (rest L)}
(condx [ {c = t} (return #t
	       		 (cons c s)) ] ;; c is the solution
	      
       [ {c > t} (ssigma-solution-exact R t s) ] ;; c is too big to be a solution
	      
       ;; c < t at this point
       ;; c is part of the solution	 
       [ exec {(c-sol sol) := (ssigma-solution-exact R (t - c) s)} ]
	      
       [ c-sol (return c-sol
		       (append (cons c s) ; c is in the solution for t, we insert it in the previous partial solution
			       sol)) ] ; we append with sol to get also the solutions of t - c,resulting in solutions of c + (t - c) = t

       ;; or c is not part of solution
       ;; or the two ! perhaps there exists 2 solutions using or not using c ! (in this case we got only the first found!)
       [ else
	  (ssigma-solution-exact R t s) ] )
}|

Note about the example above which mixes @racket[condx] and @racket[return] because it is part of a procedure not displayed here, i encourage the reader to see the full source code SssRec+.rkt to understand it better.

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

@defform[(while test  body ...)]{Initially the syntax should be as @racket[while] ... @racket[do] from Pascal but not possible as @racket[do] already exists in Scheme.}

@defform*[#:literals (while)
          ((do body ... while test)
           (do ((variable init step ...) ...) (test expr ...) body ...))]{
The @racket[do] of Scheme+ has two forms.

With @racket[while] at the end, it is the @racket[do] ... @racket[while] of C/C++, Java or Javascript: loop over @racket[body ...] while @racket[test] is true. The @racket[test] is computed after @racket[body ...], so the body is executed at least once (@racket[repeat] ... @racket[until] stops when its test is true). The @racket[body ...] can contain definitions.

@codeblock|{
{i := 0}
(do
    (display i)
    (newline)
    {i := i + 1}
  while {i < 3})

0
1
2
}|

Without @racket[while], it is the @racket[do] of Scheme (R7RS): the variables with their initial values and their steps, the test of the end and the expressions of the result, then the body, which can contain definitions (as the body of a procedure, at no cost). In a region @racket[ANNOTATION-ON], the types of the variables written with @racket[type] in the body are known by the steps and the test too (see @secref["type-form"]):

@codeblock|{
(define (sum-vector v n)
  (type #(flonum?) v)
  (type ℕ n)
  (do ((k 0 {k + 1})
       (sum 0.0 {sum + v[k]}))
      ({k = n} sum)
    (type ℕ k)
    (type flonum? sum)))

;; code generated by the annotation parser:
;; (do ((k 0 (fx+ k 1))
;;      (sum 0.0 (fl+ sum ($bracket-apply-vector-natural$ v k))))
;;     ((fx= k n) sum))
}|

@racket[(sum-vector #(1.0 2.0 3.5) 3)] gives @racket[6.5].}

@defform[(for (initialization condition update) body ...)]{Syntax of @racket[for] ... as it exists in C/C++, Javascript, Java language.}

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
(for ({i := 0} {i < n - 2} {i := i + 1}) 
    ;; create an array with 1 in front for the bias coefficient
    {z_1 := #(1) + z[i]} ; + operator has been overloaded to append scheme vectors
    {z̃[i + 1] := M[i] · z_1} ; z̃ = matrix * vector , return a vector
    {z[i + 1] := vector-map(activation_function_hidden_layer z̃[i + 1])})
}|

Example from matrix multiplication, compute element i, j of result:

@codeblock|{
(for ({k := 0} {k < p1} {k := k + 1})
    {sum := sum + M1[i][k] * M2[k][j]})
}|

As in C, @racket[(break)] leaves the loop and @racket[(continue)] goes to the next turn (the update then the condition). @racket[(break v)] makes @racket[for] return @racket[v]:

@codeblock|{
(for ({i := 0} {i < 10} {i := i + 1})
    (when (odd? i) (continue))
    (display i)
    (when {i = 6} (break 'stop)))
0246
'stop
}|

As for @racket[return] in @racket[def], @racket[for] takes a continuation (@racket[call/cc]) only when @racket[break] or @racket[continue] is written in the loop: one for the whole loop with @racket[break], one at each turn with @racket[continue], which makes the loop more than 10 times slower. Without them, @racket[for] is a simple loop, almost as fast as a @racket[do]. A @racket[break] or a @racket[continue] produced by another macro is not seen, and a @racket[break] of an inner @racket[for] gives also a continuation to the outer one.

Note about Scheme+ for Racket: the @racket[for] described above replaces the original @racket[for] of Racket: rename the original as below to use it. Scheme+ also defines @racket[in-range] and @racket[reversed] as in the Python language (@racket[in-range] already exists in Racket).

@codeblock|{
(require (only-in racket/base [for for-racket])) ;; backup original Racket 'for'
(for-racket ([i (reverse (range 1 i_output_layer))])
				{nc := vector-length(z[i])}
				{ns := vector-length(z[i + 1])}
				(for-racket ([j (range nc)])
					{ᐁ[i][j] := (for/sum ([k (range ns)])
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

@defform[($+> body ...)]{@racket[$+>] will allow also definitions in the @racket[body], the @racket[+] means you can add definitions in the environment. It is defined this way:
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
		      {a ⥆ (arg1 expr)} ; definitions, as : (define a (arg1 expr))
		      {b := (arg2 expr)} ; note: in Racket i could use also := which auto-define the variable if necessary :
		      {ae := (elim-equivalence a)}
		      {be := (elim-equivalence b)}
		      `(and (=> ,ae ,be) (=> ,be ,ae))))
   (else `(,(operator expr) ,(elim-equivalence (arg1 expr)) ,(elim-equivalence (arg2 expr))))))
}|
}

@subsection[#:tag "procedures"]{Operators and Procedures}

@defform[#:id \@ {n \@ m}]{Modulo. Same as @racket[(modulo n m)].}

@defform[#:id ** {n ** m}]{Exponentiation. Same as @racket[expt].}

Note: exponentiation can also be written with superscript numbers and expressions, see after in this document.

@defform[(√ n)]{Square root. Same as @racket[sqrt].}

@codeblock|{
(√ 2)
1.4142135623730951
}|

@defform[(∛ n)]{Cubic root.}

@defform[#:id << {n << m}]{Left shift. Same as @racket[(arithmetic-shift n m)].}

@defform[#:id >> {n >> m}]{Right shift. Same as @racket[(arithmetic-shift n (- m))].}

@defform[#:id & {n & m}]{Bitwise And}

@defform[#:id ∣ {n ∣ m}]{Bitwise Or. Note: this is U+2223  because vertical line is reserved in Racket.}

@defform[#:id ^ {n ^ m}]{Bitwise Exclusive Or}

@defform[#:id ~ {~ n}]{Bitwise Not. Same as @racket[(bitwise-not n)].

@curly-example[{~ 5} -6]

In an infix expression @racket[~] is a prefix unary operator, with the precedence of Python, as the unary minus: it binds more tightly than @racket[*], @racket[+], @racket[&], the shifts and the comparisons, and less tightly than the exponentiation. A @racket[~] in the position of an operand (at the beginning, or after an operator) applies to the term that follows it, with the chain of exponentiation after this term.

@codeblock|{
{a & ~ b}               ; (& a (~ b))
{~ a & b}               ; (& (~ a) b)
{2 * ~ a}               ; (* 2 (~ a))
{~ ~ a}                 ; (~ (~ a))
{~ a ** 2}              ; (~ (** a 2))
{~ a ²}                 ; (~ (** a 2))
{2 ** ~ a}              ; (** 2 (~ a))
}|

In a call written in prefix, or when nothing follows it, @racket[~] is simply the procedure @racket[~], used as a value: @litchar{{f := ~}} assigns it to @racket[f], and in @litchar{(map ~ l)} it is passed as an argument to @racket[map]. The prefix unary operators of Scheme+ are those of Python: @racket[-], @racket[+], @racket[~] and @racket[not].}

@defform[#:id && {expr1 && expr2}]{This is a procedural @racket[and] intended to be used both internally, inside Scheme+ source code, and externally in a Scheme+ program when parsing is required at runtime where we can not use the macro @racket[and] and thus we will use the @racket[&&] procedure. Unless your expression will be parsed at runtime do not use it and prefer @racket[and].}

@defform[#:id ∣∣ {expr1 ∣∣ expr2}]{This is a procedural @racket[or]. See explanations above. See also note about vertical line reserved in Racket above.}

Being procedures, @racket[&&] and @racket[∣∣] are not short-circuited. The @racket[and] and @racket[or] of Scheme remain usable as usual, in prefix and in infix (@litchar{{a and b}} is @racket[(and a b)]), and they are short-circuited.

@defform[#:id not {not expr}]{
@curly-example[{not #f} #t]

In an infix expression @racket[not] is a prefix unary operator, with the precedence of Python: it binds less tightly than the comparisons and the arithmetic operators, and more tightly than @racket[and] and @racket[or]. A @racket[not] in the position of an operand (at the beginning, or after an operator) applies to the terms that follow it, until the next @racket[and], @racket[or], assignment or definition. It works with the two precedence engines of Scheme+: a Pratt parser (top down operator precedence), used by default, and the original engine of Scheme+, which reduces the expression in successive passes, one for each group of operators of the same precedence, from the highest precedence to the lowest.

@codeblock|{
{not a}                 ; (not a)
{a and not b}           ; (and a (not b))
{not a and b}           ; (and (not a) b)
{not x < y}             ; (not (< x y))
{a or not b and c}      ; (or a (and (not b) c))
{not not a}             ; (not (not a))
{x := not a and b}      ; (:= x (and (not a) b))
}|

A @racket[not] in the position of an operator, between two operands, negates the comparison that follows it, as @litchar{not in} in Python but for any procedure or operator: @racket[L not X R] is @racket[(not (X L R))], the operands going until the next @racket[and], @racket[or], assignment or definition. This is the negated relation of the mathematics (@litchar{∉}, @litchar{≮}), and of @litchar{NOT IN} or @litchar{NOT LIKE} in SQL and @litchar{!in} in Kotlin; Python has only @litchar{not in}.

@codeblock|{
{5 member '(1 2 3)}         ; (member 5 '(1 2 3))
{5 not member '(1 2 3)}     ; (not (member 5 '(1 2 3)))
{x + 1 not member l and y}  ; (and (not (member (+ x 1) l)) y)
{a not < b}                 ; (not (< a b))
}|

In a call written in prefix, or when nothing follows it, @racket[not] is simply the procedure @racket[not], used as a value, and the reading does not transform it: @litchar{{f := not}} assigns the procedure @racket[not] to @racket[f], and in @litchar{(map not l)} it is passed as an argument to @racket[map].}

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

Note: some Qi operators not documented here (see @hyperlink["https://docs.racket-lang.org/qi/index.html"]{Qi documentation} for them) are recognized, for example @racket[~>] should immediately work in infix:

@codeblock|{
;; displayTrajectory3D : parse the Bepi Colombo spacecraft trajectory file text and display it in 3D
#lang reader curly-infix

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
	  {ext := src[-4 :]} ; try to get the .txt extension

	  (when (not (equal? ext ".txt"))
		(error "Not a text file."))

	  ;; read all lines
	  {input-lines := (read-lines src)}
	  {vl := (list->vector input-lines)}

	  {input-lines1 := (read-lines src1)}
	  {vl1 := (list->vector input-lines1)}

	  ;; find the basename
	  {basename := src[: -4]} ; skip the 4 char of extension

	  ;; find the spatial_unit if exist
	  {spatial_unit := (regexp-match #rx"km" vl[4])}
	  (when (not spatial_unit)
		{spatial_unit := (regexp-match #rx"Rm" vl[4])})

	  (when (not spatial_unit)
		(error "No spatial unit (km or Rm) found."))

	  {spatial_unit := (first spatial_unit)}

	  {tdl := vl[5 :]} ; TABLEDATA lines, skip the header to go to table data lines
	  {tdl1 := vl1[5 :]}
	  
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


@(define-runtime-path bepi-colombo "BepiColombo-Mercury-Planet.png")

@centerline{@image[#:scale 1.1 bepi-colombo]}


@defform[(int x)]{Return the integer part of a number.

@codeblock|{
(int 2.5)
2

(int -3.7)
-3
}|

}

@defform[#:id ! {n !}]{Factorial.

Could be used by convention as postfix operator, as in mathematics:
@codeblock|{
{12 !}
479001600
}|

Or in a classic scheme expression:
@codeblock|{
(! 5)
120
}|

}


@subsection[#:tag "superscript"]{Superscript}
Superscript characters can be used to form numbers, variable or expressions forming an exponent, thus defining an exponentiation without the need of @racket[**] operator:
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
Note also that not all alphabetic characters currently exist in superscript, here is the source code definitions:

@codeblock|{
(define superscript-string "⁻⁺⁰¹²³⁴⁵⁶⁷⁸⁹") ; super script numbers and signs
(define superscript-only-string (string-append superscript-string ; numbers and signs
					  "ᴬᵃᴮᵇᶜᴰᵈᴱᵉᶠᴳᵍᴴʰᴵᶦᴶʲᴷᵏᴸˡᴹᵐᴺⁿᴼᵒᴾᵖᴿʳˢᵗᵁᵘⱽᵛᵂʷˣʸᶻ" ; note all letters are not available (ex:C,Q,q...)
					  "⁽⁾"
					  "⸱" ; will be used as decimal separator
					  ;; "*" ; multiplication is ambiguous (both super and normal script)
					  ;; "·" ; another multiplication symbol (but ambiguous too)
					  "⸍"; division
					  ))
}|

Note under Linux and on a PC (french) keyboard superscript characters can be generated with the keystroke sequence: ^ n where n is  a number or sign (but this does not work for the alphabetic characters, as it only puts an accent on them).

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

@subsection[#:tag "postfix"]{Postfix operators}
The { } curly brackets also accept the postfix notation.
By default all existing procedures can be used postfixed in { }.

In a ( ) sub-expression, an expression whose last element is a procedure is postfix when its first element can not be a procedure: a number, a string, a character, a boolean, a quoted data, or a variable declared with a type which is not a procedure, for example @racket[(type flonum? x)]. A variable without type could contain a procedure: @racket[(x 3 +)] stays then a call of @racket[x], while @racket[(3 x +)] is @racket[(+ 3 x)]; with @racket[(type flonum? x)], @racket[(x 3 +)] is @racket[(+ x 3)] and @racket[(x sqrt)] is @racket[(sqrt x)], @racket[(#t not)] is @racket[(not #t)].

How the forms of Scheme written inside @racket[{ }] (@racket[cond], @racket[lambda], @racket[do], @racket[let] ...) are treated is described in @secref["scheme-forms"].

@curly-example[{1 2 3 4 +} 10]

@codeblock|{
{1 (4 5 *) 3 +}
24

; A mix of postfix,infix and prefix:
{2 (3 (9 - 5) *) (- 8 3) +}
19
}|

There are also some predefined postfix operators such as the factorial @racket[!], an example of use:

@codeblock|{
(define (C n k)
    {(n !) / ((k !) · ((n - k)!))})

(C 10 3)
120
}|

Also in sub expressions in ( ) parenthesis this could be used.
As there is no special parenthesis to delimit postfix expressions this could be ambiguous.
For this reason when the priority must be given to postfix the operator must be defined as also postfix by this syntax:

@defform[(declare-postfix-operator op ...)]{Define postfix operators named @racket[op ...].

Then if there are two variables that could be some procedures in a parenthesis expression: one at beginning (prefix like) and one at the end (postfix like)
the procedure taken in account will be the postfix procedure at the end if defined postfix, and the procedure at the beginning, if it is one, will be considered as an argument of the postfix one. Otherwise the normal prefix execution would have remained.
But when the parser knows that the variable at beginning is a procedure (a procedure of Racket or of your program, see @secref["both"]) the expression is a prefix call: @racket[(f 3 !)] is a call of @racket[f].

The declaration acts only on the file where it is written, and it must be written before the use of the operator.

@codeblock|{
(declare-postfix-operator *)
{x := 7}
{1 (x 5 *) 3 +}
39
}|

Another example using floating point matrix and computing the transpose of the matrix:

@codeblock|{
(require flomat)
(define ᵀ transpose)
(declare-postfix-operator ᵀ)
{A := (matrix #(#(1 2 3) #(4 5 6)))}
{A ᵀ}
(flomat: ((1.0 4.0) (2.0 5.0) (3.0 6.0)))
{(A ᵀ)ᵀ}
(flomat: ((1.0 2.0 3.0) (4.0 5.0 6.0)))
}|

}


@defform[(declare-prefix-operator op ...)]{Define prefix unary operators named @racket[op ...]. In an infix expression such an operator binds as the unary minus and as @racket[~]: more tightly than @racket[*], @racket[+] and the comparisons, less tightly than the exponentiation; it applies to the term that follows it. As for the postfix operators, the declaration acts only on the file where it is written, before the use of the operator. An operator overloaded with @racket[overload-unary-operator] is declared automatically.

@codeblock|{
(define (inv x) (/ 1 x))
(declare-prefix-operator inv)
{2 * inv 4}      ; (* 2 (inv 4))
1/2
{inv 2 ** 2}     ; (inv (** 2 2))
1/4
}|
}



@subsection[#:tag "overload"]{Overloading procedures and operators}

Overloading must be set at a module toplevel, it will not work in a procedure, and there is no need of that.
You can @racket[provide] an overloaded procedure or operator from the module where it is defined and overloaded to other modules.

Two macros are provided: @racket[overload-procedure] for the procedures and @racket[overload-operator] for the operators, because an operator can be binary or n-ary and has an associativity property that a procedure does not have.

Overloading is made in a single step: we give a specialized procedure and the predicates that will be used to test the arguments at runtime. The first time a procedure or operator is overloaded in a module, the overloaded procedure or operator is defined, the next times only the new specialization is added. The procedure or operator can already exist in Scheme (as @racket[+] or @racket[length]), then its original definition is used when no specialization matches the arguments, or it can be a new one.

With this schema we can have a same function (procedure or operator) that will work on different arguments.

Overloading an existing procedure or operator defines a new one with the same name for the whole module: in this module it must not be called before the overloading form has been evaluated, so put the overloadings at the beginning of the module.

Note that we can also extend the square brackets [ ] to other types of objects, the same way it is done in the Python language for the square brackets. This is done by overloading the underlying procedure that does the bracket apply and the assignment macros, we will see that at the end of this section.

@defform*[((overload-procedure name special (predicate ...))
           (overload-procedure name (return-predicate ...) special (predicate ...))
           (overload-procedure name module-name special (predicate ...))
           (overload-procedure name module-name (return-predicate ...) special (predicate ...)))]{Overload a procedure named @racket[name] with a specialized procedure named @racket[special] which is used when the arguments pass the @racket[predicate] tests (one predicate per argument, any number of arguments). This is done in a single step: the overloaded procedure is defined first if it is not already overloaded in the module. When @racket[name] is an existing procedure, the original is taken from @racket[racket/base], or from @racket[module-name] when given, and is used when no overload matches the arguments; for a new procedure an error is raised in that case. The optional list of @racket[return-predicate]s gives the types of the values returned by @racket[special]; it is only used by the type annotation system.

The @racket[predicate]s must accept any value and return @racket[#t] or @racket[#f] without raising an error, as for @racket[overload-operator].}

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
;; return a number in ]-1,1[
;; the dummy parameter is needed by a flomat procedure
(define (uniform-dummy dummy) {-1 + (random) * 2})
; return a random number between [inf, sup]
(define (uniform-interval inf sup)
  {gap := sup - inf}
  {inf + gap * (random)})
(overload-procedure uniform uniform-dummy (number?))
(overload-procedure uniform uniform-interval (number? number?))
}|

An already existing procedure, here @racket[length], can be overloaded to be used with different types than only @racket[list]:

@codeblock|{
(overload-procedure length vector-length (vector?))
(overload-procedure length string-length (string?))

(length #(1 2 3 4))
4

(length '(1 2 3))
3

(length "abcde")
5
}|

Note that when no defined predicate matches the arguments (in the above example neither @racket[vector?] nor @racket[string?] returns true for a @racket[list]) the overloading schema falls back to the original procedure definition that is here @racket[length] that works with @racket[list].



@defform*[((overload-operator name special (predicate))
           (overload-operator name special (predicate1 predicate2))
           (overload-operator name module-name special (predicate ...))
           (overload-operator name (return-predicate ...) special (predicate ...))
           (overload-operator name module-name (return-predicate ...) special (predicate ...)))]{Overload an operator named @racket[name] with a specialized operator named @racket[special], in a single step: the overloaded operator is defined first if it is not already overloaded in the module. This first @racket[overload-operator] of @racket[name] defines @racket[name] at the place where it is written: at the top level of the module, @racket[name] must not be used before it (Racket raises @tt{cannot reference an identifier before its definition}), while a procedure defined before it and called after it uses the overloaded operator. So write the overloads at the beginning of the module. With one predicate the overload is n-ary (all the arguments, whatever their number, must pass the @racket[predicate] test), with two predicates it is binary. When @racket[name] is an existing operator, the original is taken from @racket[racket/base], or from @racket[module-name] when given, and is used when no overload matches the arguments. The optional list of @racket[return-predicate]s gives the types of the values returned by @racket[special]; it is only used by the type annotation system.

The @racket[predicate]s must accept any value and return @racket[#t] or @racket[#f] without raising an error: at run time they are applied to the actual arguments, possibly of another type than expected, while searching for the matching overload. A partial predicate such as @racket[inexact?], @racket[zero?] or @racket[positive?] raises an error on a non-number and must be guarded, for example @racket[(λ (x) (and (number? x) (inexact? x)))].}

When an n-ary expression, such as @racket[(+ a b c)], matches no n-ary overload, the left associativity of the operator is used: it is computed as @racket[(+ (+ a b) c)], so that the binary overloads can apply.

An n-ary overload, here any number of strings:

@codeblock|{
(overload-operator + string-append (string?))

(+ "a" "b" "c")
"abc"

{"Hello" + " " + "world !"}
"Hello world !"
}|

A binary overload, the @racket[(vector?)] list is optional, it tells the type annotation system that the result is a vector:

@codeblock|{
(overload-operator + (vector?) vector-append (vector? vector?))
}|

Now the @racket[+] appends vectors as in the Python language:

@codeblock|{
{#(1 2 3) + #(4 5 6 7)}
'#(1 2 3 4 5 6 7)

{(vector 1 2 3) + (vector 4 5 6 7)}
'#(1 2 3 4 5 6 7)

;; create an array with 1 in front for the bias coefficient
{z_1 := #(1) + z[i]} ; + operator has been overloaded to append scheme vectors
}|

The same operator can have n-ary and binary overloads:

@codeblock|{
(define (add-n-lists . vn-lst)
  {map-args := (cons + vn-lst)}
  (apply map map-args))

(overload-operator + add-n-lists (list?)) ; n-ary : any number of lists

{'(1 2 3) + '(4 5 6) + '(7 8 9)}
'(12 15 18)

(define (mult-num-list k v) (map (λ (x) (* k x)) v))
(overload-operator * mult-num-list (number? list?)) ; binary : a number and a list

{3 * '(1 2 3) + '(4 5 6) + '(7 8 9)}
'(14 19 24)
}|

An example of overloading used with the type annotations, the dot product of two vectors and the product of a number by a vector, is in the section @secref["operators-annotation"].

An operator that does not come from @racket[racket/base], such as @racket[·] which is defined in @racket[Scheme+/multiply], needs the module where its original definition is:

@codeblock|{
(overload-operator · (only-in Scheme+/multiply ·) multiply-flomat-vector (flomat? vector?))
}|

@defform*[((overload-unary-operator name special (predicate))
           (overload-unary-operator name module-name special (predicate))
           (overload-unary-operator name (return-predicate ...) special (predicate))
           (overload-unary-operator name module-name (return-predicate ...) special (predicate)))]{Overload an operator named @racket[name] with a specialized procedure @racket[special] of exactly one argument. With @racket[overload-operator] a single predicate means an n-ary overload, for any number of arguments of that type: a procedure of one argument must be given with @racket[overload-unary-operator], then it is used only for a call with one argument. The binary overloads and the unary ones are tried first, the n-ary ones after, at run time as in the type annotation.

The negation and the subtraction of matrices:

@codeblock|{
(struct mat (v))
(define (mat-neg M) (mat (vector-map - (mat-v M))))
(define (mat-sub A B) (mat (vector-map - (mat-v A) (mat-v B))))

(overload-operator - (mat?) mat-sub (mat? mat?))   ; binary : A - B
(overload-unary-operator - (mat?) mat-neg (mat?))  ; unary : - M

(define (f A B)
  (type mat? A B)
  {P := - A}   ; (:= P (mat-neg A)) , P is a mat?
  {P - B})     ; (mat-sub P B)
}|

The operator can be a new one. A new symbol overloaded with @racket[overload-unary-operator] is also a prefix unary operator in infix (as @racket[~], see @racket[declare-prefix-operator]):

@codeblock|{
(overload-unary-operator ¬ (boolean?) not (boolean?))
{a and ¬ b}   ; (and a (¬ b)) , with (type boolean? b) : (and a (not b))
}|
}

@defform*[((overload-square-brackets getter setter! (pred-obj pred-coord ...))
           (overload-square-brackets (return-predicate) getter setter! (pred-obj pred-coord ...)))]{Overload square bracket [ ] for the objects that return true with the predicate @racket[pred-obj] with @racket[getter] for read and @racket[setter!] for write at coordinates of type validated by predicates @racket[pred-coord ...].
The second form gives also the type of the value returned by @racket[getter], for example @racket[(flonum?)]: it is used only by the type annotation pass (see @secref["square-brackets-annotation"]) and ignored at run time.}

Example for accessing and modifying a line of a matrix:

@codeblock|{
(struct matrix-vect (v)) ;; matrix based on vector of vectors

(define (matrix-vect-line-ref M lin)
  {v := (matrix-vect-v M)}
  {v[lin]})


(define (matrix-vect-line-set! M lin vect-line)
  {v := (matrix-vect-v M)}
  {v[lin] := vect-line})
  
(overload-square-brackets matrix-vect-line-ref matrix-vect-line-set! (matrix-vect? number?))

(define Mv (matrix-vect #(#(1 2 3)
                          #(4 5 6))))
{Mv[1]}
'#(4 5 6)
}|

You can access all elements of the matrix also by different syntaxes:

@codeblock|{
{Mv[1][0]}
4

; define getter,setter
(define (matrix-vect-ref M lin col)
  {v := (matrix-vect-v M)}
  {v[lin][col]})

(define (matrix-vect-set! M lin col x)
  {v := (matrix-vect-v M)}
  {v[lin][col] := x})

(overload-square-brackets matrix-vect-ref matrix-vect-set!  (matrix-vect? number? number?))

{Mv[1 0]}
4
}|

This works also for assignment if your object is mutable:

@codeblock|{
{a := (vector (vector 1 2 3)
              (vector 4 5 6))}

(display a)
'#(#(1 2 3) #(4 5 6))

{a[1 2] := 7}

(display a)
'#(#(1 2 3) #(4 5 7))

{a[1][2] := 10}

(display a)
#(#(1 2 3) #(4 5 10))
}|

See the full example at @url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket/blob/main/examples/racket/matrix-by-vectors+.rkt"]


@section[#:tag "annotation"]{Type annotation and inference}

The purpose of the type annotation and inference pass of Scheme+ is @bold{performance}. The features of Scheme+ are generic: an overloaded operator or procedure, the square brackets @litchar{[ ]} and the arithmetic accept any type of argument, and the code to run is chosen at run time, at each call. When the types are known, the pass makes this choice once, at read time, on three things:
@itemlist[
  @item{@bold{the overloads of procedures and operators} (@racket[overload-operator], @racket[overload-procedure]): the procedure of the overload that matches the types of the arguments is called directly (see @secref["overloads-annotation"]);}
  @item{@bold{the square brackets}: an access @litchar{{v[i]}} or an assignment @litchar{{v[i] := x}} goes directly to the accessor of the vector, of the string, of the matrix (@racket[$bracket-apply-vector$], @racket[$bracket-apply-flomat$], @racket[$bracket-apply-string$], ...) or of the square brackets overloaded with @racket[overload-square-brackets], without a test of the type at run time (see @secref["square-brackets-annotation"]); an assignment of vectors to vectors or of strings to strings is specialized too (@racket[<-vector-vector*], @racket[<-string-string*]);}
  @item{@bold{the computation}: an arithmetic operator (@racket[+], @racket[-], @racket[*], @racket[/], @racket[·], @racket[**]) or a comparison (@racket[<], @racket[≤], @racket[≠], ...) on floats or on fixnums becomes the primitive of the floats or of the fixnums (@racket[fl+], @racket[fx*], @racket[fl<=], @racket[fx≠], ...; Scheme+ provides @racket[fx≠] and @racket[fl≠], and the result of a comparison is a @racket[boolean?]); on the other numbers the generic operator of Racket is called without the dispatch of the overloads (@racket[orig-+]); the constant expressions are computed at read time (see @secref["inference"]).}]

@bold{On the neural network of the deep learning example, a retropropagation (@secref["annotation-example"]), 1000 iterations of the learning take 8.34 s without the pass and 1.15 s with @racket[ANNOTATION-ON] and the type annotations of the program, in the version with vectors of vectors of floats (@tt{examples/racket/exo_retropropagationNhidden_layers_matrix_v3_by_vectors+.rkt}): about 7 times faster.} The annotations are a few @racket[type] lines at the top of the procedures, the infix code itself is unchanged. The benchmarks are in @secref["benchmarks"].

The pass is @bold{optional}. It runs @italic{inside} the SRFI-105 Curly Infix reader, after the curly-infix and square-bracket parsing and @italic{before} the Racket compiler sees the code, so it is a read/compile time mechanism, not a run time one.

@bold{The Scheme syntax unchanged: the types apart from the definitions.} Typed Racket and Bigloo write the types inside the definitions themselves: Typed Racket in the binding forms, @racket[(define (f [x : Flonum]) : Flonum ...)] or @racket[(λ ([x : Flonum]) ...)], in a module written in another language, @racket[typed/racket]; Bigloo in the names, @tt{(define (f::double x::double) ...)}. The code is then no more the code of Scheme. In Scheme+ the definitions are not changed: the types are given apart, by a @racket[type] form written as a declaration in the body, next to the code it types. The code stays the code of Scheme (and of Scheme+), the same with and without the types: removing the @racket[type] lines, or turning the pass off, gives the same program.

@codeblock|{
;; Scheme+ : the Scheme code, the types apart
(define (sigmoid z)
  (type flonum? z)
  {1.0 / {1.0 + (exp (- z))}})

;; Typed Racket : the types in the definition
(define (sigmoid [z : Flonum]) : Flonum
  (/ 1.0 (+ 1.0 (exp (- z)))))

;; Bigloo : the types in the names
(define (sigmoid::double z::double)
  (/ 1.0 (+ 1.0 (exp (- z)))))
}|

@bold{Type only what is worth typing.} A type system as the one of Typed Racket checks all the code of a typed module: all of it must be typed and pass the checker, and the values that cross the frontier with the untyped modules are checked by contracts. With the annotation of Scheme+ nothing has to be typed: the pass types by itself what it can infer (see @secref["inference"]), and the programmer adds a type only where it is judged interesting, for example for speed the parameters of the procedures that compute with floats in a loop; the rest of the program stays untyped Scheme, generic and dispatched at run time as without the pass. The other side is that the declarations are not checked: the pass trusts them, a false declaration gives a wrong specialization.

The inference already knows the type of the literals and of the R7RS and Scheme+ primitives (that data comes from @racket[SRFI-105/scheme-types] and from the operator tables of @tt{annot+.rkt} of the SRFI-105 package). So, as soon as the pass is on, types are inferred even without any annotation (see @secref["inference"]); everything else you provide with the @racket[type] form and a couple of directives.

When the pass is @bold{off} (this is the default) every @racket[type] form is simply removed from the program, so annotated code stays valid unannotated Scheme+.

A local variable hides the type of an outer variable of the same name: the type of a parameter is written in the body of the procedure, @racket[(define (f x) (type flonum? x) ...)]. How the forms of Scheme are treated by the pass (macro definitions, local variables) is described in @secref["scheme-forms"].

@subsection[#:tag "benchmarks"]{Benchmarks}

@; the table of the benchmarks carries an HTML anchor for a link : Scheme-PLUS-for-Racket.html#benchmarks
@tabular[#:style (make-style 'boxed (list (attributes '((id . "benchmarks")))))
         #:sep @hspace[3]
         #:column-properties '(left right right right right right)
         #:row-properties '(bottom-border ())
         (list (list @bold{program (same source)} @bold{Scheme} @bold{Scheme+} @bold{Scheme+ annotated} @bold{Typed Racket} @bold{gain})
               (list "floats: Horner, sigmoid, 2·10⁷" "882 ms" "860 ms" "241 ms" "239 ms" "3.7 ×")
               (list "matrices 200×200, 20 products" "1206 ms" "1206 ms" "691 ms" "654 ms" "1.7 ×")
               (list "integers: Syracuse up to 10⁶" "1053 ms" "1060 ms" "407 ms" "415 / 1168 ms" "2.6 ×")
               (list "trapezoids, 2·10⁷ steps" "378 ms" "377 ms" "72 ms" "72 ms" "5.3 ×"))]

CPU times with Racket 9.3, the median of 3 series of 7 runs; the gain is the one of Scheme+ annotated over Scheme (Typed Racket on the integers: @racket[Fixnum] / @racket[Integer]). The sources of these benchmarks are in the sub-directory @bold{examples/benchmarks/} of Scheme+: @tt{racket run-benchmarks.rkt} runs all the versions.

@subsection[#:tag "annotation-on"]{Activating the pass}

@defidform[ANNOTATION-ON]{
@racket[ANNOTATION-ON] is a @bold{pragma} of the Curly Infix reader (SRFI-105 and SRFI-110): written as a bare symbol (not a procedure call), it is recognized by the reader while it reads the file, not by the Racket compiler, and it turns the type annotation and inference pass on. It acts by @bold{region}: on the elements that follow it in the same list (the body of the module, of a procedure, ...) and on all that is nested in them, until @racket[ANNOTATION-OFF] or the end of the list; after the end of the list the state of before comes back. Place it at the very beginning of the module, right after the @racket[require] section, to annotate the whole module.

The pragma can be written inside the module form or at the top level of the file: at the top level, before the module form, it acts on the whole module. It does not remain in the code: the annotation pass removes it, so nothing remains at run time.}

@defidform[ANNOTATION-OFF]{The pragma that turns the pass off, by region too, see @racket[ANNOTATION-ON]. In a region turned off the code is not rewritten, but the overload declarations and the @racket[type] forms are still taken into account (and the @racket[type] forms are removed from the code).}

@italic{Note :} the pass is disabled at the beginning of each file: a pragma acts only on the file where it is written, not on the modules this file requires.

@codeblock|{
(require Scheme+)

ANNOTATION-ON
}|

A region turned off is useful for example around the call of a macro that uses its argument as data, which must receive it as it is written:

@codeblock|{
(define-syntax-rule (show e)
  (begin (display 'e) (display " = ") (display e) (newline)))

(define (test-show)
  ANNOTATION-OFF   ; only in the body of test-show
  (show (+ 1 2)))  ; displays (+ 1 2) = 3 , and not 3 = 3

(test-show)
}|

While the pass is active the rewritten module is pretty-printed on the console as the file is read, which is handy to check what the inference actually produced.

@subsection[#:tag "inference"]{Type inference without annotation}

With @racket[ANNOTATION-ON], the pass infers types by itself, even in a program that has no @racket[type] annotation at all:
@itemlist[
  @item{the type of a literal: @racket[3] is a fixnum, @racket[2.5] a flonum;}
  @item{the type of the result of an arithmetic operator whose arguments have known types: the operator is then specialized (@racket[fl*], @racket[fx+], ...) and its result type is known in turn, so a whole expression is typed;}
  @item{the return type of the R7RS and Scheme+ primitives: @racket[(vector-length v)] is a fixnum;}
  @item{the propagation of these types through the definitions and assignments (@racket[:=], @racket[<-], @racket[define], ...): a variable gets the type of the value it receives;}
  @item{an integer literal with a float is written as a float: @litchar{{x * 2}} is @racket[(fl* x 2.0)], @litchar{{1 - x}} is @racket[(fl- 1.0 x)], @litchar{{x < 1}} is @racket[(fl< x 1.0)], as the generic operator would convert it at run time (not the literal @racket[0]: @racket[(* 0 2.0)] is the exact @racket[0] in Racket, nor an integer beyond 2⁵³, which is not exactly a float);}
  @item{a float and a fixnum variable: @litchar{{x + i}} is @litchar{(fl+/fx x i)}, @litchar{{i * x}} is @litchar{(fx/fl* i x)}, @litchar{{x < i}} is @litchar{(fl</fx x i)}, and so on for @racket[+], @racket[-], @racket[*], @racket[/], @racket[min] and the comparisons. These macros of Scheme+ convert the fixnum with @racket[fx->fl] and use the flonum operator, about twice as fast as the generic operator. A guard tested at run time keeps the result of the generic operator: the generic operator is called when the conversion would change the result, that is for the fixnum @racket[0] with @racket[+], @racket[-], @racket[*] and @racket[/] (@racket[(+ -0.0 0)] is @racket[-0.0], @racket[(* x 0)] is the exact @racket[0], @racket[(/ x 0)] raises an error), and for a comparison with a fixnum beyond 2⁵³. Because of the exact @racket[0], @litchar{{x * i}} is a real, not a flonum. The names @litchar{fl+/fx}, @litchar{fx/fl+}, ... are reserved;}
  @item{the constant folding of a sub-expression whose arguments are all literal numbers.}]

When the types of the arguments are unknown, or when no specialized operator fits them (for example a flonum and an exact integer that can be a bignum), the generic operator is kept and dispatches at run time, as without the pass.

@codeblock|{
ANNOTATION-ON

(define (f v)          ; no type annotation at all
  {x := 2.5}
  {y := x * 2.0 + 1.0}
  {n := (vector-length v)}
  (for ({i := 0} {i < n} {i := i + 1})
    (display {v[i]}))
  (list {2 * 3 + 4} {n - 1} {y / x} {2.5 * 4}))

;; code generated by the annotation parser:
(define (f v)
  (:= x 2.5)
  (:= y (fl+ (fl* x 2.0) 1.0))
  (:= n (vector-length v))
  (for ((:= i 0) (fx< i n) (:= i (fx+ i 1)))
    (display ($bracket-apply$ v i)))
  (list 10 (fx- n 1) (fl/ y x) 10.0))
}|

Here @racket[x] and @racket[y] are flonums (from the literals and the specialized operators), @racket[n] and @racket[i] are fixnums (from @racket[vector-length] and from the literal @racket[0]), @racket[{2 * 3 + 4}] is folded to @racket[10], @racket[{2.5 * 4}] is folded to @racket[10.0] (with a float, the literal @racket[4] is written @racket[4.0]), while @litchar{{v[i]}} stays a generic access (the type of @racket[v] is unknown). The @racket[type] form described below gives the types that cannot be inferred, for example those of the parameters of a procedure or of the elements of a vector.

@subsection[#:tag "type-form"]{Annotating variables, elements and procedures : the @racket[type] form}

@defform[(type type-spec target ...)]{
Give the type @racket[type-spec] to each @racket[target]. At run time @racket[type] expands to nothing (if the pass ever leaves one in place it prints a warning, meaning the parser forgot to consume that annotation).

@racket[type-spec] is either:
@itemlist[
  @item{a predicate symbol, from the most specific to the most general for numbers : @racket[fixnum?], @racket[exact-integer?], @racket[integer?], @racket[flonum?], @racket[real?], @racket[rational?], @racket[inexact?], @racket[complex?], @racket[number?] ; or a container / other predicate such as @racket[vector?], @racket[flomat?], @racket[string?], @racket[boolean?], @racket[procedure?] ;}
  @item{or a composite type for a homogeneous container: @litchar{#(T)} is a vector whose elements have the type @racket[T], @litchar{(T)} a list whose elements have the type @racket[T]; they compose, @litchar{#(#(flonum?))} is a vector of vectors of floats; @litchar{#()} is a vector whose elements have no given type, the same as @racket[vector?];}
  @item{or a domain / codomain description of a procedure, written @racket[[(codomain-pred ...) procedure? (domain-pred ...)]] (square brackets or parentheses both work). The codomain part is always the list of the returned types (a procedure can return multiple values), so a procedure returning a list of floats is written with @litchar{((flonum?))}.}]

@racket[target] is either an identifier, or a curly bracket access pattern @litchar{{id[]}} to type the @italic{elements} of @racket[id], @litchar{{id[][]}} to type the elements two levels deep, and so on.}

One predicate, one or several identifiers:

@codeblock|{
(type [(flonum?) procedure? ()] random) ; random with no argument returns a float

(define (uniform-interval inf sup)
  (type flonum? inf sup)
  {gap ← sup - inf}
  {inf + gap * (random)})
}|

is parsed into:

@codeblock|{
(define (uniform-interval inf sup)
  (← gap (fl- sup inf))
  (fl+ inf (fl* gap (random))))
}|

Type of a variable @italic{and} of its elements:

@codeblock|{
(type vector? z {z[]})   ; z is a vector, and each z[i] is a vector too
(type flonum? {z[][]})   ; each z[i][j] is a float
(type fixnum? {nc[]})    ; the elements of nc are fixnums
(type vector? M)          ; M is a vector ...
(type flomat? {M[]})      ; ... of floating point matrices ...
(type flonum? {M[][]})    ; ... whose coefficients are floats
}|

Domain and codomain of a procedure whose body is not annotated (a Racket or library procedure, or one you simply do not want to annotate inside):

@codeblock|{
(type [(flonum?) procedure? (flonum?)] sqr)             ; sqr : flonum? -> flonum?
(type ((flonum?) procedure? (flonum? flonum?)) der_σ)   ; parentheses also accepted
}|

The types of the numbers that make the pass write @racket[fx+] or @racket[fl*] can also be written ℕ, ℤ, ℝ and ℝ⁺, everywhere a type is written: in @racket[type], in the signatures, in the composite types and in the predicates of the overloads. They are reserved names of Scheme+, defined as predicates (so @racket[(ℝ 2.0)] is @racket[#t] and @racket[(ℕ -3)] is @racket[#f]). They are not exactly the sets of mathematics: a fixnum is bounded, and ℝ is the floats.

@defproc[(ℕ [v any/c]) boolean?]{@racket[natural-fixnum?] : a fixnum greater than or equal to 0, read by the annotation as a fixnum (and as an index without negative values between square brackets).}

@defproc[(ℤ [v any/c]) boolean?]{@racket[fixnum?], the negative ones too.}

@defproc[(ℚ [v any/c]) boolean?]{@racket[rational?], only defined, not used by the annotation for now.}

@defproc[(ℝ [v any/c]) boolean?]{@racket[flonum?].}

@defproc[(ℝ⁺ [v any/c]) boolean?]{@racket[nonnegative-flonum?] : a float that is not negative, read by the annotation as a float. As @racket[Nonnegative-Flonum] of Typed Racket, it contains @racket[-0.0] and @racket[+nan.0].}

@defproc[(ℂ [v any/c]) boolean?]{@racket[complex?], only defined, not used by the annotation for now.}

For the arithmetic, ℕ is a fixnum (@racket[{i + 1}] is @racket[(fx+ i 1)]). For an index between square brackets of a vector or a string, the type of the index gives a faster access: with an index of type ℕ, @litchar{{v[i]}} is @racket[(vector-ref v i)] (@racket[string-ref] for a string) with no test at all; with ℤ, only the test of the sign remains (a negative index counts from the end); the same for @litchar{{M[i j]}} of a vector of vectors and for the assignments @litchar{{v[i] := x}}. As the annotation is not checked, a negative index declared ℕ is an error of bounds of @racket[vector-ref].

The lengths (@racket[length], @racket[vector-length], @racket[string-length], @racket[bytes-length]…) and @racket[char->integer] have the type ℕ.

For a float, @racket[sqrt], @racket[log] and @racket[expt] stay generic: with a negative float they return a complex number (@racket[(sqrt -4.0)] is @racket[0.0+2.0i]) where @racket[flsqrt] returns @racket[+nan.0]. With an argument of type ℝ⁺ (the base for @racket[expt] and @racket[**]), they become @racket[flsqrt] (also for @racket[√]), @racket[fllog] and @racket[flexpt], which give the same results on the floats that are not negative (@racket[(log -0.0)] is @racket[-inf.0] for both). The pass knows that @racket[abs], @racket[exp], @racket[sqr] and @racket[square] of a float, the square of a float (@litchar{{x ²}}, @litchar{{x ** 2}}, @racket[(expt x 2)], @litchar{{x * x}} of a variable), @racket[flsqrt], and the sum and the product of two ℝ⁺ return a ℝ⁺, so a whole expression can stay in floats:

@codeblock|{
(define (mixed-calls x y)
  (type ℝ x y)
  (list (sqrt (abs {x - y})) (log (exp x)) {(abs x) ** y} (sqrt x)))

(define (root-plus-one s)
  (type ℝ⁺ s)
  {(sqrt s) + 1.0})

(define (hypotenuse x y)
  (type ℝ x y)
  (sqrt {x ² + y ²}))

(define (shifted-root a)
  (type ℝ⁺ a)
  (sqrt {a + 1.0}))

;; code generated by the annotation parser:
(define (mixed-calls x y)
  (list
   (flsqrt (flabs (fl- x y)))
   (fllog (flexp x))
   (flexpt (flabs x) y)
   (sqrt x)))                 ; x can be negative : generic
(define (root-plus-one s) (fl+ (flsqrt s) 1.0))
(define (hypotenuse x y) (flsqrt (fl+ (fl* x x) (fl* y y))))
(define (shifted-root a) (sqrt (fl+ a 1.0)))   ; 1.0 is a ℝ : generic
}|

A literal is a ℝ, not a ℝ⁺: in @racket[shifted-root], @litchar{{a + 1.0}} is a ℝ and @racket[sqrt] stays generic. A difference, a quotient (@racket[(/ 1.0 -0.0)] is @racket[-inf.0]) and another power (@litchar{{x ³}}) are not ℝ⁺ either. An assignment gives its variable the type of the value: after @racket[(type ℝ⁺ s)], @litchar{{s := a + 1.0}} makes @racket[s] a ℝ. As the other types, ℝ⁺ is not checked: a negative float declared ℝ⁺ gives @racket[+nan.0] with @racket[flsqrt], where @racket[sqrt] would give a complex number.

@codeblock|{
(define (f x i)
  (type ℝ x)                          ; flonum?
  (type ℕ i)                          ; a fixnum >= 0
  {x * i ²})
(type [(ℝ) procedure? (ℝ ℕ)] f)
(type #(ℝ) v)                         ; a vector of flonums
(overload-procedure plus (vec?) add-vec (vec? ℝ))
}|

Composite types are equivalent to the bracket notation, both can be used and give the same annotation. A vector of vectors of floats is written:

@codeblock|{
(type #(#(ℝ)) z)          ; z is a vector of vectors of floats

;; the same as, with the bracket notation:
(type vector? z {z[]})    ; z and each z[i] are vectors
(type flonum? {z[][]})    ; each z[i][j] is a float
}|

With either notation the annotation knows that @racket[z] has the type @litchar{#(#(flonum?))} (ℝ enters the tables as @racket[flonum?]), that a row @litchar{{z[i]}} has the type @litchar{#(flonum?)}, and that an element @litchar{{z[i j]}} or @litchar{{z[i][j]}} is a @racket[flonum?]: the operations on the elements become @racket[fl+], @racket[fl*]…, and with indexes of type ℕ the access itself is specialized (see the examples below).

As every annotation, the type neither creates the vector nor is checked: @racket[z] must still be built (with @racket[make-vector], @racket[vector]…), and if it holds an exact integer, @racket[fl*] fails at run time. ℝ is the floats: @racket[1.0] is a ℝ, @racket[1] is not.

A composite type still matches a procedure or an overload expecting the simple container predicate (@racket[vector?], @racket[list?]).

A vector of floats, a matrix of floats (a vector of vectors), a list of floats and a vector whose elements have no given type:

@codeblock|{
(define (sum-vector v)               ; a vector of floats
  (type #(ℝ) v)
  (type ℕ i)
  {s := 0.0}
  (for ({i := 0} {i < (vector-length v)} {i := i + 1})
    {s := s + v[i]})
  s)

(define (double-coefficient M i j)   ; a matrix : a vector of vectors of floats
  (type #(#(ℝ)) M)
  (type ℕ i j)
  {M[i j] * 2.0})

(define (sum-list L)                 ; a list of floats
  (type (ℝ) L)
  (apply + L))

(define (element w i)                ; a vector , the type of its elements not given
  (type #() w)
  (type ℕ i)
  {w[i]})

;; code generated by the annotation parser:
;; (define (sum-vector v)
;;   (:= s 0.0)
;;   (for
;;    ((:= i 0) (fx< i (vector-length v)) (:= i (fx+ i 1)))
;;    (:= s (fl+ s ($bracket-apply-vector-natural$ v i))))
;;   s)
;; (define (double-coefficient M i j)
;;   (fl* ($bracket-apply-vector-natural$ M i j) 2.0))
;; (define (sum-list L) (apply fl+ L))
;; (define (element w i) ($bracket-apply-vector-natural$ w i))
}|

The type of the elements specializes the operations on them (@racket[fl+], @racket[fl*]), the type of the container and of the indexes the access (@litchar{$bracket-apply-vector-natural$} is @racket[vector-ref] with no test). @litchar{#()} is the same type as @racket[vector?]: it is enough when only the access matters, @litchar{{w[i]}} is fast but its value stays of unknown type.

With the types of the elements, the inference knows the result of @racket[vector-map], @racket[map], @racket[apply], @racket[vector->list], @racket[list->vector], @racket[vector] and @racket[list], and specializes the operator or procedure passed to @racket[vector-map], @racket[map] and @racket[apply].

It knows also the result of the procedures of lists and vectors of Scheme (R7RS, SRFI 1) and of Racket whose result is made of the elements of their argument:
@itemlist[
  @item{an element: @racket[car], @racket[first] … @racket[fifth], @racket[cadr], @racket[caddr], @racket[last], @racket[list-ref], @racket[argmin], @racket[argmax], @racket[vector-ref], @racket[vector-argmin], @racket[vector-argmax];}
  @item{a container of the same elements: @racket[cdr], @racket[rest], @racket[cddr], @racket[reverse], @racket[list-copy], @racket[list-tail], @racket[take], @racket[drop], @racket[take-right], @racket[drop-right], @racket[sort], @racket[remove-duplicates], @racket[delete-duplicates], @racket[shuffle], @racket[filter], @racket[remove], @racket[remq], @racket[remv], @racket[delete], @racket[append], @racket[vector-copy], @racket[vector-take], @racket[vector-drop], @racket[vector-sort], @racket[vector-filter], @racket[vector-append];}
  @item{a container of the fill value: @racket[make-list], @racket[make-vector];}
  @item{a container of the values of a procedure on the indexes (of type ℕ): @racket[build-list], @racket[build-vector], whose @racket[lambda] has its parameter typed ℕ and its body specialized;}
  @item{an accumulation: @racket[foldl], @racket[foldr] (Racket), @racket[fold], @racket[fold-right], @racket[reduce], @racket[reduce-right] (SRFI 1).}]
They are listed in the table @racket[container-procedures-types] of @filepath{scheme-types.rkt} (SRFI-105).

As in most typed languages, the type of the elements does not change: with @racket[ANNOTATION-ON], @bold{a vector keeps the type of its initial elements}. So @racket[make-vector] is filled with a value of the type of the elements put after: @racket[(make-vector n 0.0)] for a vector of floats, not @racket[(make-vector n 0)] that is a vector of fixnums (@racket[fx+] would then fail at run time on a float, as the annotation is not checked). The same for @racket[vector]: @racket[(vector 0 0)] is a vector of fixnums. With no fill value, @racket[(make-vector n)], or a fill value of unknown type, the vector is only a @racket[vector?], its elements have no type. The lists of Racket are immutable, their elements never change.

@codeblock|{
(define (f L i)                  ; a list of vectors of fixnums
  (type (#(ℤ)) L)
  (type ℕ i)
  {v := (car L)}                 ; v : #(ℤ)
  {v[i] + 1})

(define (g L)                    ; a list of floats
  (type (ℝ) L)
  {(last (sort L <)) * 2.0})

(define (h n)
  (type ℕ n)
  {b := (build-vector n (λ (i) {i * i}))}   ; b : #(ℤ)
  {b[0] + 1})

;; code generated by the annotation parser:
;; (define (f L i)
;;   (<-vector-vector* v (car L))
;;   (fx+ ($bracket-apply-vector-natural$ v i) 1))
;; (define (g L) (fl* (last (sort L <)) 2.0))
;; (define (h n)
;;   (<-vector-vector* b (build-vector n (λ (i) (fx* i i))))
;;   (fx+ ($bracket-apply-vector-natural$ b 0) 1))
}|

The procedure passed to an accumulation is called on the elements and the accumulator: @racket[(foldl f init L)] calls @racket[f] on an element and the accumulator, whose first value is @racket[init]. The procedure is specialized, and the result typed, only when it returns the type of the accumulator, so that the accumulator keeps its type at each call: the type of @racket[init] gives it, @racket[(foldl + 0.0 L)] for a list of floats. With @racket[(foldl + 0 L)] the first sum is a fixnum plus a float, not a fixnum: nothing is specialized and the code stays generic (it does not fail). For @racket[(reduce f ridentity L)], @racket[f] is called on two elements, and the result is typed when @racket[ridentity] (the result for an empty list) has the type of the elements.

@codeblock|{
(define (sum L)                 ; a list of floats
  (type (ℝ) L)
  {(foldl + 0.0 L) * 2.0})

(define (sum-of-squares L)
  (type (ℝ) L)
  (foldl (λ (x acc) {x * x + acc}) 0.0 L))

(define (squares n)
  (type ℕ n)
  (build-vector n (λ (i) {i * i})))

;; code generated by the annotation parser:
;; (define (sum L) (fl* (foldl fl+ 0.0 L) 2.0))
;; (define (sum-of-squares L) (foldl (λ (x acc) (fl+ (fl* x x) acc)) 0.0 L))
;; (define (squares n) (build-vector n (λ (i) (fx* i i))))
}|

A @racket[lambda] (or @racket[λ]) passed to @racket[vector-map], @racket[map], @racket[vector-for-each] or @racket[for-each] needs no special syntax: its parameters get the types of the elements of the sequences (the context), or the types given by a @racket[type] annotation written in its body, which wins over the context; its body is then specialized, and its return type is deduced from the last expression of the body, so the result of @racket[vector-map] or @racket[map] is still a composite type. For example the dot product of @secref["operators-annotation"], written here with a lambda and with vectors of floats:

@codeblock|{
(define (dot-product v1 v2)
  (type #(flonum?) v1 v2)
  (apply + (vector->list (vector-map (lambda (x y) (* x y)) v1 v2))))

;; code generated by the annotation parser:
;; (apply fl+ (vector->list (vector-map (lambda (x y) (fl* x y)) v1 v2)))

(define (sum-doubles v)       ; v not typed : the lambda is typed in its body
  (apply + (vector->list (vector-map (λ (x) (type flonum? x) (* x 2.0)) v))))

;; code generated by the annotation parser:
;; (apply fl+ (vector->list (vector-map (λ (x) (fl* x 2.0)) v)))
}|

The types of the parameters stay inside the lambda (lexical scope). Only a lambda with a proper list of parameters, one per sequence, is typed; a lambda with a rest parameter, @racket[(lambda args ...)], or with another number of parameters is left unchanged, and so is a lambda whose parameters have no known type: its operators are dispatched at runtime as before. If the return type cannot be deduced (a call to a procedure without type...), typing that procedure with @racket[type] is enough.

The inference propagates these types through the definition and assignment operators (@racket[:=], @racket[<-], @racket[define], ...): the type of the right hand side is remembered for the names being bound on the left hand side.

@defform[#:id :print-type (:print-type expr)]{
At the REPL only, display the type that the type annotation (see @secref["annotation"]) knows for @racket[expr], without evaluating it, as @tt{:print-type} of Typed Racket (the REPL of PLAI also displays the types, only at the REPL). The annotation must be active at the REPL: either the REPL has been started from a file with @racket[ANNOTATION-ON], as @bold{REPL-Scheme-PLUS.rkt}, and the types of the names exported by the module of this file (@racket[provide], @racket[(all-defined-out)]) are known at the REPL, or @racket[ANNOTATION-ON] has been typed alone at the REPL (see the pragmas in the documentation of SRFI 105). In both cases the types of what is typed at the REPL are known. For a procedure annotated with a signature, @racket[:print-type] displays this signature; for a procedure given to an overload (@racket[overload-operator], @racket[overload-procedure]), its signature in the overload (@tt{...} : any number of arguments of this type, @tt{?} : no return type given); for an overloaded operator, all its overloads, one per line.
}

@codeblock|{
> {x := 2.0}
> (:print-type x)
flonum?
> (:print-type {x * 3.0})
flonum?
> (define (g a) {a * 2.0})
> (type [(flonum?) procedure? (flonum?)] g)
> (:print-type g)
(flonum? -> flonum?)
> (:print-type (g 1.5))
flonum?
> (:print-type (list 1 2))
(fixnum?)
> (define (add-n-lists . vn-lst) (apply map + vn-lst))
> (overload-operator + (list?) add-n-lists (list?))
> (:print-type add-n-lists)
(list? ... -> list?)
> (:print-type +)
(fixnum? fixnum? -> fixnum?)
(exact-integer? fixnum? -> exact-integer?)
...
(list? ... -> list?)
...
}|

The type displayed is what the annotation inferred, the type it uses to specialize the operators, not a check: the annotation trusts the declarations. When the annotation does not know the type, @racket[:print-type] displays @tt{unknown type (the annotation does not know it)}; without @racket[ANNOTATION-ON], it displays that the annotation is not active. In a module, @racket[:print-type] is a syntax error.

@subsection[#:tag "overloads-annotation"]{Overloads and type annotation}

When the types are known, the pass resolves at read time the call of an operator or of a procedure overloaded with @racket[overload-operator] or @racket[overload-procedure], and the access to square brackets overloaded with @racket[overload-square-brackets]: the specialized procedure is called directly. Otherwise the choice stays made at run time, as without the pass. An overload is known only after it in the file; in a module that imports an overloaded operator, procedure or object, the overload is restated with the corresponding @racket[require-...] form, a pure no-op at run time.

@subsubsection[#:tag "operators-annotation"]{Operators}

Example (the annotated example @tt{examples/overload-example+.rkt} of the SRFI-110 package): the operator @racket[*] is overloaded (see @racket[overload-operator]) with the dot product of two vectors and with the product of a number by a vector, written with the higher-order functions @racket[vector-map] and @racket[apply]. The lists @racket[(fixnum?)] and @racket[(#(fixnum?))] given to @racket[overload-operator] are the return types of the specialized procedures, used only by the inference: they change nothing at run time. An expression with several @racket[*] is computed by left associativity, each step using the overload that matches its two arguments:

@codeblock|{
ANNOTATION-ON

(define (dot-product v1 v2)
  (type #(fixnum?) v1 v2) ; vectors of fixnums (or with the bracket notation : (type vector? v1 v2) (type fixnum? {v1[]} {v2[]}))
  (apply + (vector->list (vector-map * v1 v2))))

(overload-operator * (fixnum?) dot-product (vector? vector?))

(define (mult-scalar-vect x v)
  (define (mult-scalar-coord c)
    (type fixnum? x c)
    {x * c})
  (vector-map mult-scalar-coord v))

; return type for the parser : vector of fixnums ; the predicates stay real predicates (called at runtime)
(overload-operator * (#(fixnum?)) mult-scalar-vect (fixnum? vector?))

{#(2 3 4) * #(3 4 5)}
38

{5 * #(2 3 4)}
'#(10 15 20)

{5 * #(2 3 4) * #(3 4 5)} ; computed as {{5 * #(2 3 4)} * #(3 4 5)}
190

{5 * #(2 3 4) * #(3 4 5) * #(1 2 3 4)}
'#(190 380 570 760)

{(dot-product #(1 2) #(3 4)) + 1}
12
}|

With the types of the elements and the return types, the inference specializes the bodies and follows the chain of overloaded operators, resolving each one at read time; a return type can be a composite type, as @racket[(#(fixnum?))] for a vector of fixnums. The annotated code of this example is:

@codeblock|{
;; code generated by the annotation parser:
(define (dot-product v1 v2)
  (apply fx+ (vector->list (vector-map fx* v1 v2))))

(define (mult-scalar-vect x v)
  (define (mult-scalar-coord c) (fx* x c))
  (vector-map mult-scalar-coord v))

;; {5 * #(2 3 4) * #(3 4 5) * #(1 2 3 4)} :
(mult-scalar-vect (dot-product (mult-scalar-vect 5 #(2 3 4)) #(3 4 5)) #(1 2 3 4))

;; {(dot-product #(1 2) #(3 4)) + 1} :
(fx+ (dot-product #(1 2) #(3 4)) 1)
}|

Every @racket[*] has been replaced by the specialized procedure (the return type @racket[(fixnum?)] of the dot product allows the next @racket[*] to be resolved to @racket[mult-scalar-vect]), and @racket[+] by @racket[fx+]: no dispatch is left at run time. The predicates given to @racket[overload-operator] remain used at run time for the calls that the inference cannot resolve.

For @racket[apply] the number of elements of the list is unknown, so only the n-ary overloads are used; note that @racket[(apply fl+ '())] is @racket[0.0] where @racket[(apply + '())] is @racket[0].

@defform*[((require-overload-operator name special (predicate ...))
           (require-overload-operator name (return-predicate ...) special (predicate ...))
           (require-overload-operator name module-name special (predicate ...))
           (require-overload-operator name module-name (return-predicate ...) special (predicate ...)))]{
When an already overloaded operator is imported from another module, the annotation pass of the current module has not seen the @racket[overload-operator] forms that were evaluated there, so it cannot specialize the calls that rely on those overloads. Restating them with @racket[require-overload-operator], with the same forms as @racket[overload-operator] (one predicate for an n-ary overload, two for a binary one), feeds that information back to the pass. It is a pure no-op at run time (the real overload tables were already filled by the imported module).}

@codeblock|{
(require (rename-in flomat (repeat repeat-flomat)))
(require (rename-in Scheme+ (· ·bak) (× ×bak)))
(require "matrix+.rkt")   ; provides an already overloaded  ·

ANNOTATION-ON

;; tell the inference of THIS module what matrix+.rkt did to  ·
(require-overload-operator · multiply-flomat-vector (flomat? vector?))
}|

@defform*[((require-overload-unary-operator name special (predicate))
           (require-overload-unary-operator name (return-predicate ...) special (predicate)))]{The same for an operator overloaded with @racket[overload-unary-operator] in another module.}

@subsubsection[#:tag "procedures-annotation"]{Procedures}

A call of a procedure overloaded with @racket[overload-procedure] is resolved the same way, from the types of the arguments and the return types given to @racket[overload-procedure].

@defform[(require-overload-procedure name ...)]{The same for a procedure overloaded with @racket[overload-procedure] in another module: same forms as @racket[overload-procedure], a pure no-op at run time.}

@subsubsection[#:tag "square-brackets-annotation"]{Square brackets}

When the type of an object is known, an access @litchar{{M[i j]}} to square brackets overloaded with @racket[overload-square-brackets] calls directly the getter, and an assignment @litchar{{M[i j] := x}} calls directly the setter, instead of searching at run time the overload whose predicates accept the arguments. The type of the object is given by a @racket[type] annotation (@racket[(type matrix-vect? M)], or @litchar{(type matrix-vect? {M[]})} for the elements of a vector @racket[M]) or found by the inference.

The overload is chosen by the type of the object and by the number of indexes: @litchar{{M[i]}} and @litchar{{M[i j]}} can be two different overloads (a line and an element of a matrix). The indexes are counted as the square brackets macros cut them: @litchar{{M[k (j + 1)]}} has 2 indexes, @litchar{{M[i + 1]}} has 1. When the overload can not be chosen (no overload or more than one for this number of indexes, or a slice with @litchar{:}), the access stays dispatched at run time, as without the pass. The predicates of the indexes are not checked by the pass.

With a return predicate in the overload, the type of the element is known too, and the operators around it are specialized:

@codeblock|{
ANNOTATION-ON

(struct fmat (v)) ;; a matrix of flonums
(define (fmat-ref M lin col) {v := (fmat-v M)} {v[lin][col]})
(define (fmat-set! M lin col x) {v := (fmat-v M)} {v[lin][col] := x})
(define (fmat-line-ref M lin) {v := (fmat-v M)} {v[lin]})
(define (fmat-line-set! M lin line) {v := (fmat-v M)} {v[lin] := line})

(overload-square-brackets (flonum?) fmat-ref fmat-set! (fmat? number? number?)) ; an element is a flonum
(overload-square-brackets (vector?) fmat-line-ref fmat-line-set! (fmat? number?)) ; a line is a vector

(define (ftrace M)
  (type fmat? M)
  {M[0 0] + M[1 1]})

(define (fscale! M a) ; multiply all the coefficients of the matrix by a
  (type fmat? M)
  (type flonum? a)
  {n := (vector-length (fmat-v M))} ; number of lines
  {p := (vector-length M[0])} ; number of columns : M[0] is a line , a vector
  (for ({i := 0} {i < n} {i := i + 1})
    (for ({j := 0} {j < p} {j := j + 1})
      {M[i j] := a * M[i j]})))

;; code generated by the annotation parser and the square brackets macros:
(define (ftrace M)
  (fl+ (fmat-ref M 0 0) (fmat-ref M 1 1)))

;; in fscale! , M[0] is (fmat-line-ref M 0) and the assignment in the loops is
(fmat-set! M i j (fl* a (fmat-ref M i j)))
}|

Without the return predicate, the type of the elements can be given object by object, with @litchar{(type flonum? {M[]})} (a declaration has priority over the return predicate). An overload is known only after it in the file; in a module that imports the object, restate it with @racket[require-overload-square-brackets].

The deep learning program @tt{examples/racket/exo_retropropagationNhidden_layers_matrix_v3_by_vectors+.rkt} uses the matrices of @tt{examples/racket/matrix-by-vectors+.rkt}, a general library with overloaded square brackets and without return predicates. The program restates the overloads with the return types of its own matrices of flonums, and declares the type of the elements of its vector of matrices @racket[M] and of the parameter @racket[M_i_o]:

@codeblock|{
(require "matrix-by-vectors+.rkt")

(require-overload-square-brackets (flonum?) matrix-vect-ref matrix-vect-set! (matrix-vect? number? number?)) ; M[i j] is a flonum
(require-overload-square-brackets (vector?) matrix-vect-line-ref matrix-vect-line-set! (matrix-vect? number?)) ; M[i] is a vector

(type matrix-vect? {M[]}) ; elements of M are matrix-vect : M[i][k j] calls matrix-vect-ref directly

(type matrix-vect? M_i_o) ; M_i_o[j (i + 1)] calls matrix-vect-ref / matrix-vect-set! directly

{M_i_o[j (i + 1)]  <-  M_i_o[j (i + 1)] + η · z_input[i] · მzⳆმz̃(z_output[j] z̃_output[j]) · ᐁ_i_o[j]}
;; becomes, η and the elements of the vectors being declared flonum? :
;; (matrix-vect-set! M_i_o j (fx+ i 1) (fl+ (matrix-vect-ref M_i_o j (fx+ i 1)) (fl* η ...)))
}|

Compared to the search at run time, this divides by about 2 the time of the learning loop of this program.

@defform*[((require-overload-square-brackets getter setter! (pred-obj pred-coord ...))
           (require-overload-square-brackets (return-predicate) getter setter! (pred-obj pred-coord ...)))]{The same for square brackets overloaded with @racket[overload-square-brackets] in another module (see @secref["square-brackets-annotation"]): same forms as @racket[overload-square-brackets], a pure no-op at run time. The return predicate can be given here even if the module that defines the overload does not give it: a general library of matrices can contain exact numbers, while the matrices of the program that imports it are matrices of flonums.}

@subsection[#:tag "annotation-example"]{A complete example}

The deep learning program @tt{examples/racket/exo_retropropagationNhidden_layers_matrix_v3+.rkt} (back propagation in a neural network with N hidden layers) uses every construct above : @racket[ANNOTATION-ON], @racket[require-overload-operator] for the imported @racket[·], overloading of @racket[+] for vectors, and many @racket[type] annotations on the network fields (@racket[z], @racket[z̃], @racket[M], @racket[ᐁ], ...) and on the local variables of the inner loops.

@codeblock|{
ANNOTATION-ON

(require-overload-operator · multiply-flomat-vector (flomat? vector?))

; overloading of +, binary ( (vector?) is the return type, for the inference )
(overload-operator + (vector?) vector-append (vector? vector?))

(type [(flonum?) procedure? ()] random)       ; random with no argument returns a float

(define (carre x)
  (type flonum? x)
  {x · x})
(type [(flonum?) procedure? (flonum?)] carre)

(define (σ z̃)
  (type flonum? z̃)
  {1.0 / (1.0 + (exp (- z̃)))})
}|

and, inside the network class :

@codeblock|{
(type vector? nc)
(type fixnum? {nc[]})

(type vector? z {z[]})   ; z and every z[i]
(type flonum? {z[][]})   ; every z[i][j]

(type vector? M)         ; a vector ...
(type flomat? {M[]})     ; ... of floating point matrices ...
(type flonum? {M[][]})   ; ... of floats
}|

With those annotations the line

@codeblock|{
{z̃[i + 1] ← M[i] · z_1}
}|

is inferred to be a @racket[flomat?] matrix times a @racket[vector?] : the overloaded @racket[·] is resolved at read time to @racket[multiply-flomat-vector], @litchar{{M[i]}} becomes a direct @racket[$bracket-apply-vector$] access, and the loops of the back propagation compile down to @racket[fl+] / @racket[fl*] and to direct @racket[$bracket-apply-flomat$] accesses instead of overloaded, dynamically dispatched operators.

Nothing changes in the way you write the code : annotating a whole procedure is just a few @racket[type] lines at its top, the body stays exactly the infix Scheme+ you would write without annotation. Here is the complete weight update procedure of that example, annotated :

@codeblock|{
(define (modification_des_poids M_i_o η z_input z_output z̃_output ᐁ_i_o მzⳆმz̃)

  (type vector? z_input z_output z̃_output ᐁ_i_o)
  (type flomat? M_i_o)
  (type flonum? η {ᐁ_i_o[]} {z_input[]} {z_output[]} {z̃_output[]} {M_i_o[]})

  {(len_layer_output len_layer_input_plus1forBias) ← (dim M_i_o)}
  {len_layer_input ← len_layer_input_plus1forBias - 1}

  (for-racket ([j (range len_layer_output)])         ; line

              (type fixnum? i)
              (for-racket ([i (range len_layer_input)])   ; column, bias excluded
                          {M_i_o[j (i + 1)] ← M_i_o[j (i + 1)]
                                              + η · z_input[i] · მzⳆმz̃(z_output[j] z̃_output[j]) · ᐁ_i_o[j]})

              ; and update the bias
              {M_i_o[j 0] ← M_i_o[j 0] - ((- η) · 1.0 · მzⳆმz̃(z_output[j] z̃_output[j]) · ᐁ_i_o[j])}))
}|

The annotation and inference pass consumes the @racket[type] forms and rewrites the whole procedure, at read time, to :

@codeblock|{
(define
  (modification_des_poids M_i_o η z_input z_output z̃_output ᐁ_i_o მzⳆმz̃)
  (← (len_layer_output len_layer_input_plus1forBias) (dim M_i_o))
  (← len_layer_input (- len_layer_input_plus1forBias 1))
  (for-racket
   ((j (range len_layer_output)))
   (for-racket
    ((i (range len_layer_input)))
    (←
     ($bracket-apply-flomat$ M_i_o j (fx+ i 1))
     (fl+
      ($bracket-apply-flomat$ M_i_o j (fx+ i 1))
      (fl*
       η
       ($bracket-apply-vector$ z_input i)
       (მzⳆმz̃
        ($bracket-apply-vector$ z_output j)
        ($bracket-apply-vector$ z̃_output j))
       ($bracket-apply-vector$ ᐁ_i_o j)))))
   (←
    ($bracket-apply-flomat$ M_i_o j 0)
    (fl-
     ($bracket-apply-flomat$ M_i_o j 0)
     (fl*
      (fl- η)
      1.0
      (მzⳆმz̃
       ($bracket-apply-vector$ z_output j)
       ($bracket-apply-vector$ z̃_output j))
      ($bracket-apply-vector$ ᐁ_i_o j))))))
}|

Every @racket[·] of the two inner loops has become an @racket[fl*], every @litchar{{i + 1}} index a @racket[fx+], every @litchar{{M_i_o[j (i + 1)]}} a direct @racket[$bracket-apply-flomat$] access and every @litchar{{z_input[i]}} a direct @racket[$bracket-apply-vector$] access ; the @racket[type] annotations themselves are gone and the assignment stays a plain in place matrix update. No run time type dispatch is left in the hot loop.

The curve below is computed by a neural network trained by this retropropagation, whose weights are updated by @racket[modification_des_poids] : it learns the damped harmonic oscillator y = sin(x)·e^(-x/10)/1.7.

@(define-runtime-path neural-harmonic-oscillator-and-equation-computed-harmonic-oscillator "neural-harmonic-oscillator-and-equation-computed-harmonic-oscillator.png")
@centerline{@image[#:scale 1.1 neural-harmonic-oscillator-and-equation-computed-harmonic-oscillator]}

See the full program at @url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket/blob/main/examples/racket/exo_retropropagationNhidden_layers_matrix_v3+.rkt"] and the simpler @url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket/blob/main/examples/racket/matrix-by-vectors+.rkt"] .


@section[#:tag "sweet-expressions"]{Sweet expressions}

Scheme+ can be written with the sweet-expressions of SRFI 110.

Here are some examples, the definitions of a module that begins with:

@codeblock|{
#lang reader SRFI-110

module sweet-example racket
  ;
  require Scheme+
  ANNOTATION-ON
}|

@codeblock|{
; ------------------------------------------------------------------ Fibonacci , the recursive version
; f{x} is (f x) : fib{n - 1} is (fib (- n 1)) , while fib(n - 1) would be (fib n - 1)
define (fib n)
  if {n < 2}
    n
    {fib{n - 1} + fib{n - 2}}
}|

@litchar{fib(25)} gives @racket[75025].

@codeblock|{
; ------------------------------------------------------------------ the prime numbers : sieve of Eratosthenes
define (primes-below n)
  type ℕ n i j              ; the indexes : vector-ref with no test
  {sieve := (make-vector n #t)}
  {primes := '()}
  for ({i := 2} {i < n} {i := i + 1})
    when sieve[i]
      {primes := (cons i primes)}
      for ({j := i * i} {j < n} {j := j + i})
        {sieve[j] := #f}
  reverse primes
}|

@litchar{primes-below(50)} gives @racket['(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)].

@codeblock|{
; ------------------------------------------------------------------ product of two matrices (vectors of lines)
define (matrix-product A B)
  type #(#(ℝ)) A B C        ; vectors of vectors of flonums
  type ℕ n m p i j k        ; the indexes : vector-ref with no test
  {n := (vector-length A)}
  {m := (vector-length B)}
  {p := (vector-length B[0])}
  {C := (make-vector n)}
  for ({i := 0} {i < n} {i := i + 1})
    {C[i] := (make-vector p 0.0)}
    for ({j := 0} {j < p} {j := j + 1})
      for ({k := 0} {k < m} {k := k + 1})
        {C[i][j] := C[i][j] + A[i][k] * B[k][j]}
  C
}|

with @racket[{M := #(#(1.0 2.0) #(3.0 4.0))}], @litchar{matrix-product(M M)} gives @racket[#(#(7.0 10.0) #(15.0 22.0))]. With the types (@secref["type-form"]), the pass writes the operators of the flonums and of the fixnums, and the accesses with no test: @litchar{C[i][j] + A[i][k] * B[k][j]} is @racket[(fl+ (vector-ref (vector-ref C i) j) (fl* (vector-ref (vector-ref A i) k) (vector-ref (vector-ref B k) j)))], and @litchar{{i < n}} is @racket[(fx< i n)].

@codeblock|{
; ------------------------------------------------------------------ Fibonacci : def allows return
def (fibonacci n)
  when {n < 2}
    return n
  {a := 0}
  {b := 1}
  for ({k := 2} {k <= n} {k := k + 1})
    {(a b) := (values b {a + b})} ; assignment of multiple values : the right hand side uses the old values
  b
}|

@litchar{fibonacci(30)} gives @racket[832040].

@codeblock|{
; ------------------------------------------------------------------ square root by the method of Newton
; with the type of x the annotation pass uses the operators of the flonums (fl* , fl- , fl/ , ...)
define (newton-sqrt x)
  type flonum? x
  {r := x / 2.0}
  while {abs{r * r - x} > 1e-12}
    {r := (r + x / r) / 2.0}
  r
}|

@litchar{newton-sqrt(2.0)} gives @racket[1.414213562373095].

@codeblock|{
; ------------------------------------------------------------------ symbolic computation : a quoted formula
; in the region CARE-OF-QUOTES-OFF the quoted lists are infix expressions (with the precedence of the operators)
CARE-OF-QUOTES-OFF
{carry := '(Bₖ · Bₖ₋₁ ⊕ Cₖ · (Bₖ ⊕ Bₖ₋₁))}
CARE-OF-QUOTES-ON
}|

@racket[carry] is @racket['(⊕ (· Bₖ Bₖ₋₁) (· Cₖ (⊕ Bₖ Bₖ₋₁)))].

@codeblock|{
; ------------------------------------------------------------------ binomial coefficient : the postfix factorial
define (binomial n k)
  {(n !) / ((k !) * ((n - k) !))}
}|

@litchar{binomial(10 3)} gives @racket[120].


@section[#:tag "pragmas"]{Pragmas}

A pragma is a bare symbol written in the source code and recognized by the Curly Infix reader (SRFI-105 or SRFI-110) while it reads the file, not by the Racket compiler: it does not remain in the code.

Scheme+ has two pragmas, @racket[ANNOTATION-ON] and @racket[ANNOTATION-OFF], which turn the type annotation and inference pass on or off, by region (the rest of the list where they are written), see @secref["annotation-on"].

The pragmas @racket[CARE-OF-QUOTES-ON] and @racket[CARE-OF-QUOTES-OFF] act by region too, on the quoted lists. With @racket[CARE-OF-QUOTES-ON], the state at the beginning of each file, the quotes are respected: a quoted list is data (see @secref["both"]). @racket[CARE-OF-QUOTES-OFF] opens a region for the symbolic computation: the quoted and quasiquoted lists are infix expressions, at all levels, with the precedence of the operators; in a quasiquote the parts @litchar{,…} and @litchar{,@"@"…} are code and stay as written. The region goes until @racket[CARE-OF-QUOTES-ON] or the end of the list where the pragma is written (written in the body of a procedure, it ends with this body); written at the top level of the file, before the module form, it acts on the whole module.

@codeblock|{
CARE-OF-QUOTES-OFF
{Ck+1 := '(Bₖ · Bₖ₋₁ ⊕ Cₖ · (Bₖ ⊕ Bₖ₋₁))}   ; '(⊕ (· Bₖ Bₖ₋₁) (· Cₖ (⊕ Bₖ Bₖ₋₁)))
{p := `(,a · x ⊕ y)}                       ; `(⊕ (· ,a x) y)
CARE-OF-QUOTES-ON
{ops := '(+ - * /)}                         ; data
}|

In a region @racket[CARE-OF-QUOTES-OFF] every quoted list is read as an infix expression: a list of data such as @racket['(+ - * /)] gives an error and @racket['(1 2 3 +)] becomes @racket['(+ 1 2 3)]. Keep the regions short, around the formulas only.

The other pragmas are the few ones of the SRFI 105 Curly infix reader described in its doc: @hyperlink["https://docs.racket-lang.org/SRFI-105/index.html"]{SRFI-105 "Curly Infix" for Racket}, or the few pragmas of SRFI 110.


@section[#:tag "makefile"]{Using Makefile and debugging Scheme+ program}

The Curly Infix (SRFI 105) reader parser can now display the line, column, and character offset in the source file if a parsing error arises at this stage.
The Racket GUI can not display the line of error in a Scheme+ program. The reason is that the Scheme+ program is pre-parsed by the SRFI 105 reader and the Racket compiler has no access to the original lines of code and does not even display the error line relative to the resulting parsed line of code.

If you have to debug your source code you must generate a Scheme file
from your Scheme+ file with @bold{curly-infix2prefix4racket} in SRFI 105 package and load the
result file in Racket GUI. Then you will have all the debug
information of Racket on the scheme file.

Another solution is to simply copy/paste the pure Racket
code generated by the Curly Infix parser and run it like a normal Racket program.
Again you will have all the information about errors with the true line number displayed.

Another method is to use the Makefile provided:

@url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket/blob/main/examples/racket/Makefile"]

Put the Makefile in the same directory as your Scheme+ program and type in the terminal @bold{make}.
The Makefile will parse all the Scheme+ programs in the same directory and generate the scheme version in @bold{MODULE_DIRECTORY=parsed_files_directory}. You can then just load and run the parsed files in Racket GUI to use all the Racket features and tools (macro stepper, etc) for debugging.

A generated file is the code that the reader gives to the Racket compiler: when the Scheme+ file has the pragma @racket[ANNOTATION-ON] it contains the annotated code (see @secref["annotation"]), and it never contains the @racket[type] forms nor the pragmas.

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

More examples can be found in this directory and subdirectories:

@url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket/blob/main/examples/"]


@(define-runtime-path neural-sine-and-sine "neural-sine-and-sine.png")
@centerline{@image[#:scale 1.1 neural-sine-and-sine]}

@section[#:tag "scheme-forms"]{Scheme forms : technical notes}

Scheme code written without @racket[{ }] and outside a region @racket[ANNOTATION-ON] is never modified. This section gathers how the Curly Infix reader (inside @racket[{ }]) and the type annotation pass (in a region @racket[ANNOTATION-ON]) treat the forms of Scheme they meet, and how the type annotation and inference pass itself is made.

@subsection[#:tag "scheme-forms-curly"]{Inside the curly brackets @racket[{ }]}

@itemlist[
  @item{The clauses of a @racket[cond], the parameters of a @racket[lambda] and the variables of a @racket[do] are not parsed as a whole, only the expressions they contain are parsed: @racket[(#t cdr)] or @racket[(else !)] in a @racket[cond] and @racket[(lambda (n !) n)] stay as written.}
  @item{The lists of datums of a @racket[case] are data: in @racket[(case op ((+ -) 'add) ...)] the list @racket[(+ -)] is not parsed, only the key and the expressions of the clauses are.}
  @item{The bindings of @racket[let], @racket[let*], @racket[letrec] are not treated: with a variable declared with a type which is not a procedure, for example @racket[(type flonum? x)], @racket[(let ((x car)) ...)] written inside @racket[{ }] would become @racket[(let ((car x)) ...)] (in Scheme+ a local variable is defined with @racket[:=] or @racket[<-]).}
  @item{The syntax quotes are data: @litchar{#'(…)}, @litchar{#`(…)} and @litchar{(quote-syntax …)} are not parsed inside @racket[{ }] and are not rewritten by the type annotation pass: @litchar{#'(a + b)} stays @racket[(a + b)] and @litchar{#'(+ 1 2)} is not folded to @racket[3]. As with a quote, only the @racket[{ }] curly brackets inside them are infix: @litchar{#'{a + b}} is @litchar{#'(+ a b)}.}
  @item{A name defined as a procedure is a procedure for the parser in the whole file: a variable of the same name in another procedure is taken as this procedure.}]

@subsection[#:tag "scheme-forms-annotation"]{In a region @racket[ANNOTATION-ON]}

@itemlist[
  @item{The pass does not enter the macro definitions (@racket[define-syntax], @racket[define-syntax-rule], @racket[define-for-syntax], @racket[begin-for-syntax], and the macros defined by @racket[let-syntax] and @racket[letrec-syntax], whose body is annotated): their patterns and templates are not code of the current scope, so the types known there do not apply to them. Only their @racket[type] forms are removed.}
  @item{A parameter of a procedure or of a @racket[lambda], a variable of a @racket[do] loop, of @racket[receive], of @racket[let], @racket[let*], @racket[letrec], @racket[letrec*], of a named @racket[let], of @racket[let-values] or @racket[let*-values], and a variable defined or declared without a known type hide the type of an outer variable of the same name. So Scheme code keeps its meaning: @racket[(let ((a 1.5) (b 2.5)) (+ a b))] stays a generic @racket[+] even if @racket[a] and @racket[b] are fixnums outside.}
  @item{The lists of datums of a @racket[case] are data: in @racket[(case op ((+) 'add) ((*) 'mul) ...)] the lists @racket[(+)] and @racket[(*)] are not folded, only the key and the expressions of the clauses are annotated.}]

@subsection[#:tag "bootstrapping"]{The type annotation and inference pass written in Scheme+ : bootstrapping}

The type annotation and inference pass is itself written in Scheme+, by the method of @hyperlink["https://en.wikipedia.org/wiki/Bootstrapping_(compilers)"]{bootstrapping} (as a compiler written in its own language): its source is @tt{bootstrapping/annot+.rkt} of the SRFI-105 package, with the curly-infix notation, the square brackets and all the features of Scheme+. The installed reader converts it to a Racket Scheme module, @tt{annot+.rkt} at the root of the package, which is the pass used by the reader. The reader @tt{main.rkt} is made the same way from @tt{bootstrapping/main+.rkt}. So each new version of the pass is converted by the previous one.


@section[#:tag "emacs"]{Emacs configuration for syntax highlighting of Scheme+}

Add the line below in your .emacs configuration file:

@codeblock|{
(font-lock-add-keywords 'scheme-mode
  '(("\\<\\(define\\|def\\|return\\|return-rec\\|:=\\|<-\\|condx\\|then\\|else\\)\\>" . font-lock-keyword-face)))
}|

If you use @hyperlink["https://www.racket-mode.com/"]{Emacs Racket mode} you can add the same as above too in your .emacs file but then replace 'scheme-mode with 'racket-mode.@hyperlink["https://www.racket-mode.com/"]{Emacs Racket mode} is great for closing the corresponding curly parenthesis.

Not all fonts support superscript characters, Monospace (Bold) with Emacs does.

@section[#:tag "thoughts"]{Thoughts}

@define-footnote[my-note make-my-note]

@; the other lisp logo at the bottom right of the doc , in the footnote (what follows make-my-note is not rendered)
@(define-runtime-path lisplogo-alien "lisplogo_128.png")

I hope this documentation is not a write-only@my-note{ « APL is a write-only language. I can write programs in APL, but I can’t read any of them ». Roy Keir @elem[#:style (make-style #f (list (attributes '((style . "display: block; text-align: right; margin-top: 2em;")))))]{@image[#:scale 0.63 lisplogo-alien]}@elem[#:style (make-style #f (list (attributes '((style . "display: block; text-align: right; font-size: 0.5em;")))))]{Public Domain Lisp Logo Set By Conrad Barski, M.D.}} one and will be useful for any reader.


@make-my-note[]
