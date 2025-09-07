#lang scribble/manual

	   
@title[#:style '(toc)]{Scheme+ for Racket}

@author[(author+email "Damien MATTEI" "Damien.MATTEI@univ-cotedazur.fr")]

source code: @url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket"]


@defmodule[SRFI-105 #:reader]{
This reader package provides the SRFI-105 Curly Infix reader/parser.
}



@defmodule[Scheme+]{
This package provides the Scheme+ language definitions.
}



Scheme+ is an extension of the syntax of the Scheme language.

Scheme+ adds to Scheme a way to use also infix notation with a compatibility near 100% and not as a sub-system of Lisp syntax as it is often done but with a complete integration in the Scheme reader/parser system and also for Racket at REPL (Read Eval Print Loop).

Scheme+ is to Scheme what a concept-car is to automobile.Scheme+ is a concept-language.It is ideally what should be a modern Scheme.

Scheme+ makes it easy the assignment of Scheme objects in infix (works also in prefix) notation with a few new operators <- (or: ← , :=),⥆ (or <+) for definitions.

Scheme+ makes it easy the access and assignment for arrays,strings,hash tables,etc by allowing the classic square brackets [ ] of other languages.


@table-of-contents[]


@section[#:tag "installation"]{Scheme+ Installation and Depandencies}

Scheme+ can be installed via the Racket Package system or downloaded from Github.
Scheme+ is designed to be used with the package SRFI-105 for Racket which is a curly infix reader also available in the same way described above.


@section[#:tag "installation"]{Scheme+ and Curly Infix SRFI-105 REPL (Read Eval Print Loop)}

As Scheme+ is designed to be used with the Curly infix syntax of SRFI-105, the latter one requires an external reader/parser and a specific REPL (Read Eval Print Loop).
The REPL must be loaded in Racket GUI or invoked in the shell if you work in command line mode.
The REPL file can be found in the source code of Scheme+ or SRFI-105 in the @bold{src/} sub-directory, his name is @bold{REPL-Scheme-PLUS.rkt}.



@section[#:tag "syntax"]{Scheme+ Syntax and Conventions}

@subsection[#:tag "curly"]{Curly Infix notation with { }}

In general Scheme+ use the same convention for infix expression than SRFI-105 Curly Infix that is an infix expression is between curly parenthesis { }.
But infix sub-expressions are allowed to be between normal parenthesis ( ). Infix or prefix is then autodetected.In case of ambiguities { } force infix mode. Inside curly infix expression surrounded by { } parenthesis associativity and operator precedence are applied.

@codeblock|{
#reader SRFI-105
(require Scheme+)
> {3 * 5 + 2}
(+ (* 3 5) 2) ; displayed by REPL/parser
17
}|

In the other examples the display of the REPL/parser in prefix will generally be removed from examples for clarity, also the prompt > and the #<eof> (end of file).

@codeblock|{
#lang reader SRFI-105
(require Scheme+)
{3 · 5 + 2 ³}
23
}|

In the following sections i will omit to rewrite the directives about lang or reader and the requirement in each example to keep the code and the web page compact.

An example of inner round brackets inside curly brackets in a full infix expression:

@codeblock|{
{3 * (2 + 4) - 1}
17
}|

Example of a mixed infix and prefix expression autodetected:

@codeblock|{
{3 * (+ 2 4) - 1}
17
}|

Another example:
@codeblock|{
{3 ² + 2 · 3 · 5 + 5 ²}
64
}|


@subsection[#:tag "square"]{Square Bracket notation with [ ]}

The square bracket notation can be used with any object that can be indexed or retrieved by a key: it works with vectors,strings of characters, arrays, hash tables,etc.


@defform[#:id \[\] {T[k]}]{
Return value of vector or in general object T indexed by k.
}

A slice notation with @bold{:} is available.It allows the same feature as the slicing of Python language and sometimes more.

@defform[#:id \: {T[begin : end]}]{
Slicing of vectors or strings.
}


@defform[#:id :: {T[begin : end : step]}]{
Slicing of vectors or strings.
}

@section[#:tag "reference"]{Scheme+ Reference}


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

Note: this is rarely used,instead assignment macros are used, but it could be usefull in case a variable used in a block must be used outside the scope of the block too.

By default the declared variables are set to @bold{NIL} : @bold{'()}

@codeblock|{
(declare x)
x
'()
}|

@subsection[#:tag "definitions"]{Definitions}


@defform[#:id <+ {name <+ value}]{
Define new variable @racket[name] and assign value @racket[value].
}

@codeblock|{
{x <+ 7}
x
7
}|

Note: this is almost never used in Racket,because Scheme+ for Racket as a special feature of autodetection if a variable needs to be defined before assignment.

There is some alias of this:

@defform[#:id ⥆ {name ⥆ value}]{}

Note: a lot of operators of Scheme+ that works left to right exist in the opposite sense, i will not document them as it is obvious,example:

@defform[#:id ⥅ {value ⥅ name}]{}

Also a lot of Scheme+ operator have infix n-arity capability , i will not completely document them at the beginning of this document but in other parts later.

@codeblock|{
{name ⥆ name2 ⥆ 7}
}|

@subsection[#:tag "assignment"]{Assignments}


@defform[#:id <- {name <- value}]{
Assign to the variable @racket[name] the value @racket[value].
}

@codeblock|{
{x <- 7}
x
7
}|

There is some alias of this:

@defform[#:id := {name := value}]{}

@codeblock|{
{index := index + 1}
}|


@defform[#:id ← {name ← value}]{}

@codeblock|{
{z ← 1.13+1.765i}
}|

You can also use the assignment or definition operators for creating tuples (like in Python) of values:

@codeblock|{
{(x y z) <- (values 1 2 3)}
(list x y z)
'(1 2 3)
}|

Example:

@codeblock{
{M_i_o[j {i + 1}]  <-  M_i_o[j {i + 1}] + η · z_input[i] · მzⳆმz̃(z_output[j] z̃_output[j]) · ᐁ_i_o[j]}
}

DOCUMENTATION TO BE CONTINUED...


Another documentation is hosted on Github:

@hyperlink["https://github.com/damien-mattei/Scheme-PLUS-for-Racket/blob/gh-pages/README.md"]{Documentation of Scheme+ for Racket on Github pages}
