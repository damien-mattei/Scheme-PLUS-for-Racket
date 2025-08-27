#lang scribble/manual

	   
@title[#:style '(toc)]{Scheme+ for Racket}

@author[(author+email "Damien Mattei" "damien.mattei@gmail.com")]

source code: @url["https://github.com/damien-mattei/Scheme-PLUS-for-Racket"]





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

@section[#:tag "syntax"]{Scheme+ Syntax and Conventions}

In general Scheme+ use the same convention for infix expression than SRFI-105 Curly Infix that is an infix expression is between curly parenthesis { }.
But infix sub-expressions are allowed to be between normal parenthesis ( ). Infix or prefix is then autodetected.In case of ambiguities { } force infix mode.

@codeblock|{
#reader SRFI-105
(require Scheme+)
{3 * 5 + 2}
17
}|


@codeblock|{
#lang reader SRFI-105
(require Scheme+)
{3 · 5 + 2 ³}
23
}|


@section[#:tag "reference"]{Scheme+ Reference}


@subsection[#:tag "declarations"]{Declarations}

@defform[(declare name1 name2 ...)]{
Declare new variables named @racket[name1],@racket[name2],....
}

Note: this is rarely used,instead assignment macros are used but could be usefull in case a variable used in a block must be used outside too.

@subsection[#:tag "definitions"]{Definitions}


@defform[#:id <+ {name <+ value}]{
Define new variable @racket[name] and assign value @racket[value].
}

Note: this is almost never used in Racket,because Scheme+ for Racket as a special feature of autodetection if a variable needs to be defined before assignment.

There is some alias of this:

@defform[#:id ⥆ {name ⥆ value}]{}

Note: a lot of operators of Scheme+ that works left to right exist in the opposite sense, i will not document them as it is obvious,example:

@defform[#:id ⥅ {value ⥅ name}]{}

@subsection[#:tag "assignment"]{Assignments}


@defform[#:id <- {name <- value}]{
Assign to the variable @racket[name] the value @racket[value].
}

@codeblock|{
#lang reader SRFI-105
(require Scheme+)
{x <- 7}
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

Example:

@codeblock{
{M_i_o[j {i + 1}]  <-  M_i_o[j {i + 1}] + η · z_input[i] · მzⳆმz̃(z_output[j] z̃_output[j]) · ᐁ_i_o[j]}
}

DOCUMENTATION TO BE CONTINUED...


Another documentation is hosted on Github:

@hyperlink["https://github.com/damien-mattei/Scheme-PLUS-for-Racket/blob/gh-pages/README.md"]{Documentation of Scheme+ for Racket on Github pages}
