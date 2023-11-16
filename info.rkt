#lang info
(define collection "Scheme-PLUS-for-Racket")
(define compile-omit-paths '("deprecated" "library" "examples" "SRFI" "compiled" "src"))
(define test-omit-paths '("deprecated" "library" "examples" "SRFI" "compiled" "src"))
(define deps '("base"
	       "srfi-lib"
	       "sci"
	       "r6rs-lib"))
(define pkg-desc "Scheme+ for Racket and SRFI-105 curly-infix with REPL")
(define version "7.2")
(define pkg-authors '(mattei))
(define scribblings '(("scribblings/scheme-plus.scrbl" ())))
(define build-deps '("scribble-lib" "racket-doc" "scribble-code-examples" "scribble-doc"))
(define license 'LGPL-3.0-or-later)
