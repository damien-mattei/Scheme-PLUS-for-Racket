#lang info
(define pkg-name "Scheme-PLUS-for-Racket")
(define collection "Scheme+")
(define compile-omit-paths '("library" "examples" "SRFI" "compiled" "src" "deprecated"))
(define test-omit-paths '("library" "examples" "SRFI" "compiled" "src" "deprecated"))
(define deps '("base"
	       "srfi-lib"
	       "sci"
	       "r6rs-lib"
	       "reprovide-lang-lib"
	       "SRFI-105-for-Racket"))
(define pkg-desc "Scheme+ for Racket")
(define version "9.7")
(define pkg-authors '(mattei))
(define scribblings '(("scribblings/scheme-plus.scrbl" ())))
(define build-deps '("scribble-lib" "racket-doc" "scribble-code-examples" "scribble-doc"))
(define license 'LGPL-3.0-or-later)
