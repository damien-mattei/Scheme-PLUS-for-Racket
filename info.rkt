#lang info
(define pkg-name "Scheme-PLUS-for-Racket")
(define collection "Scheme+")
(define compile-omit-paths '("library" "examples" "SRFI" "compiled" "src" "bootstrapping"))
(define test-omit-paths '("library" "examples" "SRFI" "compiled" "src" "bootstrapping"))
(define deps '("base"
	       "srfi-lib"
	       ;"sci"
	       "r6rs-lib"
	       "reprovide-lang-lib"
	       "SRFI-105-for-Racket"
	       "try-catch"
	       "SRFI-110-for-Racket"))
(define pkg-desc "Scheme+ for Racket")
(define version "17.7")
(define pkg-authors '(mattei))
(define scribblings '(("scribblings/Scheme-PLUS-for-Racket.scrbl" () (language))))
(define build-deps '("scribble-lib" "racket-doc" "scribble-code-examples" "scribble-doc"))
(define license 'LGPL-3.0-or-later)
