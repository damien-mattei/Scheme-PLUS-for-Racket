#lang info
(define pkg-name "Scheme-PLUS-for-Racket")
(define collection "Scheme+")
(define compile-omit-paths '("deprecated" "library" "examples" "SRFI" "compiled" "src" "bootstrapping"))
(define test-omit-paths '("deprecated" "library" "examples" "SRFI" "compiled" "src" "bootstrapping"))
(define deps '("base"
	       "srfi-lib"
	       "sci"
	       "r6rs-lib"
	       "reprovide-lang-lib"
	       "SRFI-105-for-Racket"
	       "try-catch"
	       "SRFI-110-for-Racket"
	       "curly-infix"))
(define pkg-desc "Scheme+ for Racket")
(define version "18.0")
(define pkg-authors '(mattei))
(define scribblings '(("scribblings/Scheme-PLUS-for-Racket.scrbl" () (language))))
(define build-deps '("scribble-lib" "racket-doc" "scribble-doc"))
(define license 'LGPL-3.0-or-later)
