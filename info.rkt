#lang info
(define collection "scheme-plus")
(define compile-omit-paths '("included-files" "required-files" "library" "examples" "SRFI" "compiled"))
(define test-omit-paths '("included-files" "required-files" "library" "examples" "SRFI" "compiled"))
(define deps '("racket/base"))
(define pkg-desc "Scheme+ for Racket and SRFI-105 curly-infix with REPL")
(define version "1.0")
(define pkg-authors '(mattei))
(define scribblings '(("scribblings/scheme-plus.scrbl" ())))
