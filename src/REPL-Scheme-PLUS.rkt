#! /usr/bin/env -S racket --load REPL-Scheme-PLUS.rkt --repl
;; the line above is not mandatory,only for launching a script in command line

;; in CLI you must (require Scheme+) manually

#lang reader SRFI-105 ; SRFI-105 Curly-infix-expressions


(module repl racket

  (provide (all-defined-out)) 
  (require Scheme+)

  ;; put your code here or simply use the REPL

  )
