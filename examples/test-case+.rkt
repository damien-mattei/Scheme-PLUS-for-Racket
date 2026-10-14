#! /usr/bin/env -S racket --load REPL-Scheme-PLUS.rkt --repl
;; the line above is not mandatory,only for launching a script in command line

;; in CLI you must (require Scheme+) manually 

;; but if you want to have syntax color in CLI start racket yourself and do:
;; (load "REPL-Scheme-PLUS.rkt")

#lang reader SRFI-105 ; SRFI-105 Curly-infix-expressions

(module repl racket

  (provide (all-defined-out)) 
  (require Scheme+
           (only-in racket/base [for for-racket])
           Scheme+/program-type)
  
  ANNOTATION-ON
  
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

  (scalar-to-long-rainbow-rgb 0.7)
  )