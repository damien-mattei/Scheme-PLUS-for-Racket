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
  
  ;; put your code here or simply use the REPL
  "GREETINGS SCHEMER. SHALL WE PLAY A GAME?"
  
  (display "begin REPL script") (newline)

  (define (add-n-lists . vn-lst)
    {map-args <- (cons + vn-lst)}
    (apply map map-args))

  ;; overload-operator defines the overloaded + the first time, then only adds the overloads
  ;(overload-operator + (flonum?) fl+ (flonum? flonum?)) ; binary : 2 predicates ; flonum? and not inexact? : fl+ needs flonums, and inexact? raises an error on a non-number (e.g. the lists below)
  (overload-operator + (list?) add-n-lists (list?)) ; n-ary : any number of lists

  ;(overload-procedure sin sin (flonum?))
  (type [(flonum?) procedure? (flonum?)] sin)
  
  (define-syntax mac
	  (syntax-rules ()
	    ((_ expr) (quote expr))))
  
  (type [(inexact?) procedure? ()] random)

		         	
(define (uniform-dummy dummy) {-1.0 + (random) * 2.0})
  
  #| {str := "hello"} ; type inference !
     (type string? str2)
     {str2 := (string-append "azertyuiop")} ; TODO : should be type inference too
     {str2[3 : 5] := str[1 : 3]}
     {v1 := (vector 1 2 3 4 5 6 7)}
     (define (foo) (vector -10 -20 -30 -40))
     {v2 := (foo)}
     (type vector? v1 v2 v3)
     {v3 := v2[1 :]}
     {v1[2 : 5] := v2[1 :]}
     {v1[2 : 5] := v3}
     {v2[2 : 4] := v1[3 : 5] := v3}
     (define (display-all . args)
      (display args)) 
     (list v1 v2 str2)
     (define n 3)
     {3 ⁻²·⁽ⁿ⁻⁴⁾}
   |#
  {1.0 + 2.0 ** 3.0 ** 4.0}
  {1 + 2 ** 3 ** 4}
  {two := 2}
  {three := 3}
  {four := 4}
  (type exact-integer? two three four)
  {1 + two ** three ** four}

  {$ovrld-ht$['+]} ; binary overloads of +
  {$ovrld-ht-n-ary$['+]} ; n-ary overloads of +
  {'(1 2 3) + '(4 5 6)}
  {3.3 + 4.7}
  {2.3 + (sin 0.3)}

  {x := 5}
  (for-racket ([x (in-range 10)])
              (display x))
  (display x)(newline)
  
  {x := 5}
  (for ({x := 0} {x < 10} {x := x + 1})
    (display x))
  (display x)(newline)

  #|comment multi line test |#

  {a := 100}
  {b := 10}
  {c := 5}
  {a / b / c}
  {1.0 * 2.0}

  (known-program? +)
  ;(known-program? gag)
  (known-program? '+)
  (known-program? 'gag)
  (generic-known-program? +)

  {op := '-}
  ;{x := (case op ((+ -) 'add) ((* /) 'mul) (else 'other))}
  (case op ((+ -) 'add) ((* /) 'mul) (else 'other))

  {v1 := #(2.2 3.3 4.4)}
  {v2 := #(1.2 1.3 1.4)}
  (type flonum? {v1[]} {v2[]})
  {(vector-map * v1 v2)[0] + 1.2}
  
  (let ((tmp (gensym)))
    ``(lambda (,tmp ,,tmp ,',tmp) ()))

  (define-syntax +=
    (syntax-rules ()
      ({var1 _ var2} {var1 := var1 + var2})))

  (define-syntax plus
    (syntax-rules ()
      ({var1 _ ...} {var1 + ...})))

   {str2 := (string-append "azertyuiop")} ; TODO : should be type inference too
  {str := "hello"} ; type inference !
  {str[3 :]}
  )
