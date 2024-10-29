#! /usr/bin/env racket
#lang reader SRFI-105

(module fractal_list racket
	
(require Scheme+)
	
(define num car)
(define den cadr)
(define forward2 cddr)


{src := '(55 77 13 11 546 195 11 39 13 65 1 13 #f #f)} ; list
{f := src} {acc := 131625} ;x^4 y^3 mul , note that f and src point to the same place

(while (den f)  ; the Scheme+ while loop is shorter than the C one
       (if {acc % (den f) <> 0}
	   {f := (forward2 f)} ; note thet here we really point to another place in the same list (no copy)
	   ($>  {acc := acc * (num f) / (den f)}   {f := src} ))) ; again f and src point to the same place in memory

(display "acc=") (display acc) (newline) ; r^12

0 ; as the C program returned 0

) ; end module
