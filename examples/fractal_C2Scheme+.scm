#! /usr/bin/env racket
#lang reader SRFI-105

(module fractal racket
	
(require Scheme+)
	
(define num 0)
(define den 1)


{src := #(55 77 13 11 546 195 11 39 13 65 1 13 #f #f)} ; immutable vector (works too with a mutable one)
{f := src} {acc := 131625} ;x^4 y^3 mul , note that f and src point to the same place

(while {f[den]}  ; the Scheme+ while loop is shorter than the C one
       (if {acc % f[den] <> 0}
	   {f := f[2 :]} ; note that contrary to C here we recreate a new vector from f[2] to the end as there is no way in Scheme to know the memory location of an element of a vector
	   ($>  {acc := acc * f[num] / f[den]}   {f := src} ))) ; again f and src point to the same place in memory

(display "acc=") (display acc) (newline) ; r^12

0 ; as the C program returned 0

) ; end module
