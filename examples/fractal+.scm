#! /usr/bin/env racket
#lang reader SRFI-105


(module fractal racket
	
	(require Scheme+)

(define (fractran-eval n program)
  { n := (let step ([n n] [p program])
            (if (zero? (car p))
              #f
              ($+> {nf := n * (car p)}
		   (if (integer? nf)
		       nf
		       (step n (cdr p)))))) }
    (if n (fractran-eval n program))
          n)

(display (fractran-eval 12
               '(3/2 0)))
 
) ;end module
