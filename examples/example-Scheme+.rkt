#lang reader "../SRFI/SRFI-105.rkt"

(require "../Scheme+.rkt")
;;(require Scheme-PLUS-for-Racket/Scheme+)

{2 + 3}

(declare x)
{x <- 7}

(define (fib n)
  (if {n < 2}
      n
      {(fib {n - 1}) + (fib {n - 2})} ))

