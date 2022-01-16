#lang reader "SRFI-105.rkt"

(provide fib)

{7 * 3}
(define (fib n)
  (if {n < 2}
      n
      {(fib {n - 1}) + (fib {n - 2})} ))

