#lang reader "SRFI-105.rkt"

;;(require "examples-curly-infix2.rkt")

(- (+ 3 3)
   {2 + 2})



{5 + 2}

(require "examples-curly-infix2.rkt")

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
(fibonacci 7)



(fib 11)


