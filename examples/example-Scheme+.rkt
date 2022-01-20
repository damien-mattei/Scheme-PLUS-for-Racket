#lang reader "../SRFI/SRFI-105.rkt"


(require SchemePLUS/Scheme+)

;;(require "../Scheme+.rkt")
;;(require Scheme-PLUS-for-Racket/Scheme+)




(include "../library/increment.scm")
(include "../library/for-next-step.scm")

{2 + 3}

(declare x)
{x <- 7}

(define (fib n)
  (if {n < 2}
      n
      {(fib {n - 1}) + (fib {n - 2})} ))

;; (expo-complex 3+4i 1-3i)
;; -58.560423283588165+55.59488089854708i

;; https://mathworld.wolfram.com/ComplexExponentiation.html

(define (expo-complex z1 z2)

  ;;{a <+ (real-part z1)}
  ;;{b <+ (imag-part z1)}
  
  {c <+ (real-part z2)}
  {d <+ (imag-part z2)}

  ;;{arg <+ {{c * angle(z1)} + {{d * log(sqr(a) + sqr(b))} / 2}}}
  
  {arg <+ {{c * angle(z1)} + {{d * log(sqr(magnitude(z1)))} / 2}}}

  {i <+ 0+1i} ;; imaginaire pur
  
  {{magnitude(z1) ** c} * exp{{- d} * angle(z1)} * {cos(arg) + {i * sin(arg)}}})

;; Riemann zeta fonction
;; warning: converge for Re(z) > 1
(define (ζ-expo z)
  {nmax <+ 100}
  {r <+ 1.0+0.0i}
  (for (n 2 nmax)
    {r <- {r + {1.0 / (expo-complex n z)}}}
    ;;(display r)
    ;;(newline)
    )
  r)
  
(define (ζ z)
  {nmax <+ 10}
  {r <+ 1.0}
  (for (n 2 nmax)
    {v <+ {1.0 / {n ** z}}}    
    {r <- {r + v}}
    (display v)
    (newline)
    )
  r)

(define (ζ-expr z)
  
  {nmax <+ 100000000}
  {r <+ 1.0}
  {a <+ (real-part z)}
  {b <+ (imag-part z)}
  
  (for (n 2 nmax)
    {r <- {r + (make-rectangular {cos{b * log(n)} / {n ** a}}
                                 {-1.0 * {sin{b * log(n)} / {n ** a}}})}}
    ;;(display r)
    ;;(newline)
    )
  r)
