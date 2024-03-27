#lang reader "../src/SRFI-105.rkt"

(module prefix racket

(require "../Scheme+.rkt")

(define (prefix str)
  (define char* (string->list str))
  
  (def (vhile char* result) ;; while is already defined in Scheme+
       (when (empty? char*)
	     (return (list->string (reverse result))))
       
       (define c (first char*))
       (cond [(char-upper-case? c) (list->string (reverse (cons c result)))]
	     [(char-lower-case? c) (vhile (rest char*) (cons c result))]
	     [else (vhile (rest char*) result)]))

   (vhile (rest char*) (list (first char*))))

(define examples
  '[("alfa"  "alfa")
    ("Alfa"  "Alfa")
    ("DiCAp" "DiC")
    ("BRaVo" "BR")
    ("b"     "b")
    ("B"     "B")])

;;(require rackunit)

(for-each (λ (x)
            (display (prefix (first x)))
            (display "   ")
            (display (second x))
            (newline)) examples)

) ; end module
