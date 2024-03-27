#lang reader "../src/SRFI-105.rkt"

(module prefix racket

(require "../Scheme+.rkt")

(define (prefix str)
  (define char* (string->list str))
  (define (vhile char* result) ;; while is already defined in Scheme+
    (condx
      [(empty? char*) (list->string (reverse result))]
      [exec (define c (first char*))]
      [(char-upper-case? c) (list->string (reverse (cons c result)))]
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



;; (define (prefix str)
;;   (define char* (string->list str))
;;   (define (vhile char* result) ;; while is already defined in Scheme+
;;     (cond
;;       [(empty? char*) (list->string (reverse result))]
;;       [else 
;;        (define c (first char*))
;;        (cond
;;          [(char-upper-case? c) (list->string (reverse (cons c result)))]
;;          [(char-lower-case? c) (vhile (rest char*) (cons c result))]
;;          [else (vhile (rest char*) result)])]))

;;    (vhile (rest char*) (list (first char*))))
