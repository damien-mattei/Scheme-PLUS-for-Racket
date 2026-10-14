

(module not-equal racket/base


  (provide <>
	   ≠
	   fx≠   ; used by the type annotation parser (annot+.rkt of SRFI-105) when the arguments are fixnums
	   fl≠ ) ; or flonums

  (require (only-in racket/fixnum fx=)
	   (only-in racket/flonum fl=))


;; not equal operator for numbers

;; scheme@(guile-user)> (<> 1 2)
;; #t
;; scheme@(guile-user)> {1 <> 2}
;; #t
;; scheme@(guile-user)> {1 <> 1}
;; #f


(define (<> x y)
  (not (= x y)))

(define (≠ x y)
  (not (= x y)))

;; specialized versions of ≠ (and <>) for fixnums and flonums
(define (fx≠ x y)
  (not (fx= x y)))

(define (fl≠ x y)
  (not (fl= x y)))


) ; end library
