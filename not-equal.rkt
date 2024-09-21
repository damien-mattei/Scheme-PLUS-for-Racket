

(module not-equal racket


  (provide <>
	   ≠ )


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


) ; end library
