
;; subscript definitions

;; port from scheme+ to scheme

;; Damien MATTEI


(module subscript racket/base
	

(provide string-subscript-number->string-number string-replace-chars translate-char to-lower-digit string-subscript-number->number)

(require (rename-in srfi/13 (string-hash string-hash-srfi13))) ;; strings

(require Scheme+/def)



;; scheme@(guile-user)> (string-subscript-number->string-number "₁₂₃")
;; "123"
(define (string-subscript-number->string-number str)
  (string-replace-chars str "₋₊₀₁₂₃₄₅₆₇₈₉" "-+0123456789"))

(define (string-subscript-number->number str)
  (string->number (string-subscript-number->string-number str)))


;; scheme@(guile-user)> (string-replace-chars "₁₂₃" "₋₊₀₁₂₃₄₅₆₇₈₉" "-+0123456789")
;; "123"
(define (string-replace-chars str str-before str-after)
  (string-map (lambda (c) (translate-char c str-before str-after)) str))


;; scheme@(guile-user)> (translate-char #\₁ "₋₊₀₁₂₃₄₅₆₇₈₉" "-+0123456789")
;; #\1
(def (translate-char c str-before str-after)
  (define i (string-index str-before c))
  (unless i (return c)) ;; if no match return unchanged the character c
  (string-ref str-after i))


;; scheme@(guile-user)> (string-for-each (lambda (c) (display c) (display " <-> ") (display (char->integer c)) (newline)) "₋₊₀₁₂₃₄₅₆₇₈₉")
;; ₋ <-> 8331
;; ₊ <-> 8330
;; ₀ <-> 8320
;; ₁ <-> 8321
;; ₂ <-> 8322
;; ₃ <-> 8323
;; ₄ <-> 8324
;; ₅ <-> 8325
;; ₆ <-> 8326
;; ₇ <-> 8327
;; ₈ <-> 8328
;; ₉ <-> 8329


(def (to-lower-digit str)
  (string-replace-chars str "-+0123456789" "₋₊₀₁₂₃₄₅₆₇₈₉"))


) ; end of module

