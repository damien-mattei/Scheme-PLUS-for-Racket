(define (<< x n)
  (arithmetic-shift x n))

(define (>> x n)
  (arithmetic-shift x (- n)))

(define & bitwise-and)
(define ∣ bitwise-ior) ;; this is U+2223  because vertical line is reserved in Racket


