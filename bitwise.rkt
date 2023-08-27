(define (<< x n)
  (arithmetic-shift x n))

(define (>> x n)
  (arithmetic-shift x (- n)))

(define & bitwise-and)
(define ∣ bitwise-ior) ;; this is U+2223 ∣ DIVIDES (&mid;, &shortmid;, &smid;, &VerticalBar;) see: https://en.wikipedia.org/wiki/Vertical_bar because vertical line is reserved in Racket


