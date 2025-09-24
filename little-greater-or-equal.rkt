

(module little-greater-or-equal racket/base


  (provide ≤ ≥)

  ;; {3 ≤ 4}
  ;; #t

  ;; {3 ≤ 4 ≤ 5}
  ;; #t
  (define ≤ <=)

  (define ≥ >=)


) 
