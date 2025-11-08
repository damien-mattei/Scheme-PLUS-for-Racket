(module int racket/base


  (provide int )


;; (int -3.7)
;; -3
(define (int x)
  (inexact->exact (truncate x))))
