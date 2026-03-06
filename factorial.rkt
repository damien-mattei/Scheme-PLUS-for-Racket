(module factorial racket/base

  (provide !)


(define (! n)
  (if (= n 0)
      1
      (* n (! (- n 1)))))

)


