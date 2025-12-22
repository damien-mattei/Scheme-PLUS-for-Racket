#lang reader SRFI-105

(module pyffi-infix racket/base

  (provide *
           numpy-array?
           ndarray-line-ref
           ndarray-line-set!
           set
           ·
           numpy-column->list
           list->numpy-column
           numpy-column->vector
           vector->numpy-column
           )
  
  (require (rename-in Scheme+
                      (· ·s) 
                      (:= :=s) ; for compatibility with pyffi
                      (-> s->)))


  (require pyffi
           pyffi/numpy
           ;pyffi/numpy-core
           )

  ;(set-environment-variables) ; do not know what it does?
  (initialize)
  (display "initialize passed") (newline)

  ;(display "initialize passed") (newline)
  (import-numpy)
  (display "import-numpy passed") (newline)

  ;; 2 methods below seems to work
  ;(post-initialize)
  ;(display "post-initialize passed") (newline)
  (finish-initialization)
  (display "finish-initialization passed") (newline)

  ;(declare-special-prefix numpy)

  (import numpy)

  ; getter for ndarray line
  (define (ndarray-line-ref A lin)
    (numpy.take A (numpy.array (list lin)) #:axis 0))

  ; setter for ndarray line
  (define (ndarray-line-set! A lin vect-line)
    {shp <- (numpy.shape A)}
    {size-col <- shp[0]}
    (numpy.put_along_axis A
                          (numpy.full (pytuple size-col 1)
                                      (numpy.array (list lin)))
                          vect-line
                          0))

  ; predicate for ndarray
  (define (numpy-array? A)
    (builtins.isinstance A (builtins.type (numpy.array '(0)))))

  (define (numpy-column->list col)
    (pylist->list ((.tolist ((.flatten col))))))

  (define (list->numpy-column L)
    (.T (numpy.atleast_2d (numpy.array L))))

  (define (numpy-column->vector col)
    (list->vector (numpy-column->list col)))

  (define (vector->numpy-column v)
    (list->numpy-column (vector->list v)))
  
  
  ;; > {Z[2]}
  ;; ($bracket-apply$ Z 2)
  ;; (obj "ndarray" : array([[0, 0, 0, 1, 0, 0]]))

  ;; {Z[2] <- (numpy.array '[[0 -7 0 0 0 0]])}
  ;;(overload-square-brackets ndarray-line-ref ndarray-line-set! (numpy-array? number?))

  
  ;; to be compatible with game of life original program i will use  instead of above overloading this below:
  ;; the difference is only returning [x y z ...] instead of [[x y z ...]]
  (overload-square-brackets ref ndarray-line-set! (numpy-array? number?))

  (define-overload-existing-operator *)
  
  ;; {'(1 2 3) * 5}
  ;; (* '(1 2 3) 5)
  ;; (obj "list" : [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3])
  (overload-existing-operator * mul (list? number?))
  (overload-existing-operator * matmul (numpy-array? numpy-array?))

  
  (define-overload-existing-operator · Scheme+/multiply)
  
  (overload-existing-operator · mul (list? number?))
  (overload-existing-operator · matmul (numpy-array? numpy-array?))

  ; numpy array * vector, return a vector
  (define (matmul-vector npa v)
    (numpy-column->vector (matmul npa (vector->numpy-column v))))
  
  (overload-existing-operator · matmul-vector (numpy-array? vector?))

  (define (set A x y val)
    (:= A [x y] val))

  (define (ref-float64 A x y)
    (define rval (ref A x y))
    (when (not (number? rval))
      (set! rval ((.item rval))))
    rval)

  ;; example:
  ;; {N[x y] <-  Z[x- y-] + Z[x y-] + Z[x+ y-] 
  ;;           + Z[x-  y] +           Z[x+  y]
  ;;           + Z[x- y+] + Z[x y+] + Z[x+ y+]}
  (overload-square-brackets ref-float64 set (numpy-array? number? number?))

  
  )
