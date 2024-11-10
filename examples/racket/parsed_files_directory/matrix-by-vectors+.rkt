(module matrix-by-vectors racket
  (provide matrix-vect
           matrix-vect?
           matrix-vect-v
           dim-matrix-vect
           multiply-matrix-matrix
           multiply-matrix-vector
           matrix-vect-ref
           matrix-vect-set!
           matrix-vect-line-ref
           matrix-vect-line-set!
           create-matrix-vect-by-function
           vector->matrix-column
           matrix-column->vector
           *)
  (require Scheme+)
  (define-overload-existing-operator *)
  (require (rename-in srfi/43 (vector-map vector-map-srfi-43)))
  (require Scheme+/array)
  (struct matrix-vect (v))
  (define (create-matrix-vect-by-function fct lin col)
    (matrix-vect (create-vector-2d fct lin col)))
  (define (dim-matrix-vect M)
    (when (not (matrix-vect? M)) (error "argument is not of type matrix"))
    ($nfx$ v <- (matrix-vect-v M))
    ($nfx$ lin <- (vector-length v))
    ($nfx$ col <- (vector-length ($bracket-apply$ v 0)))
    (values lin col))
  (define (multiply-matrix-matrix M1 M2)
    ($nfx$ (n1 p1) <- (dim-matrix-vect M1))
    ($nfx$ (n2 p2) <- (dim-matrix-vect M2))
    (when ($nfx$ p1 ≠ n2)
      (error
       "matrix-by-vectors.* : matrix product impossible, incompatible dimensions"))
    ($nfx$ v1 <- (matrix-vect-v M1))
    ($nfx$ v2 <- (matrix-vect-v M2))
    (define (res i j)
      ($nfx$ sum <- 0)
      (for
       (($nfx$ k <- 0) ($nfx$ k < p1) ($nfx$ k <- k + 1))
       ($nfx$
        sum
        <-
        sum
        +
        ($bracket-apply$ ($bracket-apply$ v1 i) k)
        *
        ($bracket-apply$ ($bracket-apply$ v2 k) j)))
      sum)
    ($nfx$ v <- (create-vector-2d res n1 p2))
    (matrix-vect v))
  (overload-existing-operator
   *
   multiply-matrix-matrix
   (matrix-vect? matrix-vect?))
  (define (vector->matrix-column v)
    (matrix-vect (vector-map-srfi-43 (lambda (i x) (make-vector 1 x)) v)))
  (define (matrix-column->vector Mc)
    ($nfx$ v <- (matrix-vect-v Mc))
    (vector-map-srfi-43 (lambda (i v2) ($bracket-apply$ v2 0)) v))
  (define (multiply-matrix-vector M v)
    ($nfx$ Mc <- (vector->matrix-column v))
    (matrix-column->vector ($nfx$ M * Mc)))
  (overload-existing-operator * multiply-matrix-vector (matrix-vect? vector?))
  (define (matrix-vect-ref M lin col)
    ($nfx$ v <- (matrix-vect-v M))
    ($bracket-apply$ ($bracket-apply$ v lin) col))
  (define (matrix-vect-set! M lin col x)
    ($nfx$ v <- (matrix-vect-v M))
    ($nfx$ ($bracket-apply$ ($bracket-apply$ v lin) col) <- x))
  (define (matrix-vect-line-ref M lin)
    ($nfx$ v <- (matrix-vect-v M))
    ($bracket-apply$ v lin))
  (define (matrix-vect-line-set! M lin vect-line)
    ($nfx$ v <- (matrix-vect-v M))
    ($nfx$ ($bracket-apply$ v lin) <- vect-line))
  (overload-square-brackets
   matrix-vect-ref
   matrix-vect-set!
   (matrix-vect? number? number?))
  (overload-square-brackets
   matrix-vect-line-ref
   matrix-vect-line-set!
   (matrix-vect? number?)))
