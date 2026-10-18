#lang reader SRFI-110 ; SRFI-105 ;SRFI-110

;; Matrix  based on vector of vectors

;; Author: Damien Mattei

;; /Applications/Racket\ v8.12/bin/racket curly-infix2prefix4racket.rkt  ../../../../AI_Deep_Learning/matrix-by-vectors+.rkt > ../../../../AI_Deep_Learning/matrix-by-vectors.rkt
   

;; use: (require "matrix-by-vectors+.rkt")
;; or:
;; (require "matrix-by-vectors.rkt")

;; (require "../examples/racket/matrix-by-vectors+.rkt") ; relatively from Scheme+ REPL src/ directory

(module matrix-by-vectors racket

ANNOTATION-ON

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
	 display-matrix-vect
	 *
	 ·)

;;(require srfi/43) ; vector , warning vector-map has index as extra parameter...
;;(import (srfi :43)) 


(require Scheme+)


(require (rename-in srfi/43 (vector-map vector-map-srfi-43)))  ; vector , warning vector-map has index as extra parameter...


(require Scheme+/array)

(struct matrix-vect (v)) ;; matrix based on vector of vectors

(define (display-matrix-vect M)
  (when (not (matrix-vect? M))
	(error "argument is not of type matrix"))
  (display (matrix-vect-v M))
  (newline))

;; (create-matrix-vect-by-function (lambda (i j) (+ i j)) 2 3)
(define (create-matrix-vect-by-function fct lin col)
  (matrix-vect (create-vector-2d fct lin col)))


;; return the line and column values of dimension 
(define (dim-matrix-vect M)

  (when (not (matrix-vect? M))
	(error "argument is not of type matrix"))
  
  (type vector? v {v[]}) ; a vector of vectors : v[lin][col] is specialized for vectors
  {v <- (matrix-vect-v M)}
  {lin <- (vector-length v)}
  {col <- (vector-length v[0])}
  (values lin col))


(define (multiply-matrix-matrix M1 M2)

  {(n1 p1) <- (dim-matrix-vect M1)}
  {(n2 p2) <- (dim-matrix-vect M2)}
  ;; (display "multiply-matrix-matrix : n1=") (display n1) (newline)
  ;; (display "multiply-matrix-matrix : p1=") (display p1) (newline)
  ;; (display "multiply-matrix-matrix : n2=") (display n2) (newline)
  ;; (display "multiply-matrix-matrix : p2=") (display p2) (newline)

  (when {p1 ≠ n2} (error "matrix-by-vectors.* : matrix product impossible, incompatible dimensions"))

  (type vector? v1 v2 {v1[]} {v2[]}) ; vectors of vectors : v1[i][k] and v2[k][j] are specialized for vectors
  {v1 <- (matrix-vect-v M1)}
  {v2 <- (matrix-vect-v M2)}
  
  (define (res i j)
    {sum <- 0}
    (type ℕ i j k)
    (for ({k <- 0} {k < p1} {k <- k + 1})
    	 {sum <- sum + v1[i][k] * v2[k][j]}) ; here the * should not be yet overloaded (so fast, but to be checked?)
    sum)
    ;; (for/sum ([k (range p1)])
    ;; 	     {v1[i][k] * v2[k][j]}))

  {v <- (create-vector-2d res n1 p2)}
  ;(display "v=") (display v) (newline)

  (matrix-vect v))


;; overload-operator defines the overloaded operator (original * from racket/base, original · from Scheme+/multiply) and adds the binary overload
(overload-operator * multiply-matrix-matrix (matrix-vect? matrix-vect?))
(overload-operator · (only-in Scheme+/multiply ·) multiply-matrix-matrix (matrix-vect? matrix-vect?)) ; only the original · is needed from Scheme+/multiply


(define (vector->matrix-column v)
  (matrix-vect (vector-map-srfi-43 (lambda (i x) (make-vector 1 x))
				   v)))

(define (matrix-column->vector Mc)
  {v <- (matrix-vect-v Mc)}
  ;;(display "matrix-by-vectors : matrix-column->vector : v =") (display v) (newline)
  (vector-map-srfi-43 (lambda (i v2)
			(type vector? v2) ; a line of the column matrix
			{v2[0]})
		      v))

(define (multiply-matrix-vector M v) ;; args: matrix ,vector ;  return vector
  (type matrix-vect? M Mc) ; {M * Mc} calls multiply-matrix-matrix directly (overload-operator * above)
  {Mc <- (vector->matrix-column v)}
  ;;(display "matrix-by-vectors : multiply-matrix-vector : v=") (display v) (newline)
  ;;(display Mc) (newline)
  ;;(display (matrix-vect-v Mc)) (newline)
  ;;(matrix-column->vector (multiply-matrix-matrix M Mc)))
  (matrix-column->vector {M * Mc}))
;;(matrix-column->vector (* M Mc)))


;; already overloaded above : only adds the binary overload
(overload-operator * multiply-matrix-vector (matrix-vect? vector?))
(overload-operator · multiply-matrix-vector (matrix-vect? vector?))


;; define getter,setter
(define (matrix-vect-ref M lin col)
  (type vector? v {v[]}) ; a vector of vectors : v[lin][col] is specialized for vectors
  (type ℕ lin col)
  {v <- (matrix-vect-v M)}
  {v[lin][col]})

(define (matrix-vect-set! M lin col x)
  (type vector? v {v[]}) ; a vector of vectors : v[lin][col] is specialized for vectors
  (type ℕ lin col)
  {v <- (matrix-vect-v M)}
  {v[lin][col] <- x})


;; >  (overload-square-brackets matrix-vect-ref
;; 	 matrix-vect-set!  (matrix-vect? number? number?))
;; >  (overload-square-brackets matrix-vect-line-ref
;; 	 (lambda (x) '())  (matrix-vect? number?))
;; > $ovrld-square-brackets-lst$
;; '(((#<procedure:matrix-vect?> #<procedure:number?>) (#<procedure:matrix-vect-line-ref> . #<procedure>))
;;   ((#<procedure:matrix-vect?> #<procedure:number?> #<procedure:number?>) (#<procedure:matrix-vect-ref> . #<procedure:matrix-vect-set!>)))
;; > (define Mv (matrix-vect #(#(1 2 3) #(4 5 6))))
;; > Mv
;; #<matrix-vect>
;; > (find-getter-for-overloaded-square-brackets (list Mv 1))
;; #<procedure:matrix-vect-line-ref>
;; > {Mv[1 0]}
;; 4
;; > {Mv[1]}
;; '#(4 5 6)
;; > {Mv[1][0]}
;; 4
;; > 
(define (matrix-vect-line-ref M lin)
  (type vector? v {v[]}) ; a vector of vectors : v[lin][col] is specialized for vectors
  (type ℕ lin)
  {v <- (matrix-vect-v M)}
  {v[lin]})


(define (matrix-vect-line-set! M lin vect-line)
  (type vector? v {v[]}) ; a vector of vectors : v[lin][col] is specialized for vectors
  (type ℕ lin)
  {v <- (matrix-vect-v M)}
  {v[lin] <- vect-line})


;; overload [ ] 
(overload-square-brackets matrix-vect-ref matrix-vect-set!  (matrix-vect? number? number?))
(overload-square-brackets matrix-vect-line-ref matrix-vect-line-set! (matrix-vect? number?))


) ; end module

