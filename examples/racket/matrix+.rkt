#lang reader SRFI-105


;; Matrix  file

;; Author: Damien Mattei


(module matrix racket

(provide multiply-flomat-vector
	 dim
	 *
	 ·) ; matrix-column->vector) 

#;(require (except-in (rename-in flomat (repeat repeat-flomat)  ; flomat is in package sci
		               (shape shape-flomat)
			       (transpose transpose-flomat)) ×))


;; (require (for-syntax r6rs/private/base-for-syntax)) ;; for macro syntax (for ... : identifier-syntax: undefined;


(require (rename-in flomat (repeat repeat-flomat)  ; flomat is in package sci
		    (shape shape-flomat)
		    (transpose transpose-flomat))
	 (except-in Scheme+ ×)) ; × is also provided by flomat

ANNOTATION-ON

(define (multiply-flomat-vector M v) ;; args: M : flomat, v:vector , return vector
  (flomat->vector #;(times M (matrix v)) {M × (matrix v)})) ; multiply a matrix of floats by a matrix vector column (?) and return a vector column (?)

;; overload-operator defines the overloaded operator (original * from racket/base, original · from Scheme+/multiply) and adds the binary overload
(overload-operator * multiply-flomat-vector (flomat? vector?))
(overload-operator · (only-in Scheme+/multiply ·) multiply-flomat-vector (flomat? vector?)) ; only the original · is needed from Scheme+/multiply

;; (define (matrix-column->vector C)
;;   {lgC <- (ncols C)}
;;   {v <- (make-vector lgC 0)}
;;   (for ({i <- 0} {i < lgC} {i <- i + 1})
;;        {v[i] <- C[i]})
;;   v)


(define (dim M)
  ;;(display "matrix.rkt : dim : M = ") (display M) (newline)
  {shp <- (shape-flomat M)}
  {lin <- (first shp)}
  {colonne <- (second shp)} ; WARNING do not use col or column they are Racket's procedures (flomat?)
  (values lin colonne))


) ; end module

