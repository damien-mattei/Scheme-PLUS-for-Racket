#lang reader "../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/SRFI-105.rkt"

;; Matrix  file

;; Author: Damien Mattei

(provide multiply-flomat-vector
	 dim
	 *) ; matrix-column->vector) 

(require (rename-in flomat (repeat repeat-flomat)  ; flomat is in package sci
		           (shape shape-flomat)
			   (transpose transpose-flomat)))

;; (require (for-syntax r6rs/private/base-for-syntax)) ;; for macro syntax (for ... : identifier-syntax: undefined;


(require "../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/Scheme+.rkt")

(define-overload-existing-operator *)

(define (multiply-flomat-vector M v) ;; args: M : flomat, v:vector , return vector
  (flomat->vector (times M (matrix v))))

(overload-existing-operator * multiply-flomat-vector (flomat? vector?))


;; (define (matrix-column->vector C)
;;   {lgC <+ (ncols C)}
;;   {v <+ (make-vector lgC 0)}
;;   (for ({i <+ 0} {i < lgC} {i <- i + 1})
;;        {v[i] <- C[i]})
;;   v)


(define (dim M)
  ;;(display "matrix.rkt : dim : M = ") (display M) (newline)
  {shp <+ (shape-flomat M)}
  {lin <+ (first shp)}
  {col <+ (second shp)}
  (values lin col))
