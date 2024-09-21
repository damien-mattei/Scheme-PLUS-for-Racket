;;#lang reader "../Scheme-PLUS-for-Racket/src/SRFI-105.rkt"

#lang reader SRFI-105

;;#lang reader "../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/SRFI-105.rkt"

;; Matrix  file

;; Author: Damien Mattei


(module matrix racket

(provide multiply-flomat-vector
	 dim
	 *) ; matrix-column->vector) 

(require (rename-in flomat (repeat repeat-flomat)  ; flomat is in package sci
		           (shape shape-flomat)
			   (transpose transpose-flomat)))

;; (require (for-syntax r6rs/private/base-for-syntax)) ;; for macro syntax (for ... : identifier-syntax: undefined;

(require Scheme+)

;;(require Scheme-PLUS-for-Racket)
;;(require "../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/Scheme+.rkt")

(define-overload-existing-operator *)

(define (multiply-flomat-vector M v) ;; args: M : flomat, v:vector , return vector
  (flomat->vector (times M (matrix v))))

(overload-existing-operator * multiply-flomat-vector (flomat? vector?))


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

