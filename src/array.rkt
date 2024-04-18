;; arrays


;; racket version


;; This file is part of Scheme+

;; Copyright 2021-2023 Damien MATTEI

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; could useful:

;; for macro syntax : identifier-syntax: undefined; use: (require (for-syntax r6rs/private/base-for-syntax))


;; the value v should be put before in a let to avoid multiple evaluation after macro expand

(module array racket

	(provide make-array-2d
		 array-2d-ref
		 array-2d-set!
		 create-vector-2d
		 negative-vector-index
		 function-array-n-dim-ref
		 function-array-n-dim-set!
		 display-array-2d
		 dv-2d
		 funct-array-2d-set!
		 funct-array-2d-ref
		 array-ref-set!)


(require (only-in racket/base [for for-racket])) ;; backup original Racket 'for'

(require (for-syntax r6rs/private/base-for-syntax)) ;; for macro syntax (for ... : stxparam.rkt identifier-syntax: undefined

(require srfi/25)

(include "increment.scm")
(include "for_next_step.scm") ;; with some scheme (guile) must be before use in array (see guile code for explaination)
	
(define-syntax make-array-2d
  (syntax-rules ()
    ((_ lin col) (let* ((array (make-vector lin)))
		 (for-basic (i 0 (- lin 1))
		      (vector-set! array i (make-vector col)))
		 array))
    ((_ lin col v) (let* ((array (make-vector lin)))
		   (for-basic (i 0 (- lin 1))
			(vector-set! array i (make-vector col v)))
		   array))
    ;; ((_ array sx sy) (begin
    ;; 		       (set! (quote array) (make-vector sy))
    ;; 		       (for-basic (i 0 (- sy 1))
    ;; 			    (vector-set! (quote array) i (make-vector sx)))))
    ((_ array lin col v) (begin
			 (set! (quote array) (make-vector lin))
			 (for-basic (i 0 (- lin 1))
			      (vector-set! (quote array) i (make-vector col v)))))))


;; order of indexes  match Matrix and arrays conventions: line column
(define-syntax array-2d-ref
  (syntax-rules ()
    ((_ array lin col) (vector-ref (vector-ref array lin) col))))

(define-syntax array-2d-set!
  (syntax-rules ()
    ((_ array lin col val)
       (vector-set! (vector-ref array lin) col val))))

;; Scheme+ version
;; create a vector of line and column with a function
;; (define (create-vector-2d fct lin col)
;;   {v <+ (make-array-2d lin col)}
;;   (for ({l <+ 0} {l < lin} {l <- l + 1})
;;        (for ({c <+ 0} {c < col} {c <- c + 1})
;; 	    {v[l][c] <- (fct l c)}))
;;   v)

;; create a vector (or array) of line and column with a function
(define (create-vector-2d fct lin col)
  (define v (make-vector lin))
  (for ((define l 0) (< l lin) (set! l (+ l 1)))
       (vector-set! v l (make-vector col))
       (for ((define c 0) (< c col) (set! c (+ c 1)))
	    (array-2d-set! v l c (fct l c))))
  v)



;; scheme@(guile-user)> (define arr (make-array-2d 10 7 0))
;; scheme@(guile-user)> (array-n-dim-ref arr 4 3)
;; 0
;; scheme@(guile-user)> (array-n-dim-set! arr 7 4 3)
;; scheme@(guile-user)> (array-n-dim-ref arr 4 3)
;; 7
;; (define-syntax array-n-dim-ref
;;   (syntax-rules ()
;;     ((_ array x) (vector-ref array x))
;;     ((_ array x y ...) (vector-ref (array-n-dim-ref array y ...) x))))

;; (define (funct-array-n-dim-ref array x . more-indexs)
;;   (if (null? more-indexs)
;;       (vector-ref array x)
;;       (let ((rev-args (reverse (cons x more-indexs))))
;; 	(vector-ref (apply funct-array-n-dim-ref (cons array (cdr rev-args)))
;; 		    (car rev-args)))))

;; return the negative index depending of length of vector
;; (define-syntax negative-vector-index
  
;;   (syntax-rules ()
    
;;     ((_ index v)
     
;;      (if (< index 0)
;; 	 (+ (vector-length v) index)
;; 	 index))))


(define (negative-vector-index index v)
     
  (if (< index 0)
      (+ (vector-length v) index)
      index))



;; this one is used by array.scm
(define (function-array-n-dim-ref array L-reversed-indexes)
  ;;(display L-reversed-indexes) (newline)
  (if (= 1 (length L-reversed-indexes)) ; base case : array of dimension 1 : vector
      ;; vector
      (vector-ref array
		  (negative-vector-index (car L-reversed-indexes) ;; compatible with negative indexes
					 array))

      ;; vector of vectors
      (vector-ref (function-array-n-dim-ref array (cdr L-reversed-indexes)) ; the sub-array
		  (negative-vector-index (car L-reversed-indexes)
					 array))))

  
;; (define-syntax array-n-dim-set!
;;   (syntax-rules ()
;;     ((_ array val x) (vector-set! array x val))
;;     ((_ array val x y ...) (vector-set! (array-n-dim-ref array y ...) x val))))

;; (define (funct-array-n-dim-set! array val x . more-indexs)
;;   (if (null? more-indexs)
;;       (vector-set! array x val)
;;       (let ((rev-args (reverse (cons x more-indexs))))
;; 	(vector-set! (apply funct-array-n-dim-ref (cons array (cdr rev-args)))
;; 		     (car rev-args)
;; 		     val))))

(define (function-array-n-dim-set! array val L-reversed-indexes)
  (if (= 1 (length L-reversed-indexes))
      (vector-set! array
		   (negative-vector-index (car L-reversed-indexes)
					  array)
		   val)
      (vector-set! (function-array-n-dim-ref array (cdr L-reversed-indexes))
		   (negative-vector-index (car L-reversed-indexes)
					  array)
		   val)))


(define-syntax display-array-2d
  (syntax-rules ()
    ((_ array)
     (for-basic (y 0 (- (vector-length array) 1))
	  (display (vector-ref array y)) (newline)))))


;; > (define _quai 34)
;; > (dv _quai)
;; _quai = 34
(define-syntax dv-2d 
  (syntax-rules ()
    ((_ var) (begin
	       ;;(display (symbol->string (quote var)))
	       (display (quote var))
	       (display " = ") (newline)
	       (display-array-2d var)
	       (newline)))))

;; function ? to be used with map
;; TODO: make n dimension versions recursive with variable number of parameters
(define (funct-array-2d-set! array x y val) (vector-set! (vector-ref array y) x val))

(define (funct-array-2d-ref array x y) (vector-ref (vector-ref array y) x))

;; scheme@(guile-user)> (array-ref-set! dyna 7 3 4)
;; $4 = 7
;; scheme@(guile-user)> (array-ref dyna 3 4)
;; $5 = 7
(define-syntax array-ref-set!
  (syntax-rules ()
    ((_ array expr x y) (let ((v expr))
			  (array-set! array x y v) ;; as in srfi 25 not guile where v is before indexes !
			  v))))


) ;; end module
