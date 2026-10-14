#lang reader SRFI-105

;; Test of the type annotation of the square brackets overloaded with overload-square-brackets :
;; for an object of known type the annotation pass writes ($bracket-apply-overloaded$ ...) and the getter (or the setter
;; in an assignment) is called directly , chosen from the number of indexes ; otherwise the choice is made at runtime.

;; Author: Damien Mattei

(module test-overload-square-brackets racket

  (require Scheme+
	   rackunit)

  ANNOTATION-ON

  (struct matrix-vect (v)) ;; matrix based on vector of vectors

  ;; getters and setters : an element M[lin col] , a line M[lin]
  (define (matrix-vect-ref M lin col)
    {v := (matrix-vect-v M)}
    {v[lin][col]})

  (define (matrix-vect-set! M lin col x)
    {v := (matrix-vect-v M)}
    {v[lin][col] := x})

  (define (matrix-vect-line-ref M lin)
    {v := (matrix-vect-v M)}
    {v[lin]})

  (define (matrix-vect-line-set! M lin vect-line)
    {v := (matrix-vect-v M)}
    {v[lin] := vect-line})

  (overload-square-brackets matrix-vect-ref matrix-vect-set! (matrix-vect? number? number?))
  (overload-square-brackets matrix-vect-line-ref matrix-vect-line-set! (matrix-vect? number?))

  ;; an object of known type : the getter or the setter is called directly
  (type matrix-vect? Mv)
  {Mv := (matrix-vect (vector (vector 1 2 3)
			      (vector 4 5 6)))}

  (check-equal? {Mv[1 0]} 4)          ; (matrix-vect-ref Mv 1 0)
  (check-equal? {Mv[1]} #(4 5 6))     ; (matrix-vect-line-ref Mv 1)
  (check-equal? {Mv[1][2]} 6)         ; Mv[1] is a vector

  {i :+ 0}
  {j :+ 2}
  (check-equal? {Mv[(i + 1) (j - 1)]} 5) ; computed indexes

  {Mv[1 0] := -7}                     ; (matrix-vect-set! Mv 1 0 -7)
  (check-equal? {Mv[1 0]} -7)

  {Mv[0] := (vector 7 8 9)}           ; (matrix-vect-line-set! Mv 0 (vector 7 8 9))
  (check-equal? {Mv[0 2]} 9)

  ;; a parameter of known type
  (define (trace M)
    (type matrix-vect? M)
    {M[0 0] + M[1 1]})
  (check-equal? (trace Mv) 12)

  ;; an object without declared type : the generic $bracket-apply$ chooses the getter at runtime , as before
  (define Mw (matrix-vect (vector (vector 1 2)
				  (vector 3 4))))
  (check-equal? {Mw[1 1]} 4)
  {Mw[1 1] := 10}
  (check-equal? {Mw[1 1]} 10)

  (displayln "test-overload-square-brackets+ : done"))
