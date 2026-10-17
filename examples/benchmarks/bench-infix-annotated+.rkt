#lang reader SRFI-105

;; Scheme+ as it is usually written , with the type annotation (ANNOTATION-ON) : the same text as bench-infix+.rkt with
;; the lines (type ...) of the parameters and the signatures of the procedures ; the types of the variables of := are
;; inferred from their values. To see the code produced :
;; racket <path of SRFI-105>/src/curly-infix2prefix4racket.rkt bench-infix-annotated+.rkt
;;
;; The benchmarks of the manual of Scheme+ (section "Type annotation and inference") : the same four programs written
;; five times : in Scheme (bench-scheme.rkt) ; in Scheme+ with the same prefix source , without and with the type
;; annotation (bench-scheme+.rkt , bench-annotated+.rkt) ; in Scheme+ as it is usually written , infix , := instead of
;; let , square brackets , without and with the type annotation (bench-infix+.rkt , bench-infix-annotated+.rkt).
;; To run them : racket run-benchmarks.rkt (compile first with raco make run-benchmarks.rkt , on a machine at rest).
;;
;; Author: Damien Mattei

(module bench-infix-annotated+ racket
 (require Scheme+)
 (provide bench-float bench-matrix bench-integer bench-trapezoid)
 ANNOTATION-ON
 ;; 1. floats : polynomial by Horner , sigmoid and its derivative , on n points
 (define (bench-float n)
   (type fixnum? n)
   (let loop ((i 0) (x 0.0) (acc 0.0))
     (if {i < n}
         then
           {p := 1.0 + x * (2.0 + x * (3.0 + x * 4.0))}
           {s := 1.0 / (1.0 + (exp (- p)))}
           (loop {i + 1} {x + 0.000001} {acc + s * (1.0 - s)})
         else
           acc)))

 ;; 2. product of matrices : vectors of vectors of floats
 (define (make-matrix n v)
   (build-vector n (lambda (i) (build-vector n (lambda (j) {v * (1.0 + (exact->inexact (modulo {i + j} 7)))})))))
 (define (matrix-product A B n)
   (type #(#(flonum?)) A B) ; vectors of vectors of floats
   (type fixnum? n)
   {C := (build-vector n (lambda (i) (make-vector n 0.0)))}
   (type #(#(flonum?)) C)
   (let loop-i ((i 0))
     (when {i < n}
       {Ai := A[i]}
       {Ci := C[i]}
       (let loop-j ((j 0))
         (when {j < n}
           (let loop-k ((k 0) (sum 0.0))
             (if {k < n}
                 (loop-k {k + 1} {sum + Ai[k] * B[k][j]})
                 {Ci[j] := sum}))
           (loop-j {j + 1})))
       (loop-i {i + 1})))
   C)
 (type ((#(#(flonum?))) procedure? (#(#(flonum?)) #(#(flonum?)) fixnum?)) matrix-product)
 (define (bench-matrix n times)
   (type fixnum? n times)
   {A := (make-matrix n 0.5)}
   {B := (make-matrix n 0.25)}
   (let loop ((t 0) (trace 0.0))
     (if {t < times}
         then
           {C := (matrix-product A B n)}
           {c := C[0][0]}
           (loop {t + 1} {trace + c})
         else
           trace)))

 ;; 3. integers : the lengths of the flights of Syracuse (Collatz) up to n
 (define (flight k)
   (type fixnum? k)
   (let loop ((k k) (len 0))
     (if {k = 1}
         len
         (loop (if {(remainder k 2) = 0} (quotient k 2) {3 * k + 1}) {len + 1}))))
 (type ((fixnum?) procedure? (fixnum?)) flight) ; the signature of flight : (flight k) is a fixnum
 (define (bench-integer n)
   (type fixnum? n)
   (let loop ((k 1) (total 0))
     (if {k <= n}
         then
           {l := (flight k)}
           (loop {k + 1} {total + l})
         else
           total)))

 ;; 4. mixed : integration by the method of the trapezoids of x^3 - 2x + 1 on [0,10]
 (define (f x)
   (type flonum? x)
   {x * (x * x) - 2.0 * x + 1.0})
 (type ((flonum?) procedure? (flonum?)) f) ; the signature of f : (f x) is a flonum
 (define (bench-trapezoid n)
   (type fixnum? n)
   {h := 10.0 / (exact->inexact n)}
   {acc0 := ((f 0.0) + (f 10.0)) / 2.0}
   (let loop ((i 1) (x h) (acc acc0))
     (if {i < n}
         (loop {i + 1} {x + h} {acc + (f x)})
         {acc * h})))
)
