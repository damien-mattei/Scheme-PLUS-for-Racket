#lang reader SRFI-105

;; Scheme+ as it is usually written , with the type annotation (ANNOTATION-ON) : the same text as bench-infix+.rkt with
;; the lines (type ...) of the parameters and the signatures of the procedures ; the types of the variables of := are
;; inferred from their values. To see the code produced :
;; racket <path of SRFI-105>/src/curly-infix2prefix4racket.rkt bench-infix-annotated+.rkt
;;
;; The benchmarks of the manual of Scheme+ (section "Type annotation and inference") : the same four programs written
;; six times : in Scheme (bench-scheme.rkt) ; in Scheme+ with the same prefix source , without and with the type
;; annotation (bench-scheme+.rkt , bench-annotated+.rkt) ; in Scheme+ as it is usually written , infix , := instead of
;; let , square brackets , without and with the type annotation (bench-infix+.rkt , bench-infix-annotated+.rkt) ; in
;; Typed Racket (bench-typed-racket.rkt).
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
   #;(let loop ((i 0) (x 0.0) (acc 0.0))
     (if {i < n}
         then
           {p := 1.0 + x * (2.0 + x * (3.0 + x * 4.0))}
           {s := 1.0 / (1.0 + (exp (- p)))}
           (loop {i + 1} {x + 0.000001} {acc + s * (1.0 - s)})
         else
           acc))
   ;; while of Scheme+ : the computation of p and s stays in the body (in a step of a do it would be 30 % slower)
   {i := 0}
   {x := 0.0}
   {acc := 0.0}
   (while {i < n}
     {p := 1.0 + x * (2.0 + x * (3.0 + x * 4.0))}
     {s := 1.0 / (1.0 + (exp (- p)))}
     {acc := acc + s * (1.0 - s)}
     {x := x + 0.000001}
     {i := i + 1})
   acc)

 ;; 2. product of matrices : vectors of vectors of floats
 (define (make-matrix n v)
   (build-vector n (lambda (i) (build-vector n (lambda (j) {v * (1.0 + (exact->inexact (modulo {i + j} 7)))})))))
 
 #;(define (matrix-product A B n)
   (type #(#(flonum?)) A B) ; vectors of vectors of floats
   (type ℕ n i j k)
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
 ;; the loops are do of Scheme , $+> allows the definitions := in the body of a do ; the types of the variables of
 ;; a do are written in its body , they are known by its steps and its test too
 #;(define (matrix-product A B n)
   (type #(#(flonum?)) A B) ; vectors of vectors of floats
   (type ℕ n)
   {C := (build-vector n (lambda (i) (make-vector n 0.0)))}
   (type #(#(flonum?)) C)
   (do ((i 0 {i + 1})) ({i = n})
     (type ℕ i)
     ($+> {Ai := A[i]}
          {Ci := C[i]}
          (do ((j 0 {j + 1})) ({j = n})
            (type ℕ j)
            (do ((k 0 {k + 1})
                 (sum 0.0 {sum + Ai[k] * B[k][j]})) ; the sum is a variable of the loop
                ({k = n} {Ci[j] := sum})
              (type ℕ k)
              (type flonum? sum)))))
   C)
 ;; the loops are do of Scheme (the body of a do can contain definitions) ; the types of the variables of
 ;; a do are written in its body , they are known by its steps and its test too
 (define (matrix-product A B n)
   (type #(#(flonum?)) A B) ; vectors of vectors of floats
   (type ℕ n)
   {C := (build-vector n (lambda (i) (make-vector n 0.0)))}
   (type #(#(flonum?)) C)
   (do ((i 0 {i + 1})) ({i = n})
     (type ℕ i)
     {Ai := A[i]}
     {Ci := C[i]}
     (do ((j 0 {j + 1})) ({j = n})
       (type ℕ j)
       (do ((k 0 {k + 1})
            (sum 0.0 {sum + Ai[k] * B[k][j]})) ; the sum is a variable of the loop
           ({k = n} {Ci[j] := sum})
         (type ℕ k)
         (type flonum? sum))))
   C)
 
 (type ((#(#(flonum?))) procedure? (#(#(flonum?)) #(#(flonum?)) fixnum?)) matrix-product)
 
 (define (bench-matrix n times)
   (type fixnum? n times)
   {A := (make-matrix n 0.5)}
   {B := (make-matrix n 0.25)}
   #;(let loop ((t 0) (trace 0.0))
     (if {t < times}
         then
           {C := (matrix-product A B n)}
           {c := C[0][0]}
           (loop {t + 1} {trace + c})
         else
           trace))
   {t := 0}
   {trace := 0.0}
   (while {t < times}
     {C := (matrix-product A B n)}
     {trace := trace + C[0][0]}
     {t := t + 1})
   trace)

 ;; 3. integers : the lengths of the flights of Syracuse (Collatz) up to n
 (define (flight k)
   (type fixnum? k)
   #;(let loop ((k k) (len 0))
     (if {k = 1}
         len
         (loop (if {(remainder k 2) = 0} (quotient k 2) {3 * k + 1}) {len + 1})))
   (do ((k k (if {(remainder k 2) = 0} (quotient k 2) {3 * k + 1}))
        (len 0 {len + 1}))
       ({k = 1} len)
     (type fixnum? k len)))
 
 (type ((fixnum?) procedure? (fixnum?)) flight) ; the signature of flight : (flight k) is a fixnum
 
 (define (bench-integer n)
   (type fixnum? n)
   #;(let loop ((k 1) (total 0))
     (if {k <= n}
         then
           {l := (flight k)}
           (loop {k + 1} {total + l})
         else
           total))
   (do ((k 1 {k + 1})
        (total 0 {total + (flight k)}))
       ({k > n} total)
     (type fixnum? k total)))

 
 ;; 4. mixed : integration by the method of the trapezoids of x^3 - 2x + 1 on [0,10]
 (define (f x)
   (type flonum? x)
   {x * (x * x) - 2.0 * x + 1.0})
 
 (type ((flonum?) procedure? (flonum?)) f) ; the signature of f : (f x) is a flonum
 
 (define (bench-trapezoid n)
   (type fixnum? n)
   {h := 10.0 / (exact->inexact n)}
   {acc0 := ((f 0.0) + (f 10.0)) / 2.0}
   #;(let loop ((i 1) (x h) (acc acc0))
     (if {i < n}
         (loop {i + 1} {x + h} {acc + (f x)})
         {acc * h}))
   (do ((i 1 {i + 1})
        (x h {x + h})
        (acc acc0 {acc + (f x)}))
       ({i >= n} {acc * h})
     (type fixnum? i)
     (type flonum? x acc)))
)
