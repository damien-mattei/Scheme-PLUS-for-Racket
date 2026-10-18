#lang typed/racket/base

;; Typed Racket : the same programs with the static types of Typed Racket , checked by its type checker ; with the types
;; proved its optimizer can use the operations without tests (unsafe-fl* ...) safely.
;;
;; The benchmarks of the manual of Scheme+ (section "Type annotation and inference") : the same four programs written
;; six times : in Scheme (bench-scheme.rkt) ; in Scheme+ with the same prefix source , without and with the type
;; annotation (bench-scheme+.rkt , bench-annotated+.rkt) ; in Scheme+ as it is usually written , infix , := instead of
;; let , square brackets , without and with the type annotation (bench-infix+.rkt , bench-infix-annotated+.rkt) ; in
;; Typed Racket (bench-typed-racket.rkt).
;; To run them : racket run-benchmarks.rkt (compile first with raco make run-benchmarks.rkt , on a machine at rest).
;;
;; Author: Damien Mattei

(require racket/fixnum) ; for bench-integer : the operations fx

(provide bench-float bench-matrix bench-integer bench-integer-generic bench-trapezoid)
 ;; 1. floats : polynomial by Horner , sigmoid and its derivative , on n points
 (: bench-float (-> Index Flonum))
 (define (bench-float n)
   (let loop ((i : Nonnegative-Fixnum 0) (x : Flonum 0.0) (acc : Flonum 0.0))
     (if (< i n)
         (let ((p (+ 1.0 (* x (+ 2.0 (* x (+ 3.0 (* x 4.0))))))))
           (let ((s (/ 1.0 (+ 1.0 (exp (- p))))))
             (loop (+ i 1) (+ x 0.000001) (+ acc (* s (- 1.0 s))))))
         acc)))

 ;; 2. product of matrices : vectors of vectors of floats
 (: make-matrix (-> Index Flonum (Vectorof (Vectorof Flonum))))
 (define (make-matrix n v)
   (build-vector n (lambda ([i : Index]) (build-vector n (lambda ([j : Index]) (* v (+ 1.0 (exact->inexact (modulo (+ i j) 7)))))))))
 (: matrix-product (-> (Vectorof (Vectorof Flonum)) (Vectorof (Vectorof Flonum)) Index (Vectorof (Vectorof Flonum))))
 (define (matrix-product A B n)
   (let ((C (build-vector n (lambda ([i : Index]) (make-vector n 0.0)))))
     (let loop-i ((i : Nonnegative-Fixnum 0))
       (when (< i n)
         (let ((Ai (vector-ref A i)) (Ci (vector-ref C i)))
           (let loop-j ((j : Nonnegative-Fixnum 0))
             (when (< j n)
               (let loop-k ((k : Nonnegative-Fixnum 0) (sum : Flonum 0.0))
                 (if (< k n)
                     (let ((a (vector-ref Ai k)) (b (vector-ref (vector-ref B k) j)))
                       (loop-k (+ k 1) (+ sum (* a b))))
                     (vector-set! Ci j sum)))
               (loop-j (+ j 1)))))
         (loop-i (+ i 1))))
     C))
 (: bench-matrix (-> Index Index Flonum))
 (define (bench-matrix n times)
   (let ((A (make-matrix n 0.5)) (B (make-matrix n 0.25)))
     (let loop ((t : Nonnegative-Fixnum 0) (trace : Flonum 0.0))
       (if (< t times)
           (let ((C (matrix-product A B n)))
             (let ((c (vector-ref (vector-ref C 0) 0)))
               (loop (+ t 1) (+ trace c))))
           trace))))

 ;; 3. integers : the lengths of the flights of Syracuse (Collatz) up to n
 ;; written naturally : 3k + 1 of a Fixnum can leave the fixnums , so it is an Integer for Typed Racket and the
 ;; operations stay generic (slower than Scheme)
 (: flight (-> Integer Integer))
 (define (flight k)
   (let loop ((k : Integer k) (len : Integer 0))
     (if (= k 1)
         len
         (loop (if (= (remainder k 2) 0) (quotient k 2) (+ (* 3 k) 1)) (+ len 1)))))
 (: bench-integer-generic (-> Index Integer))
 (define (bench-integer-generic n)
   (let loop ((k : Integer 1) (total : Integer 0))
     (if (<= k n)
         (let ((l (flight k)))
           (loop (+ k 1) (+ total l)))
         total)))

 ;; with the operations of racket/fixnum written by hand , typed Fixnum (an error on overflow , as fx* and fx+ chosen
 ;; by the annotation of Scheme+ from (type fixnum? k)) : at the speed of Scheme+ annotated ; the version measured as
 ;; Typed Racket , bench-integer-generic is the extra line
 (: flight-fx (-> Fixnum Fixnum))
 (define (flight-fx k)
   (let loop ((k : Fixnum k) (len : Fixnum 0))
     (if (fx= k 1)
         len
         (loop (if (fx= (fxremainder k 2) 0) (fxquotient k 2) (fx+ (fx* 3 k) 1)) (fx+ len 1)))))
 (: bench-integer (-> Fixnum Fixnum))
 (define (bench-integer n)
   (let loop ((k : Fixnum 1) (total : Fixnum 0))
     (if (fx<= k n)
         (let ((l (flight-fx k)))
           (loop (fx+ k 1) (fx+ total l)))
         total)))

 ;; 4. mixed : integration by the method of the trapezoids of x^3 - 2x + 1 on [0,10]
 (: f (-> Flonum Flonum))
 (define (f x)
   (+ (- (* x (* x x)) (* 2.0 x)) 1.0))
 (: bench-trapezoid (-> Index Flonum))
 (define (bench-trapezoid n)
   (let ((h (/ 10.0 (exact->inexact n))))
     (let ((acc0 (/ (+ (f 0.0) (f 10.0)) 2.0)))
       (let loop ((i : Nonnegative-Fixnum 1) (x : Flonum h) (acc : Flonum acc0))
         (if (< i n)
             (loop (+ i 1) (+ x h) (+ acc (f x)))
             (* acc h))))))
