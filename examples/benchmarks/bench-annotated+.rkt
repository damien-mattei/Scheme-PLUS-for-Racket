#lang reader SRFI-105

;; Scheme+ with the type annotation (ANNOTATION-ON) : with the forms (type ...) the reader specializes the operators ,
;; {x * y} of two flonums is read as (fl* x y) , {i + 1} of a fixnum as (fx+ i 1) ... To see the code produced :
;; racket <path of SRFI-105>/src/curly-infix2prefix4racket.rkt bench-annotated+.rkt
;;
;; The benchmarks of the manual of Scheme+ (section "Type annotation and inference") : the same four programs written
;; five times : in Scheme (bench-scheme.rkt) ; in Scheme+ with the same prefix source , without and with the type
;; annotation (bench-scheme+.rkt , bench-annotated+.rkt) ; in Scheme+ as it is usually written , infix , := instead of
;; let , square brackets , without and with the type annotation (bench-infix+.rkt , bench-infix-annotated+.rkt).
;; To run them : racket run-benchmarks.rkt (compile first with raco make run-benchmarks.rkt , on a machine at rest).
;;
;; Author: Damien Mattei

(module bench-annotated+ racket
 (require Scheme+)
 ANNOTATION-ON
 (provide bench-float bench-matrix bench-integer bench-trapezoid)
 ;; 1. floats : polynomial by Horner , sigmoid and its derivative , on n points
 (define (bench-float n)
   (type fixnum? n)
   (let loop ((i 0) (x 0.0) (acc 0.0))
     (if (< i n)
         (let ((p (+ 1.0 (* x (+ 2.0 (* x (+ 3.0 (* x 4.0))))))))
           (type flonum? p)
           (let ((s (/ 1.0 (+ 1.0 (exp (- 0.0 p))))))
             (type flonum? s)
             (loop (+ i 1) (+ x 0.000001) (+ acc (* s (- 1.0 s))))))
         acc)))

 ;; 2. product of matrices : vectors of vectors of floats
 (define (make-matrix n v)
   (build-vector n (lambda (i) (build-vector n (lambda (j) (* v (+ 1.0 (exact->inexact (modulo (+ i j) 7)))))))))
 (define (matrix-product A B n)
   (type fixnum? n)
   (let ((C (build-vector n (lambda (i) (make-vector n 0.0)))))
     (let loop-i ((i 0))
       (when (< i n)
         (let ((Ai (vector-ref A i)) (Ci (vector-ref C i)))
           (let loop-j ((j 0))
             (when (< j n)
               (let loop-k ((k 0) (sum 0.0))
                 (if (< k n)
                     (let ((a (vector-ref Ai k)) (b (vector-ref (vector-ref B k) j)))
                       (type flonum? a b)
                       (loop-k (+ k 1) (+ sum (* a b))))
                     (vector-set! Ci j sum)))
               (loop-j (+ j 1)))))
         (loop-i (+ i 1))))
     C))
 (define (bench-matrix n times)
   (type fixnum? n times)
   (let ((A (make-matrix n 0.5)) (B (make-matrix n 0.25)))
     (let loop ((t 0) (trace 0.0))
       (if (< t times)
           (let ((C (matrix-product A B n)))
             (let ((c (vector-ref (vector-ref C 0) 0)))
               (type flonum? c)
               (loop (+ t 1) (+ trace c))))
           trace))))

 ;; 3. integers : the lengths of the flights of Syracuse (Collatz) up to n
 (define (flight k)
   (type fixnum? k)
   (let loop ((k k) (len 0))
     (if (= k 1)
         len
         (loop (if (= (remainder k 2) 0) (quotient k 2) (+ (* 3 k) 1)) (+ len 1)))))
 (define (bench-integer n)
   (type fixnum? n)
   (let loop ((k 1) (total 0))
     (if (<= k n)
         (let ((l (flight k)))
           (type fixnum? l)
           (loop (+ k 1) (+ total l)))
         total)))

 ;; 4. mixed : integration by the method of the trapezoids of x^3 - 2x + 1 on [0,10]
 (define (f x)
   (type flonum? x)
   (+ (- (* x (* x x)) (* 2.0 x)) 1.0))
 (type ((flonum?) procedure? (flonum?)) f) ; the signature of f : (f x) is a flonum
 (define (bench-trapezoid n)
   (type fixnum? n)
   (let ((h (/ 10.0 (exact->inexact n))))
     (type flonum? h)
     (let ((acc0 (/ (+ (f 0.0) (f 10.0)) 2.0)))
       (type flonum? acc0)
       (let loop ((i 1) (x h) (acc acc0))
         (if (< i n)
             (loop (+ i 1) (+ x h) (+ acc (f x)))
             (* acc h))))))
)
