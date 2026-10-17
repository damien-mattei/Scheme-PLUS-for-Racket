#lang racket/base

;; Scheme : the generic operators of Racket (+ - * / < ...) , no type.
;;
;; The benchmarks of the manual of Scheme+ (section "Type annotation and inference") : the same four programs written
;; five times : in Scheme (bench-scheme.rkt) ; in Scheme+ with the same prefix source , without and with the type
;; annotation (bench-scheme+.rkt , bench-annotated+.rkt) ; in Scheme+ as it is usually written , infix , := instead of
;; let , square brackets , without and with the type annotation (bench-infix+.rkt , bench-infix-annotated+.rkt).
;; To run them : racket run-benchmarks.rkt (compile first with raco make run-benchmarks.rkt , on a machine at rest).
;;
;; Author: Damien Mattei

(provide bench-float bench-matrix bench-integer bench-trapezoid)
 ;; 1. floats : polynomial by Horner , sigmoid and its derivative , on n points
 (define (bench-float n)
   (let loop ((i 0) (x 0.0) (acc 0.0))
     (if (< i n)
         (let ((p (+ 1.0 (* x (+ 2.0 (* x (+ 3.0 (* x 4.0))))))))
           (let ((s (/ 1.0 (+ 1.0 (exp (- p))))))
             (loop (+ i 1) (+ x 0.000001) (+ acc (* s (- 1.0 s))))))
         acc)))

 ;; 2. product of matrices : vectors of vectors of floats
 (define (make-matrix n v)
   (build-vector n (lambda (i) (build-vector n (lambda (j) (* v (+ 1.0 (exact->inexact (modulo (+ i j) 7)))))))))
 (define (matrix-product A B n)
   (let ((C (build-vector n (lambda (i) (make-vector n 0.0)))))
     (let loop-i ((i 0))
       (when (< i n)
         (let ((Ai (vector-ref A i)) (Ci (vector-ref C i)))
           (let loop-j ((j 0))
             (when (< j n)
               (let loop-k ((k 0) (sum 0.0))
                 (if (< k n)
                     (let ((a (vector-ref Ai k)) (b (vector-ref (vector-ref B k) j)))
                       (loop-k (+ k 1) (+ sum (* a b))))
                     (vector-set! Ci j sum)))
               (loop-j (+ j 1)))))
         (loop-i (+ i 1))))
     C))
 (define (bench-matrix n times)
   (let ((A (make-matrix n 0.5)) (B (make-matrix n 0.25)))
     (let loop ((t 0) (trace 0.0))
       (if (< t times)
           (let ((C (matrix-product A B n)))
             (let ((c (vector-ref (vector-ref C 0) 0)))
               (loop (+ t 1) (+ trace c))))
           trace))))

 ;; 3. integers : the lengths of the flights of Syracuse (Collatz) up to n
 (define (flight k)
   (let loop ((k k) (len 0))
     (if (= k 1)
         len
         (loop (if (= (remainder k 2) 0) (quotient k 2) (+ (* 3 k) 1)) (+ len 1)))))
 (define (bench-integer n)
   (let loop ((k 1) (total 0))
     (if (<= k n)
         (let ((l (flight k)))
           (loop (+ k 1) (+ total l)))
         total)))

 ;; 4. mixed : integration by the method of the trapezoids of x^3 - 2x + 1 on [0,10]
 (define (f x)
   (+ (- (* x (* x x)) (* 2.0 x)) 1.0))
 (define (bench-trapezoid n)
   (let ((h (/ 10.0 (exact->inexact n))))
     (let ((acc0 (/ (+ (f 0.0) (f 10.0)) 2.0)))
       (let loop ((i 1) (x h) (acc acc0))
         (if (< i n)
             (loop (+ i 1) (+ x h) (+ acc (f x)))
             (* acc h))))))
