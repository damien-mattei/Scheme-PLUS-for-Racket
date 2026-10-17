#lang racket/base

;; Runs the five versions of the benchmarks. For each benchmark : the median of 7 cpu times in ms , after a warm-up ,
;; for each version , and a check that the five versions give the same result.
;;
;; The benchmarks of the manual of Scheme+ (section "Type annotation and inference") : the same four programs written
;; five times : in Scheme (bench-scheme.rkt) ; in Scheme+ with the same prefix source , without and with the type
;; annotation (bench-scheme+.rkt , bench-annotated+.rkt) ; in Scheme+ as it is usually written , infix , := instead of
;; let , square brackets , without and with the type annotation (bench-infix+.rkt , bench-infix-annotated+.rkt).
;; To run them : racket run-benchmarks.rkt (compile first with raco make run-benchmarks.rkt , on a machine at rest).
;;
;; Author: Damien Mattei

(require (prefix-in plain: "bench-scheme.rkt") (prefix-in splus: "bench-scheme+.rkt") (prefix-in annot: "bench-annotated+.rkt")
         (prefix-in infix: "bench-infix+.rkt") (prefix-in infix-annot: "bench-infix-annotated+.rkt")
         racket/list)

;; time-apply (Racket) calls the procedure and gives its results , the cpu time , the real time and the time of the
;; garbage collector , in ms
(define (median l) (list-ref (sort l <) (quotient (length l) 2)))
(define (measure thunk) ; median of 7 cpu times in ms , after a warm-up , and the result
  (thunk)
  (define times (for/list ([k 7]) (collect-garbage) (define-values (r cpu real gc) (time-apply thunk '())) cpu))
  (values (median times) (thunk)))

(define versions '("Scheme" "Scheme+ prefix" "Scheme+ prefix annotated" "Scheme+ infix" "Scheme+ infix annotated"))

;; for each benchmark : its name , its procedure in each version , its arguments
(define benchs
  (list (list "floats (Horner , sigmoid)"
              (list plain:bench-float splus:bench-float annot:bench-float infix:bench-float infix-annot:bench-float)
              '(20000000))
        (list "product of matrices 200x200"
              (list plain:bench-matrix splus:bench-matrix annot:bench-matrix infix:bench-matrix infix-annot:bench-matrix)
              '(200 20))
        (list "integers (Syracuse)"
              (list plain:bench-integer splus:bench-integer annot:bench-integer infix:bench-integer infix-annot:bench-integer)
              '(1000000))
        (list "trapezoids"
              (list plain:bench-trapezoid splus:bench-trapezoid annot:bench-trapezoid infix:bench-trapezoid
                    infix-annot:bench-trapezoid)
              '(20000000))))

(for ([b benchs])
  (define args (third b))
  (define measures (for/list ([proc (second b)]) ; (time result) for each version
                     (define-values (t r) (measure (lambda () (apply proc args))))
                     (list t r)))
  (define results (map second measures))
  (printf "~a :\n" (first b))
  (for ([v versions] [m measures])
    (printf "  ~a : ~a ms\n" v (first m)))
  (printf "  same results : ~a\n" (andmap (lambda (r) (= r (first results))) results)))
