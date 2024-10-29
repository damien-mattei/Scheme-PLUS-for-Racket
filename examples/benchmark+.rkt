#lang reader SRFI-105

;;#lang racket/base

(require racket/flonum)

(require Scheme+)

(define (deal)
  ;; assumption: random's performance is mostly uniform
  (exact->inexact (random -5000 5000)))
(define test-count 5000000)

(define (pair-add a b)
  (cons (fl+ (car a) (car b)) (fl+ (cdr a) (cdr b))))
(define (pair-unit p)
  (define x (car p))
  (define y (cdr p))
  (define norm (flsqrt (fl+ (fl* x x) (fl* y y))))
  (cons (fl/ x norm) (fl/ y norm)))
(define (pair-scale p scalar)
  (cons (fl* (car p) scalar) (fl* (cdr p) scalar)))
(define (test-pair)
  (define points
    (for/vector ([i (in-range test-count)])
      (cons (deal) (deal))))
  (define toadd (cons (deal) (deal)))
  (define adds (for/vector ([p (in-vector points)])
                 (pair-add p toadd)))
  (define norms (for/vector ([p (in-vector points)])
                  (pair-unit p)))
  (define scalar (deal))
  (define scales (for/vector ([p (in-vector points)])
                  (pair-scale p scalar)))
  (list (vector-length adds) (vector-length norms) (vector-length scales)))

(define (flvector-add a b)
  (flvector (fl+ (flvector-ref a 0) (flvector-ref b 0))
            (fl+ (flvector-ref a 1) (flvector-ref b 1))))
(define (flvector-unit p)
  (define x (flvector-ref p 0))
  (define y (flvector-ref p 1))
  (define norm (flsqrt (fl+ (fl* x x) (fl* y y))))
  (flvector (fl/ x norm) (fl/ y norm)))
(define (flvector-scale p scalar)
  (flvector (fl* (flvector-ref p 0) scalar) (fl* (flvector-ref p 1) scalar)))
(define (test-flvector)
  (define points
    (for/vector ([i (in-range test-count)])
      (flvector (deal) (deal))))
  (define toadd (flvector (deal) (deal)))
  (define adds (for/vector ([p (in-vector points)])
                 (flvector-add p toadd)))
  (define norms (for/vector ([p (in-vector points)])
                  (flvector-unit p)))
  (define scalar (deal))
  (define scales (for/vector ([p (in-vector points)])
                   (flvector-scale p scalar)))
  (list (vector-length adds) (vector-length norms) (vector-length scales)))


(define (vector-add a b)
  (vector (+ (vector-ref a 0) (vector-ref b 0))
            (+ (vector-ref a 1) (vector-ref b 1))))
(define (vector-unit p)
  (define x (vector-ref p 0))
  (define y (vector-ref p 1))
  (define norm (sqrt (+ (* x x) (* y y))))
  (vector (/ x norm) (/ y norm)))
(define (vector-scale p scalar)
  (vector (* (vector-ref p 0) scalar) (* (vector-ref p 1) scalar)))
(define (test-vector)
  (define points
    (for/vector ([i (in-range test-count)])
      (vector (deal) (deal))))
  (define toadd (vector (deal) (deal)))
  (define adds (for/vector ([p (in-vector points)])
                 (vector-add p toadd)))
  (define norms (for/vector ([p (in-vector points)])
                  (vector-unit p)))
  (define scalar (deal))
  (define scales (for/vector ([p (in-vector points)])
                   (vector-scale p scalar)))
  (list (vector-length adds) (vector-length norms) (vector-length scales)))


(define (vector-add+ a b)
  (vector {a[0] + b[0]}
	  {a[1] + b[1]}))
  
(define (vector-unit+ p)
  {x <- p[0]}
  {y <- p[1]}
  (define norm (sqrt {x * x + y * y}))
  (vector {x / norm} {y / norm}))

(define (vector-scale+ p scalar)
  (vector {p[0] * scalar} {p[1] * scalar}))

(define (test-vector+)
  (define points
    (for/vector ([i (in-range test-count)])
      (vector (deal) (deal))))
  (define toadd (vector (deal) (deal)))
  (define adds (for/vector ([p (in-vector points)])
                 (vector-add+ p toadd)))
  (define norms (for/vector ([p (in-vector points)])
                  (vector-unit+ p)))
  (define scalar (deal))
  (define scales (for/vector ([p (in-vector points)])
                   (vector-scale+ p scalar)))
  (list (vector-length adds) (vector-length norms) (vector-length scales)))



(struct vec2 (x y))
(define (struct-add a b)
  (vec2 (fl+ (vec2-x a) (vec2-y b))
             (fl+ (vec2-y a) (vec2-y b))))
(define (struct-unit p)
  (define x (vec2-x p))
  (define y (vec2-y p))
  (define norm (flsqrt (fl+ (fl* x x) (fl* y y))))
  (vec2 (fl/ x norm) (fl/ y norm)))
(define (struct-scale p scalar)
  (vec2 (fl* (vec2-x p) scalar) (fl* (vec2-y p) scalar)))
(define (test-struct)
  (define points
    (for/vector ([i (in-range test-count)])
      (vec2 (deal) (deal))))
  (define toadd (vec2 (deal) (deal)))
  (define adds (for/vector ([p (in-vector points)])
                 (struct-add p toadd)))
  (define norms (for/vector ([p (in-vector points)])
                  (struct-unit p)))
  (define scalar (deal))
  (define scales (for/vector ([p (in-vector points)])
                   (struct-scale p scalar)))
  (list (vector-length adds) (vector-length norms) (vector-length scales)))

(define (complex-unit p)
  (define x (flreal-part p))
  (define y (flimag-part p))
  (define norm (flsqrt (fl+ (fl* x x) (fl* y y))))
  (make-flrectangular (fl/ x norm) (fl/ y norm)))
(define (complex-scale p scalar)
  (make-flrectangular (fl* (flreal-part p) scalar) (fl* (flimag-part p) scalar)))
(define (test-complex)
  (define points
    (for/vector ([i (in-range test-count)])
      (make-flrectangular (deal) (deal))))
  (define toadd (make-flrectangular (deal) (deal)))
  (define adds (for/vector ([p (in-vector points)])
                 (+ p toadd)))
  (define norms (for/vector ([p (in-vector points)])
                  (complex-unit p)))
  (define scalar (deal))
  (define scales (for/vector ([p (in-vector points)])
                   (complex-scale p scalar)))
  (list (vector-length adds) (vector-length norms) (vector-length scales)))

(define (main)
  (display "Pairs:")
  (collect-garbage 'major)
  (time (displayln (test-pair)))

  (display "flvectors:")
  (collect-garbage 'major)
  (time (displayln (test-flvector)))

  (display "structs:")
  (collect-garbage 'major)
  (time (displayln (test-struct)))

  (display "complex:")
  (collect-garbage 'major)
  (time (displayln (test-complex)))

  (display "vectors:")
  (collect-garbage 'major)
  (time (displayln (test-vector)))

  (display "vectors+:")
  (collect-garbage 'major)
  (time (displayln (test-vector+)))

  )

(module* main #f
  (main))

