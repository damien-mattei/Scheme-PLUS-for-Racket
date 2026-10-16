#lang reader SRFI-105

;; Test suite for the extended SRFI 105 Curly Infix syntax of Scheme+
;;
;; It exercises mainly infix expressions (operator precedence and
;; associativity), infix expressions with inner round parentheses, mixed
;; prefix / infix expressions that are autodetected, prefix expressions
;; with an operator as argument, chains of in/equalities, and a few postfix
;; expressions.  Every example from the "Scheme+ Syntax and Conventions"
;; part of the manual that is a pure syntax example is reused here.
;;
;; Run it with:   racket examples/test-curly-infix+.rkt
;; A silent run (only the parser trace) means every check passed ; any
;; failure is printed by rackunit as "FAILURE ...".

(module test-curly-infix racket

  (require Scheme+)
  ANNOTATION-ON
  (require rackunit
           (only-in syntax/macro-testing convert-compile-time-error)) ; section 30
  
  ;; ================================================================
  ;; 1. Plain infix : operator precedence and associativity
  ;; ================================================================

  (check-equal? {2 + 3 * 4} 14)                 ; * before +
  (check-equal? {3 * 5 + 2} 17)
  (check-equal? {5 + 2} 7)
  (check-equal? {3 * 7 + 2 - 12 / 3 + -2} 17)   ; manual
  (check-equal? {10 - 2 - 3} 5)                 ; - is left associative
  (check-equal? {100 / 10 / 5} 2)              ; / is left associative
  (check-equal? {1 + 2 + 3 + 4 + 5} 15)         ; n-ary +
  (check-equal? {2 * 3 * 4} 24)                 ; n-ary *
  (check-equal? {-5 + 3} -2)                    ; unary minus
  (check-equal? {3 * -2} -6)

  ;; exponentiation ** and superscripts
  (check-equal? {2 ** 3} 8)
  (check-equal? {2 * 2 ** 3} 16)               ; ** binds tighter than *
  (check-equal? {3 ²} 9)
  (check-equal? {4 ² + 3 ²} 25)
  (check-equal? {3 · 5 + 2 ³} 23)              ; manual : · is * , ³ is cube
  (check-equal? {3 ² + 2 · 3 · 5 + 5 ²} 64)    ; manual
  (check-equal? {3 ² + 2 · 3 · 5 + 5 ²} (+ (expt 3 2) (* 2 3 5) (expt 5 2)))

  ;; exact rationals
  (check-equal? {1 / 2} 1/2)
  (check-equal? {1 / 2 + 1 / 4} 3/4)

  ;; modulo, bit shifts
  (check-equal? {17 % 5} 2)
  (check-equal? {1 << 5} 32)
  (check-equal? {40 >> 2} 10)

  ;; ================================================================
  ;; 2. Comparison and boolean operators in infix
  ;; ================================================================

  (check-true  {3 < 5})
  (check-true  {5 > 3})
  (check-true  {4 <= 4})
  (check-true  {4 >= 4})
  (check-true  {2 ≤ 9})
  (check-true  {5 ≥ 5})
  (check-true  {2 + 2 = 4})
  (check-true  {3 <> 4})
  (check-false {3 ≠ 3})

  (check-true  {{2 < 3} and {4 < 5}})
  (check-false {{2 < 3} and {5 < 4}})
  (check-true  (or {1 > 2} {2 > 1}))           ; infix args of a prefix or

  ;; ================================================================
  ;; 3. Infix expressions with inner round parentheses
  ;; ================================================================

  (check-equal? {3 * (2 + 4) - 1} 17)                     ; manual
  (check-equal? (let ((x {3 * (2 + 4) - 1})) x) 17)       ; manual : (define x {3 * (2 + 4) - 1})
  (check-equal? {(3 + 1) * (2 + 4)} 24)
  (check-equal? {((1 + 2) * 3) - (4 / (1 + 1))} 7)
  (check-equal? {(10 - 2) / (1 + 1) + (2 * (3 + 1))} 12)
  (check-equal? {2 ** (1 + 2)} 8)
  (check-equal? {(2 + 3) ² - (1 + 1) ³} 17)               ; 25 - 8

  ;; the two big ones from the manual (inner ( ) plus prefix calls)
  (check-equal? {3 * (- 9 4) + 2} 17)
  (check-equal? {(- 7 (3 * (+ 2 4) - 1)) + 3} -7)
  (check-= {(3 + 1) * (2 * (+ 2 1) - (sin 0.3)) + ((* 2 5) - 5)}
           (+ (* (+ 3 1) (- (* 2 (+ 2 1)) (sin 0.3))) (- (* 2 5) 5))
           1e-12)

  ;; ================================================================
  ;; 4. Mixed prefix / infix, autodetected
  ;; ================================================================

  (check-equal? {3 * (+ 2 4) - 1} 17)          ; manual : (+ 2 4) prefix inside infix
  (check-equal? (- (+ 3 3) {2 + 2}) 2)         ; manual : infix arg of a prefix -
  (check-equal? {(max 3 7) + (min 10 2)} 9)
  (check-equal? {(expt 2 3) + 1} 9)
  (check-equal? {(apply + (list 1 2 3 4)) * 2} 20)
  (check-equal? {(vector-ref (vector 10 20 30) 1) + 5} 25)
  (check-equal? {(- 10 4) * 2} 12)             ; prefix (- 10 4) as an operand of infix *
  (check-equal? {(gcd 12 18) + (lcm 3 4)} 18)  ; two prefix calls, infix +
  (check-equal? (+ {2 * 3} {4 * 5}) 26)        ; infix sub-expressions of a prefix +
  (check-equal? (list {1 + 1} {2 + 2} {3 + 3}) (list 2 4 6))
  (check-equal? {√ 16} 4)                      ; prefix-style call written in { }
  (check-equal? (√ {9 + 16}) 5)                ; infix arg of a prefix call

  ;; ================================================================
  ;; 5. Nested curly braces
  ;; ================================================================

  (check-equal? {{1 + 2} * {3 + 4}} 21)
  (check-equal? {{10 / 2} + {2 ** 3}} 13)
  (check-equal? {{{1 + 1} + {2 + 2}} * {3 + 4}} 42)

  ;; ================================================================
  ;; 6. Infix inside definitions, conditionals, recursion (manual)
  ;; ================================================================

  (define (fib n)                             ; manual
    (if {n < 2}
        n
        {(fib (n - 1)) + (fib (n - 2))}))
  (check-equal? (fib 7) 13)
  (check-equal? (fib 10) 55)

  (define (fact n)
    (if {n = 0}
        1
        {n * (fact {n - 1})}))
  (check-equal? (fact 5) 120)

  (define (norm x y) {x ² + y ²})             ; manual
  (check-equal? (norm 3 4) 25)

  (define (line-length x0 y0 x1 y1)           ; manual
    (√ {(x1 - x0) ² + (y1 - y0) ²}))
  (check-equal? (line-length 0 0 3 4) 5)

  (define (discriminant a b c) {b ² - 4 * a * c})
  (check-equal? (discriminant 1 -5 6) 1)
  (check-equal? (discriminant 2 4 2) 0)

  ;; ================================================================
  ;; 7. Curly infix inside macro definitions (manual)
  ;; ================================================================

  (define-syntax +=
    (syntax-rules ()
      ({var1 _ var2} {var1 := var1 + var2})))

  (define y 3)
  {y += 7}
  (check-equal? y 10)

  (define-syntax plus
    (syntax-rules ()
      ({var1 _ ...} {var1 + ...})))

  (check-equal? {2 plus 3} 5)
  (check-equal? {2 plus 3 plus 4 plus 5 plus 6} 20)

  ;; ================================================================
  ;; 8. Assignment operators on infix right-hand sides
  ;; ================================================================

  (define x 0)
  {x <- {3 * (2 + 4)}}
  (check-equal? x 18)
  {x := x + 2}
  (check-equal? x 20)
  {x <- (x - 5) / 3}
  (check-equal? x 5)

  ;; ================================================================
  ;; 9. Postfix expressions
  ;; ================================================================

  (check-equal? {1 2 3 4 +} 10)               ; manual : postfix n-ary +
  (check-equal? {5 !} 120)                     ; builtin postfix factorial
  (check-equal? {4 ²} 16)                      ; postfix superscript
  (check-equal? {2 ³} 8)
  (check-equal? {(2 + 3) !} 120)              ; postfix with an inner ( ) operand
  (check-equal? {(1 + 1) 3 4 +} 9)           ; postfix +, parenthesized operand
  (check-equal? { {2 + 3} ² } 25)             ; postfix superscript on a nested { }

  ;; ================================================================
  ;; 10. Plain prefix still works unchanged
  ;; ================================================================

  (check-equal? (+ 1 2 3 4) 10)
  (check-equal? (- 7 (* 3 (+ 2 4))) -11)
  (check-equal? (map (lambda (n) {n * n}) (list 1 2 3 4)) (list 1 4 9 16))

  ;; ================================================================
  ;; 11. Prefix expressions with an operator as argument, inside { }
  ;;     (not a canonical infix expression : they stay in prefix)
  ;; ================================================================

  (require (only-in srfi/1 fold reduce))
  (define L (list 1 2 3))
  (define (my-fold f init l) (foldl f init l)) ; a procedure unknown to the reader

  (check-equal? {(fold + 0 L)} 6)
  (check-equal? {(reduce + 0 L)} 6)
  (check-equal? {(vector-map + (vector 1 2 3) (vector 10 20 30))} (vector 11 22 33))
  (check-equal? {(my-fold + 0 L)} 6)
  (check-equal? {(fold + 0 L) * 2} 12)
  (check-equal? {(fold * 1 L) + (reduce max 0 L)} 9)
  {sum-L := (fold + 0 L)}
  (check-equal? sum-L 6)

  ;; ================================================================
  ;; 12. Chains of in/equalities (the examples of in-equalities.rkt)
  ;; ================================================================

  ;; the REPL examples at the end of in-equalities.rkt
  (check-false {0 ≤ 1 ≤ 2 and #f})
  (check-true  {#t and (#f or 1 < 2 <= 2)})
  (check-false {#t and (#f or 1 < 2 < 2)})
  (check-false {(or 1 < 2 < 2)})              ; the in/equalities get priority over or/and
  (check-true  {(or 1 < 2 <= 2)})
  (check-equal? {(or 1 < 2)} 1)               ; a single binary in/equality is not grouped : prefix or of 1 , < , 2
  (check-equal? {(or 1 > 2)} 1)
  (check-true  {0 ≤ 4 ≤ 4})
  (check-true  {1 < 2 < 3 <= 3})
  (check-false {3 > 2 > 2})

  ;; the in/equalities-state-1 examples of in-equalities.rkt, each one compared with its prefix form
  ;; (and binds tighter than or) for several values of the variables, giving #t and #f
  (define ineq-args '((0 1 1 5 3 0 #t)   ; x a b c d y f
                      (5 1 1 5 3 2 #t)
                      (0 5 1 5 3 1 #f)
                      (0 1 2 5 3 2 #f)
                      (0 1 1 5 1 0 #t)))

  (define (check-same-values infix-proc prefix-proc)
    (for-each (lambda (args)
                (check-equal? (apply infix-proc args) (apply prefix-proc args)))
              ineq-args))

  (define (ineq-1 x a b c d y f) {x + 2 < 3 and a + 1 < 3 * b <= c - 1 < d + 2})
  (define (ineq-1-prefix x a b c d y f)
    (and (< (+ x 2) 3)
         (< (+ a 1) (* 3 b))
         (<= (* 3 b) (- c 1))
         (< (- c 1) (+ d 2))))
  (check-same-values ineq-1 ineq-1-prefix)
  (check-true (ineq-1 0 1 1 5 3 0 #t))
  (check-false (ineq-1 0 1 2 5 3 2 #f))

  (define (ineq-2 x a b c d y f) {x + 2 < 3 and a + 1 < 3 * b <= c - 1 < d + 2 or y})
  (define (ineq-2-prefix x a b c d y f) (or (ineq-1-prefix x a b c d y f) y))
  (check-same-values ineq-2 ineq-2-prefix)

  (define (ineq-3 x a b c d y f) {x + 2 < 3 and a + 1 < 3 * b <= c - 1 < d + 2 or y * 3 > 4})
  (define (ineq-3-prefix x a b c d y f) (or (ineq-1-prefix x a b c d y f) (> (* y 3) 4)))
  (check-same-values ineq-3 ineq-3-prefix)

  (define (ineq-4 x a b c d y f) {5 < 6 < 7 or y > 4 or 1 < 2 < 3})
  (define (ineq-4-prefix x a b c d y f) (or (< 5 6 7) (> y 4) (< 1 2 3)))
  (check-same-values ineq-4 ineq-4-prefix)

  (define (ineq-5 x a b c d y f) {x + 2 < 3 and a + 1 < 3 * b <= c - 1 < d + 2 or y * 3 > 4 and f and 1 < 2 < 3})
  (define (ineq-5-prefix x a b c d y f)
    (or (ineq-1-prefix x a b c d y f)
        (and (> (* y 3) 4) f (< 1 2 3))))
  (check-same-values ineq-5 ineq-5-prefix)

  (define (seven) 7)
  (check-equal? {(abs -3)} 3)                  ; (f x) is not an in/equality
  (check-equal? {(seven)} 7)                   ; (f)

  ;; the examples of the comments inside in-equalities.rkt
  (define (ineq-6 x d xw y yw) {1 + d < x - xw < 7 and 0 < y - yw < 5})
  (define (ineq-6-prefix x d xw y yw) (and (< (+ 1 d) (- x xw) 7) (< 0 (- y yw) 5)))
  (for-each (lambda (args)
              (check-equal? (apply ineq-6 args) (apply ineq-6-prefix args)))
            '((5 1 2 4 1) (5 3 2 4 1) (9 1 1 4 1) (5 1 2 7 1)))
  (check-true (ineq-6 5 1 2 4 1))

  (define (ineq-7 x a b d xw y yw) {a + 1 < b * 3 and 1 + d < x - xw < 7 and 0 < y - yw < 5})
  (define (ineq-7-prefix x a b d xw y yw)
    (and (< (+ a 1) (* b 3)) (< (+ 1 d) (- x xw) 7) (< 0 (- y yw) 5)))
  (for-each (lambda (args)
              (check-equal? (apply ineq-7 args) (apply ineq-7-prefix args)))
            '((5 1 2 1 2 4 1) (5 6 2 1 2 4 1) (5 1 2 3 2 4 1) (5 1 2 1 2 9 1)))
  (check-true (ineq-7 5 1 2 1 2 4 1))

  (define (ineq-8 a b c d) {a < b and c < d})
  (check-true (ineq-8 1 2 3 4))
  (check-false (ineq-8 1 2 4 3))

  (define (ineq-9 x y z t) {x < y < z = t})
  (check-true (ineq-9 1 2 3 3))
  (check-false (ineq-9 1 2 3 4))
  (check-false (ineq-9 2 1 3 3))

  ;; ================================================================
  ;; 13. Chains of in/equalities in an assignment or a definition
  ;;     (the operators with a lower precedence than the in/equalities
  ;;     end the chain : and , or , ~> , assignment , definition)
  ;; ================================================================

  (define p 5)
  {b1 := 1 < 2 < 3}                            ; (:= b1 (< 1 2 3))
  (check-true b1)
  {a2 := b2 := 3 < 2 < 1}                      ; (:= a2 b2 (< 3 2 1))
  (check-false a2)
  (check-false b2)
  {b3 <+ p + 1 < 7 < 8}                        ; (<+ b3 (< (+ p 1) 7 8))
  (check-true b3)
  {b4 := 0 < p < 10 and p > 3}                 ; (:= b4 (and (< 0 p 10) (> p 3)))
  (check-true b4)
  (define b5 #f)
  {1 < 2 < 3 =: b5}                            ; (=: (< 1 2 3) b5)
  (check-true b5)
  {b6 <- 1 < 2 < 3 or #f}                      ; (<- b6 (or (< 1 2 3) #f))
  (check-true b6)

  ;; ================================================================
  ;; 14. Regions of the pragmas ANNOTATION-ON / ANNOTATION-OFF
  ;;     (a pragma acts on the elements that follow it in the same list,
  ;;     until the next pragma or the end of the list)
  ;; ================================================================

  (define-syntax-rule (quote-it e) 'e)         ; a macro that uses its argument as data

  (define (quote-off)
    ANNOTATION-OFF                             ; OFF only in the body of quote-off
    (quote-it (+ 1 2)))
  (check-equal? (quote-off) '(+ 1 2))          ; not annotated : (+ 1 2) is not folded

  (define (twice-off a)
    ANNOTATION-OFF
    (type flonum? a)                           ; removed from the code , not used
    (* a 2.0))                                 ; stays * : accepts an exact integer
  (check-equal? (twice-off 2) 4.0)

  (define (twice-on a)                         ; ON in the body of twice-on : annotated whatever the state of the module
    ANNOTATION-ON
    (type flonum? a)
    (* a 2.0))                                 ; annotated : fl* , only for flonums
  (check-equal? (twice-on 2.5) 5.0)
  (check-exn exn:fail:contract? (lambda () (twice-on 2)))

  ANNOTATION-OFF                               ; OFF at the level of the module
  (check-equal? (quote-it (* 2 3)) '(* 2 3))
  ANNOTATION-ON                                ; ON again for the rest of the module

  ;; ================================================================
  ;; 15. Syntax quotes are data, like quoted expressions :
  ;;     #'(...) , #`(...) and (quote-syntax ...) are not parsed
  ;;     as infix or postfix , even inside { } , and not annotated
  ;; ================================================================

  (check-equal? {(syntax->datum #'(a + b))} '(a + b))
  (check-equal? {(syntax->datum #`(a + b))} '(a + b))
  (check-equal? {(syntax->datum (quote-syntax (a + b)))} '(a + b))
  (check-equal? {(syntax->datum #'(1 2 3 +))} '(1 2 3 +))
  (check-equal? {(syntax->datum #'(+ - * /))} '(+ - * /))
  (check-equal? (syntax->datum #'{a + b}) '(+ a b))                ; a syntax quoted { } is still infix
  (check-equal? {(syntax->datum #`(a + #,{1 + 2}))} '(a + 3))       ; the unsyntax part is evaluated
  (check-equal? (syntax->datum #'(+ 1 2)) '(+ 1 2))                 ; not folded to 3 by the annotation pass
  (check-equal? {(syntax->datum #'(+ 1 2))} '(+ 1 2))

  ;; ================================================================
  ;; 16. The macro definitions are not annotated : their patterns and
  ;;     templates are not code of the current scope
  ;; ================================================================

  {p8-a := 1}                                  ; fixnums , outside the macros
  {p8-b := 2}
  (define-syntax-rule (add-p8 p8-a p8-b) (+ p8-a p8-b)) ; the pattern variables are not the fixnums above
  (check-equal? (add-p8 1.5 2.5) 4.0)          ; was : fx+: contract violation

  (define-syntax count-plus (syntax-rules (+) ((_ (+ e ...)) 'plus) ((_ e) 'other))) ; was : (syntax-rules 0 ...)
  (check-equal? (count-plus (+ q r)) 'plus)
  (check-equal? (count-plus q) 'other)

  (define-syntax (add2-p8 stx) (syntax-case stx () ((_ p8-a p8-b) #'(+ p8-a p8-b))))
  (check-equal? (add2-p8 1.5 2.5) 4.0)

  (define-syntax-rule (twice-p8 x) (let () (type flonum? x) (* x 2.0))) ; the type annotation is removed from the template
  (check-equal? (twice-p8 1.5) 3.0)

  {p8-w := 2.5}                                ; a flonum
  (check-equal? (let-syntax ((m2 (syntax-rules (*) ((_ (* u v)) 'product) ((_ e) 'other))))
                  (list (m2 (* q r)) (+ p8-w 1.0))) ; the local macro is not annotated , the body is (fl+)
                '(product 3.5))

  ;; ================================================================
  ;; 17. The lists of datums of case are data : they are not parsed
  ;;     inside { } and not annotated , the key and the expressions are
  ;; ================================================================

  (define p10-op '*)
  (check-equal? {(case p10-op ((+ -) 'add) ((* /) 'mul) (else 'other))} 'mul) ; was : car: contract violation
  (check-equal? {(case {1 + 1} ((2) {10 * 2}) (else 0))} 20)                   ; the key and the expressions are parsed
  (check-equal? {(case 3 ((1 2) 'low) (else (3 * 4)))} 12)

  (define (kind-p10 op) (case op ((+) 'plus) ((*) 'times) (else 'other)))      ; was : (case op (0 'plus) (1 'times) ...)
  (check-equal? (kind-p10 '+) 'plus)
  (check-equal? (kind-p10 '*) 'times)
  (check-equal? (kind-p10 '-) 'other)

  {p10-x := 2.5}                                                               ; a flonum
  (define (f-p10 op) (case op ((+) (+ p10-x 1.0)) (else (* p10-x 2.0))))       ; the expressions are annotated : fl+ , fl*
  (check-equal? (f-p10 '+) 3.5)
  (check-equal? (f-p10 '-) 5.0)

  (def+ (case-def+ n)                                                          ; def+ converts its body at the expansion :
    (case n ((1) (10 - n)) (else (n * 2))))                                    ; the clauses are syntax objects there
  (check-equal? (case-def+ 1) 9)
  (check-equal? (case-def+ 3) 6)

  ;; ================================================================
  ;; 18. A parameter or a local variable hides the type of the outer
  ;;     variable of the same name (p12-x , p12-y : fixnums outside)
  ;; ================================================================

  (require (only-in srfi/8 receive))
  {p12-x := 1}
  {p12-y := 2}
  {p12-v := (vector 1 2 3)}                                                    ; a vector of fixnums
  (define (p12-half n) (/ n 2.0))                                             ; its return type is unknown

  (define (p12-g p12-x p12-y) (+ p12-x p12-y))                                ; was : (fx+ p12-x p12-y)
  (check-equal? (p12-g 1.5 2.5) 4.0)
  (def (p12-g2 p12-x p12-y) (* p12-x p12-y))                                  ; def of Scheme+
  (check-equal? (p12-g2 1.5 2.0) 3.0)
  (define p12-h (lambda (p12-x p12-y) (- p12-x p12-y)))
  (check-equal? (p12-h 2.5 1.0) 1.5)
  (define p12-k (case-lambda ((p12-x) (* p12-x 2)) ((p12-x p12-y) (+ p12-x p12-y))))
  (check-equal? (p12-k 1.5) 3.0)
  (check-equal? (p12-k 1.5 1.0) 2.5)
  (check-equal? (do ((p12-x 0.5 (* p12-x 2.0)) (p12-y 0.25 (+ p12-y p12-y)))   ; do of R7RS
                    ((> p12-x 3.0) (+ p12-x p12-y)))
                6.0)
  (check-equal? (receive (p12-x p12-y) (values 1.5 2.0) (+ p12-x p12-y)) 3.5)  ; receive of SRFI 8
  (check-equal? (let ((p12-x 1.5) (p12-y 2.5)) (+ p12-x p12-y)) 4.0)           ; let , was (fx+ p12-x p12-y)
  (check-equal? (let* ((p12-x 1.5) (p12-y p12-x)) (+ p12-x p12-y)) 3.0)         ; let*
  (check-equal? (let loop ((p12-x 0.5) (i 0))                                  ; named let
                  (if (= i 2) p12-x (loop (+ p12-x p12-x) (+ i 1))))
                2.0)
  (check-equal? (letrec ((p12-x 1.5)) (+ p12-x p12-x)) 3.0)                     ; letrec
  (check-equal? (let-values (((p12-x p12-y) (values 1.5 2.5))) (+ p12-x p12-y)) 4.0) ; let-values
  (define (p12-d1) (declare p12-x) {p12-x := (p12-half 3)} (+ p12-x p12-x))    ; a declaration
  (check-equal? (p12-d1) 3.0)
  (define (p12-d2) (define p12-x (p12-half 3)) (+ p12-x p12-x))               ; a definition of unknown type
  (check-equal? (p12-d2) 3.0)
  (define (p12-vsum p12-v) {p12-v[0] + p12-v[1]})                             ; the elements of a vector parameter
  (check-equal? (p12-vsum (vector 1.5 2.5)) 4.0)
  (check-true (ineq-9 1.5 2.5 3.5 3.5))                                       ; section 12 : was (fx< x y)
  (check-true (ineq-1 0.5 1 1 5 3 0 #t))                                      ; section 12 : was (fx+ x 2)

  ;; still optimized : a type written in the body of the procedure
  (define (p12-f1 p12-x) ANNOTATION-ON (type flonum? p12-x) (* p12-x 2.0))    ; fl* , annotated whatever the state of the module
  (check-equal? (p12-f1 1.5) 3.0)
  (check-exn exn:fail:contract? (lambda () (p12-f1 2)))                       ; fl* refuses an exact integer

  ;; ================================================================
  ;; 19. The procedures and macros of the program are known by the parser :
  ;;     defined in the file, typed with a type of procedure, overloaded.
  ;;     An expression is postfix when its last element is a procedure and
  ;;     its first element is surely a data : a number, a string, a
  ;;     character, a boolean, a quoted data, a variable typed with a type
  ;;     which is not a procedure. The clauses of cond, the parameters of
  ;;     lambda, the variables of do are not parsed as postfix
  ;; ================================================================

  (define (p19-apply-op op a) (op a))
  (define (p19-list . args) args)
  (define (p19-call a f) (f a))
  (type procedure? vector-map)                                    ; vector-map is not in racket/base
  (type flonum? p19-z p19-w)
  {p19-z := 1.0}
  {p19-w := 2.0}
  {p19-v := (vector 1 2 3)}
  {p19-n := 5}

  ;; between { } : converted at the reading, the definition or the type must be before
  {p19-e1 := (p19-apply-op - 1)}                                  ; was (- p19-apply-op 1)
  (check-equal? p19-e1 -1)
  {p19-e2 := (vector-map - p19-v)}                                ; was (- vector-map p19-v)
  (check-equal? p19-e2 #(-1 -2 -3))
  {p19-e3 := (1 2 p19-list)}                                      ; postfix
  (check-equal? p19-e3 '(1 2))
  {p19-e4 := (p19-call 3 !)}                                      ; a call of p19-call , was (! p19-call 3)
  (check-equal? p19-e4 6)
  {p19-e5 := (3 p19-n +)}                                         ; postfix : 3 is a data
  (check-equal? p19-e5 8)
  {p19-e6 := (p19-z 3 +)}                                         ; postfix : p19-z is typed flonum? , was a call of p19-z
  (check-equal? p19-e6 4.0)
  {p19-e7 := (p19-z p19-w max)}
  (check-equal? p19-e7 2.0)
  {p19-e8 := ("a" "b" string-append)}
  (check-equal? p19-e8 "ab")
  {p19-e9 := (p19-z sqrt)}                                        ; postfix with 2 terms : p19-z is typed flonum?
  (check-equal? p19-e9 1.0)
  {p19-e10 := (#t not)}                                           ; postfix : a boolean is a data
  (check-equal? p19-e10 #f)
  {p19-e11 := (cond ((pair? p19-v) car) (#t cdr))}                ; (#t cdr) is a clause of cond , not a postfix
  (check-equal? p19-e11 cdr)
  {p19-e12 := (cond ((pair? 1) car) (else !))}                    ; (else !) is a clause of cond , was (! else)
  (check-equal? p19-e12 !)
  {p19-f := (lambda (p19-z sqrt) p19-z)}                          ; the parameters of lambda are not parsed , not (sqrt p19-z)
  (check-equal? (p19-f 5 6) 5)
  {p19-e13 := (do ((p19-w sqrt) (p19-i 0 (p19-i + 1)))             ; the variables of do are not parsed , init , step , test are
                  ((p19-i = 2) (p19-w 16)))}
  (check-equal? p19-e13 4)

  ;; ================================================================
  ;; 20. Quoted lists. CARE-OF-QUOTES-ON (the default) : the quotes are
  ;;     respected , a quoted list is data , in a quoted { } only the top
  ;;     level is infix. CARE-OF-QUOTES-OFF : a region for the symbolic
  ;;     computation , the quoted and quasiquoted lists are infix at all
  ;;     levels ; the parts ,... and ,@... are code. A pragma acts until
  ;;     the other pragma or the end of the list where it is written
  ;; ================================================================

  {p20-a := 7}
  (check-equal? '(x + y * z) (list 'x '+ 'y '* 'z))                       ; data
  (check-equal? '{x + y * z} (list '+ 'x (list '* 'y 'z)))                 ; a quoted { } is infix
  (check-equal? '{(x + y) * z} (list '* (list 'x '+ 'y) 'z))               ; but its ( ) are data
  (check-equal? '{(x + y) * z + 1} (list '+ (list '* (list 'x '+ 'y) 'z) 1)) ; whatever the number of operators
  (check-equal? '{f (x + y)} (list 'f (list 'x '+ 'y)))                    ; {a b} is (a b)
  (check-equal? `(,p20-a · x ⊕ y) (list 7 '· 'x '⊕ 'y))

  CARE-OF-QUOTES-OFF
  (check-equal? '(x + y * z) (list '+ 'x (list '* 'y 'z)))
  (check-equal? '{(x + y) * z} (list '* (list '+ 'x 'y) 'z))
  (check-equal? '{(x + y) * z + 1} (list '+ (list '* (list '+ 'x 'y) 'z) 1))
  (check-equal? '{f (x + y)} (list 'f (list '+ 'x 'y)))
  (check-equal? `(,p20-a · x ⊕ y) (list '⊕ (list '· 7 'x) 'y))              ; ,p20-a is code
  (check-equal? `(,@(list 1 2) + 1) (list '+ 1 2 1))
  (check-equal? '(Bₖ · Bₖ₋₁ ⊕ Cₖ · (Bₖ ⊕ Bₖ₋₁))
                (list '⊕ (list '· 'Bₖ 'Bₖ₋₁) (list '· 'Cₖ (list '⊕ 'Bₖ 'Bₖ₋₁))))
  (check-equal? '(a b c) (list 'a 'b 'c))                                  ; a list of data in prefix stays as it is
  (define (p20-f) CARE-OF-QUOTES-ON '(u + v))                              ; the region ON ends with the body
  (check-equal? (p20-f) (list 'u '+ 'v))
  (check-equal? '(u + v) (list '+ 'u 'v))                                  ; still in the region OFF
  CARE-OF-QUOTES-ON
  (check-equal? '(x + y) (list 'x '+ 'y))

  ;; ================================================================
  ;; 21. Superscripts (exponents) : the examples of the comments of
  ;;     superscript-parser.rkt , the cases of the simple infix shortcut
  ;;     of the readers ({2 ⁴ ²} , {- 2 ³}) and of the parenthesis
  ;;     (⁽¹⁺¹⁾·⁽¹⁺²⁾) , and other cases
  ;; ================================================================

  (require Scheme+/superscript-parser)
  (define (p21-d x) (syntax->datum (datum->syntax #f x))) ; the results contain syntax objects

  ;; the expressions of the comments of superscript-parser.rkt
  (check-equal? {3 · 5 + 2} 17)
  (check-equal? {3 · 5 + 2 ³} 23)
  (check-equal? {3 ² + 2 · 3 · 5 + 5 ²} 64)
  (check-equal? {3 ⁻²} 1/9)                    ; was "-: identifier's binding is ambiguous" in a module (the #'- of superscript-parser.rkt)
  (check-equal? {2 ¹⁰} 1024)
  (check-equal? {2 ⁴} 16)
  (check-equal? {2 ⁴ ²} 65536)                 ; 2^(4^2) , was read (⁴ 2 ²) by the simple infix shortcut
  (check-equal? {- 2 ³} -8)                    ; -(2^3) , was read (2 - ³) by SRFI-105
  (check-equal? {3 * - 2 ⁴ ² + 1} -196607)
  (define (p21-nn) (define nn 3) {3 ²·⁽ⁿⁿ⁻⁴⁾})
  (check-equal? (p21-nn) 1/9)
  (define (p21-foo) (define n 3) {3 ⁻²·⁽ⁿ⁻⁴⁾}) ; the procedure foo of the comments
  (check-equal? (p21-foo) 9)

  ;; the calls of the comments of superscript-parser.rkt
  ;; note : the multiplication in a superscript is · (U+00B7 , middle dot) of full-superscript-string ,
  ;; not ⋅ (U+22C5 , dot operator) which is not a superscript character
  (check-equal? (p21-d (superscript-operator-loop (list 3 '²))) '(3 ** 2))
  (check-equal? (p21-d (superscript-operator-loop (list 3 '²·³⁻⁴))) '(3 ** (2 · 3 - 4)))
  (check-equal? (p21-d (superscript-operator-loop (list 3 '²·⁽³⁻⁴⁾))) '(3 ** (2 · (3 - 4))))
  (check-equal? (p21-d (superscript-operator-loop (list 3 'ⁿ⁻⁴))) '(3 ** (n - 4)))
  (check-equal? (p21-d (superscript-operator-loop (list 3 '²·⁽ⁿ⁻⁴⁾))) '(3 ** (2 · (n - 4))))
  (check-equal? (p21-d (parse-superscript-token "17")) 17)
  (check-equal? (p21-d (parse-superscript-token "¹⁷")) 17)
  (check-equal? (p21-d (parse-superscript-token "3+4")) '(3 + 4))
  (check-equal? (p21-d (parse-superscript-token "3+4-5")) '(3 + 4 - 5))
  (check-equal? (p21-d (parse-superscript-token "3*(4-5)")) '(3 * (4 - 5)))
  (check-equal? (p21-d (parse-superscript-token "3+2i")) '(3 + 0+2i))
  (check-equal? (p21-d (parse-superscript-token "3-2i")) '(3 - 0+2i))
  (check-equal? (p21-d (parse-superscript-token "3*(4-5)-2")) '(3 * (4 - 5) - 2))
  (check-equal? (p21-d (parse-superscript-token "-5")) '(- 5))       ; the sign stays an operator
  (check-equal? (p21-d (parse-superscript-token "alpha2")) 'alpha2)
  (check-equal? (p21-d (parse-superscript-token "alpha+4")) '(alpha + 4))
  (check-equal? (p21-d (parse-superscript-token "²·³⁻⁴")) '(2 · 3 - 4))
  (check-equal? (p21-d (parse-superscript-token "3*(4-5)-2+3i")) '(3 * (4 - 5) - 2 + 0+3i))
  (check-equal? (p21-d (parse-superscript-token "3*(4-(5-2))-2+3i")) '(3 * (4 - (5 - 2)) - 2 + 0+3i))
  (check-equal? (p21-d (parse-superscript-token "+-3*(4-(5-2))-2+3i")) '(+ - 3 * (4 - (5 - 2)) - 2 + 0+3i))
  (check-equal? (p21-d (parse-superscript-token "+-3*(4-(5-2))/-2")) '(+ - 3 * (4 - (5 - 2)) / - 2))

  ;; the cases of the simple infix shortcut of the readers : 3 elements with a superscript
  (define (p21-x-x x) {x ² ²})
  (check-equal? (p21-x-x 2) 16)                ; 2^(2^2) , was read (² x ²)
  (define (p21-minus-x x) {- x ²})
  (check-equal? (p21-minus-x 3) -9)            ; -(3^2) , was read (x - ²) by SRFI-105
  (check-equal? {+ 2 ³} 8)                     ; was read (2 + ³) by SRFI-105

  ;; * and · are both superscript and normal characters : alone they are operators , not exponents
  ;; (superscript-only? and not superscript? in the simple infix shortcut of the readers)
  {p21-op := *}                                ; gave "Error parsing +- : ... '(*)"
  (check-equal? (p21-op 2 3) 6)
  {p21-op· := ·}                               ; gave "Error parsing +- : ... '(·)"
  (check-equal? (p21-op· 2 3) 6)

  ;; several groups between parenthesis in an exponent : the matching closing parenthesis
  (check-equal? (p21-d (parse-superscript-token "⁽¹⁺¹⁾·⁽¹⁺²⁾")) '((1 + 1) · (1 + 2)))
  (check-equal? {2 ⁽¹⁺¹⁾·⁽¹⁺²⁾} 64)
  (check-equal? {2 ⁽¹⁺¹⁾} 4)
  (check-equal? {2 ⁽¹⁺¹⁾·³} 64)

  ;; other cases
  (define (p21-x2+1 x) {x ² + 1})
  (check-equal? (p21-x2+1 2) 5)
  (check-equal? {1 + 2 ³} 9)
  (check-equal? {(- 2) ³} -8)
  (check-equal? {- (2 ³)} -8)
  (check-equal? {2 ¹⸍²} (expt 2 1/2))          ; ⸍ is the division : square root of 2
  (check-equal? {3 ⁻²⁺³ᶦ} (expt 3 -2+3i))      ; complex exponent
  (define (p21-x-n+1 x) (define n 3) {x ⁿ⁺¹})
  (check-equal? (p21-x-n+1 2) 16)
  (define x² 7)                                ; without space , x² is a name
  (check-equal? {x² + 1} 8)
  (check-equal? (p21-d (parse-superscript-token "²⸱⁵")) 2.5)  ; ⸱ is the decimal separator
  (check-equal? (p21-d (parse-superscript-token "²ʲ")) 0+2i)  ; j , the electrical convention
  (check-equal? (p21-d (parse-superscript-token "ⁿ⸍²")) '(n / 2))
  (check-equal? (p21-d (parse-superscript-token "⁻²·⁽ⁿ⁻⁴⁾")) '(- 2 · (n - 4)))

  ;; ================================================================
  ;; 22. Chains of exponentiation : right associative , 2^(3^2) , as in
  ;;     mathematics. expt has only 2 arguments : a chain of expt is
  ;;     nested , {2 expt 3 expt 2} is (expt 2 (expt 3 2)) and no more
  ;;     (expt 2 3 2) (arity error) ; ** is nested the same way
  ;; ================================================================

  ;; expt in a chain (n-ary written in infix)
  (check-equal? {2 expt 3 expt 2} 512)
  (check-equal? {2 expt 3 expt 2 expt 1} 512)
  (check-equal? {2 expt 1 expt 3} 2)
  (check-equal? {3 expt 2 expt 2} 81)
  (check-equal? {(2 expt 3) expt 2} 64)
  (check-equal? {2 expt (3 expt 2)} 512)
  (check-equal? {2 expt 3 expt 2 + 1} 513)
  (check-equal? {1 + 2 expt 3 expt 2} 513)
  (check-equal? {2 · 2 expt 3 expt 2} 1024)
  (check-equal? {2 expt 3 ** 2} 512)           ; the two exponentiation operators mixed
  (check-equal? {2 ** 3 expt 2} 512)
  (check-equal? {2.0 expt 3 expt 2} 512.0)
  (define (p22-tower x) {x expt x expt x})
  (check-equal? (p22-tower 2) 16)

  ;; ** in a chain
  (check-equal? {2 ** 3 ** 2} 512)
  (check-equal? {(2 ** 3) ** 2} 64)
  (check-equal? {2 ** (3 ** 2)} 512)
  (check-equal? {2 ** 3 ** 2 + 1} 513)

  ;; operators that are variables or procedures : the precedence is decided at run time
  ;; (the tests planned for the Pratt parser)
  (define p22-mult *)
  (define p22-add +)
  (define (p22-mult-proc) *)
  (define (p22-cinque) 5)
  (check-equal? {3 p22-mult 5 p22-add 2} 17)
  (check-equal? {3 (p22-mult-proc) 5 p22-add 2} 17)
  (check-equal? {3 (p22-mult-proc) (p22-cinque) p22-add 2} 17)

  ;; ================================================================
  ;; 23. A flonum to an integer power : {x ²} is read (** x 2) with an
  ;;     exact exponent ; with (type flonum? x) its type is flonum? and
  ;;     the operation around it is specialized :
  ;;     {1.0 - x ²} --> (fl- 1.0 (orig-** x 2))
  ;;     (in typed code the exponent 0 is written 0.0 : (expt 2.0 0) is
  ;;     the exact 1)
  ;; ================================================================
  (define (p23-d x) (type flonum? x) {1.0 - x ²})
  (check-equal? (p23-d 2.0) -3.0)
  (define (p23-cube x) (type flonum? x) {1.0 + x ³})
  (check-equal? (p23-cube 2.0) 9.0)
  (define (p23-e x) (type flonum? x) {1.0 + (expt x 2)})
  (check-equal? (p23-e 3.0) 10.0)
  (define (p23-star x) (type flonum? x) {1.0 + x ** 2})
  (check-equal? (p23-star 3.0) 10.0)

  ;; ================================================================
  ;; 24. The types char? and void? : a literal character #\a is a
  ;;     char? , an element {s[i]} of a string s a char? , a slice
  ;;     {s[i : j]} a string? , (void) a void? ; the overloads are
  ;;     then resolved at read time :
  ;;     (p24-show c) --> (p24-char c) , (p24-show-void u) --> (p24-void u)
  ;; ================================================================
  (define (p24-char c) (char->integer c))
  (define (p24-string s) (string-length s))
  (overload-procedure p24-show p24-char (char?))
  (overload-procedure p24-show p24-string (string?))
  (define (p24-void u) 'void)
  (overload-procedure p24-show-void p24-void (void?))

  (define (p24-literal)
    {c := #\a}
    (p24-show c))                                ; (p24-char c)
  (check-equal? (p24-literal) 97)

  (define (p24-element s)
    (type string? s)
    {e := s[0]}
    (p24-show e))                                ; (p24-char e)
  (check-equal? (p24-element "hello") 104)

  (define (p24-slice s)
    (type string? s)
    {t := s[1 : 3]}
    (list t (p24-show t)))                       ; (p24-string t)
  (check-equal? (p24-slice "hello") '("el" 2))

  (define (p24-unit)
    {u := (void)}
    (p24-show-void u))                           ; (p24-void u)
  (check-equal? (p24-unit) 'void)

  ;; ================================================================
  ;; 25. not as a prefix unary operator in infix , with the precedence
  ;;     of Python : below the comparisons and the arithmetic , above
  ;;     and / or. A not in the position of an operand (at the
  ;;     beginning or after an operator) takes the terms until the
  ;;     next and , or , assignment , definition , if , else :
  ;;     {a and not b} --> (and a (not b)) , {not x < y} --> (not (< x y))
  ;;     A not in the position of a value is unchanged : {f := not}
  ;; ================================================================
  (define (p25-and-not a b) {a and not b})          ; (and a (not b))
  (check-equal? (p25-and-not #t #f) #t)
  (check-equal? (p25-and-not #t #t) #f)
  (define (p25-not-and a b) {not a and b})          ; (and (not a) b)
  (check-equal? (p25-not-and #f #t) #t)
  (check-equal? (p25-not-and #t #t) #f)
  (define (p25-not-less x y) {not x < y})           ; (not (< x y))
  (check-equal? (p25-not-less 1 2) #f)
  (check-equal? (p25-not-less 3 2) #t)
  (define (p25-or-not a b c) {a or not b and c})    ; (or a (and (not b) c))
  (check-equal? (p25-or-not #f #f #t) #t)
  (check-equal? (p25-or-not #f #t #t) #f)
  (define (p25-not-not a) {not not a})              ; (not (not a))
  (check-equal? (p25-not-not 5) #t)
  (define (p25-not a) {not a})                      ; (not a)
  (check-equal? (p25-not #f) #t)
  (define (p25-sum a b c) {not a + b < c})          ; (not (< (+ a b) c))
  (check-equal? (p25-sum 1 2 4) #f)
  (define (p25-assign a b)
    {x := not a and b}                              ; (:= x (and (not a) b))
    {y := (not a or b)}                             ; in a sub-expression : (:= y (or (not a) b))
    (list x y))
  (check-equal? (p25-assign #f #t) '(#t #t))
  (check-equal? (p25-assign #t #f) '(#f #f))
  (define (p25-if a b c) {a if not b else c})       ; (if (not b) a c)
  (check-equal? (p25-if 1 #f 2) 1)
  (check-equal? (p25-if 1 #t 2) 2)
  (define (p25-typed x y)
    (type flonum? x y)
    {not x < y})                                    ; (not (fl< x y))
  (check-equal? (p25-typed 1.0 2.0) #f)
  ;; not as a value : unchanged
  {p25-f := not}
  (check-equal? (p25-f #f) #t)
  (check-equal? {(map not '(#t #f))} '(#f #t))
  (check-equal? {(not #f) and #t} #t)
  ;; in a strict SRFI 105 region , {a and not b} is ($nfx$ a and not b) , parsed by the macro $nfx$
  BEGIN-STRICT-SRFI-105-REGION
  (define (p25-strict a b x y) (list {a and not b} {not x < y}))
  END-STRICT-SRFI-105-REGION
  (check-equal? (p25-strict #t #f 3 2) '(#t #t))

  ;; ================================================================
  ;; 26. ~ (bitwise not) as a prefix unary operator in infix , with the
  ;;     precedence of Python , as the unary minus : above * + & and the
  ;;     comparisons , below the exponentiation (~ takes the following
  ;;     term and its chain of exponentiation).
  ;;     {a & ~ b} --> (& a (~ b)) , {~ a ** 2} --> (~ (** a 2))
  ;;     A ~ in the position of a value is unchanged : {f := ~}
  ;; ================================================================
  (check-equal? (~ 5) -6)
  (define (p26-not a) {~ a})                        ; (~ a)
  (check-equal? (p26-not 5) -6)
  (define (p26-and-not a b) {a & ~ b})              ; (& a (~ b))
  (check-equal? (p26-and-not 7 2) 5)
  (define (p26-not-and a b) {~ a & b})              ; (& (~ a) b)
  (check-equal? (p26-not-and 2 7) 5)
  (define (p26-plus a) {~ a + 1})                   ; (+ (~ a) 1) : - a
  (check-equal? (p26-plus 5) -5)
  (define (p26-times a) {2 * ~ a})                  ; (* 2 (~ a))
  (check-equal? (p26-times 5) -12)
  (define (p26-not-not a) {~ ~ a})                  ; (~ (~ a))
  (check-equal? (p26-not-not 5) 5)
  (define (p26-power a) {~ a ** 2})                 ; (~ (** a 2))
  (check-equal? (p26-power 3) -10)
  (define (p26-superscript a) {~ a ²})              ; (~ (** a 2))
  (check-equal? (p26-superscript 3) -10)
  (define (p26-exponent a) {2 ** ~ a})              ; (** 2 (~ a))
  (check-equal? (p26-exponent -3) 4)
  (define (p26-chain a) {~ a ** 2 ** 3})            ; (~ (** a (** 2 3)))
  (check-equal? (p26-chain 2) -257)
  (define (p26-minus a) {- ~ a})                    ; (- (~ a))
  (check-equal? (p26-minus 5) 6)
  (define (p26-with-not a) {not ~ a = 0})           ; (not (= (~ a) 0))
  (check-equal? (p26-with-not -1) #f)
  (define (p26-assign a)
    {x := ~ a}                                      ; (:= x (~ a))
    x)
  (check-equal? (p26-assign 0) -1)
  ;; ~ as a value : unchanged
  {p26-f := ~}
  (check-equal? (p26-f 0) -1)
  (check-equal? {(map ~ '(0 1))} '(-1 -2))
  BEGIN-STRICT-SRFI-105-REGION
  (define (p26-strict a b) {a & ~ b})
  END-STRICT-SRFI-105-REGION
  (check-equal? (p26-strict 7 2) 5)

  ;; ================================================================
  ;; 27. The two precedence engines : the Pratt parser (by default) and
  ;;     the original engine of Scheme+ (use-pratt-parser? set to #f)
  ;;     must give the same result. The reader uses only the default
  ;;     engine : here the parser is called directly with each engine on
  ;;     the same terms (quoted : data , not converted by the reader).
  ;; ================================================================
  (require (only-in Scheme+/infix-with-precedence-to-prefix
                    !*prec-generic-infix-parser-prepare-runtime
                    use-pratt-parser?))

  (define (p27-parse terms pratt?)
    (parameterize ((use-pratt-parser? pratt?))
      (!*prec-generic-infix-parser-prepare-runtime terms (lambda (op a b) (list op a b)))))

  (define (p27-check terms expected) ; the same expected result with the two engines
    (check-equal? (p27-parse terms #t) expected)
    (check-equal? (p27-parse terms #f) expected))

  ;; arithmetic , comparisons , logic , assignment
  (p27-check '(3 * 4 + 2) '(+ (* 3 4) 2))
  (p27-check '(a < b <= c) '(and (< a b) (<= b c)))
  (p27-check '(a and b or c) '(or (and a b) c))
  (p27-check '(a and b and c or d) '(or (and a b c) d)) ; the chains of and / or are n-ary
  (p27-check '(a or b or c and d) '(or a b (and c d)))
  (p27-check '(x := y + 1) '(:= x (+ y 1)))
  ;; exponentiation , signs , superscripts
  (p27-check '(2 expt 3 expt 2) '(expt 2 (expt 3 2)))
  (p27-check '(- 2 ** 4) '(- (** 2 4)))
  (p27-check '(3 * - 2 ** 4 + 1) '(+ (* 3 (- (** 2 4))) 1))
  (p27-check '(a * - b) '(* a (- b)))
  (p27-check '(x ²·³⁻⁴) '(** x (- (· 2 3) 4)))
  ;; not (section 25)
  (p27-check '(a and not b) '(and a (not b)))
  (p27-check '(not x < y) '(not (< x y)))
  (p27-check '(a or not b and c) '(or a (and (not b) c)))
  (p27-check '(not a and not b and not c or c and not d) '(or (and (not a) (not b) (not c)) (and c (not d))))
  (p27-check '(not not a) '(not (not a)))
  (p27-check '(x := (not a and b)) '(:= x (and (not a) b)))
  (p27-check '(a if not b else c) '(if (not b) a c))
  ;; ~ (section 26)
  (p27-check '(a & ~ b) '(& a (~ b)))
  (p27-check '(~ a + 1) '(+ (~ a) 1))
  (p27-check '(~ a ** 2 ** 3) '(~ (** a (** 2 3))))
  (p27-check '(2 ** ~ a ** 3) '(** 2 (~ (** a 3))))
  (p27-check '(~ a ²) '(~ (** a 2)))
  (p27-check '(not ~ a = 0) '(not (= (~ a) 0)))

  ;; ================================================================
  ;; 28. The numeric procedures typed by the type of their arguments :
  ;;     with (type flonum? x) , (sin x) is (flsin x) of type flonum? ,
  ;;     so {(sin x) * 2.0} is (fl* (flsin x) 2.0) ; with fixnums
  ;;     (quotient k 2) is (fxquotient k 2) ... Without such an entry
  ;;     the call stays generic : (sin 0) is the exact 0 , (sqrt x) of a
  ;;     negative float is a complex number , (exp 0) is the exact 1.
  ;; ================================================================
  (define (p28-float x)
    (type flonum? x)
    (list {(sin x) * 2.0} {(cos x) + (tan x)} {(atan x) - 1.0} {(exp x) * x}
          {(abs x) + 1.0} {(min x 1.0) + (max x 1.0)}))
  (check-equal? (p28-float 0.5) (list (* (sin 0.5) 2.0) (+ (cos 0.5) (tan 0.5)) (- (atan 0.5) 1.0) (* (exp 0.5) 0.5)
                                      1.5 1.5))
  (check-equal? (p28-float -2.0) (list (* (sin -2.0) 2.0) (+ (cos -2.0) (tan -2.0)) (- (atan -2.0) 1.0) (* (exp -2.0) -2.0)
                                       3.0 -1.0))
  (define (p28-fixnum k n)
    (type fixnum? k n)
    (list {(quotient k 2) + 1} {(remainder k 2) * 3} {(modulo k 3) + 1} {(abs k) + (max k n) + (min k n)}
          {(exact->inexact k) * 2.0} {(exact->inexact n) / 2.0})) ; not inexact (R7RS) : unbound in Racket without annotation
  (check-equal? (p28-fixnum 7 -3) '(4 3 2 11 14.0 -1.5))
  (check-equal? (p28-fixnum -7 2) '(-2 -3 3 2 -14.0 1.0))
  (define (p28-generic x n)
    (type flonum? x)
    (type fixnum? n)
    (list (sin 0) (sqrt x) {(exp n) + 1})) ; no entry : the calls stay generic
  (check-equal? (p28-generic -4.0 0) (list 0 (sqrt -4.0) 2)) ; (sqrt -4.0) is a complex number , (exp 0) the exact 1
  ;; the lengths and the codes of the characters are fixnums (scheme-types.rkt) : the operations around them are fx
  (define (p28-lengths l s v)
    (type list? l)
    (type string? s)
    (type vector? v)
    (list {(length l) * 2} {(string-length s) + 1} {(vector-length v) - 1} {(char->integer (string-ref s 0)) - 65}))
  (check-equal? (p28-lengths '(1 2 3) "ABC" #(1 2)) '(6 4 1 0))
  ;; rounding and square : of a float a float (flfloor ...) , of a fixnum a fixnum
  (define (p28-rounding x k)
    (type flonum? x)
    (type fixnum? k)
    (list {(floor x) + 1.0} {(ceiling x) * 2.0} {(round x) - 0.5} {(truncate x) / 2.0}
          {(floor k) + 1} {(round k) * 2} {(sqr x) + 1.0} {(sqr k) + 1}))
  (check-equal? (p28-rounding -2.5 7) '(-2.0 -4.0 -2.5 -1.0 8 14 7.25 50)) ; (round -2.5) is -2.0 : to even
  (check-equal? (p28-rounding 2.5 -3) '(3.0 6.0 1.5 1.0 -2 -6 7.25 10))

  ;; ================================================================
  ;; 29. not in the position of an operator : the negation of a
  ;;     comparison , as not in of Python , for any procedure or
  ;;     operator : {5 not member l} --> (not (member 5 l)) ; the
  ;;     operands go until and , or , an assignment ...
  ;; ================================================================
  (define p29-l '(1 2 3))
  (check-equal? {5 member p29-l} #f)                      ; (member 5 p29-l)
  (check-equal? {5 not member p29-l} #t)                  ; (not (member 5 p29-l))
  (check-equal? {2 not member p29-l} #f)
  (define (p29-and x) {x not member p29-l and x > 0})     ; (and (not (member x p29-l)) (> x 0))
  (check-equal? (p29-and 5) #t)
  (check-equal? (p29-and 2) #f)
  (define (p29-sum x) {x + 1 not member p29-l})           ; (not (member (+ x 1) p29-l))
  (check-equal? (p29-sum 4) #t)
  (check-equal? (p29-sum 1) #f)
  (define (p29-assign x)
    {y := x not member p29-l}                             ; (:= y (not (member x p29-l)))
    y)
  (check-equal? (p29-assign 7) #t)
  (check-equal? {3 not < 2} #t)                           ; (not (< 3 2))
  (define (in x l) (and (member x l) #t))                 ; an in of Python , defined by the user
  (check-equal? {5 not in p29-l} #t)                      ; (not (in 5 p29-l))
  ;; unchanged : a not that is a value , a call with not as argument
  {p29-f := not}
  (check-equal? (p29-f #f) #t)
  (check-equal? {(list 1 not 2 3)} (list 1 not 2 3))
  ;; the two precedence engines
  (p27-check '(5 not member l and x) '(and (not (member 5 l)) x))
  (p27-check '(x + 1 not member l) '(not (member (+ x 1) l)))
  (p27-check '(a and b not member l or c) '(or (and a (not (member b l))) c))

  ;; ================================================================
  ;; 30. The walrus operator :=> of Python (:= in Python) : assigns
  ;;     and returns the value , {y := {x :=> 5} + 1} ; the variable
  ;;     must exist before (a new name is refused at expansion time).
  ;; ================================================================
  (declare p30-x p30-y p30-n p30-a p30-b)
  {p30-y := {p30-x :=> 5} + 1}
  (check-equal? (list p30-x p30-y) '(5 6))
  (define (p30-length l)
    (if {(p30-n :=> (length l)) > 2} p30-n 0))
  (check-equal? (p30-length '(1 2 3 4)) 4)
  (check-equal? (p30-length '(1)) 0)
  (check-equal? {p30-a :=> p30-b :=> 3} 3)                 ; chained : (:=> p30-a p30-b 3)
  (check-equal? (list p30-a p30-b) '(3 3))
  (define p30-v (vector 0 0 0))
  (define p30-count 0)
  (define (p30-next) {p30-count := p30-count + 1} p30-count)
  (check-equal? {p30-v[(p30-next)] :=> 7} 7)              ; the index is computed once
  (check-equal? (list p30-v p30-count) '(#(0 7 0) 1))
  (define (p30-local x)
    {x :=> x * 2})                                          ; a parameter exists
  (check-equal? (p30-local 4) 8)
  ;; a new name is refused : it would be defined in the let of :=> and lost
  (check-exn exn:fail:syntax?
             (lambda () (convert-compile-time-error (let () (:=> p30-new 1) p30-new))))
  ;; annotated : (:=> x expr) has the type of expr
  (define (p30-float a)
    ANNOTATION-ON
    (type flonum? a)
    (declare x)
    {y := {x :=> a * 2.0} * 3.0}                            ; (:= y (fl* (:=> x (fl* a 2.0)) 3.0))
    (list x y))
  (check-equal? (p30-float 1.5) '(3.0 9.0))
  (define (p30-vector v)
    ANNOTATION-ON
    (type vector? v)
    (declare w)
    (eq? {w :=> v} v))                                       ; the value is returned , not a copy
  (check-true (p30-vector (vector 1 2)))
  ;; the two precedence engines
  (p27-check '(y := (x :=> 5) + 1) '(:= y (+ (:=> x 5) 1)))
  (p27-check '(n :=> a + b * c) '(:=> n (+ a (* b c))))
  (p27-check '(a :=> not b and c) '(:=> a (and (not b) c)))

  ;; If rackunit printed no FAILURE / ERROR block above, every check passed.
  (displayln "test-curly-infix+ : done")

  ) ; end module
