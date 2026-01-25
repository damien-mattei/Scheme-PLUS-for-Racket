;; This file is part of Scheme+

;; Copyright 2025 Damien MATTEI

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



(module operators racket/base


  (provide operator? ; for symbols
	   operator-syntax?
	   operator-symbol-or-syntax?
	   not+-?
	   NO-OP?
	   arithmetic-operator?
	   in/equalities-operator?

	   operator
	   arg1
	   arg2
	   arg
	   args

	   function-without-parameters?
	   unary-operation?
	   binary-operation?
	   

	   EQUAL-op?
	   NOT-EQUAL-op?
	   LESS-op?
	   GREATER-op?
	   LESS-OR-EQUAL-op?
	   GREATER-OR-EQUAL-op?
	   IN-EQUALITY-op?

	   ;; logic operators
	   AND-op?
	   OR-op?
	   NOT-op?
	   LOGIC-N-ARY-op?
	   
	   XOR-op? ; Warning: Bitwise operator
	   
	   
	   ADD-op?
	   MINUS-op?
	   SIGN-op?
	   IMPLIC-op?
	   EQUIV-op?
	   DEFINE-op?
	   ;;SINGLE-VARIABLE-ASSIGNMENT-op?
	   ASSIGNMENT-op?
	   FLOW-op?
	   COMPOSITION-op?
	   EXPONENTIATION-op?
	   MULTIPLY-op?
	   
	   is-associative-operator?
	   

	   ;; expression tests
	   isADD?	   
	   isMULTIPLY?
	   isOR?
	   isAND?
	   isOR-AND?
	   isNOT?
	   isIMPLIC?
	   isEQUIV?
	   isXOR?
	   isDEFINE?
	   isASSIGNMENT?
	   isFLOW?
	   isCOMPOSITION?
	   isEXPONENTIATION?
	   isASSOCIATIVE?

	   ;; group test
	   exponentiation-operators-group?
	   strict-precedence-over-minus?
	   precedence-rank
	   operator-precedence>?
	   operator-precedence=?

   )

  
  
  (require (only-in srfi/1 first member)
	   Scheme+/syntax
	   Scheme+/operators-list
	   Scheme+/multiply
	   Scheme+/not-equal
	   Scheme+/little-greater-or-equal)

  
(define rest cdr)




;; operators predicates
(define (operator? x)
  (member x operators-lst))

;; syntax version
;; works with any (procedure,symbol,syntax)

;; (operator-syntax? +)
;; #t

;; (operator-syntax? '+)
;; #t


;; (operator-syntax? #'+)
;; #t

(define (operator-syntax? x)
  (member-generic x operators-lst-syntax))

(define (operator-symbol-or-syntax? x)
  (or (operator? x)
      (operator-syntax? x)))

(define (arithmetic-operator? x)
  (member-generic x arithmetic-operator-lst-syntax))




;; (in/equalities-operator? <)
;; #t

;; (in/equalities-operator? '<=)
;; #t

;; (in/equalities-operator? #'<=)
;; #t

;; (in/equalities-operator? #'+)
;; #f
(define (in/equalities-operator? x)
  (member-generic x in/equalities-operator-syntax))


(define (not+-? x)
  (not (SIGN-op? x)))


(define (NO-OP? elem)
  (not (operator-symbol-or-syntax? elem)))





;; return the operator of an operation
(define (operator expr)
  (car expr))


;; return the first argument of a binary operation
(define (arg1 expr)
  ;;(display "arg1") (newline)
  (first (rest expr)))

;; return the second argument of a binary operation
(define (arg2 expr)
  ;;(display "arg2") (newline)
  (first (rest (rest expr))))

(define (arg expr)
  (arg1 expr))

;; return the arguments of an operation
(define (args expr)
  (cdr expr))

;; function without parameters
(define (function-without-parameters? expr)
  (null? (rest expr)))

;; (unary-operation? '(not a)) -> #t
(define (unary-operation? expr)
  (null? (rest (rest expr))))

;; (binary-operation? '(and a b)) -> #t
(define (binary-operation? expr)
  ;;(null? (rest (rest (rest expr)))))
  (and (pair? expr)
       (pair? (rest expr))
       (pair? (rest (rest expr)))
       (null? (rest (rest (rest expr))))))








;; operators test

;; test if an operator is AND
(define (AND-op? oper)
  (or (datum=? oper 'and)
      (datum=? oper '·)))
  ;;(or (eqv? oper 'and) (check-syntax=? oper #'and)))
  ;;(or (eqv? oper 'and) (eqv? oper 'AND) (eqv? oper '·)))

;; test if an operator is OR
(define (OR-op? oper)
  (datum=? oper 'or))
  ;;(or (eqv? oper 'or) (check-syntax=? oper #'or)))
;;(or (eqv? oper 'or) (eqv? oper 'OR)  (eqv? oper '➕))) ;; middle dot


(define (LOGIC-N-ARY-op? oper)
  (or (AND-op? oper)
      (OR-op? oper)))

;; Warning : no difference here between 'logic' and 'bitwise' operators
(define (XOR-op? oper) ;; note: logxor in Guile, xor in Racket
  (or (datum=? oper 'logxor)
      (datum=? oper 'xor)
      (datum=? oper '^)
      (datum=? oper '⊕)))

  ;;(or (eqv? oper 'xor) (check-syntax=? oper #'xor)))
  ;;(or (eqv? oper 'xor) (eqv? oper 'XOR)  (eqv? oper '⊕))) ;; ⨁


;; test if an operator is NOT
(define (NOT-op? oper)
  (datum=? oper 'not))
  ;;(or (eqv? oper 'not) (check-syntax=? oper #'not))) ; not sure it is usefull in syntax 
  ;;(or (eqv? oper 'not) (eqv? oper 'NOT)))


(define (EQUAL-op? oper)
  (or (eqv? oper =)
      (eqv? oper equal?)
      (datum=? oper '=)
      (datum=? oper 'equal?)))

(define (NOT-EQUAL-op? oper)
  (or (eqv? oper ≠)
      (eqv? oper <>)
      (datum=? oper '≠)
      (datum=? oper '<>)))


(define (LESS-op? oper)
  (or (eqv? oper <)
      (datum=? oper '<)))

(define (GREATER-op? oper)
  (or (eqv? oper >)
      (datum=? oper '>)))

(define (LESS-OR-EQUAL-op? oper)
  (or (eqv? oper <=)
      (datum=? oper '<=)
      (eqv? oper ≤)
      (datum=? oper '≤)))

(define (GREATER-OR-EQUAL-op? oper)
  (or (eqv? oper >=)
      (datum=? oper '>=)
      (eqv? oper ≥)
      (datum=? oper '≥)))


(define (IN-EQUALITY-op? oper)
  (or (EQUAL-op? oper)
      (NOT-EQUAL-op? oper)
      (LESS-op? oper)
      (GREATER-op? oper)
      (LESS-OR-EQUAL-op? oper)
      (GREATER-OR-EQUAL-op? oper)))


(define (ADD-op? oper)
  (or (eqv? oper +)
      (datum=? oper '+)))
;;(or (eqv? oper +) (eqv? oper '+) (check-syntax=? oper #'+)))

(define (MINUS-op? oper)
  (or (eqv? oper -)
      (datum=? oper '-)))

(define (SIGN-op? oper)
  (or (ADD-op? oper)
      (MINUS-op? oper)))


(define (MULTIPLY-op? oper)
  (or (eqv? oper *)
      (eqv? oper ·)
      (datum=? oper '*)
      (datum=? oper '·)))

(define (IMPLIC-op? oper)
  (or (eqv? oper '⟹) (eqv? oper '=>)))

(define (EQUIV-op? oper)
  (or (eqv? oper '⟺) (eqv? oper '<=>)))


(define (DEFINE-op? oper)
  ;; (or (eqv? oper '<+) (eqv? oper '+>)
  ;;     (eqv? oper '←) (eqv? oper '+>)
  ;;     (eqv? oper '<-) (eqv? oper '->)
  ;;     (eqv? oper '←) (eqv? oper '+>)))

  (or (memv oper definition-operator)
      (member-generic oper definition-operator-syntax)))


  
(define (ASSIGNMENT-op? oper)
  ;;(or (eqv? oper '<-) (eqv? oper '->)))
  (or (memv oper assignment-operator)
      (member-generic oper assignment-operator-syntax)))

(define (FLOW-op? oper)
  (datum=? oper '~>))

(define (COMPOSITION-op? oper)
  (datum=? oper '∘))

;; (define (SINGLE-VARIABLE-ASSIGNMENT-op? oper)
;;   (member-generic oper single-variable-assignment-operator-syntax))


(define (EXPONENTIATION-op? oper)
  (or (memv oper exponentiation-operator)
      (member-generic oper exponentiation-operator-syntax)))

(define (exponentiation-operators-group? grp)
  (member-generic 'expt grp)) 


(define (is-associative-operator? op)
  (or (AND-op? op)
      (OR-op? op)
      (XOR-op? op)
      (EQUIV-op? op)
      (ADD-op? op)
      (MULTIPLY-op? op)
      (FLOW-op? op)
      (COMPOSITION-op? op)
      (DEFINE-op? op)
      (ASSIGNMENT-op? op)))










;; expression tests

;; test for prefix expression only

(define (isADD? expr)
  (and (pair? expr) (ADD-op? (car expr))))



  ;;(or (eqv? oper *) (eqv? oper '*) (check-syntax=? oper #'*)))

(define (isMULTIPLY? expr)
  (and (pair? expr) (MULTIPLY-op? (car expr))))

;; test if an expression is a OR
(define (isOR? expr)
  ;;(and (pair? expr) (equal? (car expr) 'or)))
  (and (pair? expr) (OR-op? (car expr))))

;; test if an expression is a AND
(define (isAND? expr)
  ;;(and (pair? expr) (equal? (car expr) 'and)))
  (and (pair? expr) (AND-op? (car expr))))


;; is expression an (OR or AND) ?
(define (isOR-AND? expr)
  (or (isOR? expr)  (isAND? expr)))

(define (isNOT? expr)
  (and (pair? expr) (NOT-op? (car expr))))

(define (isIMPLIC? expr)
  (and (pair? expr) (IMPLIC-op? (operator expr))))

(define (isEQUIV? expr)
  (and (pair? expr) (EQUIV-op? (operator expr))))

(define (isXOR? expr)
  (and (pair? expr) (XOR-op?  (operator expr))))

(define (isDEFINE? expr)
  (and (pair? expr) (DEFINE-op?  (operator expr))))

(define (isASSIGNMENT? expr)
  (and (pair? expr) (ASSIGNMENT-op?  (operator expr))))

(define (isFLOW? expr)
  (and (pair? expr) (FLOW-op?  (operator expr))))

(define (isCOMPOSITION? expr)
  (and (pair? expr) (COMPOSITION-op?  (operator expr))))

(define (isEXPONENTIATION? expr)
  (and (pair? expr) (ASSIGNMENT-op?  (operator expr))))


(define (isASSOCIATIVE? expr)
  ;;(display "isASSOCIATIVE?") (newline)
  (is-associative-operator? (first expr)))


(define (strict-precedence-over-minus? op)
  ;;(display "strict-precedence-over-minus? : op =") (display op) (newline)
  ;;(display "strict-precedence-over-minus? : strict-precedence-over-minus =") (display strict-precedence-over-minus) (newline)
  (define rv (member-generic op strict-precedence-over-minus))
  ;;(display "strict-precedence-over-minus? : rv =") (display rv) (newline)
  rv
  )


;; (precedence-rank '+)
;; 5

;; (precedence-rank #'**)
;; 1
(define (precedence-rank op)
  (define i -1)
  (for ([op-group infix-operators-lst-for-parser-syntax])
       #:final (member-generic op op-group)
       (set! i (+ i 1)) ; will be computed even when final is true
       ;;(display "i=") (display i) (newline)
       ;;(display "op-group=") (display op-group) (newline)
       )
  i)


(define (operator-precedence>? op1 op2)
  ;;(display "operator-precedence>? op1=") (display op1) (newline)
  ;;(display "operator-precedence>? op2=") (display op2) (newline)
  (> (precedence-rank op2)
     (precedence-rank op1)))


(define (operator-precedence=? op1 op2)
  (display "operator-precedence>? op1=") (display op1) (newline)
  (display "operator-precedence>? op2=") (display op2) (newline)
  (= (precedence-rank op2)
     (precedence-rank op1)))


) ; end module
