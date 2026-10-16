
;; overload

;; Damien Mattei

;; This file is part of Scheme+

;; Copyright 2021-2025 Damien MATTEI

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

;; Racket examples are in logic-syracuse code

;; Warning: overload is now a module to prevent infinite recursion in case someone overload a scheme procedure used in the implementation of any of the procedures provided by overload.scm (example: length !)

;; in a real module file ,no lang....

(module overload racket/base

  ; define-overload* crée une procédure proc qui gére le calcul qu'il soit binaire (cas particulier de n-aire) ou vraiment n-aire acdeptant plusieurs arguments de type identiques en utilisant la presence de l'operateur dans les tables binaires ou n-aires

(provide $ovrld-ht$
	 $ovrld-ht-n-ary$
	 $ovrld-ht-proc$

	 ;define-overload-procedure ; replaced by overload-procedure
	 ;define-overload-procedure-annot
	 overload-procedure ; official one : defines the overloaded procedure if needed and adds the overload
	 require-overload-procedure ; official one
	 
	 ;define-overload-existing-procedure ; replaced by overload-procedure
	 ;define-overload-existing-procedure-annot
	 ;overload-existing-procedure

         ; define-overload-operator : binary or n-ary do a single procedure and check if the operator is in binary or n-ary hashtable (this requires to have different hash table like in annot for storing overloads or to check the number of arguments , 1 for n-ary all equals or 2 for binary
	 
	 define-overload-operator
	 ;define-overload-operator-annot
	 overload-operator ; official one : defines the overloaded operator if needed and adds the overload

	 define-overload-existing-operator
	 define-overload-existing-operator-annot
	 define-overloaded-operator ; official one
	 overload-existing-operator

	 require-overload-operator ; official one

	 overload-unary-operator ; official one : an overload with exactly one argument , (- M) for a matrix
	 require-overload-unary-operator ; official one

	 define-overload-n-ary-associative-operator
	 ;define-overload-n-ary-associative-operator-annot
	 overload-n-ary-operator
	 
	 define-overload-existing-n-ary-associative-operator
	 define-overload-existing-n-ary-associative-operator-annot
	 overload-existing-n-ary-operator

	 
	 $ovrld-square-brackets-lst$
	 
	 overload-square-brackets
	 require-overload-square-brackets ; for the type annotation pass only
	 ;;find-getter-and-setter-for-overloaded-square-brackets
	 find-getter-for-overloaded-square-brackets
	 find-setter-for-overloaded-square-brackets
	 
	 )

(require srfi/69 ;; Basic hash tables
	 Scheme+/condx
         (only-in srfi/1 first second)
         Scheme+/def-function
         Scheme+/program-type)

(require (for-syntax racket/base))
         

;; TODO relocate it?
;; > (id-from-proc +)
;; orig-+: undefined;
;;  cannot reference an identifier before its definition
(define-syntax (id-from-proc stx)
  (syntax-case stx ()
    [(_ proc)
     (datum->syntax
      stx
      (string->symbol (string-append "orig-" (symbol->string (syntax-e #'proc)))))]))

;; three separate hash tables, partitioned by overload category (formerly one shared $ovrld-ht$)
(define $ovrld-ht$ (make-hash-table)) ;; BINARY operator overloads only: define-overload-operator*, define-overload-existing-operator*
(define $ovrld-ht-n-ary$ (make-hash-table)) ;; N-ARY associative operator overloads only: define-overload-n-ary-associative-operator*, define-overload-existing-n-ary-associative-operator*
(define $ovrld-ht-proc$ (make-hash-table)) ;; plain PROCEDURE overloads only: define-overload-procedure*, define-overload-existing-procedure*

(define $ovrld-square-brackets-lst$ '()) ;; for square brackets

;; select-ovrld-ht : choose the table an overload entry goes into from the KIND of the adder
;; macro used -- binary operator (overload-[existing-]operator), n-ary operator
;; (overload-[existing-]n-ary-operator) or procedure (overload-[existing-]procedure) --
;; never from "the first table already holding the key" alone.
;; That former rule (find-ovrld-ht) broke as soon as an operator is BOTH binary and n-ary:
;; define-overloaded-operator registers the key in both
;; $ovrld-ht$ and $ovrld-ht-n-ary$, so the first table tested ($ovrld-ht$) always won and every
;; n-ary overload of such an operator was silently filed as a binary one (never used by its
;; n-ary dispatch).
;; Operators defined by the older single-table definers keep working as before:
;; - a binary overload of an operator defined ONLY as n-ary
;;   (define-overload-[existing-]n-ary-associative-operator) goes into $ovrld-ht-n-ary$ : that
;;   dispatcher tells exact multi-predicate entries from one-predicate n-ary entries by the
;;   shape of the predicate list, and existing code relies on it ;
;; - an n-ary overload of an operator defined ONLY as binary
;;   (define-overload-[existing-]operator) is an error : that dispatcher only does exact
;;   matching, so the entry would never act as an n-ary one.
(define (select-ovrld-ht kind qorig-funct)
  (define in-binary? (hash-table-exists? $ovrld-ht$ qorig-funct))
  (define in-n-ary? (hash-table-exists? $ovrld-ht-n-ary$ qorig-funct))
  (define (not-overloadable)
    (error 'overload "~a is not a registered overloadable ~a ; overload it with overload-operator (or a define-overload-* macro for a procedure). Note: an operator defined in the same module as a plain procedure is not overloadable, its original must come from racket/base or another module" qorig-funct kind))
  (case kind
    ((procedure) (if (hash-table-exists? $ovrld-ht-proc$ qorig-funct)
		     $ovrld-ht-proc$
		     (not-overloadable)))
    ((unary) (if in-binary? ; the table of the overloads with an exact number of arguments , as the binary ones
		 $ovrld-ht$
		 (not-overloadable)))
    ((binary) (cond (in-binary? $ovrld-ht$)
		    (in-n-ary? $ovrld-ht-n-ary$) ; operator defined only as n-ary : its dispatcher also handles exact entries
		    (else (not-overloadable))))
    ((n-ary) (cond (in-n-ary? $ovrld-ht-n-ary$)
		   (in-binary? (error 'overload "~a was defined as a binary-only operator (define-overload-operator or define-overload-existing-operator) : an n-ary overload of it would never be used ; define it with define-overloaded-operator instead" qorig-funct))
		   (else (not-overloadable))))
    (else (error 'overload "unknown overload kind ~a" kind))))

(define-syntax overload

  (syntax-rules ()

    ;; arguments are kind of overload (binary, n-ary or procedure), function to be overloaded, procedure that do the overloading, list of predicate to check the arguments

    ((_ kind orig-funct funct (pred-arg1 ...))

     (let* ((qorig-funct (quote orig-funct)) ; quoted original function
	    (target-ht (select-ovrld-ht (quote kind) qorig-funct)) ; the table matching the kind of this overload ; NOT itself one of the globals, just a local reference to the one that applies
	    (ovrld-lst (hash-table-ref target-ht qorig-funct))) ; retrieve list of the overloaded
       ;(display qorig-funct) (newline)
       (hash-table-set! target-ht qorig-funct
			; construct the new list of the overloaded
			(cons (list funct ;; example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))
				    (list pred-arg1 ...))
			      ovrld-lst))))))


#;(define-syntax overload-procedure

  (syntax-rules ()

    ((_ orig-funct funct (pred-arg1 ...))

     (overload procedure orig-funct funct (pred-arg1 ...)))))

#;(define-syntax overload-existing-procedure

  (syntax-rules ()

    ((_ orig-funct funct (pred-arg1 ...))

     (overload procedure orig-funct funct (pred-arg1 ...)))))

(define-syntax overload-existing-operator

  (syntax-rules ()

    ((_ orig-funct funct (pred-arg1 ...))

     (overload binary orig-funct funct (pred-arg1 ...)))

    ((_ orig-funct (ret-val1 ...) funct (pred-arg1 ...)) ;  a 4 params overload procedure for annotation (return types of procedures)

     (overload binary orig-funct funct (pred-arg1 ...)))))

(define-syntax overload-n-ary-operator

  (syntax-rules ()

    ((_ orig-funct funct (pred-arg1 ...))

     (overload n-ary orig-funct funct (pred-arg1 ...)))))

(define-syntax overload-existing-n-ary-operator

  (syntax-rules ()

    ((_ orig-funct funct (pred-arg1 ...))

     (overload n-ary orig-funct funct (pred-arg1 ...)))

    ((_ orig-funct (ret-val1 ...) funct (pred-arg1 ...)) ;  a 4 params overload procedure

     (overload n-ary orig-funct funct (pred-arg1 ...)))))

;; information for the type annotation parser only, which works on one file at a time : restates, in a
;; module importing an operator already overloaded elsewhere, an overload done there with
;; overload-operator, so that the parser knows it. Same forms as overload-operator.
;; A no-op at runtime : the real overload tables were already filled by the imported module.
;; example : (require-overload-operator · multiply-flomat-vector (flomat? vector?))
(define-syntax require-overload-operator

  (syntax-rules ()

    ((_ . args) (void))))

;; args must be the same number as predicates and their types must match , good for binary too
;;  (check-arguments '() '())
;; #t
(define (check-arguments pred-list args)
  ;;(display "check-arguments : pred-list=") (display pred-list) (newline)
  ;;(display "check-arguments : args=")(display args) (newline)
  (if (= (length pred-list) (length args))
      (let ((pred-arg-list (map cons pred-list args))) ; create a list of pairs of the form (predicate . argument)
	(andmap (λ (p) ((car p) (cdr p))) ; example (list? '(1 2 3))
	;; replace andmap with every in Guile
	;;(every (λ (p) ((car p) (cdr p)))
		pred-arg-list))
      #f))

;; args can be not the same number as predicates and their types must match with the first predicate type !
; 1 to n arguments of same type ( the first predicate type)
(define (check-arguments-for-n-arity pred-list args)
  (define type (car pred-list)) ;; i suppose all predicate are same (? anyway we use in code a more restrict function before or-ed with this:check-arguments)
  ; we take the first predicate type
  ;;(display "check-arguments-for-n-arity : type = ") (display type) (newline)
  ;;(display "check-arguments-for-n-arity : args = ") (display args) (newline)
  (define lbd-assign (lambda (arg) (cons type arg))) ; type is always the same predicate
  (define pred-arg-list (map lbd-assign args)) ; type is always the same predicate
  (andmap (λ (p) ((car p) (cdr p))) ; example (list? '(1 2 3))
	  ;; replace andmap with every in Guile
	  ;;(every (λ (p) ((car p) (cdr p)))
	  pred-arg-list))
   

;; (define-overload-existing-procedure length)
;; > (overload length vector-length (vector?))
;; > (length #(1 2 3 4))
;; 4
;; > (length '(1 2 3))
;; 3
;; > (overload length string-length (string?))
;; length
;; > (length "abcde")
;; 5
#;(define-syntax define-overload-existing-procedure

  (syntax-rules ()

    ((_ proc) (define-overload-existing-procedure proc racket/base))

    ((_ proc module-name)

     (begin

       (require (rename-in module-name (proc
       				        orig-proc))) ; note: here orig-proc will be hygienically modified !

       (define qproc (quote proc))

       (define (proc . args-lst)

	 ;;(display proc) (newline)
	 ;;(define ht (hash-table->alist $ovrld-ht-proc$))
	 ;;(display ht) (newline)
	 (define proc-lst (hash-table-ref $ovrld-ht-proc$ qproc)) ;;  example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))
	 ;;(display proc-lst)
	 ;;(newline)

	 (define (check-args-lst pred-list) ; check arguments list match predicates
	   (check-arguments pred-list args-lst))

	 (define (test-proc proc-pred-list) ; test the procedure if it matches with arguments
	   (if (check-args-lst (car (cdr proc-pred-list)))
	       (car proc-pred-list)
	       #f))

	 (define proc-search-result (ormap test-proc  proc-lst)) ; search for a procedure matching arguments

	 (if proc-search-result
	     (apply proc-search-result args-lst)
	     (apply orig-proc args-lst)))

       (hash-table-set! $ovrld-ht-proc$ qproc '())))))

      
; the difference of *-annot functions is that they have an extra parameters which is a symbol name of the backup procedure, ex: orig-*
#;(define-syntax define-overload-existing-procedure-annot

  (syntax-rules ()

    ((_ proc orig-proc) (define-overload-existing-procedure-annot proc orig-proc racket/base))

    ((_ proc orig-proc module-name)

     (begin

       (require (rename-in module-name (proc
       				        orig-proc)))

       (define qproc (quote proc))

       (define (proc . args-lst)

	 ;;(display proc) (newline)
	 ;;(define ht (hash-table->alist $ovrld-ht-proc$))
	 ;;(display ht) (newline)
	 (define proc-lst (hash-table-ref $ovrld-ht-proc$ qproc)) ;;  example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))
	 ;;(display proc-lst)
	 ;;(newline)

	 (define (check-args-lst pred-list) ; check arguments list match predicates
	   (check-arguments pred-list args-lst))

	 (define (test-proc proc-pred-list) ; test the procedure if it matches with arguments
	   (if (check-args-lst (car (cdr proc-pred-list)))
	       (car proc-pred-list)
	       #f))

	 (define proc-search-result (ormap test-proc  proc-lst)) ; search for a procedure matching arguments

	 (if proc-search-result
	     (apply proc-search-result args-lst)
	     (apply orig-proc args-lst)))

       (hash-table-set! $ovrld-ht-proc$ qproc '())))))

;; > (define-overload-procedure area)
;; > (define (area-square x) (* x x))
;; > (area-square 4)
;; 16
;; > (hash-table->alist $ovrld-ht-proc$)
;; '((#<procedure:area>))
;; > (overload area area-square (number?))
;; > (hash-table->alist $ovrld-ht-proc$)
;; '((#<procedure:area> (#<procedure:area-square> (#<procedure:number?>))))
;; > (area 2)
;; 4
;; > (define (area-rectangle x y) (* x y))
;; > (overload area area-rectangle (number? number?))
;; > (area 2 5)
;; 10

#;(define-syntax define-overload-procedure

  (syntax-rules ()

    ((_ proc)

     (begin

       (define qproc (quote proc))

       (define (proc . args-lst)

	 (define proc-lst (hash-table-ref $ovrld-ht-proc$ qproc)) ;;  return example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))
	 ;;(display proc-lst)
	 ;;(newline)

	 (define (check-args-lst pred-list) ; check arguments list match predicates
	   (check-arguments pred-list args-lst))

	 (define (test-proc proc-pred-list) ; test the procedure if it matches with arguments
	   (if (check-args-lst (car (cdr proc-pred-list)))
	       (car proc-pred-list)
	       #f))

	 (define proc-search-result (ormap test-proc  proc-lst)) ; search for a procedure matching arguments

	 (if proc-search-result
	     (apply proc-search-result args-lst)
	     (error 'overload "failed because procedure ~a can not be applied to arguments list ~a" qproc args-lst)))

       (hash-table-set! $ovrld-ht-proc$ qproc '())))))

;; note: AI generated
#;(define-syntax define-overload-procedure-annot

  (syntax-rules ()

    ((_ proc orig-proc)

     (begin

       (define qproc (quote proc))
       (define qorig-proc (quote orig-proc))

       (define (proc . args-lst)

	 (define proc-lst (hash-table-ref $ovrld-ht-proc$ qproc))

	 (define (check-args-lst pred-list)
	   (check-arguments pred-list args-lst))

	 (define (test-proc proc-pred-list)
	   (if (check-args-lst (car (cdr proc-pred-list)))
	       (car proc-pred-list)
	       #f))

	 (define proc-search-result (ormap test-proc proc-lst))

	 (if proc-search-result
	     (apply proc-search-result args-lst)
	     (error 'overload "failed because procedure ~a can not be applied to arguments list ~a" qproc args-lst)))

       (hash-table-set! $ovrld-ht-proc$ qproc '())))))



(define-syntax define-overload-operator ; binary operator here

  (syntax-rules ()

    ((_ proc)

     (begin

       (define qproc (quote proc)) 
       
       (define (proc . args-lst)
	 ; note : the example below is bad for operator, arguments are better for procedure and body of lambda is not like operator
	 (define proc-lst (hash-table-ref $ovrld-ht$ qproc)) ;;  example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))
	 ;;(display proc-lst)
	 ;;(newline)

	 (define (check-args-lst pred-list) ; check arguments list match predicates ,example from above : (number? string?)
	   (check-arguments pred-list args-lst))

	 (define (test-proc proc-pred-list) ; test the procedure if it matches with arguments
	   (if (check-args-lst (car (cdr proc-pred-list))) ; example : (check-args-lst (number? string?))
	       (car proc-pred-list) ; should return the non overloaded procedure for those arguments types
               ; (lambda (n s) (display n) (display s) (newline)) in the above example
	       #f))

	 (define proc-search-result (ormap test-proc proc-lst)) ; search for a procedure matching exact arguments with the exact types

	 (if proc-search-result
	     (apply proc-search-result args-lst)
	     (error 'overload "failed because procedure ~a can not be applied to arguments list ~a" qproc args-lst)))
       ; end define proc

       (hash-table-set! $ovrld-ht$ qproc '())

       ))))

;; note: AI generated
; the difference of *-annot macros is that they have an extra parameter which is the symbol name of the backup procedure, ex: orig-*
; Warning: this procedure seems useless because orig-proc does not exist ,orig-proc and qorig-proc
; are only symbols and and are never used at runtime
#;(define-syntax define-overload-operator-annot ; binary operator here

  (syntax-rules ()

    ((_ proc orig-proc)

     (begin

       (define qproc (quote proc))
       (define qorig-proc (quote orig-proc))

       (define (proc . args-lst)
	 ; note : the example below is bad for operator, arguments are better for procedure and body of lambda is not like operator
	 (define proc-lst (hash-table-ref $ovrld-ht$ qproc)) ;;  example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))
	 ;;(display proc-lst)
	 ;;(newline)

	 (define (check-args-lst pred-list) ; check arguments list match predicates ,example from above : (number? string?)
	   (check-arguments pred-list args-lst))

	 (define (test-proc proc-pred-list) ; test the procedure if it matches with arguments
	   (if (check-args-lst (car (cdr proc-pred-list))) ; example : (check-args-lst (number? string?))
	       (car proc-pred-list) ; should return the non overloaded procedure for those arguments types
               ; (lambda (n s) (display n) (display s) (newline)) in the above example
	       #f))

	 (define proc-search-result (ormap test-proc proc-lst)) ; search for a procedure matching exact arguments with the exact types

	 (if proc-search-result
	     (apply proc-search-result args-lst)
	     (error 'overload "failed because procedure ~a can not be applied to arguments list ~a" qproc args-lst)))
       ; end define proc

       (hash-table-set! $ovrld-ht$ qproc '())

       ))))



;; every name that racket/base provides (values and syntax), fetched once at macro-expansion
;; time via module->exports. Used below to decide automatically whether proc already denotes
;; a racket/base operator, instead of maintaining a hand-picked list -- so any racket/base
;; binding is recognized (sqrt, expt, ...), not just +,-,*,/. Matching is by NAME (symbol),
;; the same granularity the (_ proc module-name) clause already uses when it requires+renames
;; proc's name from racket/base, regardless of what proc happened to be bound to at the call
;; site (this also correctly excludes names overload.rkt itself imports from elsewhere, e.g.
;; srfi/1's 'first', which module->exports on 'racket/base does not list).
;; module->exports is Racket-specific (like identifier-binding): no Scheme standard exposes a
;; module's export list this way.
(define-for-syntax racket-base-export-names
  (let-values ([(vars stxs) (module->exports 'racket/base)])
    (map car (append (apply append (map cdr vars))
                      (apply append (map cdr stxs))))))

(define-for-syntax (racket-base-operator? id)
  (and (memq (syntax-e id) racket-base-export-names) #t))

;; Shared body of an overloaded operator's definition (define qproc, def with the binary-op /
;; n-ary-op / left-associativity dispatch, hash-table-set!). Both clauses of
;; define-overloaded-operator below need this same logic, differing only in:
;; - the prelude: the arithmetic/orig-proc fast path, when an original procedure exists
;; - the tail (default case): fall back to orig-proc when it exists, else error out
;; orig-proc-stx is the identifier bound elsewhere by
;; (require (rename-in module-name (proc orig-proc))), or #f for a brand new operator with
;; nothing to fall back to.
;;
;; Written as ONE phase-1 function building the WHOLE template in one go (not two separately
;; written macro clauses each spelling out the shared logic), so that qproc/args-lst are the
;; same identifiers everywhere below, including inside prelude/tail. This matters: a fragment
;; of syntax written by a macro's CALLER, merely spliced into a callee's template, does NOT
;; hygienically resolve to identifiers the callee's OWN template introduces (verified: a
;; caller-written qproc/args-lst reference given as a macro argument comes back "unbound
;; identifier", even though it ends up lexically nested where they're defined) -- this is the
;; classic anaphoric-macro hygiene trap. Building prelude/tail here, in the same with-syntax
;; call that also names qproc/args-lst, avoids it.
(define-for-syntax (overloaded-operator-body proc-stx orig-proc-stx)
  (with-syntax
      ([proc proc-stx]
       [qproc #'qproc]
       [args-lst #'args-lst]
       [dispatch #'dispatch]
       ;; body of proc, the entry point : a plain procedure, so that the common case of numbers
       ;; goes straight to the original operator without paying the two call/cc that def (used by
       ;; dispatch for return) installs at each call -- measured about 2.7 times slower otherwise
       ;; on (· a b c d) with numbers
       ;; the arithmetic operators (· and × are the Scheme+ multiplications) still exist as they are for
       ;; numbers !!! (n-ary and fast) ; the operator name is known here, at expansion time, so
       ;; the test on it costs nothing at runtime
       [entry (if (and orig-proc-stx
		       (memq (syntax-e proc-stx) '(+ - * / · ×)))
		  (with-syntax ([orig-proc orig-proc-stx])
		    #'(if (andmap number? args-lst)
			  (apply orig-proc args-lst)
			  (apply dispatch args-lst)))
		  #'(apply dispatch args-lst))]
       [tail (if orig-proc-stx
                 (with-syntax ([orig-proc orig-proc-stx])
                   #'(apply orig-proc args-lst)) ; fall back on the original operator (but with limited use for binary operation after associativity split for example)
                 #'(error 'overload "failed because procedure ~a can not be applied to arguments list ~a" qproc args-lst))])

    #'(begin

        (define qproc (quote proc))

        ;; dispatch and the entry point are internal definitions : at the top level (REPL), a
        ;; forward reference from proc to a macro-introduced top-level dispatch would not be linked
        ;; to its definition ("dispatch: undefined"). Inside, proc is the operator itself, so the
        ;; left associativity split below calls the overloaded operator again.
        (define proc ; the overloaded operator
         (let ()

          (def (dispatch . args-lst) ; search of the overload matching the arguments ; def allows return

          ; find if operator is only binary or both binary and n-ary
          (define binary-op (and (hash-table-exists? $ovrld-ht$ qproc)
                                 (not (null? (hash-table-ref $ovrld-ht$ qproc)))))
          (define n-ary-op (and (hash-table-exists? $ovrld-ht-n-ary$ qproc)
                                (not (null? (hash-table-ref $ovrld-ht-n-ary$ qproc)))))

          (define nb-args (length args-lst))

          ; the overloads with an exact number of arguments , binary (2) and unary (1) , are checked first , for any number of
          ; arguments : check-arguments compares the number of predicates with the number of arguments
          (when binary-op ; we will check the arguments match exactly the types , old version : (and (= nb-args 2) binary-op)

            (define whole-proc-pred-lst (hash-table-ref $ovrld-ht$ qproc))

            (define (check-args-lst pred-list) ; check arguments list match predicates (types) list
              ;;(display "pred-list=") (display pred-list) (newline)
              ;;(display "args-lst=") (display args-lst) (newline)
              (check-arguments pred-list args-lst))

            (define (test-proc proc-pred-list) ; test the procedure-predicate list if it matches with arguments list
              ;;(display "proc-pred-list=") (display proc-pred-list) (newline)
              (if (check-args-lst (car (cdr proc-pred-list))) ;; check args match predicates
                  (car proc-pred-list) ;; return procedure
                  #f))

            (define proc-search-result (ormap test-proc whole-proc-pred-lst)) ; search for a procedure matching arguments

            ;;(display "proc-search-result=") (display proc-search-result) (newline)

            (when proc-search-result
              (return
               (apply proc-search-result args-lst)))) ; apply specialized procedure to args


          ; search for an overload with one type only (the n-ary operator) and apply if all args are of the type
          (when n-ary-op ; n-ary operator
            ; retrieve the whole overload list for this operator containing proc-pred list elements
            (define whole-proc-pred-lst (hash-table-ref $ovrld-ht-n-ary$ qproc)) ;;  bad example (being simple binary , not n-ary): ((lambda (n s) (display n) (display s) (newline)) (number? string?))
            ;;(display "overload.rkt : whole-proc-pred-lst=") (display whole-proc-pred-lst) (newline)

            ;; case : an overload registered with a predicate list holding only ONE predicate (in the list)
            ;; if we have one or more values in args-lst and all of them match that unique predicate
            ;; then we apply the procedure that was after the predicate list to the whole args-lst
            (define (test-proc-n-arity-single-pred proc-pred-list) ; if arguments types match return the procedure
              (let ((pred-lst-elem (car (cdr proc-pred-list)))) ; the predicate list of this overload
                (if (and ;(list? pred-lst-elem) ; we assume from this code version that n-ary operator arguments are all of same type
                     ;(= (length pred-lst-elem) 1) ; the predicate list contains only one predicate
                     (pair? args-lst) ; one or more values in args-lst
                     (andmap (car pred-lst-elem) args-lst)) ; all the arguments match that unique predicate
                    (car proc-pred-list) ; return the procedure that was after the predicate list
                    #f)))

            ; search for a procedure matching the argument list in the whole overload list of this operator
            (define n-arity-single-pred-search-result (ormap test-proc-n-arity-single-pred whole-proc-pred-lst))
            ;;(display "overload.rkt : proc : n-arity-single-pred-search-result=") (display n-arity-single-pred-search-result) (newline)

            (when n-arity-single-pred-search-result ; we find a matching procedure
              (return
               (apply n-arity-single-pred-search-result args-lst))))


          ; LEFT associativity property application in case of need:
          ; if the basic n-ary has failed to find a match we try to use the associativity property of operator
          ; we try to find possible match by dividing recursively (if necessary) the expression using its LEFT associativity property,
          ;  example: (+ a b c) = (+1 (+2 a b) c) , and check if the types of the divided expression match with the types of the n-ary operator/procedure

          (when (and (> nb-args 2)
                     binary-op)
            ;(display "overload.rkt : proc : applying LEFT associativity") (newline)

            (return
             (apply proc (cons (proc (first args-lst)
                                     (second args-lst)) ; why does we need doing that? this arise when we have 2 binary operators used in one n-ary expression : ex: n * v1 * v2  , n number, v vectors, * both binary operator product and dot product !
                               (cddr args-lst))))) ; n-ary

          tail)

          (define (proc . args-lst) ; entry point
            entry)

          proc))

        ;; create the (empty) lists of overloaded procedures only if absent : the operator can be
        ;; overloaded several times, and a redefinition (at the REPL, overload-operator cannot see
        ;; top-level definitions, so it defines the operator again at each call) must not erase
        ;; the overloads already added
        (unless (hash-table-exists? $ovrld-ht-n-ary$ qproc)
          (hash-table-set! $ovrld-ht-n-ary$ qproc '()))
        (unless (hash-table-exists? $ovrld-ht$ qproc)
          (hash-table-set! $ovrld-ht$ qproc '())))))

;; define-overloaded-operator dispatches, at macro-expansion time, between:
;; - proc already denoting a racket/base operator (racket-base-operator? above): delegate to
;;   the (_ proc module-name) clause, which requires/renames it from racket/base and keeps
;;   the numeric fast path via orig-proc
;; - anything else: define a brand new operator from scratch, with no orig-proc fallback
;; (See discussion: unlike the earlier "generic-known-program? + if-flag" attempt, which was
;; broken in two ways: (1) if-flag used a syntax-case pattern variable as a plain runtime
;; boolean, which Racket rejects at compile time ("pattern variable used outside of a
;; template") ; (2) even fixed, a *runtime*-computed flag can never select between two
;; different sets of top-level define's -- a module's shape must be statically known. The
;; check above makes the choice at expansion time instead, so only one branch's definitions
;; ever reach the expanded code.)

(define-syntax define-overloaded-operator

  (lambda (stx)
    (syntax-case stx ()

      ((_ proc)

       (if (racket-base-operator? #'proc)
           #'(define-overloaded-operator proc racket/base)
           (overloaded-operator-body #'proc #f)))


    ((_ proc module-name) ; if there is a module-name provide then we know the proc operator already exists

     (with-syntax ([body (overloaded-operator-body #'proc #'orig-proc)])
       #'(begin
           ;(display "define-overload-existing-n-ary-associative-operator : proc=") (display proc) (newline)
           (require (rename-in module-name (proc orig-proc)))
           body))))))


;; is id bound to a definition of the module being expanded (for example one made by a
;; previous overload-operator on the same operator) ?
;; A pure query at expansion time, no phase-1 side effect (a registry filled during expansion
;; could be wrong if Racket expands a form more than once). Racket-specific, like
;; module->exports above. Top-level (REPL) definitions are not module bindings : there it
;; answers #f and the operator is simply defined again (see the table initialization in
;; overloaded-operator-body, which keeps the overloads already added).
(define-for-syntax (defined-in-this-module? id)
  (let ((binding (identifier-binding id)))
    (and (pair? binding)
	 (let-values (((name base) (module-path-index-split (car binding))))
	   (not (or name base)))))) ; #f #f : the module itself

;; expansion of overload-operator : (begin <definition of the overloaded operator, if needed> <added overload>)
(define-for-syntax (overload-operator-expansion stx proc-stx module-name-stx funct-stx preds-stx (unary? #f))

  ;; the number of predicates chooses the kind of overload , except for overload-unary-operator
  (define kind
    (if unary?
	(syntax-case preds-stx () ; exactly one argument
	  ((pred) #'unary)
	  (_ (raise-syntax-error 'overload-unary-operator "expected 1 predicate (the type of the single argument)" stx preds-stx)))
    (syntax-case preds-stx ()
      ((pred) #'n-ary) ; one predicate : all the arguments must have this type, whatever their number
      ((pred1 pred2) #'binary)
      (_ (raise-syntax-error 'overload-operator
			     "expected 1 predicate (n-ary overload : all arguments of that type) or 2 predicates (binary overload)"
			     stx preds-stx)))))

  (define definition
    (cond ((defined-in-this-module? proc-stx) #'(void)) ; already overloaded in this module : only add the overload
	  (module-name-stx ; a module is given : the original operator exists there, nothing to check
	   (with-syntax ((proc proc-stx) (module-name module-name-stx))
	     #'(define-overloaded-operator proc module-name)))
	  ((or (racket-base-operator? proc-stx) ; original operator in racket/base
	       (not (identifier-binding proc-stx))) ; or a brand new operator
	   (with-syntax ((proc proc-stx))
	     #'(define-overloaded-operator proc)))
	  (else (raise-syntax-error 'overload-operator
				    "operator imported from a module other than racket/base : give that module, as in (overload-operator proc module-name funct (pred ...))"
				    stx proc-stx))))

  (with-syntax ((proc proc-stx) (kind kind) (funct funct-stx) (preds preds-stx) (definition definition))
    #'(begin
	definition
	(overload kind proc funct preds))))

;; heads of the require sub-forms that can make up a module name given to overload-operator,
;; e.g. (except-in Scheme+/multiply ×)
(define-for-syntax require-spec-heads
  '(only-in except-in prefix-in rename-in combine-in relative-in only-meta-in only-space-in
    for-syntax for-template for-label for-meta for-space submod file lib planet quote))

;; in (overload-operator proc x funct (pred ...)), is x a module name (identifier, string or
;; require spec) rather than the list of return values ?
(define-for-syntax (module-name? stx)
  (syntax-case stx ()
    ((head . rest) (and (identifier? #'head)
			(memq (syntax-e #'head) require-spec-heads)
			#t))
    (_ (or (identifier? stx)
	   (string? (syntax-e stx))))))

;; define the overloaded operator if needed, then add an overload to it :
;; (overload-operator + string-append (string?))                  ; n-ary : any number of strings
;; (overload-operator + vector-append (vector? vector?))          ; binary
;; (overload-operator · Scheme+/multiply multiply-matrix-matrix (matrix-vect? matrix-vect?)) ; original operator taken from a module
;; (overload-operator + (vector?) vector-append (vector? vector?)) ; with the list of return values
;; (overload-operator · Scheme+/multiply (matrix-vect?) multiply-matrix-matrix (matrix-vect? matrix-vect?)) ; both
;; The list of return values is not stored anywhere : it is only read by the type annotation
;; parser, which runs before compilation and runtime.
;; Rule : the predicates must accept ANY value and answer #t or #f, never raise an error. The
;; runtime dispatcher really calls them on the actual arguments, trying every overload until one
;; matches, so it can call a predicate on a value of another type (for example a list). A partial
;; predicate like inexact?, zero?, positive? or even? raises an error on a non-number and stops the
;; whole dispatch ; guard it first, as in (λ (x) (and (number? x) (inexact? x))).
(define-syntax overload-operator

  (lambda (stx)
    (syntax-case stx ()

      ((_ proc funct preds)
       (overload-operator-expansion stx #'proc #f #'funct #'preds))

      ((_ proc x funct preds) ; x is a module name or the list of return values
       (cond ((module-name? #'x)
	      (overload-operator-expansion stx #'proc #'x #'funct #'preds))
	     ((syntax->list #'x) ; list of return values : ignored here
	      (overload-operator-expansion stx #'proc #f #'funct #'preds))
	     (else (raise-syntax-error 'overload-operator
				       "expected a module name or a list of return values"
				       stx #'x))))

      ((_ proc module-name (ret-val ...) funct preds) ; list of return values ignored here
       (overload-operator-expansion stx #'proc #'module-name #'funct #'preds)))))


;; ---------------------------------------------------------------------------------------
;; procedures : overload-procedure, the same single step as overload-operator but simpler
;; (no binary / n-ary, no associativity, no numeric fast path) : the overloads are searched
;; with an exact match of the arguments on the predicates (check-arguments) in $ovrld-ht-proc$
;; ---------------------------------------------------------------------------------------

;; definition of an overloaded procedure : the dispatcher of the former
;; define-overload-existing-procedure / define-overload-procedure, a plain define (no def,
;; no call/cc). orig-proc-stx is the identifier bound to the original procedure by the
;; caller's require, or #f for a brand new procedure (then error if no overload matches).
;; Built in one with-syntax so that qproc/args-lst are the same identifiers everywhere.
;; (overload-unary-operator proc [module-name] [(ret-val ...)] funct (pred)) : an overload of proc with exactly one argument ,
;; (overload-unary-operator - (matrix?) negate-matrix (matrix?)) : (- M) is (negate-matrix M) , but not (- M1 M2) ;
;; with (overload-operator - (matrix?) negate-matrix (matrix?)) , n-ary , negate-matrix would be called with any number of matrices
(define-syntax overload-unary-operator
  (lambda (stx)
    (syntax-case stx ()
      ((_ proc funct preds)
       (overload-operator-expansion stx #'proc #f #'funct #'preds #t))
      ((_ proc x funct preds) ; x is a module name or the list of return values
       (cond ((module-name? #'x)
	      (overload-operator-expansion stx #'proc #'x #'funct #'preds #t))
	     ((syntax->list #'x) ; list of return values : ignored here
	      (overload-operator-expansion stx #'proc #f #'funct #'preds #t))
	     (else (raise-syntax-error 'overload-unary-operator
				       "expected a module name or a list of return values"
				       stx #'x))))
      ((_ proc module-name (ret-val ...) funct preds) ; list of return values ignored here
       (overload-operator-expansion stx #'proc #'module-name #'funct #'preds #t)))))

;; for the type annotation pass in a module that imports the overload (see require-overload-operator)
(define-syntax require-overload-unary-operator
  (syntax-rules ()
    ((_ . args) (void))))

(define-for-syntax (overloaded-procedure-body proc-stx orig-proc-stx)
  (with-syntax
      ([proc proc-stx]
       [qproc #'qproc]
       [args-lst #'args-lst]
       [tail (if orig-proc-stx
		 (with-syntax ([orig-proc orig-proc-stx])
		   #'(apply orig-proc args-lst)) ; fall back on the original procedure
		 #'(error 'overload "failed because procedure ~a can not be applied to arguments list ~a" qproc args-lst))])

    #'(begin

	(define qproc (quote proc))

	(define (proc . args-lst)

	  (define proc-lst (hash-table-ref $ovrld-ht-proc$ qproc)) ;;  example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))

	  (define (test-proc proc-pred-list) ; test the procedure if it matches with arguments
	    (if (check-arguments (car (cdr proc-pred-list)) args-lst)
		(car proc-pred-list)
		#f))

	  (define proc-search-result (ormap test-proc proc-lst)) ; search for a procedure matching arguments

	  (if proc-search-result
	      (apply proc-search-result args-lst)
	      tail))

	;; create the (empty) list of overloads only if absent : the procedure can be overloaded
	;; several times, a redefinition (at the REPL) must not erase the overloads already added
	(unless (hash-table-exists? $ovrld-ht-proc$ qproc)
	  (hash-table-set! $ovrld-ht-proc$ qproc '())))))

;; expansion of overload-procedure : (begin <definition of the overloaded procedure, if needed> <added overload>)
;; same decisions as overload-operator-expansion
(define-for-syntax (overload-procedure-expansion stx proc-stx module-name-stx funct-stx preds-stx)

  (unless (syntax->list preds-stx)
    (raise-syntax-error 'overload-procedure "expected a list of predicates" stx preds-stx))

  (define (definition-from module-stx) ; the original procedure is taken from module-stx
    (with-syntax ([proc proc-stx]
		  [module-name module-stx]
		  [body (overloaded-procedure-body proc-stx #'orig-proc)])
      #'(begin
	  (require (rename-in module-name (proc orig-proc)))
	  body)))

  (define definition
    (cond ((defined-in-this-module? proc-stx) #'(void)) ; already overloaded in this module : only add the overload
	  (module-name-stx (definition-from module-name-stx)) ; a module is given : the original procedure exists there, nothing to check
	  ((racket-base-operator? proc-stx) (definition-from #'racket/base)) ; original procedure in racket/base
	  ((not (identifier-binding proc-stx)) (overloaded-procedure-body proc-stx #f)) ; a brand new procedure
	  (else (raise-syntax-error 'overload-procedure
				    "procedure imported from a module other than racket/base : give that module, as in (overload-procedure proc module-name funct (pred ...))"
				    stx proc-stx))))

  (with-syntax ((proc proc-stx) (funct funct-stx) (preds preds-stx) (definition definition))
    #'(begin
	definition
	(overload procedure proc funct preds))))

;; define the overloaded procedure if needed, then add an overload to it (any number of predicates, exact match) :
;; (overload-procedure area area-square (number?))
;; (overload-procedure area area-rect (number? number?))
;; (overload-procedure length vector-length (vector?))          ; original length from racket/base
;; (overload-procedure uniform (flonum?) uniform-interval (flonum? flonum?)) ; with the list of return values
;; (overload-procedure foo "foo-module.rkt" foo-vector (vector?)) ; original procedure taken from a module
;; The list of return values is not stored anywhere : it is only read by the type annotation parser.
;; Rule : the predicates must accept ANY value and answer #t or #f, never raise an error (see overload-operator).
(define-syntax overload-procedure

  (lambda (stx)
    (syntax-case stx ()

      ((_ proc funct preds)
       (overload-procedure-expansion stx #'proc #f #'funct #'preds))

      ((_ proc x funct preds) ; x is a module name or the list of return values
       (cond ((module-name? #'x)
	      (overload-procedure-expansion stx #'proc #'x #'funct #'preds))
	     ((syntax->list #'x) ; list of return values : ignored here
	      (overload-procedure-expansion stx #'proc #f #'funct #'preds))
	     (else (raise-syntax-error 'overload-procedure
				       "expected a module name or a list of return values"
				       stx #'x))))

      ((_ proc module-name (ret-val ...) funct preds) ; list of return values ignored here
       (overload-procedure-expansion stx #'proc #'module-name #'funct #'preds)))))

;; information for the type annotation parser only (see require-overload-operator) : restates, in
;; a module importing a procedure already overloaded elsewhere, an overload done there.
;; Same forms as overload-procedure. A no-op at runtime.
(define-syntax require-overload-procedure

  (syntax-rules ()

    ((_ . args) (void))))



(define-syntax define-overload-n-ary-associative-operator

  (syntax-rules ()

    ((_ proc)

     (begin

       (define qproc (quote proc)) 
       
       (define (proc . args-lst)

	 (condx  ; search for an overload with one type only (the n-ary operator) and apply if all args are of the type
                (exec ; ? the define in condx after arithmetic cases for optimisation ?
                 (define proc-lst (hash-table-ref $ovrld-ht-n-ary$ qproc)) ;;  example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))
                 ;;(display "overload.rkt : proc-lst=") (display proc-lst) (newline)

                 ;; case : an overload registered with a predicate list holding only ONE predicate
                 ;; if we have one or more values in args-lst and all of them match that unique predicate
                 ;; then we apply the procedure that was after the predicate list to the whole args-lst
                 (define (test-proc-n-arity-single-pred proc-pred-list)
                   (let ((pred-lst-elem (car (cdr proc-pred-list)))) ; the predicate list of this overload
                     (if (and (list? pred-lst-elem)
                              (= (length pred-lst-elem) 1) ; the predicate list contains only one predicate
                              (pair? args-lst) ; one or more values in args-lst
                              (andmap (car pred-lst-elem) args-lst)) ; all the arguments match that unique predicate
                         (car proc-pred-list) ; return the procedure that was after the predicate list
                         #f)))

                 (define n-arity-single-pred-search-result (ormap test-proc-n-arity-single-pred proc-lst))
                 ;;(display "overload.rkt : proc : n-arity-single-pred-search-result=") (display n-arity-single-pred-search-result) (newline)
                 )

                (n-arity-single-pred-search-result
                  ;;(display "overload.rkt : proc : args-lst=") (display args-lst) (newline)
                  (apply n-arity-single-pred-search-result args-lst))

                
                ; specialized overload search with exact match
                (exec ; defines only used by the proc-search-result case below
                 (define (check-args-lst pred-list) ; check arguments list match predicates
                   (check-arguments pred-list args-lst))

                 (define (test-proc proc-pred-list) ; test the procedure if it matches with arguments
                   (if (check-args-lst (car (cdr proc-pred-list)))
                       (car proc-pred-list) ; will return the procedure in case of match
                       #f))

                 (define proc-search-result (ormap test-proc proc-lst)) ; search for a procedure matching arguments
                 ;;(display "overload.rkt : proc : proc-search-result=") (display proc-search-result) (newline)
                 )

                (proc-search-result
                  ;;(display "overload.rkt : proc : args-lst=") (display args-lst) (newline)
                  (apply proc-search-result args-lst))

                ; associativity property application in cas of need
                (exec
                 (define nb-args (length args-lst)))
                
                ((> nb-args 2) ;;(display "overload.rkt : proc : applying associativity") (newline)
                               (proc (car args-lst) ; why does we need doing that? this arise when we have 2 binary operators used in one n-ary expression : ex: n * v1 * v2  , n number, v vectors, * both binary operator product and dot product !
                                     (apply proc (cdr args-lst)))) ; n-ary

		(else (error 'overload "failed because procedure ~a can not be applied to arguments list ~a" qproc args-lst))))

       (hash-table-set! $ovrld-ht-n-ary$ qproc '())))))

;; note: AI generated
; the difference of *-annot macros is that they have an extra parameter which is the symbol name of the backup procedure, ex: orig-*
; seems useless ,see comment above
#;(define-syntax define-overload-n-ary-associative-operator-annot

  (syntax-rules ()

    ((_ proc orig-proc)

     (begin

       (define qproc (quote proc))
       (define qorig-proc (quote orig-proc))

       (define (proc . args-lst)

	 (condx  ; search for an overload with one type only (the n-ary operator) and apply if all args are of the type
                (exec ; the define in condx after arithmetic cases for optimisation
                 (define proc-lst (hash-table-ref $ovrld-ht-n-ary$ qproc)) ;;  example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))
                 ;;(display "overload.rkt : proc-lst=") (display proc-lst) (newline)

                 ;; case : an overload registered with a predicate list holding only ONE predicate
                 ;; if we have one or more values in args-lst and all of them match that unique predicate
                 ;; then we apply the procedure that was after the predicate list to the whole args-lst
                 (define (test-proc-n-arity-single-pred proc-pred-list)
                   (let ((pred-lst-elem (car (cdr proc-pred-list)))) ; the predicate list of this overload
                     (if (and (list? pred-lst-elem)
                              (= (length pred-lst-elem) 1) ; the predicate list contains only one predicate
                              (pair? args-lst) ; one or more values in args-lst
                              (andmap (car pred-lst-elem) args-lst)) ; all the arguments match that unique predicate
                         (car proc-pred-list) ; return the procedure that was after the predicate list
                         #f)))

                 (define n-arity-single-pred-search-result (ormap test-proc-n-arity-single-pred proc-lst))
                 ;;(display "overload.rkt : proc : n-arity-single-pred-search-result=") (display n-arity-single-pred-search-result) (newline)
                 )

                (n-arity-single-pred-search-result
                  ;;(display "overload.rkt : proc : args-lst=") (display args-lst) (newline)
                  (apply n-arity-single-pred-search-result args-lst))

                ; specialized overload search with exact match
                (exec ; defines only used by the proc-search-result case below
                 (define (check-args-lst pred-list) ; check arguments list match predicates
                   (check-arguments pred-list args-lst))

                 (define (test-proc proc-pred-list) ; test the procedure if it matches with arguments
                   (if (check-args-lst (car (cdr proc-pred-list)))
                       (car proc-pred-list) ; will return the procedure in case of match
                       #f))

                 (define proc-search-result (ormap test-proc proc-lst)) ; search for a procedure matching arguments
                 ;;(display "overload.rkt : proc : proc-search-result=") (display proc-search-result) (newline)
                 )

                (proc-search-result
                  ;;(display "overload.rkt : proc : args-lst=") (display args-lst) (newline)
                  (apply proc-search-result args-lst))

                ; associativity property application in cas of need
                (exec
                 (define nb-args (length args-lst)))

                ((> nb-args 2) ;;(display "overload.rkt : proc : applying associativity") (newline)
                               (proc (car args-lst) ; why does we need doing that? this arise when we have 2 binary operators used in one n-ary expression : ex: n * v1 * v2  , n number, v vectors, * both binary operator product and dot product !
                                     (apply proc (cdr args-lst)))) ; n-ary

		(else (error 'overload "failed because procedure ~a can not be applied to arguments list ~a" qproc args-lst))))

       (hash-table-set! $ovrld-ht-n-ary$ qproc '())))))

(define-syntax define-overload-existing-operator

  (syntax-rules ()

    ((_ proc) (define-overload-existing-operator proc racket/base))

    ;; example: (define-overload-existing-operator · Scheme+/multiply)
    ((_ proc module-name)

     (begin

       ;(display "define-overload-existing-operator : proc=") (display proc) (newline)
       (require (rename-in module-name (proc orig-proc))) ; hygienic macro prevents error in case of reuse of orig-proc in another overloading

       (define qproc (quote proc)) 
       
       (define (proc . args-lst)

	 ;;(display "proc=") (display proc) (newline)
	 ;;(define ht (hash-table->alist $ovrld-ht$))
	 ;;(display ht) (newline)

	 (define proc-lst (hash-table-ref $ovrld-ht$ qproc)) ;;  example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))
	 ;;(display "proc-lst=") (display proc-lst)
	 ;;(newline)
	 
	 (define (check-args-lst pred-list) ; check arguments list match predicates
	   ;;(display "pred-list=") (display pred-list) (newline)
	   ;;(display "args-lst=") (display args-lst) (newline)
	   (check-arguments pred-list args-lst))

	 (define (test-proc proc-pred-list) ; test the procedure if it matches with arguments
	   ;;(display "proc-pred-list=") (display proc-pred-list) (newline)
	   (if (check-args-lst (car (cdr proc-pred-list))) ;; check args
	       (car proc-pred-list) ;; return procedure
	       #f))
	 
	 (define proc-search-result (ormap test-proc proc-lst)) ; search for a procedure matching arguments
	 
	 ;;(display "proc-search-result=") (display proc-search-result) (newline)
	 
	 (if proc-search-result
	     (apply proc-search-result args-lst)
	     (apply orig-proc args-lst)))
       
       ;;(hash-table-set! $ovrld-ht$ qproc (list (list orig-proc (list number? number?)))) ; error: we do not have always numbers !
       (hash-table-set! $ovrld-ht$ qproc '())
       
       ))))



(define-syntax define-overload-existing-operator-annot

  (syntax-rules ()

    ((_ proc orig-proc) (define-overload-existing-operator-annot proc orig-proc racket/base))

    ;; example: (define-overload-existing-operator · Scheme+/multiply)
    ((_ proc orig-proc module-name)

     (begin

       (require (rename-in module-name (proc orig-proc)))

       (define qproc (quote proc)) 
       
       (define (proc . args-lst)

	 ;;(display "proc=") (display proc) (newline)
	 ;;(define ht (hash-table->alist $ovrld-ht$))
	 ;;(display ht) (newline)

	 (define proc-lst (hash-table-ref $ovrld-ht$ qproc)) ;;  example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))
	 ;;(display "proc-lst=") (display proc-lst)
	 ;;(newline)
	 
	 (define (check-args-lst pred-list) ; check arguments list match predicates
	   ;;(display "pred-list=") (display pred-list) (newline)
	   ;;(display "args-lst=") (display args-lst) (newline)
	   (check-arguments pred-list args-lst))

	 (define (test-proc proc-pred-list) ; test the procedure if it matches with arguments
	   ;;(display "proc-pred-list=") (display proc-pred-list) (newline)
	   (if (check-args-lst (car (cdr proc-pred-list))) ;; check args
	       (car proc-pred-list) ;; return procedure
	       #f))

	 
	 (define proc-search-result (ormap test-proc proc-lst)) ; search for a procedure matching arguments

	 
	 ;;(display "proc-search-result=") (display proc-search-result) (newline)
	 
	 (condx (proc-search-result (apply proc-search-result args-lst))
		(exec
		 (define nb-args (length args-lst)))
		#;((> nb-args 2)   ;;(display ">2 args") (newline)
		 (proc (car args-lst) (apply proc (cdr args-lst))))
		(else
		 ;;(display "else") (newline)
		 (apply orig-proc args-lst))))
       
       (hash-table-set! $ovrld-ht$ qproc '())

       ))))

;; warning : all operators must be n-arity ,example: + for numbers is ,so if you wrote a + for lists ,you must provide it n-arity too, otherwise use overload for binary operators or another symbol than +,example +b for binary operators

;; FR : ce dispatcher runtime implémente le MÊME algorithme (recherche directe puis division associative gauche
;;      récursive) que operator-associativity-recursion / operator-associativity-recursion-and-get-return-type-of-procedure
;;      dans SRFI-105-for-Racket/bootstrapping/annot+.rkt, mais sur des valeurs réelles (prédicats Racket) au lieu
;;      de types/symboles inférés statiquement : c'est la procédure réellement appelée quand annot+.rkt n'a pas pu
;;      résoudre statiquement un appel d'opérateur. Voir l'analyse comparative complète (FR/EN/IT) dans annot+.rkt,
;;      au-dessus de operator-associativity-recursion-and-get-return-type-of-procedure.
;; EN : this runtime dispatcher implements the SAME algorithm (flat/direct lookup, then recursive left-associative
;;      split) as operator-associativity-recursion / operator-associativity-recursion-and-get-return-type-of-procedure
;;      in SRFI-105-for-Racket/bootstrapping/annot+.rkt, but over real values (Racket predicates) instead of
;;      statically-inferred types/symbols: it is the actual procedure called when annot+.rkt could not resolve an
;;      operator call statically. See the full comparative analysis (FR/EN/IT) in annot+.rkt, above
;;      operator-associativity-recursion-and-get-return-type-of-procedure.
;; IT : questo dispatcher a runtime implementa lo STESSO algoritmo (ricerca diretta poi divisione associativa
;;      sinistra recursiva) di operator-associativity-recursion / operator-associativity-recursion-and-get-return-type-of-procedure
;;      in SRFI-105-for-Racket/bootstrapping/annot+.rkt, ma su valori reali (predicati Racket) invece che su
;;      tipi/simboli inferiti staticamente : è la procedura effettivamente chiamata quando annot+.rkt non è
;;      riuscito a risolvere staticamente una chiamata di operatore. Vedi l'analisi comparativa completa
;;      (FR/EN/IT) in annot+.rkt, sopra operator-associativity-recursion-and-get-return-type-of-procedure.
(define-syntax define-overload-existing-n-ary-associative-operator

  (syntax-rules ()

    ((_ proc) (define-overload-existing-n-ary-associative-operator proc racket/base))
    
    ((_ proc module-name)

     (begin
       ;(display "define-overload-existing-n-ary-associative-operator : proc=") (display proc) (newline)
       (require (rename-in module-name (proc orig-proc)))

       (define qproc (quote proc)) 
       
       (define (proc . args-lst)
	 
	 (condx ((and (member qproc '(+ - * /)) ; arithmetic operators still exists as they are !!! (n-ary and fast)
                      (andmap number? args-lst))
                 (apply orig-proc args-lst))
               
                ; search for an overload with one type only (the n-ary operator) and apply if all args are of the type
                (exec ; the define in condx after arithmetic cases for optimisation
                 (define proc-lst (hash-table-ref $ovrld-ht-n-ary$ qproc)) ;;  example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))
                 ;;(display "overload.rkt : proc-lst=") (display proc-lst) (newline)

                 ;; case : an overload registered with a predicate list holding only ONE predicate
                 ;; if we have one or more values in args-lst and all of them match that unique predicate
                 ;; then we apply the procedure that was after the predicate list to the whole args-lst
                 (define (test-proc-n-arity-single-pred proc-pred-list)
                   (let ((pred-lst-elem (car (cdr proc-pred-list)))) ; the predicate list of this overload
                     (if (and (list? pred-lst-elem)
                              (= (length pred-lst-elem) 1) ; the predicate list contains only one predicate
                              (pair? args-lst) ; one or more values in args-lst
                              (andmap (car pred-lst-elem) args-lst)) ; all the arguments match that unique predicate
                         (car proc-pred-list) ; return the procedure that was after the predicate list
                         #f)))

                 ; a pure n-arity match : return the procedure or #f
                 (define n-arity-single-pred-search-result (ormap test-proc-n-arity-single-pred proc-lst))
                 ;;(display "overload.rkt : proc : n-arity-single-pred-search-result=") (display n-arity-single-pred-search-result) (newline)
                 )

                ; searched a pure n-arity match , if true apply procedure to arguments
                (n-arity-single-pred-search-result
                  ;;(display "overload.rkt : proc : args-lst=") (display args-lst) (newline)
                  (apply n-arity-single-pred-search-result args-lst))

                
                ; specialized overload search with exact match of all arguments to each different types
                (exec ; defines only used by the proc-search-result case below
                 (define (check-args-lst pred-list) ; check arguments list match predicates
                   (check-arguments pred-list args-lst))
                 ;(or (check-arguments pred-list args-lst)
                 ;   (check-arguments-for-n-arity  pred-list args-lst)))

                 ;(check-arguments-for-n-arity pred-list args-lst)) ;;  this condition is more permissive than the above one : all arguments just have to match the same first predicate type

                 (define (test-proc proc-pred-list) ; test the procedure if it matches with arguments
                   (if (check-args-lst (car (cdr proc-pred-list)))
                       (car proc-pred-list) ; will return the procedure in case of match
                       #f))

                 (define proc-search-result (ormap test-proc proc-lst)) ; search for a procedure matching arguments
                 ;;(display "overload.rkt : proc : proc-search-result=") (display proc-search-result) (newline)
                 )

                (proc-search-result
                  ;;(display "overload.rkt : proc : args-lst=") (display args-lst) (newline)
                  (apply proc-search-result args-lst))

                
                ; LEFT associativity property application in case of need
                (exec
                 (define nb-args (length args-lst)))
                
                ((> nb-args 2) ;(display "overload.rkt : proc : applying LEFT associativity") (newline)
                 (apply proc (cons (proc (first args-lst)
                                         (second args-lst)) ; why does we need doing that? this arise when we have 2 binary operators used in one n-ary expression : ex: n * v1 * v2  , n number, v vectors, * both binary operator product and dot product !
                                   (cddr args-lst)))) ; n-ary

                ; fall back on the original operator (but with limited use for binary operation after associativity split)
                (else
                 (apply orig-proc args-lst)))

         )
       
       (hash-table-set! $ovrld-ht-n-ary$ qproc '()) ; set the list of overloaded procedures empty after define overload
       ;;(replace-operator! orig-proc proc)
       ))))

;; FR/EN/IT : même mécanisme que define-overload-existing-n-ary-associative-operator ci-dessus — voir son
;;            commentaire (lien avec annot+.rkt / overload.rkt) juste au-dessus / same mechanism as
;;            define-overload-existing-n-ary-associative-operator above — see its comment (link with
;;            annot+.rkt / overload.rkt) right above it / stesso meccanismo di
;;            define-overload-existing-n-ary-associative-operator sopra — vedi il suo commento (collegamento
;;            con annot+.rkt / overload.rkt) appena sopra.
(define-syntax define-overload-existing-n-ary-associative-operator-annot

  (syntax-rules ()

    ((_ proc orig-proc) (define-overload-existing-n-ary-associative-operator-annot proc orig-proc racket/base))
    
    ((_ proc orig-proc module-name)

     (begin
     
       (require (rename-in module-name (proc
       				        orig-proc)))

       (define qproc (quote proc)) 
       
       (define (proc . args-lst)

	 (condx ((and (member qproc '(+ - * /)) ; arithmetic operators still exists as they are !!! (n-ary and fast)
		      (andmap number? args-lst))
		 (apply orig-proc args-lst))

		; search for an overload with one type only (the n-ary operator) and apply if all args are of the type
		(exec ; the define in condx after arithmetic cases for optimisation
		 (define proc-lst (hash-table-ref $ovrld-ht-n-ary$ qproc)) ;;  example: ((lambda (n s) (display n) (display s) (newline)) (number? string?))

		 ;; case : an overload registered with a predicate list holding only ONE predicate
		 ;; if we have one or more values in args-lst and all of them match that unique predicate
		 ;; then we apply the procedure that was after the predicate list to the whole args-lst
		 (define (test-proc-n-arity-single-pred proc-pred-list)
		   (let ((pred-lst-elem (car (cdr proc-pred-list)))) ; the predicate list of this overload
		     (if (and (list? pred-lst-elem)
			      (= (length pred-lst-elem) 1) ; the predicate list contains only one predicate
			      (pair? args-lst) ; one or more values in args-lst
			      (andmap (car pred-lst-elem) args-lst)) ; all the arguments match that unique predicate
			 (car proc-pred-list) ; return the procedure that was after the predicate list
			 #f)))

		 (define n-arity-single-pred-search-result (ormap test-proc-n-arity-single-pred proc-lst)))

		(n-arity-single-pred-search-result (apply n-arity-single-pred-search-result args-lst))

		; specialized overload search with exact match, this should works also for BINARY with 2 arguments! (as all are stored in the same list in hash table value of the operator key)
		(exec ; defines only used by the proc-search-result case below
		 (define (check-args-lst pred-list) ; check arguments list match predicates
		   (or (check-arguments pred-list args-lst)
                       (check-arguments-for-n-arity  pred-list args-lst)))
		   ;(check-arguments-for-n-arity  pred-list args-lst)) ;;  this condition is more permissive than the above one

		 (define (test-proc proc-pred-list) ; test the procedure if it matches with arguments
		   (if (check-args-lst (car (cdr proc-pred-list)))
		       (car proc-pred-list) ; will return the procedure in case of match
		       #f))

		 (define proc-search-result (ormap test-proc  proc-lst))) ; search for a procedure matching arguments

		(proc-search-result (apply proc-search-result args-lst))

		; LEFT associativity property application in cas of need
		(exec
		 (define nb-args (length args-lst)))

		((> nb-args 2) (apply proc (cons (proc (first args-lst)
                                                       (second args-lst)) ; why does we need doing that? this arise when we have 2 binary operators used in one n-ary expression : ex: n * v1 * v2  , n number, v vectors, * both binary operator product and dot product !
                                                 (cddr args-lst)))) ; n-ary

		; fall back on the original operator (but with limited use for binary operation after associativity split)
		(else
		 (apply orig-proc args-lst)))

         )
       
       (hash-table-set! $ovrld-ht-n-ary$ qproc '())
       ;;(replace-operator! orig-proc proc)
       ))))

;; (overload-square-brackets vector-ref vector-set! (vector? number?))
;; > $ovrld-square-brackets-lst$
;; '(((vector? number?) (#<procedure:vector-ref> . #<procedure:vector-set!>)))

;; > matrix-vect?
;; #<procedure:matrix-vect?>
;; >  (overload-square-brackets matrix-vect-ref
;; 	 matrix-vect-set!  (matrix-vect? number? number?))
;; > $ovrld-square-brackets-lst$
;; '(((#<procedure:matrix-vect?> #<procedure:number?> #<procedure:number?>) (#<procedure:matrix-vect-ref> . #<procedure:matrix-vect-set!>)))
;; > (define Mv (matrix-vect #(#(1 2 3) #(4 5 6))))
;; > Mv
;; #<matrix-vect>
;; > (matrix-vect? Mv)
;; #t
;; > (find-getter-for-overloaded-square-brackets (list Mv 1 0))
;; #<procedure:matrix-vect-ref>
;; > {Mv[1 0]}
;; 4

;; >  (overload-square-brackets matrix-vect-ref matrix-vect-set!  (matrix-vect? number? number?))
;; > (define Mv (matrix-vect (vector (vector 1 2 3) (vector 4 5 6))))
;; > (matrix-vect-v Mv)
;; '#(#(1 2 3) #(4 5 6))
;; > {Mv[1 0] <- -7}
;; -7
;; > (matrix-vect-v Mv)
;; '#(#(1 2 3) #(-7 5 6))
;; > {Mv[1][0] <- 10}
;; . . ../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/overload.rkt:820:13: $bracket-apply$: no matching found in $ovrld-square-brackets-lst$ : failed with those arguments list (#<matrix-vect> 1)
;; > (overload-square-brackets matrix-vect-line-ref matrix-vect-line-set! (matrix-vect? number?))
;; > {Mv[1][0] <- 10}
;; 10
;; > (matrix-vect-v Mv)
;; '#(#(1 2 3) #(10 5 6))

(define-syntax overload-square-brackets

  (syntax-rules ()

    ;; with the list of the return values of the getter , for the type annotation pass (ignored here) :
    ;; (overload-square-brackets (flonum?) matrix-vect-ref matrix-vect-set! (matrix-vect? number? number?))
    ((_ (ret-val ...) getter setter (pred-arg pred-arg1 ...))

	(overload-square-brackets getter setter (pred-arg pred-arg1 ...)))

    ((_ getter setter (pred-arg pred-arg1 ...))   ;; getter setter and list of predicate to check the arguments

	(modify-$ovrld-square-brackets-lst$ (list (list pred-arg pred-arg1 ...)
						  (cons getter setter))))))

;; information for the type annotation pass only (see require-overload-procedure) : restates, in a module
;; importing an object whose square brackets are overloaded in another module, this overload.
;; Same form as overload-square-brackets. A no-op at runtime.
;; example : (require-overload-square-brackets matrix-vect-ref matrix-vect-set! (matrix-vect? number? number?))
(define-syntax require-overload-square-brackets

  (syntax-rules ()

    ((_ . args) (void))))

;; avoid: set!: cannot mutate module-required identifier in: $ovrld-square-brackets-lst$
(define (modify-$ovrld-square-brackets-lst$ arg)
  (set! $ovrld-square-brackets-lst$ (cons arg $ovrld-square-brackets-lst$)))

;; example, return : '(#<procedure:vector-ref> . #<procedure:vector-set!>)
(define (find-getter-and-setter-for-overloaded-square-brackets args-lst) 

  	 
	 (define (check-args-lst pred-list) ; check arguments list match predicates
	   (check-arguments pred-list args-lst))
	 
	 (define (test-proc pred-proc-list) ; test the procedure if it matches with arguments
	   (if (check-args-lst (car pred-proc-list))
	       (car (cdr  pred-proc-list))
	       #f))
	 
	 (define proc-search-result (ormap test-proc $ovrld-square-brackets-lst$ )) ; search for a procedure matching arguments
	 
	 (if proc-search-result
	     proc-search-result
	     (error '$bracket-apply$ "failed with those arguments list : ~a" args-lst)))
       

;; > (find-getter-for-overloaded-square-brackets '(#(1 2 3) 1))
;; #<procedure:vector-ref>
(define (find-getter-for-overloaded-square-brackets args-lst) 

  
  (define (check-args-lst pred-list) ; check arguments list match predicates
    ;;(display "check-args-lst : pred-list =") (display pred-list) (newline)
    (check-arguments pred-list args-lst))
	 
  (define (test-proc pred-proc-list) ; test the procedure if it matches with arguments
    ;;(display "test-proc : pred-proc-list =") (display pred-proc-list) (newline)
    (if (check-args-lst (car pred-proc-list))
	(car (cdr  pred-proc-list))
	#f))
  
  (define proc-search-result (ormap test-proc $ovrld-square-brackets-lst$ )) ; search for a procedure matching arguments

  ;;(display "find-getter-for-overloaded-square-brackets : args-lst = ") (display args-lst) (newline) (newline)
  
  (if proc-search-result
      (car proc-search-result)
	     (error '$bracket-apply$ "no matching found in $ovrld-square-brackets-lst$ : failed with those arguments list ~a" args-lst)))

(define (find-setter-for-overloaded-square-brackets args-lst) 
	 
	 (define (check-args-lst pred-list) ; check arguments list match predicates
	   (check-arguments pred-list args-lst))
	 
	 (define (test-proc pred-proc-list) ; test the procedure if it matches with arguments
	   (if (check-args-lst (car pred-proc-list))
	       (car (cdr  pred-proc-list))
	       #f))
	 
	 (define proc-search-result (ormap test-proc $ovrld-square-brackets-lst$ )) ; search for a procedure matching arguments
	 
	 (if proc-search-result
	     (cdr proc-search-result)
	     (error '$bracket-apply$ "no matching found in $ovrld-square-brackets-lst$ : failed with those arguments list ~a" args-lst)))
       
) ; end module

;; other example 

;; ; first stage overloading
;; (define-overload-existing-operator +)
;; (define-overload-existing-operator *)

;; ; second stage overloading
;; (overload-existing-operator + vector-append (vector? vector?))
;; (overload-existing-operator * multiply-matrix-vector (matrix-vect? vector?))

;; ;; overload [ ] 
;; (overload-square-brackets matrix-vect-ref matrix-vect-set!  (matrix-vect? number? number?))
;; (overload-square-brackets matrix-vect-line-ref matrix-vect-line-set! (matrix-vect? number?))
