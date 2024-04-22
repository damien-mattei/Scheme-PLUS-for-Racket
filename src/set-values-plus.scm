;;; Copyright © 2022 Maxime Devos
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;; modified by Damien Mattei

;; this version can set values for arrays,hash tables,etc
;; it uses the Scheme+ assignment operator: <-
(define-syntax set!-values-plus
  (syntax-rules ()
    ((_ (var var* ...) exp)
     (call-with-values
       (lambda () exp)
       (lambda (value . rest)
         (<- var value) ;; instead of set! i use the Scheme+ assignment operator
         (set!-values-plus (var* ...) (apply values rest)))))
    ((_ () exp)
     (call-with-values
       (lambda () exp)
       (lambda () (values)))))) ; nothing to do!

;; (define x)
;; (define y)
;; (define z)
;; (set!-values (x y z) (values 0 1 2))
;; (pk x y z)


;; this define a variable or if it already exists set it to '()
(define-syntax define-or-clear-values
  (syntax-rules ()
    ((_ var ...) (begin
		    (<- var '()) ; in racket <- can possibly define the value (if not already defined) 
		    ...))))

;; define or/and set! values
(define-syntax define-or/and-set!-values
  (syntax-rules ()
    ((_ (var ...) expr)
     (begin
       (define-or-clear-values var ...)
       (set!-values-plus (var ...) expr)))))


;; examples:

;; {(a b c d e) <- (values 1 2 3 4 5)}
;; id=.#<syntax a>
;; if-defined : where=#f

;; id=.#<syntax b>
;; if-defined : where=#f

;; id=.#<syntax c>
;; if-defined : where=#f

;; id=.#<syntax d>
;; if-defined : where=#f

;; id=.#<syntax e>
;; if-defined : where=#f

;; id=.#<syntax a>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> a #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> a 0 0 0)

;; id=.#<syntax b>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> b #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> b 0 0 0)

;; id=.#<syntax c>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> c #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> c 0 0 0)

;; id=.#<syntax d>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> d #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> d 0 0 0)

;; id=.#<syntax e>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> e #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> e 0 0 0)

;; > (list a b c d e)
;; '(1 2 3 4 5)



;; (define T (make-vector 5))
;; {(a T[3] c d e) <- (values 1 -2 3 4 5)}
;; id=.#<syntax a>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> a #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> a 0 0 0)

;; id=.#<syntax c>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> c #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> c 0 0 0)

;; id=.#<syntax d>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> d #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> d 0 0 0)

;; id=.#<syntax e>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> e #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> e 0 0 0)

;; id=.#<syntax a>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> a #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> a 0 0 0)

;; id=.#<syntax c>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> c #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> c 0 0 0)

;; id=.#<syntax d>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> d #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> d 0 0 0)

;; id=.#<syntax e>
;; if-defined : where=(#<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> e #<module-path-index="/Users/mattei/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/REPL-Scheme-PLUS.rkt"> e 0 0 0)

;; > {list(a T[3] c d e)}
;; '(1 -2 3 4 5)
;; > T
;; '#(0 0 0 -2 0)
;; > 



;; > (return-values (values 1 2 3))
;; 1
;; 2
;; 3
;; > (return-values (values 1))
;; 1
;; > (return-values 1)
;; 1
;; > (return-values (sin 0.7))
;; 0.644217687237691
(define-syntax return-values
    (syntax-rules ()
      ((_ expr) (call-with-values (lambda () expr)
                                  values))))

;; > ((create-return-values 3))
;; 3
;; > ((create-return-values (values 1 2 3)))
;; 1
;; 2
;; 3
(define-syntax create-return-values
    (syntax-rules ()
      ((_ expr) (lambda () (return-values expr)))))
