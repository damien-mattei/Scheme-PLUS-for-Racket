#lang reader "../SRFI/SRFI-105.rkt"

;; assignment version Racket

;; This file is part of Scheme+

;; Copyright 2022 Damien MATTEI

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

(require srfi/69) ;; Basic hash tables
(require srfi/25) ;; Multi-dimensional Array Primitives
;;(require math/array) ;; warning: construction primitive is different than SRFI 47 and Guile
;; accessors have same syntax for arrays


(provide <- ← -> →)

(include "../included-files/array.scm")

;; scheme@(guile-user)> {a[2 4] <- 7}
;; $1 = 7

;; scheme@(guile-user)> {a[2 4]}
;; $1 = 999
;; scheme@(guile-user)> {a[2 4] <- 7}
;; $2 = 7
;; scheme@(guile-user)> {a[2 4]}
;; $3 = 7
;; scheme@(guile-user)> {1 -> a[2 4]}
;; $4 = 1
;; scheme@(guile-user)> {a[2 4]}
;; $5 = 1
;; {x <- 2}
;;
;; (define T (make-vector 5))
;; scheme@(guile-user)> {T[3] <- 7}
;; <- : vector or array set!
;; $1 = 7
;;
;; scheme@(guile-user)> {T[3]}
;; $bracket-apply$
;; $3 = 7
;;
;; scheme@(guile-user)> {T[2] <- 4}
;; <- : vector or array set!
;; $2 = 4

;; scheme@(guile-user)> {T[3] <- T[2]}
;; $bracket-apply$
;; <- : vector or array set!
;; $4 = 4
;; scheme@(guile-user)> {T[3]}
;; $bracket-apply$
;; $5 = 4

;; TODO : make this works:
;; scheme@(guile-user)> '{x <- y <- 7}
;; $1 = (<- x y 7)
(define-syntax <-
  
  (syntax-rules ()
    ;;  special form like : (<- ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    
    ;; one dimension array, example: {a[4] <- 7}
    ;; $bracket-apply$ of SRFI 105
    ((_ ($bracket-apply$ container index) expr)
     (let ((value expr)) ;; to avoid compute it twice
						 
       ;; (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
       
       ;; normal case
       ;; {T[2] <- 4}
       ;; {T[3] <- T[2]}
       ;;(begin
       ;;(display "<- : vector or array set! or hash-table set!") (newline)
       (cond ((vector? container) (vector-set! container index value))
	     ((hash-table? container) (hash-table-set! container index value))
	     (else (array-set! container index value)));)
       
       ;; rare case  (to prevent any error)
       ;; (let ((var (funct-or-macro container index))) ;; MUST be in a variable , otherwise:
       ;; While compiling expression:
       ;;  Syntax error:
       ;;  unknown location: quote: bad syntax in form quote
       ;; 	<- : variable set! after creation
       ;;  (set! var value)))
       
       value))


    ;; multi dimensions array :  {a[2 4] <- 7}
    ;; $bracket-apply$ of SRFI 105
    ((_ ($bracket-apply$ array index1 index2 ...) expr)
     (let ((value expr)) ;; to avoid compute it twice
  						 
       ;; (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
       ;; normal case
       ;;(begin
       ;;(display "<- : multidimensional vector or array set!") (newline)
       (if (vector? array)
	   (array-n-dim-set! array value index1 index2 ...)
	   (array-set! array index1 index2 ... value));)
						     
	 ;; rare case (to prevent any error)
	 ;; (let ((var (funct-or-macro array index ...))) ;; MUST be in a variable
	 ;;   (display "<- : variable set! after creation (multidimensional)") (newline)
	 ;;   (set! var value)))
	 value))

    ;; not sure this case will be usefull
    ((_ (funct-or-macro arg ...) expr)
     (let ((var (funct-or-macro arg ...))
	   (value expr)) ;; to avoid compute it twice
       (set! var value)
       var))

    
    ;;(<- x 5)
    ((_ var expr)
     
     (begin
       ;;(display "<- : variable set!") (newline)
       (set! var expr)
       var))

    
    ;; (declare x y z t)
    ;; {x <- y <- z <- t <- 7}
    ;; 7
    ;; (list x y z t)
    ;; (7 7 7 7)

    ;; (declare I)
    ;; {I <- (make-array 0 4 4)}
    ;; #2((0 0 0 0)
    ;;    (0 0 0 0)
    ;;    (0 0 0 0)
    ;;    (0 0 0 0))
    ;;
    ;; {I[0 0] <- I[1 1] <- I[2 2] <- I[3 3] <- 1}
    ;; 1
    ;; 
    ;; I
    ;; #2((1 0 0 0)
    ;;    (0 1 0 0)
    ;;    (0 0 1 0)
    ;;    (0 0 0 1))

    ((_ var var1 var2 ...)
     (<- var (<- var1 var2 ...)))
     
    ))







(define-syntax ->
  (syntax-rules ()
    ;;  special form like : (-> ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    ;; changé en (<- expr (funct-or-macro container index))
    ;;((_ expr (funct-or-macro container index)) {container[index] <- expr}  )
    ;; ((_ expr (funct-or-macro container index)) (<- (funct-or-macro container index) expr))
    
    ;; ;;((_ expr (funct-or-macro array index ...)) {array[index ...] <- expr} )
    ;; ((_ expr (funct-or-macro array index ...)) (<- (funct-or-macro array index ...) expr))
    
    ;; (-> 5 x)
    ;; note: it is short and infix but seems to work in all case indeed!
    ((_ expr var) {var <- expr})


    ;; (declare I)
    ;; {I <- (make-array 0 4 4)}
    ;; #2((0 0 0 0)
    ;;    (0 0 0 0)
    ;;    (0 0 0 0)
    ;;    (0 0 0 0))
    ;; {1 -> I[0 0] -> I[1 1] -> I[2 2] -> I[3 3]}
    ;; 1
    ;;
    ;; I
    ;; #2((1 0 0 0)
    ;;    (0 1 0 0)
    ;;    (0 0 1 0)
    ;;    (0 0 0 1))

    ((_ expr var var1 ...)
     (-> (-> expr var) var1 ...))

    ))





(define-syntax → ;; under Linux this symbol can be typed with the
  ;; combination of keys: Ctrl-Shift-u 2192 where 2192 is the unicode of right arrow

  (syntax-rules () 

    ;; note: it is short and infix but seems to work in all case indeed!
    ((_ expr var) {var <- expr})

    ((_ expr var var1 ...)
     (-> (-> expr var) var1 ...))
    
    ))


;; Mac OS use CTRL+CMD+space to bring up the characters popover, then type in u + unicode and hit Enter to get it)

;; (declare I)
;; {I <- (make-array 0 2 2)}
;; #2((0 0)
;;    (0 0))
;;
;; {I[0 0] ← I[1 1] ← 1}
;; 1
;;
;; I
;; #2((1 0)
;;    (0 1))

(define-syntax ← ;; under Linux this symbol can be typed with the
  ;; combination of keys: Ctrl-Shift-u 2190 where 2190 is the unicode of left arrow

  (syntax-rules ()
   
    ;; note: it is short and infix but seems to work in all case indeed!
    ((_ var expr) {var <- expr})

    ((_ var var1 var2 ...)
     (<- var (<- var1 var2 ...)))

    ))
