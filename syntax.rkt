;; This file is part of Scheme+

;; Copyright 2024 Damien MATTEI

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


;; code from Scheme+R6RS

(module syntax racket/base
  
  (provide datum=?  
	   member-generic
	   procedure->string
	   procedure->symbol
	   var-syntax2list)


  (require (only-in srfi/1 any member))


  
;; (procedure->string +)
;; "+"

;; (procedure->string foo:3>)
;; "foo:3>"
  (define (procedure->string p)
    (define o (open-output-string))
    (write p o)
    (define str (get-output-string o))
    (substring str
	       (string-length "#<procedure:")
	       (- (string-length str) 1)))


  (define (procedure->symbol p)
    (string->symbol (procedure->string p)))



  ;; (datum=? #'* *)
  ;; #t

  ;; (define m *)
  ;; (define sm #'*)
  ;; (datum=? m sm)
  ;; #t


  ;; racket does not allow to syntax->datum an object not being (syntax something)
  (define (datum=? obj1 obj2)
       ;;(display "datum=? : obj1 =") (display obj1) (newline)
       ;;(display "datum=? : obj2 =") (display obj2) (newline)

       (when (syntax? obj1)
	 (set! obj1 (syntax->datum obj1)))

       (when (syntax? obj2)
	 (set! obj2 (syntax->datum obj2)))

       (when (procedure? obj1)
	 (set! obj1 (procedure->symbol obj1)))

       (when (procedure? obj2)
	 (set! obj2 (procedure->symbol obj2)))

       (define rv (equal? obj1 obj2))
       ;;(display "datum=? rv =") (display rv) (newline)
       rv)

;; Welcome to DrRacket, version 8.13 [cs].
;; Language: r6rs, with debugging; memory limit: 8192 MB.
;; > (define op-lst (list #'* #'+ #'- #'/))
;; > (member-generic #'+ op-lst)
;; #t



;; (member-generic '+ '(- + / *))
;; #t
;; (member-generic + (list - + / *))
;; #t
;; (member-generic + (list - / *))
;; #f
;; (member-generic '+ '(- / *))
;; #f

;; > (member-generic #'+ (list - + / *))
;;#f

  (define (member-generic x lst)
    ;;(display "member-generic : x=") (display x) (newline)
    ;;(display "member-generic : lst=") (display lst) (newline)
    (any (lambda (y)
	   (datum=? x y))
	 lst))


  
  ;; transform any syntax in a list if possible
  ;; note:this macro made a side effect on the input variable, modifying it
  ;; note: why not use syntax->datum ???
  (define-syntax var-syntax2list
    (syntax-rules ()
      ((_ var)
       
       (let ((var-inter #f)) ; intermediate variable

	 ;;(display "var-syntax2list : var=") (display var) (newline)
	 (when (syntax? var)
	       ;; (display "var-syntax2list : detected syntax,passing from syntax to list (will be used if it is a list)")
	       ;; (display " --> var-syntax2list : var=") (display var) (newline)
	       (set! var-inter (syntax->list var))
	       (when var-inter
		     ;;(display "var-syntax2list : got a list") (display " --> var-syntax2list : var=") (display var)(newline)(newline)
		     (set! var var-inter)))))))
      

) ; end library


