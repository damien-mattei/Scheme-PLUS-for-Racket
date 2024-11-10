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
	   member-syntax
	   ;;member-normal&syntax
	   )


  (require (only-in srfi/1 any member))
  

  ;; racket does not allow to syntax->datum an object not being (syntax something)
  (define (datum=? obj1 obj2)

    (cond ((and (syntax? obj1) (syntax? obj2))
	   (eq? (syntax->datum obj1)
		(syntax->datum obj2)))
	  ((and (syntax? obj1) (not (syntax? obj2)))
	   (eq? (syntax->datum obj1)
		obj2))
	  ((and (not (syntax? obj1)) (syntax? obj2))
	   (eq? obj1
		(syntax->datum obj2)))
	  (else
	   (eq? obj1 obj2))))
	  

;; Welcome to DrRacket, version 8.13 [cs].
;; Language: r6rs, with debugging; memory limit: 8192 MB.
;; > (define op-lst (list #'* #'+ #'- #'/))
;; > (member-syntax #'+ op-lst)
;; #t



;; (member-syntax '+ '(- + / *))
;; #t
;; (member-syntax + (list - + / *))
;; #t
;; (member-syntax + (list - / *))
;; #f
;; (member-syntax '+ '(- / *))
  ;; #f

 ;; > (member-syntax #'+ (list - + / *))
;;#f

  (define (member-syntax x lst)
    (any (lambda (y)
	   (datum=? x y))
	 lst))


  ;; (define (member-normal&syntax x lst)
  ;; (or (member x lst)
  ;;     (member-syntax x lst)))



) ; end library


