#lang reader "../src/SRFI-105.rkt"


;; Sub Set Sum problem
;; Dynamic solution
;; Racket version

;; Copyright 2021-2024 Damien MATTEI

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


;; if necessary some files can be retrieved here: https://github.com/damien-mattei/library-FunctProg


(module sssrec racket

;;(require "../Scheme+.rkt")
(require "../main.rkt")

{L-init <+ '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027)}
{t-init <+ 35267}

{cpt <+ 0}


(define (ssigma L t)

  {cpt <- cpt + 1}
  ;;(display L) (display " ") (display t) (newline)
  
  (if (null? L)
      
      (begin
	;; (display "null L")
	;; (newline)
	;; (newline)
	#f)
      
      ($+>
       {c <+ (first L)}
       {R <+ (rest L)}
       (cond [ {c = t} #t ] ;; c is the solution
	     [ {c > t} (ssigma R t) ] ;; c is to big to be a solution but can be an approximation
	     ;; c < t at this point
	     ;; c is part of the solution or his approximation
	     ;; or c is not part of solution or his approximation
	     [ else {(ssigma R {t - c}) or (ssigma R t)} ] ))))



) ; end module

