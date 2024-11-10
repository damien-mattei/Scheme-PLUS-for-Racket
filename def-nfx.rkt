;; This file is part of Scheme+

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





(module def-nfx racket/base


	(provide define)

	(require (only-in racket/base [define define-scheme]) ;; backup original Scheme 'define'
		 Scheme+/nfx)


	;;> (define x  3 * 5 + 2)


	;; (define x  3 * 5 + 2)
	;; $nfx$: #'(e1 op1 e2 op2 e3 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:63:76 (3 * 5 + 2)>
	;; $nfx$: (syntax->list #'(e1 op1 e2 op2 e3 op ...))=(.#<syntax 3> .#<syntax *> .#<syntax 5> .#<syntax +> .#<syntax 2>)
	;; $nfx$ : parsed-args=.#<syntax (+ (* 3 5) 2)>


	;; #<eof>
	;; > x


	;; x
	;; 17

	
	;; > (define x  3 * (5 - 3) + 1)
	;; (define x 3 * (5 - 3) + 1)
	;; $nfx$: #'(e1 op1 e2 op ...)=.#<syntax:Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/nfx.rkt:66:69 (3 * (5 - 3) + 1)>
	;; $nfx$: (syntax->list #'(e1 op1 e2 op ...))=(.#<syntax 3> .#<syntax *> .#<syntax (5 - 3)> .#<syntax +> .#<syntax 1>)
	;; $nfx$ : parsed-args=.#<syntax (+ (* 3 (- 5 3)) 1)>
	;; #<eof>
	;; > x
	;; x
	;; 7


	(define-syntax define

	  (syntax-rules ()

	    ;; original define
	    ((define (name arg ...) body ...)
	     (define-scheme (name arg ...) body ...))

	    ((define (name arg ... . rest-id) body ...)
	       (define-scheme (name arg ... . rest-id) body ...))

	    ((define name expr)
	     (define-scheme name expr))

	    ;; infix define
	    ((define name arg1 arg2 arg3 ...)
	     (define-scheme name ($nfx$ arg1 arg2 arg3 ...)))))


	;; (define-syntax define

	;;   (lambda (stx)
	    
	;;     (syntax-case stx ()

	;;       ;; original define
	;;       ((define (name arg ...) body ...)
	;;        #'(define-scheme (name arg ...) body ...))

	;;       ((define (name arg ... . rest-id) body ...)
	;;        #'(define-scheme (name arg ... . rest-id) body ...))

	;;       ((define name expr)
	;;        #'(define-scheme name expr))

	;;       ;; infix define
	;;       ((define name arg1 arg2 arg3 ...)
	;;        #'(define-scheme name ($nfx$ arg1 arg2 arg3 ...))))))



	)
  
