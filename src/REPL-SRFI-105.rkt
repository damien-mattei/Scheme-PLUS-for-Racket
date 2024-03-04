#lang racket

;; Copyright (C) 2012 David A. Wheeler and Alan Manuel K. Gloria. All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; modification for Racket by Damien Mattei

;; use with: #lang reader "SRFI-105.rkt"

;; example in DrRacket :
;; #lang reader "Dropbox/git/Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/SRFI/SRFI-105.rkt"

(require syntax/strip-context) ;; is this useful?

(require srfi/31) ;; for 'rec in def.scm



(include "operation-redux.scm")
(include "optimize-infix.scm")
(include "assignment-light.scm")
(include "block.scm")
(include "declare.scm")
(include "slice.scm")
(include "def.scm")
(include "optimize-infix-slice.scm")

(include "SRFI-105.scm")


(set! nfx-optim #f)
(set! slice-optim #f)




;; the current read interaction handler, which is procedure that takes an arbitrary value and an input port 
(define (literal-read-syntax-for-repl src in)

  (define result (curly-infix-read in))
  
  (if (eof-object? result)
      ;;(begin (display "eof") (newline) result)
      result
      (datum->syntax #f result))) ;; current-eval wait for a syntax object to pass to eval-syntax for evaluation
      

 


;; Welcome to DrRacket, version 8.2 [cs].
;; Language: reader "SRFI-105.rkt", with debugging; memory limit: 128 MB.
;; > (define x 3)
;; > {x + 1}
;; 4
(current-read-interaction literal-read-syntax-for-repl) ;; this procedure will be used by Racket REPL:
 ;; the current read interaction handler, which is procedure that takes an arbitrary value and an input port 

