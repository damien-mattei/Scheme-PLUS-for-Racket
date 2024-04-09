#lang racket

;; Copyright (C) 2012 David A. Wheeler and Alan Manuel K. Gloria. All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;; modification for Racket by Damien Mattei

;; use with: racket curly-infix2prefix4racket.scm [options] file2parse.scm > parsedfile.scm

;; example in DrRacket :
;; /Applications/Racket\ v8.12/bin/racket curly-infix2prefix4racket.rkt  ../../../../AI_Deep_Learning/exo_retropropagationNhidden_layers_matrix_v2_by_vectors+.rkt > ../../../../AI_Deep_Learning/exo_retropropagationNhidden_layers_matrix_v2_by_vectors.rkt

;; options:

;; --srfi-105 : set strict compatibility mode with SRFI-105


;;(require syntax/strip-context) ;; is this useful?

(require racket/pretty) ;; pretty print

;;(require racket/cmdline) ;; (command-line) does not work like in other scheme implementations



(require srfi/31) ;; for 'rec in def.scm


(include "operation-redux.scm")
(include "optimize-infix.scm")
(include "assignment-light.scm")
(include "block.scm")
(include "declare.scm")
(include "slice.scm")
(include "def.scm")
(include "optimize-infix-slice.scm")

(include "when-unless.rkt")
(include "while-do.scm")

(define stderr (current-error-port))

(include "condx.scm")

(include "SRFI-105.scm")

(define srfi-105 #f)

(define flag-r6rs #f)

(define (skip-comments-and-empty-lines in)
 
  (do
      while (or (regexp-try-match #px"^[[:space:]]" in)  ; skip space,tab,new line,...
		(regexp-try-match #px"^;[^\n]*\n" in)))  ; and also comments
  )
   

(define (literal-read-syntax src)

  (define in (open-input-file src))
  (define lst-code (process-input-code-tail-rec in))
  ;; (if lang-reader
  ;;     `(module aschemeplusprogram racket ,@lst-code)
  lst-code)
 ;;)
;;(cons 'module (cons 'aschemeplusprogram (cons 'racket lst-code))))
;; (strip-context `(module aschemeplusprogram racket ,@lst-code))) ;; is this useful?




;; read all the expression of program
;; DEPRECATED (replaced by tail recursive version)
;; (define (process-input-code-rec in)
;;   (define result (curly-infix-read in))  ;; read an expression
;;   (if (eof-object? result)
;;       '()
;;       (cons result (process-input-code-rec in))))


;; read all the expression of program
;; a tail recursive version
(define (process-input-code-tail-rec in) ;; in: port


  (display "SRFI-105 Curly Infix parser with operator precedence by Damien MATTEI" stderr) (newline stderr)
  (display "(based on code from David A. Wheeler and Alan Manuel K. Gloria.)" stderr) (newline stderr) (newline stderr)
  

  (when srfi-105
	(display "Options :" stderr) (newline stderr) (newline stderr)
	(display "SRFI-105 strict compatibility mode is ON." stderr))
  (newline stderr)

  (newline stderr)

  
  ;; internal define
  (define (process-input acc)
    
    (define result (curly-infix-read in))  ;; read an expression

    (unless (eof-object? result)
	    (pretty-print result
	    		  stderr
	    		  1) ;; quote-depth : remove global quote of
	    ;;(write result stderr) ;; without 'write' but 'display' string delimiters disappears !
	    ;;(newline stderr)
	    (newline stderr))
    
    (if (eof-object? result)
	(reverse acc)
	(process-input (cons result acc))))
  ;; end internal define

  
  ;; check the #lang ... first line
  (define header-reader-length (string-length "#lang reader"))
  (define frst-line-beginning (peek-string header-reader-length 0 in))
  ;;(display "frst-line-beginning = " stderr) (display frst-line-beginning stderr) (newline stderr)
  (define rv '())
  
  (when (string=? "#lang reader"
		frst-line-beginning)
  
	(display "#lang reader detected " stderr)
	(newline stderr)(newline stderr)
	(consume-to-eol in) ; skip the first line #lang reader ... SRFI-105.rkt
	;;(set! lang-reader #t)
	)

  (port-count-lines! in) ; turn on counting on port
  
  (display "Possibly skipping some header's lines containing space,tabs,new line,etc  or comments." stderr) (newline stderr) (newline stderr)
  (skip-comments-and-empty-lines in)

  (when (regexp-try-match #px"^#!r6rs[[:blank:]]*\n" in)
	(set! flag-r6rs #t)
	(display "Detected R6RS code. (#!r6rs)") (newline) (newline))

  (declare lc cc pc)
  (set!-values (lc cc pc) (port-next-location in))
  (display "SRFI-105.rkt : number of skipped lines (comments, spaces, directives,...) at header's beginning : " stderr)
  (display lc stderr)
  (newline stderr)
  (newline stderr)
  
  (display "Parsed curly infix code result = " stderr) (newline stderr) (newline stderr)

  (when flag-r6rs
	(display "#!r6rs" stderr) (newline stderr)
	(newline stderr))
  
  ;;(display "(module aschemeplusprogram racket " stderr)
  ;;(newline stderr)
  (set! rv (process-input '()))
  ;;(display ")" stderr)
	
     
  (newline stderr)

  rv)



; parse the input file from command line
(define cmd-ln-vect (current-command-line-arguments))

;;(display "cmd-ln-vect=") (display cmd-ln-vect) (newline)

(define cmd-ln (vector->list cmd-ln-vect))

(define options cmd-ln)
;;(display "options= ") (display options) (newline)

(when (member "--help" options)
      (display "curly-infix2prefix4racket.scm documentation: (see comments in source file for more examples)") (newline) (newline) 
      (display "racket curly-infix2prefix4racket.scm [options] file2parse.scm") (newline) (newline)
      (display "options:") (newline)(newline)
      (display "  --srfi-105 : set strict compatibility mode with SRFI-105 ") (newline) (newline)
      (exit))


;; SRFI-105 strict compatibility option
(when (member "--srfi-105" options)
      (set! nfx-optim #f)
      (set! slice-optim #f))


;; TODO quiet mode that do not display on standart error the code

(define file-name (car (reverse cmd-ln)))

(when (string=? (substring file-name 0 2) "--")
      (error "filename start with -- ,this is confusing with options."))

(define code-lst (literal-read-syntax file-name))


(define (wrt-expr expr)
  (write expr) ;; without 'write' string delimiters disappears !
  (newline)
  (newline))


;;(for-each wrt-expr code-lst)
;;(wrt-expr code-lst)

;; (if lang-reader
;;     (pretty-print code-lst
;; 		  (current-output-port)
;; 		  1) ;; quote-depth : remove global quote of expression


(when flag-r6rs
      (display "#!r6rs") (newline)
      (newline))

(for-each (lambda (expr) (pretty-print expr
				       (current-output-port)
				       1)) ;; quote-depth : remove global quote of
	  code-lst)

;;)
