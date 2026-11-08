#lang racket/base

;; the bench of the source locations of the readers SRFI-105 and SRFI-110
;;
;; racket check.rkt dump 105|110 file > datum      the code given by read-syntax , as a datum
;; racket check.rkt report 105|110 file            the locations of this code , compared to the text of the file
;;
;; an identifier is exact when the text of the file at its location is its name ; inherited when its location is the
;; one of a form (generated code , for example fl+ that replaces +) ; wrong otherwise.
;; a form is exact when the text at its location begins as a form can begin.

(require racket/port racket/string racket/list)

(define args (current-command-line-arguments))
(define mode (vector-ref args 0))
(define reader (vector-ref args 1))
(define file (path->complete-path (vector-ref args 2)))
(define text (call-with-input-file file port->string))

(define (read-program)
  (define in (open-input-file file))
  (port-count-lines! in)
  (when (regexp-match? #px"^#!\\s*/" text) (read-line in))
  (define stx
    (parameterize ((current-output-port (open-output-nowhere))
		   (read-accept-reader #t)
		   (read-accept-lang #t)
		   (current-load-relative-directory (let-values (((base name dir?) (split-path file))) base)))
      (if (regexp-match? #px"^(#!\\s*/[^\n]*\n)?(\\s|;[^\n]*\n)*#(reader|lang)" text)
	  (read-syntax file in)
	  ((dynamic-require (string->symbol (string-append "SRFI-" reader)) 'read-syntax) file in))))
  (close-input-port in)
  stx)

(define exact-ids 0) (define inherited-ids 0) (define wrong-ids 0) (define no-loc-ids 0)
(define exact-forms 0) (define approximate-forms 0) (define wrong-forms 0) (define no-loc-forms 0)
(define wrong-examples '())
(define shared-ids 0) (define seen-positions (make-hasheqv))

(define (at-location stx)
  (define pos (syntax-position stx))
  (define span (syntax-span stx))
  (and pos span (> span 0) (<= (+ pos -1 span) (string-length text))
       (substring text (- pos 1) (+ pos -1 span))))

(define (remember! kind stx found)
  (when (< (length wrong-examples) 12)
    (set! wrong-examples (cons (list kind (syntax->datum stx) (syntax-line stx) (syntax-column stx) found) wrong-examples))))

(define (form-text? s)
  (or (regexp-match? #px"^[\\(\\[\\{'`,#]" s) ; a list , a quote , a vector ...
      (regexp-match? #px"^[^\\s\\(\\)\\[\\]\\{\\}]+[\\(\\[\\{]" s))) ; neoteric : f(x) , x[i] , f{x}

(define (walk stx)
  (define e (syntax-e stx))
  (cond ((symbol? e)
	 (define found (at-location stx))
	 (cond ((and (not found) (real-location? stx)) (set! inherited-ids (+ 1 inherited-ids))) ; a span of 0 : generated
	       ((not found) (set! no-loc-ids (+ 1 no-loc-ids)))
	       ((string=? found (symbol->string e))
		(set! exact-ids (+ 1 exact-ids))
		(when (hash-ref seen-positions (syntax-position stx) #f) ; a copy , or a wrong occurrence
		  (set! shared-ids (+ 1 shared-ids))
		  (eprintf "  shared : ~s line ~a column ~a\n" e (syntax-line stx) (syntax-column stx)))
		(hash-set! seen-positions (syntax-position stx) #t))
	       ((form-text? found) (set! inherited-ids (+ 1 inherited-ids)))
	       (else (set! wrong-ids (+ 1 wrong-ids)) (remember! 'id stx found))))
	((or (pair? e) (null? e))
	 (when (pair? e)
	   (define found (at-location stx))
	   (cond ((not found) (set! no-loc-forms (+ 1 no-loc-forms)))
		 ((or (form-text? found) (operands-in-text? stx found)) (set! exact-forms (+ 1 exact-forms)))
		 ((shares-symbol? stx found) (set! approximate-forms (+ 1 approximate-forms))) ; a partial span
		 (else (set! wrong-forms (+ 1 wrong-forms)) (remember! 'form stx found))))
	 (let loop ((x e))
	   (cond ((pair? x) (walk (car x)) (loop (cdr x)))
		 ((syntax? x) (walk-tail x))
		 (else (void)))))
	(else (void))))

;; a form made by the infix parser , (* b c) at the location of b * c : the text has all its operands
(define (operands-in-text? stx found)
  (define l (syntax->list stx))
  (and l (pair? l) (pair? (cdr l))
       (for/and ((x (in-list (cdr l))) #:when (symbol? (syntax-e x)))
	 (string-contains? found (symbol->string (syntax-e x))))))

(define (shares-symbol? stx found)
  (let search ((x (syntax->datum stx)))
    (cond ((pair? x) (or (search (car x)) (search (cdr x))))
	  ((symbol? x) (string-contains? found (symbol->string x)))
	  (else #f))))

(define (real-location? stx)
  (and (syntax-position stx)
       (not (and (eqv? (syntax-line stx) 1) (eqv? (syntax-column stx) 0) (eqv? (syntax-position stx) 1)))))

;; the tail of a list can be a syntax object : it is the rest of the same list
(define (walk-tail stx)
  (define e (syntax-e stx))
  (if (or (pair? e) (null? e))
      (let loop ((x e))
	(cond ((pair? x) (walk (car x)) (loop (cdr x)))
	      ((syntax? x) (walk-tail x))
	      (else (void))))
      (walk stx)))

(define stx (read-program))
(cond ((string=? mode "dump")
       (write (syntax->datum stx))
       (newline))
      (else
       (walk stx)
       (printf "ids exact ~a inherited ~a wrong ~a none ~a | forms exact ~a approximate ~a wrong ~a none ~a\n"
	       exact-ids inherited-ids wrong-ids no-loc-ids exact-forms approximate-forms wrong-forms no-loc-forms)
       (eprintf "shared ~a\n" shared-ids)
       (for ((w (reverse wrong-examples))) (eprintf "  ~s\n" w))))
