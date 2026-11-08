;; This file is part of Scheme+

;; Copyright 2026 Damien MATTEI

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


;; The locations in the source file of the code read by the readers SRFI-105 and SRFI-110 (Racket only).
;;
;; The readers and the passes that follow them (infix to prefix , quotes , types , type annotation) work on plain lists
;; and symbols , not on syntax objects. The locations are kept beside the code , in tables indexed by the pairs :
;; - the location of a form : the pair that begins it ;
;; - the location of an atom : the pair that holds it in its car (a symbol is interned , only its pair identifies it).
;; A pass that builds new pairs links them to the pairs they come from ; at the end the code becomes syntax objects
;; with these locations. Without location a form inherits the location of its parent , and an atom too but with a span of
;; 0. What comes from the text of the source file is marked as original (syntax-original?) : the tools (Check Syntax , the
;; language servers , the debuggers) look only at it. The generated code is not marked : it is not underlined as unused.

(module source-locations racket/base

  (require (only-in syntax/readerr raise-read-error))

  (provide reset-source-locations!
	   enter-form!
	   leave-form!
	   call-with-located-errors
	   location-start
	   location-from
	   record-form!
	   record-element!
	   record-datum!
	   reset-read-atoms!
	   record-atom!
	   relink-read-atoms!
	   link-form!
	   link-cell!
	   relink-symbols!
	   relink-form!
	   datum->located-syntax)

  (define form-locations (make-weak-hasheq))    ; pair -> #(line column position span) of the form it begins
  (define element-locations (make-weak-hasheq)) ; pair -> #(line column position span) of the atom in its car
  (define linked-sources (make-weak-hasheq))    ; the pairs already linked to the forms made from them

  ;; called at the beginning of each file
  (define (reset-source-locations! (src #f))
    (set! current-source src)
    (set! current-form-location #f)
    (hash-clear! form-locations)
    (hash-clear! element-locations)
    (hash-clear! linked-sources))


  ;; ------------- at reading -------------

  ;; the current location of the port : #f if the port does not count the lines
  (define (location-start port)
    (let-values (((line column position) (port-next-location port)))
      (and line column position
	   (vector line column position))))

  ;; the location of what has been read on the port since start
  (define (location-from start port)
    (and start
	 (let-values (((line column position) (port-next-location port)))
	   (vector (vector-ref start 0)
		   (vector-ref start 1)
		   (vector-ref start 2)
		   (- position (vector-ref start 2))))))

  ;; the first location recorded is the good one : the inner reading skips the comments that precede the form
  (define (record-form! form location)
    (when (and location
	       (pair? form)
	       (not (hash-ref form-locations form #f)))
      (hash-set! form-locations form location)))

  (define (record-element! cell location)
    (when (and location (pair? cell))
      (hash-set! element-locations cell location)))

  ;; cell holds datum in its car : a form or an atom. Returns cell.
  (define (record-datum! cell datum location)
    (if (pair? datum)
	(record-form! datum location)
	(record-element! cell location))
    cell)


  ;; the reader SRFI-110 , whose indented expressions are built far from the reading of their atoms : the symbols read
  ;; for one expression , in order , then given to the pairs of the expression
  (define read-atoms (make-hasheq)) ; symbol -> its locations , the last read first

  (define (reset-read-atoms!)
    (hash-clear! read-atoms))

  (define (record-atom! a location)
    (when (and location (symbol? a) (> (vector-ref location 3) 0))
      (let ((known (hash-ref read-atoms a '())))
	(unless (and (pair? known) ; the same atom given twice by nested readers : the first one , the innermost , is exact
		     (= (vector-ref (car known) 2) (vector-ref location 2)))
	  (hash-set! read-atoms a (cons location known))))))

  ;; each symbol of expression takes the location of the next occurrence of its name. Returns expression.
  (define (relink-read-atoms! expression)
    (when (pair? expression)
      (for ((k (in-list (hash-keys read-atoms))))
	(hash-set! read-atoms k (reverse (hash-ref read-atoms k))))
      (let ((visited (make-hasheq)))
	(let walk ((x expression))
	  (when (and (pair? x) (not (hash-ref visited x #f)))
	    (hash-set! visited x #t)
	    (let ((a (car x)))
	      (if (symbol? a)
		  (let ((free (hash-ref read-atoms a '())))
		    (when (pair? free)
		      (unless (hash-ref element-locations x #f)
			(hash-set! element-locations x (car free)))
		      (hash-set! read-atoms a (cdr free))))
		  (walk a)))
	    (walk (cdr x))))))
    expression)


  ;; ------------- in the passes -------------

  ;; new is the form made from old. Returns new.
  (define (link-form! new old)
    (when (and (pair? new) (pair? old) (not (eq? new old)))
      (hash-set! linked-sources old #t)
      (let ((location (hash-ref form-locations old #f)))
	(when (and location (not (hash-ref form-locations new #f)))
	  (hash-set! form-locations new location))))
    new)

  ;; new is a copy of the pair old , in a pass that copies the lists pair by pair. Returns new.
  (define (link-cell! new old)
    (when (and (pair? new) (pair? old) (not (eq? new old)))
      (let ((location (hash-ref form-locations old #f)))
	(when (and location (not (hash-ref form-locations new #f)))
	  (hash-set! form-locations new location)))
      (when (eq? (car new) (car old))
	(let ((location (hash-ref element-locations old #f)))
	  (when location
	    (hash-set! element-locations new location)))))
    new)

  ;; out is made from in by a rewriting that moves the symbols (infix to prefix , type annotation) : each symbol of the
  ;; new pairs of out takes the location of the next occurrence of the same name in in. The pairs of out that come from
  ;; in unchanged keep their locations , the forms already linked by an inner rewriting are not visited , so that an
  ;; occurrence is not taken by an other one of the same name. Returns out.
  (define (relink-symbols! out in)
    (when (and (pair? out) (pair? in) (not (eq? out in)))
      (let ((out-pairs (make-hasheq))
	    (in-pairs (make-hasheq))
	    (occurrences (make-hasheq))) ; symbol -> the locations in in , in order
	(let walk ((x out))
	  (when (and (pair? x) (not (hash-ref out-pairs x #f)))
	    (hash-set! out-pairs x #t)
	    (walk (car x))
	    (walk (cdr x))))
	(let walk ((x in))
	  (when (and (pair? x)
		     (not (hash-ref in-pairs x #f))
		     (not (hash-ref out-pairs x #f))
		     (or (eq? x in) (not (hash-ref linked-sources x #f))))
	    (hash-set! in-pairs x #t)
	    (let ((a (car x))
		  (location (hash-ref element-locations x #f)))
	      (when (and (symbol? a) location)
		(hash-set! occurrences a (cons location (hash-ref occurrences a '())))))
	    (walk (car x))
	    (walk (cdr x))))
	(for ((k (in-list (hash-keys occurrences))))
	  (hash-set! occurrences k (reverse (hash-ref occurrences k))))
	(let ((visited (make-hasheq)))
	  (let walk ((x out))
	    (when (and (pair? x)
		       (not (hash-ref visited x #f))
		       (not (hash-ref in-pairs x #f)))
	      (hash-set! visited x #t)
	      (let ((a (car x)))
		(cond ((symbol? a)
		       (unless (hash-ref element-locations x #f)
			 (let ((free (hash-ref occurrences a '())))
			   (when (pair? free)
			     (hash-set! element-locations x (car free))
			     (hash-set! occurrences a (cdr free))))))
		      ((and (pair? a) (not (hash-ref form-locations a #f)))
		       (walk a))))
	      (walk (cdr x)))))))
    out)

  ;; the form new is made from the form old by a rewriting. Returns new.
  (define (relink-form! new old)
    (link-form! new old)
    (relink-symbols! new old))


  ;; ------------- the errors of a pass -------------

  (define current-source #f) ; the source given by Racket to the reader
  (define current-form-location #f) ; the location of the innermost form with a location that the pass is processing

  ;; the pass enters the form s : returns what must be given to leave-form! when it has processed s
  (define (enter-form! s)
    (let ((saved current-form-location))
      (when (pair? s)
	(let ((location (hash-ref form-locations s #f)))
	  (when location
	    (set! current-form-location location))))
      saved))

  (define (leave-form! saved)
    (set! current-form-location saved))

  ;; an error raised by the pass in thunk becomes a read error at the location of the form it was processing
  (define (call-with-located-errors thunk)
    (set! current-form-location #f)
    (with-handlers ((exn:fail? (lambda (e)
				 (if (and current-source current-form-location)
				     (raise-read-error (exn-message e)
						       current-source
						       (vector-ref current-form-location 0)
						       (vector-ref current-form-location 1)
						       (vector-ref current-form-location 2)
						       (vector-ref current-form-location 3))
				     (raise e)))))
      (thunk)))


  ;; ------------- at the end -------------

  ;; the property of a syntax object read in a text : syntax-original? is true for what receives it
  (define original-property (read-syntax 'original (open-input-string "original")))

  ;; the code with its locations , a syntax object ; src is the source of the reader
  (define (datum->located-syntax datum src)

    (define (full location) ; a location of the tables , with the source
      (vector src
	      (vector-ref location 0)
	      (vector-ref location 1)
	      (vector-ref location 2)
	      (vector-ref location 3)))

    (define (without-span location) ; an atom of the generated code , at the beginning of its form
      (vector (vector-ref location 0)
	      (vector-ref location 1)
	      (vector-ref location 2)
	      (vector-ref location 3)
	      0))

    (define in-progress (make-hasheq)) ; a circular list has no syntax object : datum->syntax says it

    ;; a form that has no location of its own (made by the infix parser or by a pass , or an indented expression) : from
    ;; its first located element to the end of its last one
    (define (derived-location form)
      (let ((first-position #f) (first-location #f) (end #f) (visited (make-hasheq)))
	(define (consider! location)
	  (let ((position (vector-ref location 2))
		(stop (+ (vector-ref location 2) (vector-ref location 3))))
	    (when (or (not first-position) (< position first-position))
	      (set! first-position position)
	      (set! first-location location))
	    (when (or (not end) (> stop end))
	      (set! end stop))))
	(let walk ((p form))
	  (when (and (pair? p) (not (hash-ref visited p #f)))
	    (hash-set! visited p #t)
	    (let ((a (car p)))
	      (cond ((pair? a)
		     (cond ((hash-ref form-locations a #f) => consider!)
			   (else (walk a))))
		    ((hash-ref element-locations p #f) => consider!)))
	    (walk (cdr p))))
	(and first-location
	     (vector (vector-ref first-location 0)
		     (vector-ref first-location 1)
		     first-position
		     (- end first-position)))))

    (define (convert-form form parent)
      (let* ((own (hash-ref form-locations form #f))
	     (location (cond (own (full own))
			     ((derived-location form) => full)
			     (else parent))))
	(datum->syntax #f (convert-list form location) location (and own original-property))))

    (define (convert-list p location)
      (cond ((null? p) '())
	    ((pair? p)
	     (when (hash-ref in-progress p #f)
	       (datum->syntax #f datum location)) ; raises the error of datum->syntax on a cycle
	     (hash-set! in-progress p #t)
	     (let* ((a (car p))
		    (converted (cond ((pair? a) (convert-form a location))
				     ((hash-ref element-locations p #f)
				      => (lambda (found) (datum->syntax #f a (full found) original-property)))
				     (else (datum->syntax #f a (without-span location)))))
		    (rest (convert-list (cdr p) location)))
	       (hash-remove! in-progress p)
	       (cons converted rest)))
	    (else (datum->syntax #f p (without-span location)))))

    (let ((default (vector src 1 0 1 0)))
      (if (pair? datum)
	  (convert-form datum default)
	  (datum->syntax #f datum default)))))
