
;; warning: 'do is already part of R6RS (reserved keyword) 'while is not in R5RS,R6RS, R7RS-small

;; but 'do in Scheme has a painful syntax

;; syntax defined in this file are inspired from Pascal language

;; scheme@(guile-user)> (use-modules (Scheme+))
;; scheme@(guile-user)> (define i 0)
;; scheme@(guile-user)> (define do '())
;; scheme@(guile-user)> (while {i < 4}
;;                           do
;;                              (display i)
;;                              (newline)
;;                              {i <- {i + 1}})
;; 0
;; 1
;; 2
;; 3
;; $1 = #f

;; (while {i < 4}
;;    do
;;      (display i)
;;      (newline)
;;      {i <- {i + 1}})

;; (define-syntax while
;;   (syntax-rules (while do)
;;     ((_ pred do b1 ...)
;;      (let loop () (when pred b1 ... (loop))))))

;; bug: 1+ return #f with SRFI 105 reader of Racket

;; (do ((i 1 (1+ i))
;;      (p 3 (* 3 p)))
;;     ((> i 4)
;;      p)
;;   (format #t "3**~s is ~s\n" i p))
;; 3**1 is 3
;; 3**2 is 9
;; 3**3 is 27
;; 3**4 is 81
;; $1 = 243

;; scheme@(guile-user)> (do ((i 1 (1+ i))
;;      (p 3 (* 3 p)))
;;     ((> i 4)
;;      p)
;;   (set! p (+ p i)))
;; $1 = 417


;; with a definition inside only the new version works:
;; (do ((i 1 (+ 1 i))
;;      (p 3 (* 3 p)))
;;     ((> i 4)
;;      p)
;;   (define x 7)
;;   (set! p (+ p i x)))
;; $3 = 1257


;; 'do is redefined here only to allow 'define in body as allowed in Scheme+
;; (define-syntax do

;;   (syntax-rules ()

;;     ((do ((var init step ...) ...)

;;          (test expr ...)

;;          command ...)

;;      (letrec

;;        ((loop

;;          (lambda (var ...)

;;            (if test

;;                ;;(begin
;; 	       (let ()

;;      		 ;;#f ; avoid empty begin but with (let () i don't care ! (bigloo use: (if #f #f) which creates a never executed instruction !)
;; 		 '() ;; avoid while-do-when-unless.scm: let: bad syntax (missing binding pairs or body) in: (let ())
;;                  expr ...)

;;                ;;(begin
;; 	       (let ()

;;                  command

;;                  ...

;;                  (loop (do "step" var step ...)

;;                        ...))))))

;;        (loop init ...)))

;;     ((do "step" x)

;;      x)

;;     ((do "step" x y)

;;      y)))



;; > (define i 0) 
;; > (do (display-nl "toto") (set! i (+ i 1)) while (< i 4)) 
;; toto
;; toto
;; toto
;; toto
;; Warning: this 'do' break the one of scheme !
(define-syntax do
  (syntax-rules (do)
    ((do b1 ...
       while pred)
     (let loop () b1 ... (when pred (loop))))))




;; definitions redefined here only to allow 'define in body as allowed in Scheme+

;; implémenté de base en Racket
;; (define-syntax when
;;   (syntax-rules ()
;;     ;;((when test result1 result2 ...)
;;     ((when test result1  ...)
;;      (if test
;;          ;;(begin result1 result2 ...)))))
;; 	 ;;(let () result1 result2 ...)))))
;; 	 (let () result1 ...)))))

(define-syntax unless
  (syntax-rules ()
    ;;((unless test result1 result2 ...)
    ((unless test result1 ...)
     (when (not test)
       	 ;;(let () result1 result2 ...)))))
	  result1 ...))))
