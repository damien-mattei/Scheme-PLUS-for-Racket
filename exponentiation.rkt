
(module exponentiation racket/base


  (provide ** )



;; coding hint: use only macro when necessary

;; (define-syntax **
;;   (syntax-rules ()
;;     ((_ a b) (expt a b))))

;; (define (** a b)
;;   (expt a b))

;;(define ** expt)

  ;; {2 ** 3 ** 4}
  ;; 2417851639229258349412352

  (define (**-not-tail-rec b . pQ)
    (cond ((null? pQ) 1) ; by convention 2^() = 2⁰ = 1 it is like this in Mathematics
	  ((null? (cdr pQ)) (expt b (car pQ))) ; example : 2³ 
	  (else
	   (expt b (apply **-not-tail-rec pQ))))) ; example 2 ³ ⁴ =  2^(3^4) still like this in Mathematics


  ;; a tail recursive version with an accumulator
  (define (** b . pQ)
    (define (**-tail-rec p Q)
      (if (null? p)
	  Q ; when finish we return the accumulator
	  (**-tail-rec (cdr p)
		       (expt (car p) Q)))) ; p ^ Q 

    (if (null? pQ)
	1 ; by convention 2^() = 2⁰ = 1 it is like this in Mathematics
	(let ((Qp (reverse pQ))) ; because exponentiation is right-associative we will compute from right to left, so we need to reverse the list
	  (expt b (**-tail-rec (cdr Qp) (car Qp)))))) ; start with Q,a number in the accumulator and p being a list

) ; end library


;; note: under Linux and on a PC (french) keyboard superscript characters
;; can be generated with the keystroke sequence: ^ n where n is  a number or sign
;; under MacOS the keystroke sequence is : Command Shift + n (you really need to type + key for superscript)


