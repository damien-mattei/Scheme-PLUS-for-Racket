(module return racket/base

	(provide return return-rec)

	(require racket/stxparam
		 (for-syntax racket/base))

(define err-string "can only be used inside def,def+,lambda+ and defun (defun being only used internally in Scheme+ source code)")
	
(define-syntax-parameter return-rec
  ;;(lambda (stx)
  (syntax-rules ()
     ((_ . _)
      (raise-syntax-error 'return-rec err-string)))
  )

(define-syntax-parameter return
  ;;(lambda (stx)
  (syntax-rules ()
     ((_ . _)
    (raise-syntax-error 'return err-string)))
  )

)
