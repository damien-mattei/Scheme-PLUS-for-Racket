(module return racket/base


	(provide return return-rec)

	(require racket/stxparam
		 (for-syntax racket/base))

(define-syntax-parameter return-rec
  ;;(lambda (stx)
  (syntax-rules ()
     ((_ . _)
      (raise-syntax-error 'return-rec "can only be used inside def,def+ and defun")))
  )

(define-syntax-parameter return
  ;;(lambda (stx)
  (syntax-rules ()
     ((_ . _)
    (raise-syntax-error 'return "can only be used inside def,def+ and defun")))
  )

)
