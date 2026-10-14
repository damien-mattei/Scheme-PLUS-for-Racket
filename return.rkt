(module return racket/base

	(provide return return-rec)

	(require racket/stxparam
		 (for-syntax racket/base))

#;(define err-string "can only be used inside def,def+,lambda+")
(define err-string "can only be used inside def") ; def+ and lambda+ are deprecated
	
(define-syntax-parameter return-rec
  ;;(lambda (stx)
  (syntax-rules ()
     ((_ . _)
      #;(raise-syntax-error 'return-rec "can only be used inside def,def+,rec+")
      (raise-syntax-error 'return-rec "can only be used inside def"))) ; def+ and rec+ are deprecated
  )

(define-syntax-parameter return
  ;;(lambda (stx)
  (syntax-rules ()
     ((_ . _)
    #;(raise-syntax-error 'return "can only be used inside def,def+,lambda+")
    (raise-syntax-error 'return "can only be used inside def"))) ; def+ and lambda+ are deprecated
  )

)
