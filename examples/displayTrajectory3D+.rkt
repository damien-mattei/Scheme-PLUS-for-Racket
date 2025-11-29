;; parse the Bepi Colombo spacecraft trajectory file text and display it in 3D
;; displayTrajectory3D


#lang reader SRFI-105


;; Damien MATTEI

;; export PATH=/Applications/Racket/bin:$PATH
;; _www@device-88 drive % pwd
;; /opt/homebrew/var/www/drive
;; make
;;Welcome to DrRacket, version 8.14 [cs].
;;Language: Determine language from source; memory limit: 8192 MB.

;; (displayTrajectory3D-1file "/private/var/tmp/VIqognG.txt")

;; (displayTrajectory3D  "/opt/homebrew/var/www/drive/BepiColombo-Mio_MSO-orbit_1min_short.txt" "/opt/homebrew/var/www/drive/BepiColombo-Mio_MSO-FlyBy_1min_short.txt")

(module displayTrajectory3D racket
	
	(require Scheme+)	
	(require xml
		 (except-in 2htdp/batch-io xexpr?)) ; for: read-lines
	(require plot)
	(require racket/pretty) ; pretty print
	;;(require srfi/210) ; Procedures and Syntax for Multiple Values
	(require qi qi/list) ; Qi flow language
	

	(provide displayTrajectory3D
		 apply-values)
	

	;; macro to apply a procedures to values
	;; not used
	(define-syntax apply-values
	  (syntax-rules ()
	    ((_ proc some-values) (call-with-values (lambda () some-values)
				    proc))))

	(define (displayTrajectory3D src src1)

	  (display "Input file:")
	  (display src)
	  (newline)

	  ;; check we have a .txt file
	  {ext <- src[-4 :]} ; try to get the .txt extension
	  (display "Extension find:")
	  (display ext)
	  (newline)

	  (when (not (equal? ext ".txt"))
		(error "Not a text file."))

	  ;; read all lines
	  {input-lines <- (read-lines src)}
	  {vl <- (list->vector input-lines)}

	  {input-lines1 <- (read-lines src1)}
	  {vl1 <- (list->vector input-lines1)}

	  ;; find the basename
	  {basename <- src[: -4]} ; skip the 4 char of extension

	  ;; find the spatial_unit if exist
	  {spatial_unit <- (regexp-match #rx"km" vl[4])}
	  (when (not spatial_unit)
		{spatial_unit <- (regexp-match #rx"Rm" vl[4])})

	  (when (not spatial_unit)
		(error "No spatial unit (km or Rm) found."))

	  {spatial_unit <- (first spatial_unit)}

	  {tdl <- vl[5 :]} ; TABLEDATA lines, skip the header to go to table data lines
	  {tdl1 <- vl1[5 :]}
	  
	  ;; for points3d we must have a list of vectors of x,y,z

	  (display "before Lplot") (newline)
	  
	  ;; (define Lplot  (for/list ([tr tdl])
	  ;; 			   (list->vector (map string->number (rest (string-split tr))))))

	  (define Lplot1  (for/list ([tr tdl1])
				    (list->vector (map string->number (rest (string-split tr))))))

	  ;;(define-qi-foreign-syntaxes $nfx$)
	  (define-qi-foreign-syntaxes $bracket-apply$)

	  ;; (define-qi-syntax-rule ($nfx$ e ...)
	  ;;   (esc ($nfx$ e ...)))


	  ;; good one:
	  ;; (define-qi-syntax-rule ($nfx$ e0 op e ...)
	  ;;   (esc (lambda (v)
	  ;; 	   (display "In $nfx$ e0 op e of define-qi-syntax-rule") (newline)
	  ;; 	   ($nfx$ (v) op e0 op e ...))))

	  ;; (define-qi-syntax-rule ($nfx$ e0 op other ...)
	  ;;   (esc (lambda (v)
	  ;; 	   (display "In $nfx$ e0 op other of define-qi-syntax-rule") (newline)
	  ;; 	   ($nfx$ e0 op other ...))))
	  

	  ;; (define-qi-syntax-rule ($bracket-apply$ container arg-bracket ...)
	  ;;   (esc (lambda (v)
	  ;; 	   (display "In $bracket-apply$ of define-qi-syntax-rule") (newline)
	  ;; 	   ($bracket-apply$ (v)  container arg-bracket ...))))
  
	  ;; Qi version 5
	  ;; (define Lplot
	  ;;   (~> (tdl)
	  ;; 	(map (~> string-split rest (map string->number) list->vector))))

	  ;;(define Lplot (map (☯ (~> string-split rest (map string->number _) list->vector)) (vector->list tdl)))
	  
	  ;;(define Lplot (~> (tdl) vector->list (△ (~> string-split rest (△ string->number) vector)) ▽))

	  ;;(define-qi-foreign-syntaxes $bracket-apply$)

	  ;;(define Lplot (~> (src) read-lines cddddr cdr (△ (~> string-split rest (△ string->number) vector)) ▽))

	  ;; not yet working:
	  ;;(define Lplot {((tdl) ~> vector->list) ~> (△ (string-split ~> rest ~> (△ string->number) ~> vector)) ~> ▽})

	  ;;(define Lplot {(tdl) ~> vector->list ~> (△ {string-split ~> rest ~> (△ string->number) ~> vector}) ~> ▽})

	  ;;(define Lplot {(tdl) ~> vector->list ~> (△ (string-split ~> rest ~> (△ string->number) ~> vector)) ~> ▽})

	  ;;{Lplot := ((tdl) ~> vector->list ~> (△ (string-split ~> rest ~> (△ string->number) ~> vector)) ~> ▽)}

	  ;;{Lplot := (tdl) ~> vector->list ~> (△ (string-split ~> rest ~> (△ string->number) ~> vector)) ~> ▽}

	  ;;(define Lplot {(tdl) ~> vector->list ~> (△ (esc (lambda (v) {(v) ~> string-split ~> rest ~> (△ string->number) ~> vector}))) ~> ▽})

	  ;; buggy at least with no strict srfi support
	  ;;(define Lplot {(src) ~> read-lines ~> list->vector ~> {_[5 :]} ~> vector->list ~> (△ {string-split ~> rest ~> (△ string->number) ~> vector}) ~> ▽})

	  ;;(display '{(src) ~> read-lines ~> list->vector ~> {_[5 :]} ~> vector->list ~> (△ {string-split ~> rest ~> (△ string->number) ~> vector}) ~> ▽}) (newline)
	  
	  ;;(define Lplot {(src) ~> read-lines ~> list->vector ~> _[5 :] ~> vector->list ~> (△ {string-split ~> rest ~> (△ string->number) ~> vector}) ~> ▽})

	  ;;  (define vct #(1 2 3 4 5 6 7))
	  
	  ;; (define Lplot {(src) ~> read-lines ~> list->vector ~> vct[5 :] ~> vector->list ~> (△ (string-split ~> rest ~> (△ string->number) ~> vector)) ~> ▽})
	  
	 
	  
	  ;;(define Lplot {(src) ~> read-lines ~> list->vector ~> vct[5 :] ~> vector->list ~> (△ (string-split ~> rest ~> (△ string->number) ~> vector)) ~> ▽})

	  ;;(define Lplot {(src) ~> read-lines ~> list->vector ~> (lambda (x) x[5 :]) ~> vector->list ~> (△ (string-split ~> rest ~> (△ string->number) ~> vector)) ~> ▽})

	  ;;(define Lplot {(src) ~> read-lines ~> list->vector ~> (vector-copy _ 5) ~> vector->list ~> (△ (string-split ~> rest ~> (△ string->number) ~> vector)) ~> ▽})

	  ;;(define Lplot {(src) ~> read-lines ~> list->vector ~> (vector-copy _ 5) ~> vector->list ~> (△ {string-split ~> rest ~> (△ string->number) ~> vector}) ~> ▽})

	  ;;{Lplot := (src) ~> read-lines ~> list->vector ~> _[5 :] ~> vector->list ~> (△ {string-split ~> rest ~> (△ string->number) ~> vector}) ~> ▽}

	  {Lplot := (src) ~> read-lines ~> list->vector ~> _[5 :] ~> vector->list ~> (△ (string-split ~> rest ~> (△ string->number) ~> vector)) ~> ▽}
	  
	  (display "after Lplot") (newline)
	  (display "Lplot length=") (display (length Lplot)) (newline) 
	  ;;(display "Lplot=") (newline)
	  ;;(display Lplot)
	  ;;(pretty-print Lplot)
	  ;;(newline)

	  (display "Lplot1 length=") (display (length Lplot1)) (newline) 
	  ;;(display "Lplot1=") (newline)
	  ;;(display Lplot1)
	  ;;(pretty-print Lplot1)
	  ;;(newline)
	  
	  (plot3d (list (points3d Lplot
				  #:sym 'dot
				  #:color "blue")

			(points3d Lplot1			  
				  #:sym 'dot
				  #:color "red")

			(polar3d (λ (θ ϕ) 1) #:color "gray"))

		  #:title "BepiColombo FlyBy (red) over Mercury planet and Injection (blue)"
		  
		  #:x-min -6	 	 	 	 
		  #:x-max 6	 	 	 	 
		  #:y-min -6	 	 	 	 
		  #:y-max 6	 	 	 	 
		  #:z-min -6	 	 	 	 
		  #:z-max 6
		  )

	  )


	(define (displayTrajectory3D-1file src)

	  (display "Input file:")
	  (display src)
	  (newline)

	  ;; check we have a .txt file
	  {ext <- src[-4 :]} ; try to get the .txt extension
	  (display "Extension find:")
	  (display ext)
	  (newline)

	  (when (not (equal? ext ".txt"))
		(error "Not a text file."))

	  ;; read all lines
	  {input-lines <- (read-lines src)}
	  {vl <- (list->vector input-lines)}


	  ;; find the basename
	  {basename <- src[: -4]} ; skip the 4 char of extension

	  ;; find the spatial_unit if exist
	  {spatial_unit <- (regexp-match #rx"km" vl[4])}
	  (when (not spatial_unit)
		{spatial_unit <- (regexp-match #rx"Rm" vl[4])})

	  (when (not spatial_unit)
		(error "No spatial unit (km or Rm) found."))

	  {spatial_unit <- (first spatial_unit)}

	  {tdl <- vl[5 :]} ; TABLEDATA lines, skip the header to go to table data lines
	  
	  ;; for points3d we must have a list of vectors of x,y,z

	  (display "before Lplot") (newline)
	  
	  (define Lplot  (for/list ([tr tdl])
	  			   (list->vector (map string->number (rest (string-split tr))))))

	  (display "after Lplot") (newline)
	  (display "Lplot length=") (display (length Lplot)) (newline) 
	  ;; (display "Lplot=") (newline)
	  ;; (display Lplot)
	  ;;(pretty-print Lplot)
	  ;;(newline)

	  
	  (plot3d (list (points3d Lplot
				  #:x-min -6 ;-9	 	 	 	 
				  #:x-max 6	 	 	 	 
				  #:y-min -6	 	 	 	 
				  #:y-max 6	 	 	 	 
				  #:z-min -6	 	 	 	 
				  #:z-max 6
				  #:sym 'dot
				  #:color "blue")

			(polar3d (λ (θ ϕ) 1) #:color "gray")

			)
		  )

	  )

	) ; end module
