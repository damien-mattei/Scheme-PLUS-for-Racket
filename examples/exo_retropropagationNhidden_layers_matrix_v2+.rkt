;;#lang reader "../Scheme-PLUS-for-Racket/src/SRFI-105.rkt"

#lang reader SRFI-105

;;#lang reader "../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/SRFI-105.rkt" ; SRFI-105 Curly-infix-expressions


; Deep Learning : back propagation, gradient descent, neural network with N hidden layers

; L'algorithme de rétro-propagation du gradient dans un
; réseau de neurones avec N couches cachées.

;  D. Mattei	


; MacOS users : use MacVim to show ALL the characters of this file (not Emacs, not Aquamacs)
;; jeu de couleurs: Torte ou Koehler / Peachpuff ou Retrobox la nuit

;; use in GUI 
;; use in command line:
;; (base) mattei@pc-mattei:~/Dropbox/git/AI_Deep_Learning$ racket
;; Welcome to Racket v8.6 [cs].
;; > (require "exo_retropropagationNhidden_layers_matrix_v2+.rkt")


(module exo_retropropagationNhidden_layers_matrix_v2 racket


(provide (all-defined-out)) 

(require plot)

;(require srfi/42) ; Eager Comprehensions

(require (rename-in srfi/42
	(: s42:))) ; Eager Comprehensions


(require (rename-in flomat (repeat repeat-flomat)
			   (shape shape-flomat)
			   (transpose transpose-flomat)))

(require "matrix+.rkt")

(require Scheme+)
;;(require Scheme-PLUS-for-Racket)
;;(require "../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/src/Scheme+.rkt")

(require (only-in racket/base [for for-racket]))

; first stage overloading
(define-overload-existing-operator +)
(define-overload-procedure uniform)


; second stage overloading
(overload-existing-operator + vector-append (vector? vector?))


;; return a number in ]-1,1[
;; the dummy parameter is needed by a flomat procedure
(define (uniform-dummy dummy) {(random) * (if {(random 2) = 0}  1 -1)}); we randomly choose the sign of the random number
		         	

; return a random number between [inf, sup]
(define (uniform-interval inf sup)
  {gap <- {sup - inf}}
  {inf + gap * (random)})

(overload-procedure uniform uniform-dummy (number?))

(overload-procedure uniform uniform-interval (number? number?))


; sigmoïde
(define (σ z̃) 
  {1 / {1 + (exp (- z̃))}})

; some derivatives
(define (der_tanh z z̃)
  {1 - z ** 2})	

(define (der_σ z z̃)
    {z * {1 - z}})

(define (der_atan z z̃)
  {1 / {1 + z̃ ** 2}})



#| this is a Scheme multi line comment,
but will it works with Scheme+ parser?
|#


; (make-object ReseauRetroPropagation)
; (define net (new ReseauRetroPropagation (nc #(1 2 3 4))))
; (get-field z net)
; '#(#(0) #(0 0) #(0 0 0) #(0 0 0 0))
; (send net accepte_et_propage #(1.7)) ; should no more works,define/public versus define
; #(-0.4839735172378113 -0.40037108475276867 -0.34185694789193694)

(define ReseauRetroPropagation

  (class object%

	 (super-new)
	 
	 ; this is the initialisation parameters
	 (init-field (nc #(2 3 1)) ;; on crée le tableau des couches du réseau
	       	     (nbiter 10000)
	       	     (ηₛ 1.0)
		     (activation_function_hidden_layer tanh)
		     (activation_function_output_layer tanh)
		     (activation_function_hidden_layer_derivative der_tanh)
		     (activation_function_output_layer_derivative der_tanh))
	 
	 {lnc <- (vector-length nc)}

	 ; les entrées concrètes seront fournies avec la méthode accepte
	 ;; (field (z (vector-ec (: i (vector-length nc)) (make-vector {nc[i]} 0))))
	 (field (z (vector-ec (:vector lg nc)
			      (make-vector lg 0))))
	 ;; (field (z (for/vector ([lg nc])
	 ;; 		       (make-vector lg 0))))

	 (display "z=") (display z) (newline)


	 ; z̃[0] is not used as z[0] is x, the initial data
	 (field (z̃ (vector-ec (:vector lg nc)
			      (make-vector lg 0))))

	 (display "z̃=") (display z̃) (newline)

	 (define-pointwise-unary uniform) ;; flomat library feature

	 (field (M (vector-ec (s42: n {lnc - 1}) ; vectors by eager comprehension (SRFI 42)
			  (.uniform! (zeros {nc[n + 1]} {nc[n] + 1}))))) ;; flomat Matrix
					   
	 (display "M=") (display M) (newline)

	 ;(field (ᐁ (for/vector ([lg nc])
	 ;		       (make-vector lg 0))))

	 ; here ᐁ  is not a field but a variable (could not be accessed outside the class,no getter...)
	 {ᐁ <- (for/vector ([lg nc])
			       (make-vector lg 0))}


	 
	 (display "ᐁ=") (display ᐁ) (newline)

	 (display "nbiter=") (display nbiter) (newline)

	 (field (error 0))


	 ; forward propagation
    
         ; z_* sans le coef. 1 constant pour le bias
	 (define (accepte_et_propage x) ; on entre des entrées et on les propage
		
		(when {vector-length(x) ≠ vector-length(z[0])} 
		  (display "Mauvais nombre d'entrées !") (newline)
		  (exit #f))

		{z[0] <- x} ; on ne touche pas au biais

		;; propagation des entrées vers la sortie

		{n <- vector-length(z)}
		;(display "n=") (display n) (newline)

		;; hidden layers
		(declare z_1)

		(declare i) ; because the variable will be used outside the 'for' loop too
		
		;(for-racket ([i (in-range {n - 2})]) ; warning : in Racket the variable 'i' 
		; is only seen inside the 'for-racket' but i need it ouside too
		(for ({i <- 0} {i < n - 2} {i <- i + 1}) ; personnal 'for' definition as in Javascript,C,C++,Java

		     ;; calcul des stimuli reçus par la couche cachée d'indice i+1 à-partir de la précedente

		     ;; create an array with 1 in front for the bias coefficient
		    
		     {z_1 <- #(1) + z[i]} ; + operator has been overloaded to append scheme vectors

		     {z̃[i + 1] <- M[i] * z_1} ; z̃ = matrix * vector , return a vector

		     ;(display "z̃[i + 1] = ") (display {z̃[i + 1]}) (newline)

		     #| calcul des réponses des neurones cachés
		     
		     i also use Neoteric Expression :https://sourceforge.net/p/readable/wiki/Rationale-neoteric/
		     example: {map(sin '(0.2 0.7 0.3))}
		     '(0.19866933079506122 0.644217687237691 0.29552020666133955)
		     
		     i also use Neoteric Expression to easily port Python code to Scheme+
		     
		     the original Python code was:
		     z[i+1] = list(map(self.activation_function_hidden_layer,z̃[i+1]))
		     the Scheme+ port is below: |#
		     {z[i + 1] <- vector-map(activation_function_hidden_layer z̃[i + 1])}

		     ;(display "z[i + 1] = ") (display {z[i + 1]}) (newline)

		  ) ; end for


		 ; output layer
        	 ;{i <- i + 1} ; was used with for-racket
		 ;(display "i=") (display i) (newline)


		 ; calcul des stimuli reçus par la couche cachée d'indice i+1 à-partir de la précedente

        	 ; create a list with 1 in front for the bias coefficient
        	 {z_1 <- #(1) + z[i]}

		 {z̃[i + 1] <- M[i] * z_1} ; z̃ = matrix * vector , return a vector

		 ; calcul des réponses des neurones de la couche de sortie
		 {z[i + 1] <- vector-map(activation_function_output_layer z̃[i + 1])}
		 ;(display "z[i + 1] = ") (display {z[i + 1]}) (newline)
	
	) ; end define/public


		
	 
	(define/public (apprentissage Lexemples) ; apprentissage des poids par une liste d'exemples
	   
	  {ip <- 0} ; numéro de l'exemple courant

	  (declare x y)
	  (for-racket ([it (in-range nbiter)]) ; le nombre d'itérations est fixé !

		      (when {it % 1000 = 0}
			(display it)(newline))

		      ;(display it)(newline)
		      
		      {err <- 0.0} ; l'erreur totale pour cet exemple

		      {x <- (car Lexemples[ip])}         ; un nouvel exemple à apprendre
		      {y <- (cdr Lexemples[ip])} 

		      ;; PROPAGATION VERS L'AVANT
		      (accepte_et_propage x)       ; sorties obtenues sur l'exemple courant, self.z_k et z_j sont mis à jour

		      ; RETRO_PROPAGATION VERS L'ARRIERE, EN DEUX TEMPS

		      {i <- i_output_layer <- vector-length(z) - 1} ; start at index i of the ouput layer

		      {ns <- vector-length(z[i])}
		     

		      ;; TEMPS 1. calcul des gradients locaux sur la couche k de sortie (les erreurs commises)
		      (for-racket ([k (in-range ns)])
				  {ᐁ[i][k] <- y[k] - z[i][k]}     ; gradient sur un neurone de sortie (erreur locale)
				  {err <- err + ᐁ[i][k] ** 2})    ; l'erreur quadratique totale

		      {err <- err * 0.5}

		      (when {it = nbiter - 1}
			{error <- err})               ; mémorisation de l'erreur totale à la dernière itération


		      ;; modification des poids de la matrice de transition de la derniére couche de neurones cachés à la couche de sortie

		      {მzⳆმz̃ <- activation_function_output_layer_derivative}

		      {modification_des_poids(M[i - 1] ηₛ z[i - 1] z[i] z̃[i] ᐁ[i] მzⳆმz̃)}

		      ;; TEMPS 2. calcul des gradients locaux sur les couches cachées (rétro-propagation), sauf pour le bias constant

		      {მzⳆმz̃ <- activation_function_hidden_layer_derivative}

		      (for-racket ([i (reversed (in-range 1 i_output_layer))])
				{nc <- vector-length(z[i])}
				{ns <- vector-length(z[i + 1])}
				(for-racket ([j (in-range nc)])
					{ᐁ[i][j] <- (for/sum ([k (in-range ns)])
							{მzⳆმz̃(z[i + 1][k] z̃[i + 1][k]) * M[i][k {j + 1}] * ᐁ[i + 1][k]})})
				; modification des poids de la matrice de transition de la couche i-1 à i
         			{modification_des_poids(M[i - 1] ηₛ  z[i - 1] z[i] z̃[i] ᐁ[i] მzⳆმz̃)})

		      ; et l'on passe à l'exemple suivant
            
            	      {ip <- random(vector-length(Lexemples))}

		 ) ; end for it
	  ) ; end define/public


	 
	; modify coefficients layer
	(define (modification_des_poids M_i_o η z_input z_output z̃_output ᐁ_i_o მzⳆმz̃) ; derivative of activation function of the layer
	 
	  ; the length of output and input layer with coeff. used for bias update
	  {(len_layer_output len_layer_input_plus1forBias) <- (dim M_i_o)} ; use values and define-values to create bindings
        
	  {len_layer_input <- {len_layer_input_plus1forBias - 1}}

	  (for-racket ([j (in-range len_layer_output)]) ; line
		
		(for-racket ([i (in-range len_layer_input)]) ; column , parcours les colonnes de la ligne sauf le bias
		    
		    {M_i_o[j {i + 1}]  <-  M_i_o[j {i + 1}] - {(- η) * z_input[i] * მzⳆმz̃(z_output[j] z̃_output[j]) * ᐁ_i_o[j]}})

		; and update the bias
            	{M_i_o[j 0]  <-  M_i_o[j 0] - {(- η) * 1.0 * მzⳆმz̃(z_output[j] z̃_output[j]) * ᐁ_i_o[j]}}))
	

	(define/public (test Lexemples)

          (display "Test des exemples :") (newline)
          {err <- 0}

	  (declare entree sortie_attendue ᐁ)
	  (for-racket ([entree-sortie_attendue Lexemples])
		{entree <- (car entree-sortie_attendue)} 
		{sortie_attendue <- (cdr entree-sortie_attendue)} ; use pairs in Scheme instead of tuples and vectors in Python
		(accepte_et_propage entree)
		(printf "~a --> ~a : on attendait ~a" entree {z[vector-length(z) - 1]} sortie_attendue) (newline)
		{ᐁ <- sortie_attendue[0] - z[vector-length(z) - 1][0]} ; erreur sur un element
		{error <- error + ᐁ ** 2})                             ; l'erreur quadratique totale
		
	  {err <- err * 0.5}
	  (display "Error on examples=") (display error) (newline))


	
	; compute the points for plotting
	(define/public (DL-data-2D)

		(list-ec (s42: n 100)
	      		($+>
			    {xp <- (- pi) / 2 + pi * n / 100}
			    (accepte_et_propage (vector xp))
			    {xp-DL <- z[vector-length(z) - 1][0]}
			    (vector xp xp-DL))))


	; plot in 2D the points of the input/output layers
	(define/public (DL-plot)
		
		{Lplot-DL <- (DL-data-2D)}

		(plot (points Lplot-DL  #:sym 'fullcircle1
            	         		#:color "red")))


	


   ) ; end class

) ; end define





(printf "################## NOT ##################")
(newline)

{r1 <- (new ReseauRetroPropagation (nc #(1 2 1))
				   (nbiter 5000)
				   (ηₛ 10)
				   (activation_function_hidden_layer σ)
				   (activation_function_output_layer σ)
				   (activation_function_hidden_layer_derivative der_σ)
				   (activation_function_output_layer_derivative der_σ))}

{Lexemples1 <- #((#(1) . #(0)) (#(0) . #(1)))}  ; use pairs in Scheme instead of vectors in Python

(send r1 apprentissage Lexemples1)

(send r1 test Lexemples1)

{precision <- 100.0}

(display "precision=") (display precision) (newline)

(define (trunc x) ; truncate a number x to log10(precision) decimals
	{round{precision * x} / precision})

(define-pointwise-unary trunc) ; flomat library feature that create an unary function .trunc!

(define (trunc-matrix mt) ; truncate coefficient of a matrix 
	(.trunc! mt))

{M <- (get-field M r1)} ; get the vector of matrices in the retro-propagation class

(newline)
(display "Matrix vector M=") (newline)
(display M)
(newline)

; truncate all the transitional matrices of the deep neural network
(for-racket ([mt M])
	(trunc-matrix mt)) 

(display "Matrix vector modified M=") (newline)
(display M)
(newline)

(send r1 test Lexemples1)
(newline)


(printf "################## XOR ##################")
(newline)

{r2 <- (new ReseauRetroPropagation (nc #(2 3 1))
				   (nbiter 250000)
				   (ηₛ 10)
				   (activation_function_hidden_layer σ)
				   (activation_function_output_layer σ)
				   (activation_function_hidden_layer_derivative der_σ)
				   (activation_function_output_layer_derivative der_σ))}

{Lexemples2 <- #( (#(1 0) . #(1))  (#(0 0) . #(0))  (#(0 1) . #(1))  (#(1 1) . #(0)))}  ; use pairs in Scheme instead of vectors in Python

(send r2 apprentissage Lexemples2)

(send r2 test Lexemples2)

{M <- (get-field M r2)} ; get the vector of matrices in the retro-propagation class

(newline)
(display "Matrix vector M=") (newline)
(display M)
(newline)

; truncate all the transitional matrices of the deep neural network
(for-racket ([mt M])
	(trunc-matrix mt)) 

(display "Matrix vector modified M=") (newline)
(display M)
(newline)

(send r2 test Lexemples2)
(newline)



(printf "################## SINUS ##################")
(newline)

{r3 <- (new ReseauRetroPropagation (nc #(1 70 70 1))
				   (nbiter 50000)
				   (ηₛ 0.01)
				   (activation_function_hidden_layer atan) ;tanh) ; atan)
				   (activation_function_output_layer tanh)
				   (activation_function_hidden_layer_derivative der_atan) ;der_tanh); der_atan)
				   (activation_function_output_layer_derivative der_tanh))}

{Llearning <- (vector-ec (:list x (list-ec (s42: n 10000)
					   (uniform (- pi) pi)))
			 (cons (vector x) (vector (sin x))))   ; vectors by eager comprehension (SRFI 42)
	   }  ; use pairs in Scheme instead of vectors in Python

{Ltest <- (vector-ec (:list x (list-ec (s42: n 10)
				       (uniform {(- pi) / 2} {pi / 2})))
		     (cons (vector x) (vector (sin x))))   ; vectors by eager comprehension (SRFI 42)
       }  ; use pairs in Scheme instead of vectors in Python

;{Lplot <- (list-ec (:list x (list-ec (s42: n 100)
;				{{(- pi) / 2} + {n / 100} * pi}))
;		   (vector x (sin x)))}

;(display "Lplot=") (newline)
;(display Lplot)
;(newline)


{Lplot-sin <- (list-ec (s42: n 100)
	      	     ($+> ; begin-def
			{xp <- (- pi) / 2 + pi * n / 100}
			(vector xp (sin xp))))}

(display "Lplot-sin =") (newline)
(display Lplot-sin )
(newline)

(plot (points Lplot-sin  #:sym 'fullcircle1
            	         #:color "blue"
			 #:label "y = sin(x)"))

 

(send r3 apprentissage Llearning)

(send r3 test Ltest)

(send r3 DL-plot)

{Lplot-DL-main <- (send r3 DL-data-2D)} ; bug possibly cause by Lplot-DL being defined in 2 places (see define/public method)

(plot (list (points Lplot-sin  #:sym 'fullcircle1
                	       #:color "blue"
			       #:label "y = sin(x)")
	    (points Lplot-DL-main   #:sym 'circle1
            	               	    #:color "red"
				    #:label "neural sine")))


{M <- (get-field M r3)} ; get the vector of matrices in the retro-propagation class

(newline)
(display "Matrix vector M=") (newline)
(display M)
(newline)


{precision <- 1000.0}

(display "precision=") (display precision) (newline)

(define (trunc3 x) ; truncate a number x to log10(precision) decimals
	{round{precision * x} / precision})

(define-pointwise-unary trunc3) ; flomat library feature that create an unary function .trunc!

(define (trunc3-matrix mt) ; truncate coefficient of a matrix 
	(.trunc3! mt))


; truncate all the transitional matrices of the deep neural network
(for-racket ([mt M])
	(trunc3-matrix mt)) 

(display "Matrix vector modified M=") (newline)
(display M)
(newline)

(send r3 test Ltest)
(newline)

{Lplot-DL-trunc <- (send r3 DL-data-2D)} 

(plot (list (points Lplot-DL-trunc  #:sym 'circle1
                	            #:color "green"
				    #:label "neural sine - matrices with truncated numbers")
	    (points Lplot-DL-main   #:sym 'circle1
            	               	    #:color "red"
				    #:label "neural sine")))


) ; end module

