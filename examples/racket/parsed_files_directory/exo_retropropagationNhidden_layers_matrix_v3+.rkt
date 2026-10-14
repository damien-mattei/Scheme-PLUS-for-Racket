(module exo_retropropagationNhidden_layers_matrix_v3 racket
  (provide (all-defined-out))
  (require plot
           (rename-in srfi/42 (: s42:))
           (rename-in
            flomat
            (repeat repeat-flomat)
            (shape shape-flomat)
            (transpose transpose-flomat))
           (rename-in Scheme+ (· ·bak) (× ×bak))
           "matrix+.rkt"
           (only-in racket/base (for for-racket)))
  (require-overload-operator · multiply-flomat-vector (flomat? vector?))
  (overload-operator + (vector?) vector-append (vector? vector?))
  (random-seed 1027)
  (define (uniform-dummy dummy) (fl+ -1.0 (fl* (random) 2.0)))
  (define (uniform-interval inf sup)
    (← gap (fl- sup inf))
    (fl+ inf (fl* gap (random))))
  (overload-procedure uniform uniform-dummy (number?))
  (overload-procedure uniform uniform-interval (real? real?))
  (overload-procedure uniform uniform-interval (flonum? flonum?))
  (define (carre x) (fl* x x))
  (define (σ z̃) (fl/ 1.0 (fl+ 1.0 (exp (fl- z̃)))))
  (define (der_tanh z z̃) (fl- 1.0 (carre z)))
  (define (der_σ z z̃) (fl* z (fl- 1.0 z)))
  (define (der_atan z z̃) (fl/ 1.0 (fl+ 1.0 (sqr z̃))))
  (define ReseauRetroPropagation
    (class object%
      (super-new)
      (init-field
       (nc #(2 3 1))
       (nbiter 10000)
       (ηₛ 1.0)
       (activation_function_hidden_layer tanh)
       (activation_function_output_layer tanh)
       (activation_function_hidden_layer_derivative der_tanh)
       (activation_function_output_layer_derivative der_tanh)
       (mesure-temps #f))
      (← lnc (vector-length nc))
      (field (z (vector-ec (:vector lg nc) (make-vector lg 0))))
      (display "z=")
      (display z)
      (newline)
      (field (z̃ (vector-ec (:vector lg nc) (make-vector lg 0))))
      (display "z̃=")
      (display z̃)
      (newline)
      (define-pointwise-unary uniform-dummy)
      (display "after define-pointwise-unary")
      (newline)
      (field
       (M
        (vector-ec
         (s42: n (fx- lnc 1))
         (.uniform-dummy!
          (zeros
           ($bracket-apply-vector$ nc n + 1)
           (fx+ ($bracket-apply-vector$ nc n) 1))))))
      (display "M=")
      (display M)
      (newline)
      (← ᐁ (for/vector ((lg nc)) (make-vector lg 0.0)))
      (display "ᐁ=")
      (display ᐁ)
      (newline)
      (display "nbiter=")
      (display nbiter)
      (newline)
      (field (eror 0.0))
      (define
       (accepte_et_propage x)
       (when (fx≠
              (vector-length x)
              (vector-length ($bracket-apply-vector$ z 0)))
         (display "accepte_et_propage : Mauvais nombre d'entrées !")
         (newline)
         (exit #f))
       (<-vector-vector* ($bracket-apply-vector$ z 0) x)
       (← n (vector-length z))
       (declare z_1)
       (declare i)
       (for
        ((← i 0) (fx< i (fx- n 2)) (← i (fx+ i 1)))
        (<-vector-vector*
         z_1
         (vector-append #(1.0) ($bracket-apply-vector$ z i)))
        (←
         ($bracket-apply-vector$ z̃ i + 1)
         (multiply-flomat-vector ($bracket-apply-vector$ M i) z_1))
        (<-vector-vector*
         ($bracket-apply-vector$ z i + 1)
         (vector-map
          activation_function_hidden_layer
          ($bracket-apply-vector$ z̃ i + 1))))
       (<-vector-vector*
        z_1
        (vector-append #(1.0) ($bracket-apply-vector$ z i)))
       (←
        ($bracket-apply-vector$ z̃ i + 1)
        (multiply-flomat-vector ($bracket-apply-vector$ M i) z_1))
       (<-vector-vector*
        ($bracket-apply-vector$ z i + 1)
        (vector-map
         activation_function_output_layer
         ($bracket-apply-vector$ z̃ i + 1))))
      (define/public
       (apprentissage Lexemples)
       (← ip 0)
       (declare x y)
       (declare temps-debut temps-fin)
       (when mesure-temps (← temps-debut (current-inexact-milliseconds)))
       (for-racket
        ((it (range nbiter)))
        (when (fx= (fxmodulo it 1000) 0)
          (display it)
          (newline)
          (when (and mesure-temps (fx> it 0))
            (← temps-fin (current-inexact-milliseconds))
            (display "temps écoulé pour les 1000 dernières itérations (ms) : ")
            (display (fl- temps-fin temps-debut))
            (newline)
            (← temps-debut temps-fin)))
        (← err 0.0)
        (← x (car ($bracket-apply-vector$ Lexemples ip)))
        (← y (cdr ($bracket-apply-vector$ Lexemples ip)))
        (accepte_et_propage x)
        (← i i_output_layer (fx- (vector-length z) 1))
        (← ns (vector-length ($bracket-apply-vector$ z i)))
        (for-racket
         ((k (range ns)))
         (←
          ($bracket-apply-vector$ ($bracket-apply-vector$ ᐁ i) k)
          (-
           ($bracket-apply$ y k)
           ($bracket-apply-vector$ ($bracket-apply-vector$ z i) k)))
         (←
          err
          (fl+
           err
           (carre ($bracket-apply-vector$ ($bracket-apply-vector$ ᐁ i) k)))))
        (← err (fl* err 0.5))
        (when (fx= it (fx- nbiter 1)) (← eror err))
        (← მzⳆმz̃ activation_function_output_layer_derivative)
        (modification_des_poids
         ($bracket-apply-vector$ M i - 1)
         ηₛ
         ($bracket-apply-vector$ z i - 1)
         ($bracket-apply-vector$ z i)
         ($bracket-apply-vector$ z̃ i)
         ($bracket-apply-vector$ ᐁ i)
         მzⳆმz̃)
        (← მzⳆმz̃ activation_function_hidden_layer_derivative)
        (for-racket
         ((i (reverse (range 1 i_output_layer))))
         (← nc1 (vector-length ($bracket-apply-vector$ z i)))
         (← ns1 (vector-length ($bracket-apply-vector$ z i + 1)))
         (for-racket
          ((j (range nc1)))
          (←
           ($bracket-apply-vector$ ($bracket-apply-vector$ ᐁ i) j)
           (for/sum
            ((k (range ns1)))
            (fl*
             (მzⳆმz̃
              ($bracket-apply-vector$ ($bracket-apply-vector$ z i + 1) k)
              ($bracket-apply-vector$ ($bracket-apply-vector$ z̃ i + 1) k))
             ($bracket-apply-flomat$ ($bracket-apply-vector$ M i) k (fx+ j 1))
             ($bracket-apply-vector$ ($bracket-apply-vector$ ᐁ i + 1) k)))))
         (modification_des_poids
          ($bracket-apply-vector$ M i - 1)
          ηₛ
          ($bracket-apply-vector$ z i - 1)
          ($bracket-apply-vector$ z i)
          ($bracket-apply-vector$ z̃ i)
          ($bracket-apply-vector$ ᐁ i)
          მzⳆმz̃))
        (← ip (random (vector-length Lexemples)))))
      (define
       (modification_des_poids M_i_o η z_input z_output z̃_output ᐁ_i_o მzⳆმz̃)
       (← (len_layer_output len_layer_input_plus1forBias) (dim M_i_o))
       (← len_layer_input (- len_layer_input_plus1forBias 1))
       (for-racket
        ((j (range len_layer_output)))
        (for-racket
         ((i (range len_layer_input)))
         (←
          ($bracket-apply-flomat$ M_i_o j (fx+ i 1))
          (fl+
           ($bracket-apply-flomat$ M_i_o j (fx+ i 1))
           (fl*
            η
            ($bracket-apply-vector$ z_input i)
            (მzⳆმz̃
             ($bracket-apply-vector$ z_output j)
             ($bracket-apply-vector$ z̃_output j))
            ($bracket-apply-vector$ ᐁ_i_o j)))))
        (←
         ($bracket-apply-flomat$ M_i_o j 0)
         (fl-
          ($bracket-apply-flomat$ M_i_o j 0)
          (fl*
           (fl- η)
           1.0
           (მzⳆმz̃
            ($bracket-apply-vector$ z_output j)
            ($bracket-apply-vector$ z̃_output j))
           ($bracket-apply-vector$ ᐁ_i_o j))))))
      (define/public
       (test Lexemples)
       (display "Test des exemples :")
       (newline)
       (← err 0.0)
       (declare entree sortie_attendue ᐁ)
       (for-racket
        ((entree-sortie_attendue Lexemples))
        (← entree (car entree-sortie_attendue))
        (← sortie_attendue (cdr entree-sortie_attendue))
        (accepte_et_propage entree)
        (printf
         "~a --> ~a : on attendait ~a"
         entree
         ($bracket-apply-vector$ z (vector-length z) - 1)
         sortie_attendue)
        (newline)
        (←
         ᐁ
         (-
          ($bracket-apply$ sortie_attendue 0)
          ($bracket-apply-vector$
           ($bracket-apply-vector$ z (vector-length z) - 1)
           0)))
        (← eror (+ eror (** ᐁ 2))))
       (← err (fl* err 0.5))
       (display "Error on examples=")
       (display eror)
       (newline))
      (define/public
       (DL-data-2D)
       (list-ec
        (s42: n 100.0)
        ($+>
         (← xp (+ (fl/ (fl- pi) 2.0) (/ (· pi n) 100.0)))
         (accepte_et_propage (vector xp))
         (←
          xp-DL
          ($bracket-apply-vector$
           ($bracket-apply-vector$ z (vector-length z) - 1)
           0))
         (vector xp xp-DL))))
      (define/public
       (DL-data-2D-GI)
       (list-ec
        (s42: n 100.0)
        ($+>
         (← xp (+ (fl/ (fl- pi) 2.0) (/ (· (fl/ (fl* 3.0 pi) 2.0) n) 100.0)))
         (accepte_et_propage (vector xp))
         (←
          xp-DL
          ($bracket-apply-vector$
           ($bracket-apply-vector$ z (vector-length z) - 1)
           0))
         (vector xp xp-DL))))
      (define/public
       (DL-data-2D-HO)
       (list-ec
        (s42: n 100.0)
        ($+>
         (← iinfp (+ iinf 1.0))
         (← isupp (- isup 1.5))
         (← interv (- isupp iinfp))
         (← xp (+ iinfp (/ (· interv n) 100.0)))
         (accepte_et_propage (vector xp))
         (←
          xp-DL
          ($bracket-apply-vector$
           ($bracket-apply-vector$ z (vector-length z) - 1)
           0))
         (vector xp xp-DL))))
      (define/public
       (DL-plot)
       (← Lplot-DL (DL-data-2D))
       (plot (points Lplot-DL #:sym 'fullcircle1 #:color "red")))
      (define/public
       (DL-plot-GI)
       (← Lplot-DL (DL-data-2D-GI))
       (plot (points Lplot-DL #:sym 'fullcircle1 #:color "red")))
      (define/public
       (DL-plot-HO)
       (← Lplot-DL (DL-data-2D-HO))
       (plot (points Lplot-DL #:sym 'fullcircle1 #:color "red")))))
  (newline)
  (printf "################## NOT ##################")
  (newline)
  (←
   r1
   (new
    ReseauRetroPropagation
    (nc #(1 2 1))
    (nbiter 5000)
    (ηₛ 10.0)
    (activation_function_hidden_layer σ)
    (activation_function_output_layer σ)
    (activation_function_hidden_layer_derivative der_σ)
    (activation_function_output_layer_derivative der_σ)))
  (<-vector-vector* Lexemples1 #((#(1.0) . #(0.0)) (#(0.0) . #(1.0))))
  (send r1 apprentissage Lexemples1)
  (send r1 test Lexemples1)
  (← precision 100.0)
  (display "precision=")
  (display precision)
  (newline)
  (define (trunc x) (/ (round (* precision x)) precision))
  (define-pointwise-unary trunc)
  (define (trunc-matrix mt) (.trunc! mt))
  (← M (get-field M r1))
  (newline)
  (display "Matrix vector M=")
  (newline)
  (display M)
  (newline)
  (for-racket ((mt M)) (trunc-matrix mt))
  (display "Matrix vector modified M=")
  (newline)
  (display M)
  (newline)
  (send r1 test Lexemples1)
  (newline)
  (printf "################## XOR ##################")
  (newline)
  (←
   r2
   (new
    ReseauRetroPropagation
    (nc #(2 3 1))
    (nbiter 250000)
    (ηₛ 10.0)
    (activation_function_hidden_layer σ)
    (activation_function_output_layer σ)
    (activation_function_hidden_layer_derivative der_σ)
    (activation_function_output_layer_derivative der_σ)))
  (<-vector-vector*
   Lexemples2
   #((#(1.0 0.0) . #(1.0))
     (#(0.0 0.0) . #(0.0))
     (#(0.0 1.0) . #(1.0))
     (#(1.0 1.0) . #(0.0))))
  (send r2 apprentissage Lexemples2)
  (send r2 test Lexemples2)
  (← M (get-field M r2))
  (newline)
  (display "Matrix vector M=")
  (newline)
  (display M)
  (newline)
  (for-racket ((mt M)) (trunc-matrix mt))
  (display "Matrix vector modified M=")
  (newline)
  (display M)
  (newline)
  (send r2 test Lexemples2)
  (newline)
  (←
   Lplot-sin
   (list-ec
    (s42: n 100)
    ($+>
     (← xp (+ (fl/ (fl- pi) 2.0) (/ (· pi n) 100.0)))
     (vector xp (sin xp)))))
  (printf "################## Harmonic Oscillator ##################")
  (newline)
  (define (ho x) (/ (· (sin x) (exp (/ (- x) 10.0))) 1.7))
  (←
   r5
   (new
    ReseauRetroPropagation
    (nc #(1 200 40 40 1))
    (nbiter 70000)
    (ηₛ 0.01)
    (activation_function_hidden_layer atan)
    (activation_function_output_layer tanh)
    (activation_function_hidden_layer_derivative der_atan)
    (activation_function_output_layer_derivative der_tanh)
    (mesure-temps #t)))
  (display "r5 : after ReseauRetroPropagation")
  (newline)
  (← iinf -7.0)
  (← isup 11.0)
  (←
   Llearning-HO
   (vector-ec
    (:list x (list-ec (s42: n 30000) (uniform-interval iinf isup)))
    (cons (vector x) (vector (ho x)))))
  (←
   Ltest-HO
   (vector-ec
    (:list x (list-ec (s42: n 10) (uniform-interval iinf isup)))
    (cons (vector x) (vector (ho x)))))
  (display "r5 : before Lplot-HO")
  (newline)
  (←
   Lplot-HO
   (list-ec
    (s42: n 100.0)
    ($+>
     (← iinfp (fl+ iinf 1.0))
     (← isupp (fl- isup 2.5))
     (← interv (fl- isupp iinfp))
     (← xp (+ iinfp (/ (· interv n) 100.0)))
     (vector xp (ho xp)))))
  (display "r5 : before apprentissage")
  (newline)
  (send r5 apprentissage Llearning-HO)
  (send r5 test Ltest-HO)
  (send r5 DL-plot-HO)
  (← Lplot-DL-main-HO (send r5 DL-data-2D-HO))
  (plot
   (list
    (points
     Lplot-HO
     #:sym
     'fullcircle1
     #:color
     "blue"
     #:label
     "y = sin(x)·e^(-x/10)/1.7")
    (points
     Lplot-DL-main-HO
     #:sym
     'circle1
     #:color
     "red"
     #:label
     "Neural Harmonic Oscillator")))
  (printf "################## SINUS ##################")
  (newline)
  (←
   r3
   (new
    ReseauRetroPropagation
    (nc #(1 70 70 1))
    (nbiter 50000)
    (ηₛ 0.01)
    (activation_function_hidden_layer atan)
    (activation_function_output_layer tanh)
    (activation_function_hidden_layer_derivative der_atan)
    (activation_function_output_layer_derivative der_tanh)))
  (←
   Llearning
   (vector-ec
    (:list x (list-ec (s42: n 10000) (uniform-interval (fl- pi) pi)))
    (cons (vector x) (vector (sin x)))))
  (←
   Ltest
   (vector-ec
    (:list
     x
     (list-ec (s42: n 10) (uniform-interval (fl/ (fl- pi) 2.0) (fl/ pi 2.0))))
    (cons (vector x) (vector (sin x)))))
  (display "Lplot-sin =")
  (newline)
  (display Lplot-sin)
  (newline)
  (plot
   (points Lplot-sin #:sym 'fullcircle1 #:color "blue" #:label "y = sin(x)"))
  (send r3 apprentissage Llearning)
  (send r3 test Ltest)
  (send r3 DL-plot)
  (← Lplot-DL-main (send r3 DL-data-2D))
  (plot
   (list
    (points Lplot-sin #:sym 'fullcircle1 #:color "blue" #:label "y = sin(x)")
    (points Lplot-DL-main #:sym 'circle1 #:color "red" #:label "neural sine")))
  (← M (get-field M r3))
  (newline)
  (display "Matrix vector M=")
  (newline)
  (display M)
  (newline)
  (← precision 1000.0)
  (display "precision=")
  (display precision)
  (newline)
  (define (trunc3 x) (/ (round (· precision x)) precision))
  (define-pointwise-unary trunc3)
  (define (trunc3-matrix mt) (.trunc3! mt))
  (for-racket ((mt M)) (trunc3-matrix mt))
  (display "Matrix vector modified M=")
  (newline)
  (display M)
  (newline)
  (send r3 test Ltest)
  (newline)
  (← Lplot-DL-trunc (send r3 DL-data-2D))
  (plot
   (list
    (points
     Lplot-DL-trunc
     #:sym
     'circle1
     #:color
     "green"
     #:label
     "neural sine - matrices with truncated numbers")
    (points Lplot-DL-main #:sym 'circle1 #:color "red" #:label "neural sine")))
  (printf "################## SINUS greater interval ##################")
  (newline)
  (←
   r4
   (new
    ReseauRetroPropagation
    (nc #(1 70 70 1))
    (nbiter 50000)
    (ηₛ 0.01)
    (activation_function_hidden_layer atan)
    (activation_function_output_layer tanh)
    (activation_function_hidden_layer_derivative der_atan)
    (activation_function_output_layer_derivative der_tanh)))
  (←
   Llearning-GI
   (vector-ec
    (:list x (list-ec (s42: n 10000) (uniform-interval (fl- pi) pi)))
    (cons (vector x) (vector (sin x)))))
  (←
   Ltest-GI
   (vector-ec
    (:list x (list-ec (s42: n 10) (uniform-interval (fl/ (fl- pi) 2.0) pi)))
    (cons (vector x) (vector (sin x)))))
  (send r4 apprentissage Llearning-GI)
  (send r4 test Ltest-GI)
  (send r4 DL-plot-GI)
  (← Lplot-DL-main-GI (send r4 DL-data-2D-GI))
  (plot
   (list
    (points Lplot-sin #:sym 'fullcircle1 #:color "blue" #:label "y = sin(x)")
    (points
     Lplot-DL-main-GI
     #:sym
     'circle1
     #:color
     "red"
     #:label
     "neural sine"))))
