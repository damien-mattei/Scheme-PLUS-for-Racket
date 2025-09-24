(module exo_retropropagationNhidden_layers_matrix_v3 racket
  (provide (all-defined-out))
  (require plot)
  (require (rename-in srfi/42 (: s42:)))
  (require (rename-in
            flomat
            (repeat repeat-flomat)
            (shape shape-flomat)
            (transpose transpose-flomat)))
  (require (rename-in Scheme+ (· ·bak)))
  (require "matrix+.rkt")
  (require (only-in racket/base (for for-racket)))
  (define-overload-existing-operator +)
  (define-overload-procedure uniform)
  (overload-existing-operator + vector-append (vector? vector?))
  (random-seed 7)
  (define (uniform-dummy dummy) (+ -1 (* (random) 2)))
  (define (uniform-interval inf sup)
    (<- gap (- sup inf))
    (+ inf (* gap (random))))
  (overload-procedure uniform uniform-dummy (number?))
  (overload-procedure uniform uniform-interval (number? number?))
  (define (σ z̃) (/ 1 (+ 1 (exp (- z̃)))))
  (define (der_tanh z z̃) (- 1 (** z 2)))
  (define (der_σ z z̃) (· z (- 1 z)))
  (define (der_atan z z̃) (/ 1 (+ 1 (** z̃ 2))))
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
       (activation_function_output_layer_derivative der_tanh))
      (<- lnc (vector-length nc))
      (field (z (vector-ec (:vector lg nc) (make-vector lg 0))))
      (display "z=")
      (display z)
      (newline)
      (field (z̃ (vector-ec (:vector lg nc) (make-vector lg 0))))
      (display "z̃=")
      (display z̃)
      (newline)
      (define-pointwise-unary uniform)
      (display "after define-pointwise-unary")
      (newline)
      (field
       (M
        (vector-ec
         (s42: n (- lnc 1))
         (.uniform!
          (zeros ($bracket-apply$ nc n + 1) (+ ($bracket-apply$ nc n) 1))))))
      (display "M=")
      (display M)
      (newline)
      (<- ᐁ (for/vector ((lg nc)) (make-vector lg 0)))
      (display "ᐁ=")
      (display ᐁ)
      (newline)
      (display "nbiter=")
      (display nbiter)
      (newline)
      (field (eror 0))
      (define
       (accepte_et_propage x)
       (when (≠ (vector-length x) (vector-length ($bracket-apply$ z 0)))
         (display "accepte_et_propage : Mauvais nombre d'entrées !")
         (newline)
         (exit #f))
       (<- ($bracket-apply$ z 0) x)
       (<- n (vector-length z))
       (declare z_1)
       (declare i)
       (for
        ((<- i 0) (< i (- n 2)) (<- i (+ i 1)))
        (<- z_1 (+ #(1) ($bracket-apply$ z i)))
        (<- ($bracket-apply$ z̃ i + 1) (· ($bracket-apply$ M i) z_1))
        (<-
         ($bracket-apply$ z i + 1)
         (vector-map
          activation_function_hidden_layer
          ($bracket-apply$ z̃ i + 1))))
       (<- z_1 (+ #(1) ($bracket-apply$ z i)))
       (<- ($bracket-apply$ z̃ i + 1) (· ($bracket-apply$ M i) z_1))
       (<-
        ($bracket-apply$ z i + 1)
        (vector-map
         activation_function_output_layer
         ($bracket-apply$ z̃ i + 1))))
      (define/public
       (apprentissage Lexemples)
       (<- ip 0)
       (declare x y)
       (for-racket
        ((it (range nbiter)))
        (when (= (% it 1000) 0) (display it) (newline))
        (<- err 0.0)
        (<- x (car ($bracket-apply$ Lexemples ip)))
        (<- y (cdr ($bracket-apply$ Lexemples ip)))
        (accepte_et_propage x)
        (<- i i_output_layer (- (vector-length z) 1))
        (<- ns (vector-length ($bracket-apply$ z i)))
        (for-racket
         ((k (range ns)))
         (<-
          ($bracket-apply$ ($bracket-apply$ ᐁ i) k)
          (- ($bracket-apply$ y k) ($bracket-apply$ ($bracket-apply$ z i) k)))
         (<- err (+ err (** ($bracket-apply$ ($bracket-apply$ ᐁ i) k) 2))))
        (<- err (* err 0.5))
        (when (= it (- nbiter 1)) (<- eror err))
        (<- მzⳆმz̃ activation_function_output_layer_derivative)
        (modification_des_poids
         ($bracket-apply$ M i - 1)
         ηₛ
         ($bracket-apply$ z i - 1)
         ($bracket-apply$ z i)
         ($bracket-apply$ z̃ i)
         ($bracket-apply$ ᐁ i)
         მzⳆმz̃)
        (<- მzⳆმz̃ activation_function_hidden_layer_derivative)
        (for-racket
         ((i (reverse (range 1 i_output_layer))))
         (<- nc1 (vector-length ($bracket-apply$ z i)))
         (<- ns1 (vector-length ($bracket-apply$ z i + 1)))
         (for-racket
          ((j (range nc1)))
          (<-
           ($bracket-apply$ ($bracket-apply$ ᐁ i) j)
           (for/sum
            ((k (range ns1)))
            (·
             (მzⳆმz̃
              ($bracket-apply$ ($bracket-apply$ z i + 1) k)
              ($bracket-apply$ ($bracket-apply$ z̃ i + 1) k))
             ($bracket-apply$ ($bracket-apply$ M i) k (+ j 1))
             ($bracket-apply$ ($bracket-apply$ ᐁ i + 1) k)))))
         (modification_des_poids
          ($bracket-apply$ M i - 1)
          ηₛ
          ($bracket-apply$ z i - 1)
          ($bracket-apply$ z i)
          ($bracket-apply$ z̃ i)
          ($bracket-apply$ ᐁ i)
          მzⳆმz̃))
        (<- ip (random (vector-length Lexemples)))))
      (define
       (modification_des_poids M_i_o η z_input z_output z̃_output ᐁ_i_o მzⳆმz̃)
       (<- (len_layer_output len_layer_input_plus1forBias) (dim M_i_o))
       (<- len_layer_input (- len_layer_input_plus1forBias 1))
       (for-racket
        ((j (range len_layer_output)))
        (for-racket
         ((i (range len_layer_input)))
         (<-
          ($bracket-apply$ M_i_o j (+ i 1))
          (-
           ($bracket-apply$ M_i_o j (+ i 1))
           (·
            (- η)
            ($bracket-apply$ z_input i)
            (მzⳆმz̃ ($bracket-apply$ z_output j) ($bracket-apply$ z̃_output j))
            ($bracket-apply$ ᐁ_i_o j)))))
        (<-
         ($bracket-apply$ M_i_o j 0)
         (-
          ($bracket-apply$ M_i_o j 0)
          (·
           (- η)
           1.0
           (მzⳆმz̃ ($bracket-apply$ z_output j) ($bracket-apply$ z̃_output j))
           ($bracket-apply$ ᐁ_i_o j))))))
      (define/public
       (test Lexemples)
       (display "Test des exemples :")
       (newline)
       (<- err 0)
       (declare entree sortie_attendue ᐁ)
       (for-racket
        ((entree-sortie_attendue Lexemples))
        (<- entree (car entree-sortie_attendue))
        (<- sortie_attendue (cdr entree-sortie_attendue))
        (accepte_et_propage entree)
        (printf
         "~a --> ~a : on attendait ~a"
         entree
         ($bracket-apply$ z (vector-length z) - 1)
         sortie_attendue)
        (newline)
        (<-
         ᐁ
         (-
          ($bracket-apply$ sortie_attendue 0)
          ($bracket-apply$ ($bracket-apply$ z (vector-length z) - 1) 0)))
        (<- eror (+ eror (** ᐁ 2))))
       (<- err (* err 0.5))
       (display "Error on examples=")
       (display eror)
       (newline))
      (define/public
       (DL-data-2D)
       (list-ec
        (s42: n 100)
        ($+>
         (<- xp (+ (/ (- pi) 2) (/ (· pi n) 100)))
         (accepte_et_propage (vector xp))
         (<-
          xp-DL
          ($bracket-apply$ ($bracket-apply$ z (vector-length z) - 1) 0))
         (vector xp xp-DL))))
      (define/public
       (DL-plot)
       (<- Lplot-DL (DL-data-2D))
       (plot (points Lplot-DL #:sym 'fullcircle1 #:color "red")))))
  (newline)
  (printf "################## NOT ##################")
  (newline)
  (<-
   r1
   (new
    ReseauRetroPropagation
    (nc #(1 2 1))
    (nbiter 5000)
    (ηₛ 10)
    (activation_function_hidden_layer σ)
    (activation_function_output_layer σ)
    (activation_function_hidden_layer_derivative der_σ)
    (activation_function_output_layer_derivative der_σ)))
  (<- Lexemples1 #((#(1) . #(0)) (#(0) . #(1))))
  (send r1 apprentissage Lexemples1)
  (send r1 test Lexemples1)
  (<- precision 100.0)
  (display "precision=")
  (display precision)
  (newline)
  (define (trunc x) (/ (round (* precision x)) precision))
  (define-pointwise-unary trunc)
  (define (trunc-matrix mt) (.trunc! mt))
  (<- M (get-field M r1))
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
  (<-
   r2
   (new
    ReseauRetroPropagation
    (nc #(2 3 1))
    (nbiter 250000)
    (ηₛ 10)
    (activation_function_hidden_layer σ)
    (activation_function_output_layer σ)
    (activation_function_hidden_layer_derivative der_σ)
    (activation_function_output_layer_derivative der_σ)))
  (<-
   Lexemples2
   #((#(1 0) . #(1)) (#(0 0) . #(0)) (#(0 1) . #(1)) (#(1 1) . #(0))))
  (send r2 apprentissage Lexemples2)
  (send r2 test Lexemples2)
  (<- M (get-field M r2))
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
  (printf "################## SINUS ##################")
  (newline)
  (<-
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
  (<-
   Llearning
   (vector-ec
    (:list x (list-ec (s42: n 10000) (uniform (- pi) pi)))
    (cons (vector x) (vector (sin x)))))
  (<-
   Ltest
   (vector-ec
    (:list x (list-ec (s42: n 10) (uniform (/ (- pi) 2) (/ pi 2))))
    (cons (vector x) (vector (sin x)))))
  (<-
   Lplot-sin
   (list-ec
    (s42: n 100)
    ($+> (<- xp (+ (/ (- pi) 2) (/ (· pi n) 100))) (vector xp (sin xp)))))
  (display "Lplot-sin =")
  (newline)
  (display Lplot-sin)
  (newline)
  (plot
   (points Lplot-sin #:sym 'fullcircle1 #:color "blue" #:label "y = sin(x)"))
  (send r3 apprentissage Llearning)
  (send r3 test Ltest)
  (send r3 DL-plot)
  (<- Lplot-DL-main (send r3 DL-data-2D))
  (plot
   (list
    (points Lplot-sin #:sym 'fullcircle1 #:color "blue" #:label "y = sin(x)")
    (points Lplot-DL-main #:sym 'circle1 #:color "red" #:label "neural sine")))
  (<- M (get-field M r3))
  (newline)
  (display "Matrix vector M=")
  (newline)
  (display M)
  (newline)
  (<- precision 1000.0)
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
  (<- Lplot-DL-trunc (send r3 DL-data-2D))
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
    (points
     Lplot-DL-main
     #:sym
     'circle1
     #:color
     "red"
     #:label
     "neural sine"))))
