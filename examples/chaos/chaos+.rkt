#lang reader SRFI-105 ; 110

(module chaos racket

(require Scheme+
	 (only-in srfi/41 stream-iterate)
	 plot
	 racket/gui/base
	 colors
	 (only-in racket/base [for for-racket]))


;; ← or <- or := are equivalent to 'define' (or 'set!' if the variable already exist !)
;; (define i +1i)
{i ← +1i} ;; imaginaire pur / i complex

;; (define xws 500)
{xws ← 500} ;; X window size
{yws ← 500} ;; Y window size

;; return the coordonates of the center of graph
(define (center-coords)
  (values (quotient xws 2)
	  (quotient yws 2)))

{(xo yo) ← (center-coords)} ; multiple values assignment (or definition !)

{unit-axis-in-pixel ← 10} ;; will change dynamically for each graph

{no-pen ← (new pen% [style 'transparent])}
{no-brush ← (new brush% [style 'transparent])}
{blue-brush ← (new brush% [color "blue"])}


;; draw a vector point
(def+ (draw-vect-point dc z-vect point-brush)
  (send dc set-pen no-pen)
  (send dc set-brush point-brush) ; blue-brush)
  ;; size of the ellipse / point in pixels
  {ga ← 3} ; grand axe / big axis
  {pa ← 3} ; petit axe / little axis
  {x ← z-vect[0]}
  {y ← z-vect[1]}
  ;;(display "before (z ← x + i * y)") (newline)
  (z ← x + i * y) ; complex number
  ;;(display "after (z ← x + i * y)") (newline)
  ((x y) ← (to-screen-multi-values z))
  {x ← x - (quotient ga 2)}
  (y ← y - (quotient pa 2))
  (send dc draw-ellipse x y ga pa))


;; convert in screen coordinates
(def+ (to-screen-multi-values z0) ; z0 is a complex number
  {re ← (real-part z0)}
  {im ← (imag-part z0)}
  {xs ← re * unit-axis-in-pixel}
  {ys ← im * unit-axis-in-pixel}
  (values (round (xo + xs))
	  (round (yo - ys))))


(define (norm x y)
  ;;{x ** 2 + y ** 2})
  {x ² + y ²})

;; remove a gap at minimum and maximum and return a value in [gap,1-gap]
(define (remove-extrema x)
  (define gap 0.3)
  {gap + (1.0 - 2 * gap) * x})

(define (f x y)
  (abs {sin(x) * cos(y)}))

(define (f-trunc x y)
  (remove-extrema (f x y)))

(def+ (g x y)
  ;;(abs (sin (sqrt (x ** 2 + y ** 2)))))
  (abs (sin (√ (x ² + y ²)))))

(define (g-trunc x y)
  (remove-extrema (g x y))) 

(define (h x y)
  ;;(abs {cos{x + y} * sin{x - y}}))
  ;;{abs {cos{x + y} * sin{x - y}}})
  ;;{abs(cos{x + y} * sin{x - y})} )
  ;;{abs (cos{x + y} * sin{x - y})} )
  {abs ((cos (x + y)) * (sin (x - y))) } )

;;(display "(h .2 .3) = ") (display (h .2 .3)) (newline)

(define (h-trunc x y)
  (remove-extrema (h x y)))

(define (red-value x y)
  (f x y))

(define (green-value x y)
  (g x y))

;;(display "h=") (display h) (newline)

(define (blue-value x y)
  ;;(display "h=") (display h) (newline)
  ;;(display "x=") (display x) (newline)
  ;;(display "y=") (display y) (newline)
  (h x y))

;; get a normalized scalar between [0,1] and return the values of red, green and blue of the color in the long rainbow
(def+ (scalar-to-long-rainbow-rgb s)
  {a := (1 - s) / 0.2} ; invert and group
  {x := (inexact->exact (floor a))} ; this is the integer part
  {y := (inexact->exact (floor (255 * (a - x))))} ; fractional part from 0 to 255
  (case x
    ((0) (values 255 y 0))
    ((1) (values (255 - y) 255 0))
    ((2) (values 0 255 y))
    ((3) (values 0 (255 - y) 255))
    ((4) (values y 0 255))
    ((5) (values 255 0 255))
    (else
     (display "s=") (display s) (newline)
     (display "a=") (display a) (newline)
     (display "x=") (display x) (newline)
     (error "else in scalar-to-long-rainbow-rgb"))))


(define (yellow-to-red s)
  {a := 1 - s}
  ;;(define+ y (inexact->exact (floor (255 * a))))
  {y <- (inexact->exact (floor (255 * a)))}
  (values 255 y 0))

  
;; get max coordinate of list of points (compare also x and y)
;; get into the vectors of the list to get x and y and find the max of list recursively
;; (define (max-list-xy ls)
;;   {max-xy <- (max (car ls)[0] (car ls)[1])} ; 0 indexed is x,1 indexed is y
;;   (if (null? (cdr ls))
;;       max-xy
;;       (max max-xy (max-list-xy (cdr ls)))))

(define (max-list-norm-x-y ls)
  {max-norm-x-y <- (norm (car ls)[0] (car ls)[1])} ; 0 indexed is x,1 indexed is y
  (if (null? (cdr ls))
      max-norm-x-y
      (max max-norm-x-y (max-list-norm-x-y (cdr ls)))))


(def+ (chaos p q d x0 y0)
  
  ;;(define a {2 * cos{2 * pi * p / q}}) ; or {2 * (cos {2 * pi * p / q})} or {2 * cos({2 * pi * p / q})}
  ;;(define a   2 * (cos (2 * pi * p / q)))
  (def a   2 * (cos (2 * pi * p / q)))
  (def+ ksx  (√ ((2 + a) / 2)) ) ;; (sqrt {(2 + a) / 2})) ; or sqrt{{2 + a} / 2}
  {ksy := (√ ((2 - a) / 2))}    ; (sqrt {(2 - a) / 2})} ; or (define ksy (sqrt {{2 - a} / 2}))
  
  (stream-map (lambda (z)
                (match-let (((vector x y) z))
                  (vector ((ksx / (√ 2)) * (x + y))
			  {(ksy / (√ 2)) * (- x + y)}))) ; here { } could be replaced by ( )
                  (stream-iterate (lambda (z)
                                    (match-let (((vector x y) z))
                                      (vector
                                       ;;((a * x) + y + (d * x) / (add1 (x ** 2))) ; infix left to right evaluation avoid extra parenthesis but is hard for humans
				       ((a * x) + y + (d * x) / (add1 (x ²)))
				       (- x))))
				  (vector x0 y0))))


(define *data* '((1  34 5 0.1 0 60000)
                 (1  26 5 0.1 0 90000)
                 (1  25 5 0.1 0 60000)
                 (1  13 5 0.1 0 60000)
                 (1  10 5 0.5 0 60000)
                 (1   8 5 0.1 0 60000)
                 (1   7 5 1   0 60000)
                 (2  13 5 1   0 60000)
                 (1   5 5 0.1 0 60000)
                 (3  14 5 0.1 0 60000)
                 (2   9 5 0.1 0 60000)
		 (3  13 5 0.1 0 60000)
                 (3  10 5 1   0 60000)
                 (8  25 5 0.1 0 60000)
                 (1   3 5 0.1 0 60000)
                 (6  17 5 0.5 0 60000)
                 (3   8 5 1   0 60000)
                 (5  13 5 0.1 0 60000)
                 (2   5 5 1   0 60000)
                 (7  17 5 0.1 0 60000)
                 (11 25 5 0.1 0 60000)
                 (6  13 5 1   0 90000)
                 (8  17 5 0.1 1 60000)))



;; split it for plot as it overload memory
;; (define *data* '((1  34 5 0.1 0 60000)
;;                  (1  26 5 0.1 0 90000)
;;                  (1  25 5 0.1 0 60000)
;;                  (1  13 5 0.1 0 60000)
;;                  (1  10 5 0.5 0 60000)
;;                  (1   8 5 0.1 0 60000)
;;                  (1   7 5 1   0 60000)
;;                  (2  13 5 1   0 60000)
;;                  (1   5 5 0.1 0 60000)
;;                  (3  14 5 0.1 0 60000)
;;                  (2   9 5 0.1 0 60000)))

;;(define *data* '((3  13 5 0.1 0 60000)
;;                  (3  10 5 1   0 60000)
;;                  (8  25 5 0.1 0 60000)
;;                  (1   3 5 0.1 0 60000)
;;                  (6  17 5 0.5 0 60000)
;;                  (3   8 5 1   0 60000)
;;                  (5  13 5 0.1 0 60000)
;;                  (2   5 5 1   0 60000)
;;                  (7  17 5 0.1 0 60000)
;;                  (11 25 5 0.1 0 60000)
;;                  (6  13 5 1   0 90000)
;;                  (8  17 5 0.1 1 60000)))

;; compute the RGB color from hsv
(def+ (compute-rgb-color-from-hsv point max-norm-x-y)

  ;; extract the coordonates
  {x <- point[0]}
  {y <- point[1]}

  ;; with various colormaps
  {h0 <- (√ ((norm x y) / max-norm-x-y))} ; normalized scalar
  ;;(display "compute-rgb-color-from-hsv : h=") (display h) (newline)
  
  (when (h0 = 1.0) ;; strange? hue can not be 1.0 when converting to RGB after: it causes a strange error:
                  ;; kw.rkt:1263:25: color-conversion: nonsense hue: 1, internal: 6
    ;;(display h)
    ;;(newline)
    (h0 <- 0.999))

  (hsv->color (hsv (1 - h0) 1 1)))



;; compute the RGB color
(define (compute-rgb-color point max-norm-x-y)

  ;; extract the coordonates
  {x <- point[0]}
  {y <- point[1]}

  ;; with various functions
  ;; (define+ red-val (inexact->exact (abs (round (255 * (red-value x y))))))
  ;; (define green-val (inexact->exact (abs (round {255 * (green-value x y)}))))
  ;; (define blue-val (inexact->exact (abs (round {255 * (blue-value x y)}))))

  ;; with various colormaps
  ;;(define s (sqrt {(norm x y) / max-norm-x-y})) ; normalized scalar
  {s <- (√ ((norm x y) / max-norm-x-y))} ; normalized scalar

  ;; rainbow
  {(red-val green-val blue-val) <- (scalar-to-long-rainbow-rgb s)} ;; multi-values assignment/definition

  ;; yellow to red
  ;;{(red-val green-val blue-val) <- (yellow-to-red s)}
  
  ;; create the color
  (make-object color% red-val
	              green-val
	              blue-val
		      ;;0.5 ; translucent
		      ))
  



;; (module+ main
;;   (parameterize ((plot-x-label #f)
;;                  (plot-y-label #f))
    
(declare rv cv) ; or (define rv '()) ;; declaring cv remove error : last statement is not an expression (but a definition)
(define lgt-*data* (length *data*))
(define frm (make-vector lgt-*data*))
(define my-canvas% (make-vector lgt-*data*))
(define index 0)


(define graphic-mode #t)
(define plot-mode #t)


(for-racket ([datum *data*]) ; 'for-racket' is the original 'for' of Racket but renamed

	    (display "index=") (display index) (newline)
	    
	    (match-let ((`(,p ,q ,d ,x ,y ,n) datum))
	      
	      {lst-points <- (stream->list
			      (stream-take
			       (chaos p q d x y) n))}

	      (define max-norm-x-y (max-list-norm-x-y lst-points)) ; maximum

	      (when graphic-mode ; no memory overloading in this mode
		;; Make a frame by instantiating the frame% class
		{frm[index] ← (new frame% [label (format "Chaos+ ~a" index)]
				          [width xws]
				          [height yws])}

		(send {frm[index]} show #t)

		;; Derive a new canvas (a drawing window) class to handle events
		{my-canvas%[index] ← (class canvas% ; The base class is canvas%
					    
					    ;; Define overriding method to handle mouse events
					    ;; (define/override (on-event event)
					    ;;   (send cv refresh))
					    
					    ;; Call the superclass init, passing on all init args
					    (super-new))}

		
		{cv ← (new my-canvas%[index]

			   [parent frm[index]] ; variable cv could be used with mouse event (but currently not)
			   
			   [paint-callback
			    
			    (λ (canvas dc) ;; dc: Drawing Context
			      ;; cf. https://docs.racket-lang.org/draw/overview.html#%28tech._drawing._context%29	
			      (send dc erase)
			      (send dc set-pen "black" 1 'solid)

			      ;; compute the units of graph
			      {unit-axis-in-pixel ← (min xws yws) / (2 * (√ max-norm-x-y))}
			      
			      ;; display the points
			      (for-racket ([point lst-points])
					  
					  ;; compute the RGB color
					  (define rgb-color (compute-rgb-color point max-norm-x-y))
					  ;;(define rgb-color (compute-rgb-color-from-hsv point max-norm-x-y))
					  
					  ;; create brush
					  (define point-brush (new brush% [color rgb-color]))
					  
					  (draw-vect-point dc point point-brush)))])})
		

	      ; warning memory overload
	      (when {plot-mode and index > 19} 
		;; multichrome plot
		(define plt
		  (plot
		   (map (λ (point)
			  (points (list point)
				  ;;#:alpha 0.4
				  #:sym 'fullcircle1
				  #:color (compute-rgb-color-from-hsv point max-norm-x-y) ;(compute-rgb-color point max-norm-x-y) ;; change the color of a point according to its co-ordinate
				  ))
			lst-points)))

		;; monochrome plot
		;; (define plt (plot
		;; 		   (points lst-points
		;; 			   #:alpha 0.4
		;; 			   #:sym 'fullcircle1
		;; 			   #:color "blue"))) ;; how to change the color of a point, according to its co-ordinate?
		
		{rv <- (cons plt rv)})

	      {index := index + 1} ; or you can use <- ,set!, etc ...

	      ) ;  end match-let
	    
	    ) ; end for-racket

(reverse rv) ; reverse the return value list as it has been build with cons

  ;; ) ; end parameterize

;;) ; end module main
	 


) ; end module chaos
