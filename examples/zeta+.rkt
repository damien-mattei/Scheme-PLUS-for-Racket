#lang reader SRFI-105
 
(module zeta racket

;; example in Scheme+ that plot the convergence of the ζ Riemann complex serie (without Analytic continuation)

(require Scheme+)
	

(require Scheme+/increment)


(require racket/gui/base)


{animation-mode ← #t}


{xws ← 1000} ;; X window size
{yws ← 800} ;; Y window size

{ywsp ← yws - 200} ;; Y window size for plot

; Make a frame by instantiating the frame% class
{frame0 ← (new frame% [label "Example"]
	              [width xws]
		      [height yws])}


; Make a static text message in the frame
{msg ← (new message% [parent frame0]
	             [label "No events so far..."])}
 
;; Make a button in the frame
(new button% [parent frame0]
             [label "Exit"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click")
			 (exit))])

{no-pen ← (new pen% [style 'transparent])}
{no-brush ← (new brush% [style 'transparent])}
{blue-brush ← (new brush% [color "blue"])}
{yellow-brush ← (new brush% [color "yellow"])}


{z ← 1.13+1.765i}

{unit-axis-in-pixel ← 200}


(define (draw-z-point dc)
  (send dc set-pen no-pen)
  (send dc set-brush blue-brush)
  {ga ← 8} ; grand axe / big axis
  {pa ← 8} ; petit axe / little axis
  {(x y) ← (to-screen-multi-values z)}
  {x ← x - (quotient ga 2)}
  {y ← y - (quotient pa 2)}
  (send dc draw-ellipse x y ga pa))

;; convert to screen coords
(define (to-screen z0)
  {re ← (real-part z0)}
  {im ← (imag-part z0)}
  {xs ← re * unit-axis-in-pixel}
  {ys ← im * unit-axis-in-pixel}
  (make-rectangular (round {xo + xs})
		    (round {yo - ys})))

(define (to-screen-multi-values z0)
  {re ← (real-part z0)}
  {im ← (imag-part z0)}
  {xs ← re * unit-axis-in-pixel}
  {ys ← im * unit-axis-in-pixel}
  (values (round {xo + xs})
	  (round {yo - ys})))



{nmax ← 10000000}

(define (draw-zeta dc)
  
  {zi ← 0}
  
  {flag-color ← #t}
  ;;(newline)
  (for ({n ← 1} {n ≤ nmax} {n ← n + 1})
       (if flag-color
	   (send dc set-pen "blue" 1 'solid)
	   (send dc set-pen "green" 1 'solid))
       {flag-color ← (not flag-color)}
       ;;(display "draw-zeta : n =") (display n) (newline)
       {zp ← 1.0 / n ᶻ} ; ** z}
       ;; (display "draw-zeta : z =") (display z) (newline)
       ;; (display "draw-zeta : zp =") (display zp) (newline)
       ;; (display "draw-zeta : zi =") (display zi) (newline)
       {zxtrm  ← zi + zp}
       ;;(display "draw-zeta : zxtrm =") (display zxtrm) (newline)
       {zie ← (to-screen zi)}
       ;;(display "draw-zeta : zie =") (display zie) (newline)
       {zxtrme ← (to-screen zxtrm)}
       ;;(display "draw-zeta : zxtrme =") (display zxtrme) (newline)
       {x0 ←  (real-part zie)}
       {y0 ←  (imag-part zie)}
       {x1 ←  (real-part zxtrme)}
       {y1 ←  (imag-part zxtrme)}

       ;; we plot only in the window
       (when {x0 ≥ 0 and x0 ≤ xws  and x1 ≥ 0 and x1 ≤ xws and
	      y0 ≥ 0 and y0 ≤ ywsp and y1 ≥ 0 and y1 ≤ ywsp}
	     (send dc draw-line
		   x0 y0
		   x1 y1))
       {zi ← zxtrm}))


(def (draw-zeta-multi-values dc)

  (when {(real-part z) ≤ 0.5}
    (return))
  
  {zi ← 0}
  {flag-color ← #t}
  {dmin ← 2} ;; minimal length  in pixel to draw line
  {n ← 1}
  (newline)
  
  (repeat
   
       (if flag-color
	   (send dc set-pen "blue" 1 'solid)
	   (send dc set-pen "green" 1 'solid))
       {flag-color ← (not flag-color)}
       ;;(display "draw-zeta-multi-values : n =") (display n) (newline)
       {zp ← 1.0 / n ᶻ} ; ** z} ;  try a power in superscript
       ;;{zp ← 1.0 / n ᶻ} ; does not works with syntax transformers only flag set, works in external parser
       {zxtrm  ← zi + zp}
       ;;(display "draw-zeta-multi-values : zxtrm =") (display zxtrm) (newline)
 
       {(x0 y0) ← (to-screen-multi-values zi)} 
       {(x1 y1) ← (to-screen-multi-values zxtrm)}

       ;; we plot only in the window
       ;; (when {x0 ≥ 0 and x0 ≤ xws  and x1 ≥ 0 and x1 ≤ xws and
       ;; 	      y0 ≥ 0 and y0 ≤ ywsp and y1 ≥ 0 and y1 ≤ ywsp}
       ;; 	     (send dc draw-line
       ;; 		   x0 y0
       ;; 		   x1 y1))

       (when {0 ≤ x0 ≤ xws  and 0 ≤ x1 ≤ xws and
	      0 ≤ y0 ≤ ywsp and 0 ≤ y1 ≤ ywsp}
	     (send dc draw-line
		   x0 y0
		   x1 y1))

       {len-line ← (line-length x0 y0 x1 y1)}
       {zi ← zxtrm}
       {n ← n + 1}
       
       until {len-line < dmin and n < nmax})

  (display "draw-zeta-multi-values : z =") (display z) (newline)
  (display "draw-zeta-multi-values : Riemann Zeta(z) = zi =") (display zi) (newline)

  )


;;(sqrt {(x1 - x0) ** 2 + (y1 - y0) ** 2 }))
  ;;(sqrt ((x1 - x0) ** 2 + (y1 - y0) ** 2 )))
(define+ (line-length x0 y0 x1 y1)
  (√ ((x1 - x0) ² + (y1 - y0) ²)))

;; (new button% [parent frame0]
;;              [label "Pause"]
;;              [callback (λ (button event) (sleep 5))])


;; {panel ← (new horizontal-panel% [parent frame0])}

;; (new button% [parent panel]
;;              [label "Left"]
;;              [callback (λ (button event)
;;                          (send msg set-label "Left click"))])
;; (new button% [parent panel]
;;              [label "Right"]
;;              [callback (λ (button event)
;;                          (send msg set-label "Right click"))])

{z-old ← z}

; Derive a new canvas (a drawing window) class to handle events
{my-canvas% ←
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
     
      {window-x ← (send event get-x)}
      {window-y ← (send event get-y)}
      (when animation-mode
	{z ← (ret-z window-x window-y)})
      
      ;;{str ← (string-append "(" (number->string window-x) " , " (number->string window-y) ")")}
      (when {z ≠ z-old}
	    {z-old ← z}
	    {str ← (number->string z)} 
	    (send msg set-label str)
	    (send cv refresh))
      
      )
    
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (send msg set-label "Canvas keyboard"))
    ; Call the superclass init, passing on all init args
    (super-new))}


{cv ← (new my-canvas% [parent frame0]
	   [paint-callback
	    (λ (canvas dc) ;; dc: Drawing Context
	      ;; cf. https://docs.racket-lang.org/draw/overview.html#%28tech._drawing._context%29
	      
	      ;; (send dc draw-rectangle
	      ;; 	    (random 10) 10   ; Top-left at (0, 10), 10 pixels down from top-left
	      ;; 	    30 10) ; 30 pixels wide and 10 pixels high
	      ;; (send dc draw-line
	      ;; 	    (random 10) 0    ; Start at (0, 0), the top-left corner
	      ;; 	    30 30) ; and draw to (30, 30), the bottom-right corner

	      (send dc erase)
	      (send dc set-pen "black" 1 'solid)
	      (draw-axes dc)
	      (draw-units dc)
	      (draw-z-point dc)

	      (if animation-mode
		  (draw-zeta-multi-values dc)
		  (draw-zeta dc))
	      
	      ;; (send dc set-scale 3 3)
	      ;; (send dc set-text-foreground "blue")
	      ;; (send dc draw-text "Don't Panic!" 0 0)
	      )])}




(define (center-coords)
  (values (quotient xws 2)
	  (quotient ywsp 2)))

{(xo yo) ← (center-coords)}


(define (draw-axes dc)
  (send dc draw-line ;; Ox
	0 yo xws yo)
  (send dc draw-line ;; Oy
	xo 0 xo ywsp))

(define (draw-units dc)
  ;;X
  {nun ← (quotient xo unit-axis-in-pixel)}
  (for ({n ← 1} {n ≤ nun} {n ← n + 1})
       {xu ← xo + n * unit-axis-in-pixel}
       (send dc draw-line
	     xu {yo - 3}
	     xu {yo + 3})
       {xum ← xo - n * unit-axis-in-pixel}
       (send dc draw-line
	     xum {yo - 3}
	     xum {yo + 3}))

  ;; Y
  {nuny ← (quotient yo unit-axis-in-pixel)}
  (for ({n ← 1} {n ≤ nuny} {n ← n + 1})
       {yu ← yo - n * unit-axis-in-pixel}
       (send dc draw-line
	     {xo - 3} yu
	     {xo + 3} yu)
       {yum ← yo + n * unit-axis-in-pixel}
       (send dc draw-line
	     {xo - 3} yum
	     {xo + 3} yum)))

(send frame0 show #t)

;; return the z complex from canvas plane where is the mouse pointer
(define (ret-z x y)
  {i ← +1i} ;; imaginaire pur, check +i does not works, only +1i or 0+1i
  {re ← x - xo}
  {re ← re / unit-axis-in-pixel}
  ;;{im ← (- {y - yo})} ;; or yo - y
  {im ←  - (y - yo)}
  {im ← im / unit-axis-in-pixel}
  (exact->inexact {re + i * im}))


) ; end of module

