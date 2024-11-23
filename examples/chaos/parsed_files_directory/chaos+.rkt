(module chaos racket
  (require Scheme+
           (only-in srfi/41 stream-iterate)
           plot
           racket/gui/base
           colors
           (only-in racket/base (for for-racket)))
  ($nfx$ i ← 0+1i)
  ($nfx$ xws ← 500)
  ($nfx$ yws ← 500)
  (define (center-coords) (values (quotient xws 2) (quotient yws 2)))
  ($nfx$ (xo yo) ← (center-coords))
  ($nfx$ unit-axis-in-pixel ← 10)
  ($nfx$ no-pen ← (new pen% (style 'transparent)))
  ($nfx$ no-brush ← (new brush% (style 'transparent)))
  ($nfx$ blue-brush ← (new brush% (color "blue")))
  (define (draw-vect-point dc z-vect point-brush)
    (send dc set-pen no-pen)
    (send dc set-brush point-brush)
    ($nfx$ ga ← 3)
    ($nfx$ pa ← 3)
    ($nfx$ x ← ($bracket-apply$ z-vect 0))
    ($nfx$ y ← ($bracket-apply$ z-vect 1))
    ($nfx$ z ← x + i * y)
    ($nfx$ (x y) ← (to-screen-multi-values z))
    ($nfx$ x ← x - (quotient ga 2))
    ($nfx$ y ← y - (quotient pa 2))
    (send dc draw-ellipse x y ga pa))
  (define (to-screen-multi-values z0)
    ($nfx$ re ← (real-part z0))
    ($nfx$ im ← (imag-part z0))
    ($nfx$ xs ← re * unit-axis-in-pixel)
    ($nfx$ ys ← im * unit-axis-in-pixel)
    (values (round ($nfx$ xo + xs)) (round ($nfx$ yo - ys))))
  (define (norm x y) ($nfx$ x ** 2 + y ** 2))
  (define (remove-extrema x)
    (define gap 0.3)
    ($nfx$ gap + (1.0 - 2 * gap) * x))
  (define (f x y) (abs ($nfx$ (sin x) * (cos y))))
  (define (f-trunc x y) (remove-extrema (f x y)))
  (define (g x y) (abs (sin (sqrt ($nfx$ x ** 2 + y ** 2)))))
  (define (g-trunc x y) (remove-extrema (g x y)))
  (define (h x y) (abs ($nfx$ (cos ($nfx$ x + y)) * (sin ($nfx$ x - y)))))
  (define (h-trunc x y) (remove-extrema (h x y)))
  (define (red-value x y) (f x y))
  (define (green-value x y) (g x y))
  (define (blue-value x y) (h x y))
  (define (scalar-to-long-rainbow-rgb s)
    ($nfx$ a := (1 - s) / 0.2)
    ($nfx$ x := (inexact->exact (floor a)))
    ($nfx$ y := (inexact->exact (floor ($nfx$ 255 * (a - x)))))
    (case x
      ((0) (values 255 y 0))
      ((1) (values ($nfx$ 255 - y) 255 0))
      ((2) (values 0 255 y))
      ((3) (values 0 ($nfx$ 255 - y) 255))
      ((4) (values y 0 255))
      ((5) (values 255 0 255))
      (else
       (display "s=")
       (display s)
       (newline)
       (display "a=")
       (display a)
       (newline)
       (display "x=")
       (display x)
       (newline)
       (error "else in scalar-to-long-rainbow-rgb"))))
  (define (yellow-to-red s)
    ($nfx$ a := 1 - s)
    (define y (inexact->exact (floor ($nfx$ 255 * a))))
    (values 255 y 0))
  (define (max-list-norm-x-y ls)
    ($nfx$
     max-norm-x-y
     <-
     (norm ($bracket-apply$ (car ls) 0) ($bracket-apply$ (car ls) 1)))
    (if (null? (cdr ls))
      max-norm-x-y
      (max max-norm-x-y (max-list-norm-x-y (cdr ls)))))
  (define (chaos p q d x0 y0)
    (define a 2 * (cos (2 * pi * p / q)))
    (define-infix ksx (sqrt ((2 + a) / 2)))
    ($nfx$ ksy := (sqrt ((2 - a) / 2)))
    (stream-map
     (lambda (z)
       (match-let
        (((vector x y) z))
        (vector
         ($nfx$ (ksx / (sqrt 2)) * (x + y))
         ($nfx$ (ksy / (sqrt 2)) * ((- x) + y)))))
     (stream-iterate
      (lambda (z)
        (match-let
         (((vector x y) z))
         (vector ($nfx$ (a * x) + y + (d * x) / (add1 ($nfx$ x ** 2))) (- x))))
      (vector x0 y0))))
  (define *data*
    '((1 34 5 0.1 0 60000)
      (1 26 5 0.1 0 90000)
      (1 25 5 0.1 0 60000)
      (1 13 5 0.1 0 60000)
      (1 10 5 0.5 0 60000)
      (1 8 5 0.1 0 60000)
      (1 7 5 1 0 60000)
      (2 13 5 1 0 60000)
      (1 5 5 0.1 0 60000)
      (3 14 5 0.1 0 60000)
      (2 9 5 0.1 0 60000)
      (3 13 5 0.1 0 60000)
      (3 10 5 1 0 60000)
      (8 25 5 0.1 0 60000)
      (1 3 5 0.1 0 60000)
      (6 17 5 0.5 0 60000)
      (3 8 5 1 0 60000)
      (5 13 5 0.1 0 60000)
      (2 5 5 1 0 60000)
      (7 17 5 0.1 0 60000)
      (11 25 5 0.1 0 60000)
      (6 13 5 1 0 90000)
      (8 17 5 0.1 1 60000)))
  (define (compute-rgb-color-from-hsv point max-norm-x-y)
    ($nfx$ x <- ($bracket-apply$ point 0))
    ($nfx$ y <- ($bracket-apply$ point 1))
    (define h (sqrt ($nfx$ (norm x y) / max-norm-x-y)))
    (when (= h 1.0) (set! h 0.999))
    (hsv->color (hsv ($nfx$ 1 - h) 1 1)))
  (define (compute-rgb-color point max-norm-x-y)
    ($nfx$ x <- ($bracket-apply$ point 0))
    ($nfx$ y <- ($bracket-apply$ point 1))
    (define s (sqrt ($nfx$ (norm x y) / max-norm-x-y)))
    ($nfx$ (red-val green-val blue-val) <- (scalar-to-long-rainbow-rgb s))
    (make-object color% red-val green-val blue-val))
  (declare rv cv)
  (define lgt-*data* (length *data*))
  (define frm (make-vector lgt-*data*))
  (define my-canvas% (make-vector lgt-*data*))
  (define index 0)
  (define graphic-mode #t)
  (define plot-mode #t)
  (for-racket
   ((datum *data*))
   (display "index=")
   (display index)
   (newline)
   (match-let
    ((`(,p ,q ,d ,x ,y ,n) datum))
    ($nfx$ lst-points <- (stream->list (stream-take (chaos p q d x y) n)))
    (define max-norm-x-y (max-list-norm-x-y lst-points))
    (when graphic-mode
      ($nfx$
       ($bracket-apply$ frm index)
       ←
       (new
        frame%
        (label (format "Chaos+ ~a" index))
        (width xws)
        (height yws)))
      (send ($bracket-apply$ frm index) show #t)
      ($nfx$ ($bracket-apply$ my-canvas% index) ← (class canvas% (super-new)))
      ($nfx$
       cv
       ←
       (new
        ($bracket-apply$ my-canvas% index)
        (parent ($bracket-apply$ frm index))
        (paint-callback
         (λ (canvas dc)
           (send dc erase)
           (send dc set-pen "black" 1 'solid)
           ($nfx$
            unit-axis-in-pixel
            ←
            (min xws yws)
            /
            (2 * (sqrt max-norm-x-y)))
           (for-racket
            ((point lst-points))
            (define rgb-color (compute-rgb-color-from-hsv point max-norm-x-y))
            (define point-brush (new brush% (color rgb-color)))
            (draw-vect-point dc point point-brush)))))))
    (when ($nfx$ plot-mode and index > 19)
      (define plt
        (plot
         (map
          (λ (point)
            (points
             (list point)
             #:sym
             'fullcircle1
             #:color
             (compute-rgb-color-from-hsv point max-norm-x-y)))
          lst-points)))
      ($nfx$ rv <- (cons plt rv)))
    ($nfx$ index := index + 1)))
  (reverse rv))
