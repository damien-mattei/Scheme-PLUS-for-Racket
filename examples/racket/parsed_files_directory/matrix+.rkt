(module matrix racket
  (provide multiply-flomat-vector dim * ·)
  (require (rename-in
            flomat
            (repeat repeat-flomat)
            (shape shape-flomat)
            (transpose transpose-flomat)))
  (require Scheme+)
  (define-overload-existing-operator *)
  (define-overload-existing-operator · Scheme+/multiply)
  (define (multiply-flomat-vector M v) (flomat->vector (times M (matrix v))))
  (overload-existing-operator * multiply-flomat-vector (flomat? vector?))
  (overload-existing-operator · multiply-flomat-vector (flomat? vector?))
  (define (dim M)
    ($nfx$ shp <- (shape-flomat M))
    ($nfx$ lin <- (first shp))
    ($nfx$ colonne <- (second shp))
    (values lin colonne)))
