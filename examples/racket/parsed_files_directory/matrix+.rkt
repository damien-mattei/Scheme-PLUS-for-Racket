(module matrix racket
  (provide multiply-flomat-vector dim * ·)
  (require (rename-in
            flomat
            (repeat repeat-flomat)
            (shape shape-flomat)
            (transpose transpose-flomat))
           (except-in Scheme+ ×))
  (define (multiply-flomat-vector M v) (flomat->vector (× M (matrix v))))
  (overload-operator * multiply-flomat-vector (flomat? vector?))
  (overload-operator
   ·
   (only-in Scheme+/multiply ·)
   multiply-flomat-vector
   (flomat? vector?))
  (define (dim M)
    (<- shp (shape-flomat M))
    (<- lin (first shp))
    (<- colonne (second shp))
    (values lin colonne)))
