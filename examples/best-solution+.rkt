#reader SRFI-105


(module best-solution racket/base

	(provide best-sol
		 best-sol3)

	(require Scheme+)

;; (best-sol 100 '(101) '(90 4 3))
;; (101)

(def (best-sol t L1 L2)
  ;; (display "L1=")
  ;; (display L1)
  ;; (newline)
  ;; (display "L2=")
  ;; (display L2)
  ;; (newline)
  {s1 := (apply + L1)}
  {s2 := (apply + L2)}
  {d1 := (abs (t - s1))}
  {d2 := (abs (t - s2))}
  (when {d1 = d2} ; same distance solutions
    (if {(length L1) < (length L2)} ; return the shortest solution
	(return L1)
	(return L2)))
  (if {d1 < d2} ; return the best solution
      L1
      L2))


(define (best-sol3 t L1 L2 L3)
  ;; (display "best-sol3") (newline)
  ;; (display "t=") (display t) (newline)
  ;; (display "L1=")
  ;; (display L1)
  ;; (newline)
  ;; (display "L2=")
  ;; (display L2)
  ;; (newline)
  ;; (display "L3=")
  ;; (display L3)
  ;; (newline)
  {L22 := (best-sol t L2 L3)}
  (best-sol t L1 L22))

)
