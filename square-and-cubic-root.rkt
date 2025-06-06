(module square-and-cubic-root racket/base


  (provide  √ ∛)


  ;; (√ 2)
  ;; 1.4142135623730951
  (define √ sqrt)

  ;; (∛ 7)
  ;; 1.912931182772389
  (define ∛ (lambda (x) (expt x (/ 1 3))))

)
