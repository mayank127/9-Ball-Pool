(define player%
  (class object%
    (init-field name)
    (field (score 0))
    (define/public (score-inc s)
      (set! score (+ score s)))
    (define/public (get-score)
      score)
    (define/public (get-name)
      name)))