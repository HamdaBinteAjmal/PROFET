;;; test-probability.lisp

(deftest probability
  "Test code for Bayes nets, temporal models."
  ((setq burglary-bn (load-bn "probability/domains/burglary.bn")))
  ((setq e (alist->event '((JohnCalls . true) (MaryCalls . true)) burglary-bn)))
  ((enumeration-ask-by-name 'Burglary e burglary-bn) (< 0.28416 (cdr (assoc 'true *)) 0.28418))
  ((elimination-ask-by-name 'Burglary e burglary-bn) (< 0.28416 (cdr (assoc 'true *)) 0.28418))
  ((rejection-sampling-ask-by-name 'Burglary e burglary-bn 100000) (< 0.2 (cdr (assoc 'true *)) 0.4))
  ((likelihood-weighting-ask-by-name 'Burglary e burglary-bn 100000) (< 0.2 (cdr (assoc 'true *)) 0.4))
  ((mcmc-ask-by-name 'Burglary e burglary-bn 100000) (< 0.2 (cdr (assoc 'true *)) 0.4))
  )


