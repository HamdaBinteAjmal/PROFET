

(defstruct (random-walk-trap-problem (:include problem (initial-state 1)))
  "The random walk trap environment expressed as a search problem. [2e p 127]"
  (n 11)   ;;; Number of states - must be odd. Numbered from left to right, 1 to n.
  )

;;;; Random-Walk-Trap domain functions

(defmethod actions ((problem random-walk-trap-problem) state)
  "Return a list of actions in current state."
  (if (oddp state)  ;;; in top row of states
      (let ((n (random-walk-trap-problem-n problem)))
	(append (when (< state n) '(+2))
		(when (> state 1) '(-1 -2))))
    '(-1)))

(defmethod result ((problem random-walk-trap-problem) action state)
  (+ state action))

(defmethod goal-test ((problem random-walk-trap-problem) state)
  "Is this a goal state?"
  (= state (random-walk-trap-problem-n problem)))

