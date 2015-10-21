;;; search/domains/poproblems.lisp

;;; A partially observable problem is a problem in which the state
;;; is not directly observed, but is accessible only through a 
;;; deterministic get-percept method.


(defstruct (poproblem (:include problem)))

(defmethod get-percept ((problem poproblem) state)
  "Given the current true state, return the percept received by the agent.
   By default, the percept is just the state (the observable case).
   This method should be specialized for any particular poproblem."
  state)

