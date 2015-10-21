;;; search/domains/poproblem-env.lisp

;;; Code for defining environments based on partially observable problems.
;;; In these environments, the agent's percept is obtained by applying the
;;; get-percept method of the underlying poproblem.

(defstruct (poproblem-environment (:include problem-environment))
  "An environment in which to solve problems.  The state of the environment
  is one of the states from the problem, starting with the initial state.")

(defmethod get-percept ((env poproblem-environment) agent)
  "Percept is generated using the percept function of the poproblem."
  (declare-ignore agent)
  (get-percept (problem-environment-problem env) (environment-state env)))

(defun poproblem->environment (poproblem &key (agents (list (new-random-problem-solving-agent :problem poproblem))))
  "Convert a partially observable problem into an environment, with a random agent."
  (make-poproblem-environment :agents agents :problem poproblem))

