;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: prob-solve.lisp
;;; 
 ;; PROFET Copyright 2015 (c) Data Mining and Machine Learning Group,
 ;; National University of Ireland Galway.  
 ;; This file is a part of AIMA Framework   
 ;;
 ;; Licensed under the Apache License, Version 2.0 (the "License");
 ;; you may not use this file except in compliance with the License.
 ;; You may obtain a copy of the License at
 ;;
 ;;      http://www.apache.org/licenses/LICENSE-2.0
 ;;
 ;; Unless required by applicable law or agreed to in writing, software
 ;; distributed under the License is distributed on an "AS IS" BASIS,
 ;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 ;; See the License for the specific language governing permissions and
 ;; limitations under the License.
;;;
;;; A problem-solving environment is defined by a single problem
;;; instance, including the initial state, state space, and actions.
;;; All the standard environment functions can be defined easily
;;; using the problem definition itself. The standard way to make
;;; a problem-solving environment is first to make a problem and then
;;; to convert it to an environment using problem->environment.

(defstruct (problem-environment (:include environment (history (make-short-history))))
  "An environment in which to solve problems.  The state of the environment
  is one of the states from the problem, starting with the initial state."
  (problem (required)))

(defmethod get-percept ((env problem-environment) agent)
  "All agents can access the complete state of the environment."
  (declare-ignore agent)
  (environment-state env))

;;; update-history is inherited directly for short-history type

(defmethod update-state ((env problem-environment))
  "Set the state to the result of executing the agent's action."
  (setf (environment-state env) 
	(result (problem-environment-problem env)
		(agent-action (first (environment-agents env)))
		(environment-state env))))

(defmethod performance-measure ((env problem-environment) agent history state previous-score)
  "Agent's score is updated by the step cost of the action it took."
  (- previous-score (step-cost (problem-environment-problem env)
			       (short-history-state history) (agent-action agent) state)))

(defmethod legal-actions ((env problem-environment))
  "A list of the action operators that an agent can do."
  (actions (problem-environment-problem env) (environment-state env)))

(defmethod initialize ((env problem-environment))
  "Get the initial state from the problem, and then initialize the 
   environment as usual. Make sure there is one agent."
  (let ((problem (problem-environment-problem env)))
    (setf (environment-state env) (problem-initial-state problem)))
  (call-next-method)
  (assert (= 1 (length (environment-agents env)))))

(defmethod termination? ((env problem-environment))
  "Stop when the problem is solved, or when an agent says stop."
  (or (goal-test (problem-environment-problem env) 
		 (environment-state env))
      (find :stop (environment-agents env) :key #'agent-action)))

;;;; Converting a Problem to an Environment

(defun problem->environment (problem &key (agents (list (new-simple-problem-solving-agent :problem problem))))
  "Convert a problem into an environment.  Then we can pass the environment
  to RUN-ENVIRONMENT, and the agent will search for a solution and execute it.
  By default, the agent will be a problem-solving agent, but one could also
  use online search agents."
  (make-problem-environment :agents agents :problem problem))

