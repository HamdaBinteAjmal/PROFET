;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: mdp.lisp
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
;;;; Definitions for Markov Decision Processes and Reinforcement Learning

(defstruct (mdp-environment (:include environment (history (make-short-history))))
  "An MDP-environment is driven by an MDP (Markov decision process),
  which (probabilistically) says what state to transition to for each action.
  The environment keeps a short history (s, a, s') so that rewards can be determined."
  (mdp (required))
  )

;;;; Generic Functions for MDP-Environments

(defmethod initialize ((env mdp-environment))
  "Get the initial state from the problem, and then initialize the 
   environment as usual.  Make sure there is one agent."
  (setf (environment-state env) (mdp-initial-state (mdp-environment-mdp env)))
  (call-next-method)
  (assert (= 1 (length (environment-agents env)))))

(defmethod get-percept ((env mdp-environment) agent)
  "The percept is the current state, the reward, and whether this is terminal.
   For s-type rewards, the *current* state is used."
  (declare (ignore agent))
  (let* ((mdp (mdp-environment-mdp env))
	 (old-state (short-history-state (environment-history env)))
	 (action (agent-action (first (environment-agents env))))
	 (state (environment-state env)))
    (make-mdp-percept
     :state state
     :reward (case (mdp-reward-type mdp)
	       (sas (reward mdp old-state action state))
	       (sa (reward mdp old-state action))
	       (s (reward mdp state)))
     :terminalp (terminal? mdp state))))

(defmethod legal-actions ((env mdp-environment))
  "A list of the actions that an agent can do."
  (actions (mdp-environment-mdp env) (environment-state env)))


(defmethod update-state ((env mdp-environment))
  "We update by transitioning probabilistically to a new state.
   Any action taken in a terminal state leads to a fictitious
   null state, which causes the environment to terminate."
  (let ((mdp (mdp-environment-mdp env))
	(state (environment-state env))
	(action (agent-action (first (environment-agents env)))))
    (setf (environment-state env) 
	  (if (terminal? mdp state) nil
	    (random-from-enumerated (results mdp action state))))))


(defmethod performance-measure ((env mdp-environment) agent history state previous-score)
  "Returns total rewards received by agent to date."
  (+ previous-score (reward (mdp-environment-mdp env)
			    (short-history-state history) (agent-action agent) state)))

(defmethod termination? ((env mdp-environment))
  "Stop when a terminal state is reached, or when an agent says stop."
  (or (null (environment-state env))
      (find :stop (environment-agents env) :key #'agent-action)))


;;;; Converting an MDP to an Environment

(defun mdp->environment (mdp &key (agents (list (new-simple-mdp-solving-agent :mdp mdp))) (stream t) (max-steps 1000))
  "Convert a mdp into an environment. By default, the agent will be an
   mdp-solving agent, but one could also use reinforcement learning agents."
  (make-mdp-environment :agents agents :mdp mdp :stream stream :max-steps max-steps))

