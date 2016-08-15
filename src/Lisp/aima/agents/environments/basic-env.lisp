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
;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The basic environment simulator code

;;; This file defines the environment simulator function: RUN-ENVIRONMENT.  It
;;; provides a capability for placing agents in simulated environments
;;; and examining their behavior. 
;;; To define a new class of environments, simply define methods for 
;;; supplying percepts, updating the state, evaluating agent performance, etc.
;;; Each environment also has a stream that dictates where
;;; intermediate results are displayed; set to NIL if you don't want any display.

(defstruct environment
  "The world in which agents exist."
  (agents '())       ;; A list of the agents in the environment
  (step 0)           ;; The number of time steps simulated so far
  (max-steps 1000)   ;; Stop the simulation after this number
  (stream t)         ;; Stream to display output on
  (state nil)        ;; Current state of the environment; subtypes
                     ;; may add new slots to hold fixed information
                     ;; but should keep all variable state in this slot.
  (history (make-full-history))      ;; records all states and actions
  )

(defstruct full-history
  "A history of what has happened in the environment,
   or at least that part required to update the performance scores.
   By default, we keep everything."
  (states nil)             ;; list of states, most recent first
  (action-lists nil)       ;; lists of lists of actions (one per agent)
  )

(defstruct short-history
  "Just the most recent state and actions."
  state                    ;; most recent state
  actions                  ;; list of actions (one per agent)
  )

(defstruct no-history
  "For environments that need just look at the current state for evaluation.")


;;;; Top level functions

(defun run-environment (env)
  "Basic environment simulator.  It gives each agent its percept, gets an
  action from each agent, and updates the environment. It also keeps score
  for each agent, and optionally displays intermediate results."
  (initialize env)
  (display-environment env)
  (loop repeat (environment-max-steps env) do
    (incf (environment-step env))

    ;; Deliver percept and get action from each agent
    (loop for agent in (environment-agents env) do
	 (setf (agent-percept agent) (get-percept env agent))
	 (setf (agent-action agent) 
	       (funcall (agent-program agent) (agent-percept agent))))

    ;; Save away a copy of current state and all actions
    (update-history (environment-history env)
		    (copy-state (environment-state env))
		    (mapcar #'agent-action (environment-agents env)))

    ;; Execute the actions and otherwise update the world
    (update-state env)

    ;; Update the agent scores, then optionally display the current state
    (loop for agent in (environment-agents env) do
      (setf (agent-score agent) 
	    (performance-measure env agent 
	     (environment-history env) (environment-state env) (agent-score agent))))

    (display-environment env)
    (when (termination? env) (RETURN)))
  env)

(defun agent-trial (environment-generator n)
  "Run n environments with an identical agent in each, and average the scores.
   The environment-generator is a function that returns an environment.
   This could be a randomly generated new instance of a deterministic environment class,
   or the same instance of a stochastic environment class, or a combination."
  (let ((total 0))
    (loop for i from 1 to n do
	 (let* ((env (funcall environment-generator)))
	   (setf (environment-stream env) nil)
	   (run-environment env)
	   (incf total (agent-score (first (environment-agents env))))))
    (float (/ total n))))


;;;; Generic Functions that must be defined for each environment

;;; For each new type of environment you want to define, you will need a
;;; defstruct that inherits from (includes) ENVIRONMENT, and you will need
;;; to write new methods (or inherit existing methods) for each of the
;;; following eight functions.  Here are the ones that will change for each
;;; new environment:

(defmethod get-percept ((env environment) agent)
  "Return the percept for this agent."
  (declare-ignore env agent)
  nil)

(defmethod update-history ((history full-history) state actions)
  "Save the most recent state and actions in the environment history."
  (push state (full-history-states history))
  (push actions (full-history-action-lists history)))

(defmethod update-history ((history short-history) state actions)
  "Save the most recent state and actions in the environment history."
  (setf (short-history-state history) state)
  (setf (short-history-actions history) actions))

(defmethod update-history ((history no-history) state actions)
  "No environment history, so does nothing."
  (declare (ignore state actions)))

(defmethod update-state ((env environment))
  "Modify the environment, based on agents actions, etc.
   The default method simply invokes each action as a function call."
  (execute-agent-actions env))

(defmethod performance-measure ((env environment) agent history state previous-score)
  "Return a number saying how well this agent is doing."
  (declare (ignore agent history state))
  (1- previous-score))

(defmethod legal-actions ((env environment))
  "A list of the action operators that an agent can do.
   The environment simulator needs to know this, otherwise
   agents would be able to define and execute illegal actions."
  nil)

;;; Here are the ones that can usually be inherited:

(defmethod initialize ((env environment))
  "Called once to do whatever is necessary to set up the environment
  for running the simulation. More specific environments may do 
  ADDITIONAL initializations but will usually call this method too
  using call-next-method."
  (initialize-agents env)
  (setf (environment-step env) 0)
  env)

(defun initialize-agents (env)
  "Name the agents 1, 2, ... if they don't yet have a name.
   Initialize their percepts, actions, and scores."
  (loop for agent in (environment-agents env) do
    (setf (agent-percept agent) nil)
    (setf (agent-action agent) nil)
    (setf (agent-score agent) 0)
    (when (null (agent-name agent))
      (let ((i (+ 1 (position agent (environment-agents env))))
	    (body (agent-body agent)))
	(setf (agent-name agent) i)
	(when (and body (null (agent-body-name body)))
	  (setf (agent-body-name body) i))))))

(defmethod termination? ((env environment))
  "Return true if the simulation should end now."
  (> (environment-step env) (environment-max-steps env)))

(defmethod display-environment ((env environment))
  "Display the current state of the environment."
  ;; You probably won't need to specialize this, unless you want to do
  ;; a fancy graphical user interface
  (let ((stream (environment-stream env)))
    (when stream 
      (format stream "~&At Time step ~D:~%" (environment-step env))
      (when (> (environment-step env) 0)
	(loop for agent in (environment-agents env) do
	     (format stream 
		     "~&Agent ~A perceives ~A~%~6Tand does ~A~%"
		     agent (agent-percept agent)
		     (agent-action agent))))
      (display-environment-snapshot env))))

(defmethod display-environment-snapshot ((env environment))
  "Display a 'picture' of the current state of the environment."
  ;; This is what you will specialize 
  (print env (environment-stream env)))

(defmethod copy-state (state) 
  "Return a copy of state sharing no mutable structure.
   The default method just returns the state itself, so
   this method should be specialized as needed."
  state)



(defun execute-agent-actions (env)
  "Default for executing actions: Each agent gets to act in sequence.
  Specific environment types may have better ways of resolving conflicts."
  (loop for agent in (environment-agents env) do
       (let ((act (agent-action agent)))
	 (when (member (op act) (legal-actions env))
	   (apply (op act) env (agent-body agent) (args act))))))

(defmethod print-object ((env environment) stream)
  (format stream "#<~A; Step: ~D; State: ~A; Agents:~{ ~A~}>"
	  (type-of env) (environment-step env) (environment-state env)
	  (environment-agents env)))





