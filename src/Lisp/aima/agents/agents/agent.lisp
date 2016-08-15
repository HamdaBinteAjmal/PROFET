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
;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig


;;;; Definition of agent bodies

(defstruct agent-body
  "Agent bodies are part of the environment and record the agent's physical state and properties."
  (alive? t) 
  (name nil))

;;;; Definition of basic AGENT type and associated functions

;;; An agent is something that perceives and acts.  As such, each agent has a
;;; slot to hold its current percept, and its current action.  The action
;;; will be handed back to the environment simulator to perform (if legal).
;;; Each agent also has a slot for the agent program, and one for its score
;;; as determined by the performance measure.

(defstruct agent
  "Agents take actions (based on percepts and the agent program) and receive
  a score (based on the performance measure).  An agent has a body which can
  take action, and a program to choose the actions, based on percepts."
  (program #'nothing)			; fn: percept -> action
  (body (make-agent-body))
  (score 0)
  (percept nil)
  (action nil)
  (name nil))

(defstruct (ask-user-agent (:include agent (program 'ask-user)))
  "An agent that asks the user to type in an action.")

(defun ask-user (percept)
  "Ask the user what action to take."
  (format t "~&Percept is ~A; action? " percept)
  (read))

(defmethod print-object ((agent agent) stream)
  "Agents are printed by showing their name (or body) and score."
  (format stream "[~A = ~D]" (or (agent-name agent) (agent-body agent))
	  (agent-score agent)))

;;;; A simple table-driven agent

(defun new-table-driven-agent-agent (&key table)
  "Given a hash table index by percept sequence, return an agent
   that uses the table to decide what to do. [2e p 45]"
  (make-agent
   :program (let ((percepts nil))
	      #'(lambda (percept) 
		  (setf percepts (nconc percepts percept))
		  (let ((action (gethash percepts table)))
		    action)))))


;;;; Design Decision Notes

;; We have decided that the agent and its body are two separate objects.
;; We could have combined the agent and its body into one object.  But then
;; each new type of agent would need to inherit from both AGENT and some
;; body type, such as OBJECT.  This would require multiple inheritance,
;; which is part of CLOS, but is not compatible with our decision
;; to use defstruct rather than defclass. (Plus, it's messy.)  We think that
;; separating agents from agent-bodies is a good thing for this
;; implementation.  (Just don't take it too far and assume that this says
;; anything about the mind-body problem.)
;;
;; We could have defined the agent program as a generic function on the
;; agent.  But that would mean that everytime you want to try out a
;; slightly different agent program, you would need to define a new type.  You
;; would also need to hold state information in slots rather than in local
;; variables, and we would need to have some kind of discipline to ensure
;; that the slots representing intermediate state could be accessed and
;; modified by the agent program, while the slot representing, say, the score
;; could not.  All in all, a closure (a different one for every agent) is
;; exactly what we want in an agent program: a closure encapsulates local
;; state and behavior, and can access only its arguments and closed-over
;; variables.

