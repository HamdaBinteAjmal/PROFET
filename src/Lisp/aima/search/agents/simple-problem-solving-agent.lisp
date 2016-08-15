;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: problem-solving.lisp
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
;;; A problem-solving environment is essentially an environment
;;; defined from a single problem. The percepts it presents to the agent
;;; are the states of the problem; the actions it accepts are the actions
;;; of the problem. Since the environment cannot supply the agent with
;;; the problem definition, the agent must come with one. Therefore,
;;; in the simplest case, an agent is constructed for a specific
;;; problem. It comes to the environment with the problem definition,
;;; solves it, then executes the solution.

(defun new-simple-problem-solving-agent (&key problem (search-algorithm #'a*-graph-search))
  "Given a search algorithm, return an agent that at the start searches
  for a solution, then executes the steps of the solution, then stops. [2e p61]"
  (make-agent
   :program (let ((seq :start))
	      (defun simple-problem-solving-agent (percept) 
		(declare (ignore percept)) ;; These agents ignore percepts!
		(when (eq seq :start)
		  (setf seq (funcall search-algorithm problem)))
		(if (or (null seq) (eq seq :failure)) :stop
		  (let ((action (first seq)))
		    (setf seq (rest seq))
		    action)))
	      #'simple-problem-solving-agent)))

(defun new-random-problem-solving-agent (&key problem)
  "Given a problem, return an agent that selects randomly from the available actions.
   Works for fully observable problems and partially observable problems in which the
   actions function can also accept the percept as input instead of the true state."
  (make-agent
   :program (let ()
	      (defun random-problem-solving-agent (percept) 
		(random-element (actions problem percept)))
	      #'random-problem-solving-agent)))
