;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: problems.lisp
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
;;;; Defining Problems

(defstruct problem
  "A problem is defined by the initial state, successor function,
   goal test, and path cost (defined, in turn, by step cost). [2e p 62]"
  (initial-state (required)) ; A state in the domain
  )

;;; When we define a new subtype of problem, we need to specify eeither
;;; 1) a SUCCESSOR-FN method; or
;;; 2) ACTIONS and RESULT methods. 
;;; If one or the other is not done, an infinite loop will result!
;;; We may also need to define methods for GOAL-TEST, H-COST, and
;;; STEP-COST, but they have default methods which may be appropriate.
;;; In addition, there is a technicality: states and actions require
;;; hash keys, although a default is provided that often works (see below).

(defmethod successor-fn ((problem problem) state)
  "Return a list of (action . state) pairs that can be reached from this state."
  (mapcar #'(lambda (action) (cons action (result problem action state)))
	  (actions problem state)))

(defmethod actions ((problem problem) state) 
  "Return an list of actions possible in this state;
   use this default method only if successor-fn is independently defined!"
  (mapcar #'car (successor-fn problem state)))

(defmethod result ((problem problem) action state)
  "Return the state resulting from executing action in state;
   use this default method only if successor-fn is independently defined!"
  (cdr (assoc action (successor-fn problem state)
	      :test #'(lambda (a1 a2)
			(equalp (action-hash-key problem a1)
				(action-hash-key problem a2))))))

(defmethod sequence-result ((problem problem) action-sequence state)
  "Return the state resulting from executing action-sequence in state.
   Useful for checking that a proposed solution sequence achieves the goal."
  (if (null action-sequence) 
      state
    (sequence-result problem (rest action-sequence) 
		     (result problem (first action-sequence) state))))

(defmethod successor-states ((problem problem) state)
  "Return a list of states that can be reached from this state.
   This ignores actions, and is appropriate only for offline local search."
  (mapcar #'(lambda (action) (result problem action state))
	  (actions problem state)))



(defmethod goal-test ((problem problem) state)
  "Return true or false: is this state a goal state?"
  (declare-ignore state)
  (required))

(defmethod step-cost ((problem problem) state1 action state2)
  "The cost of going from state1 to state2 by taking action.
  This default method counts 1 for every action.  Provide a method for this if 
  your subtype of problem has a different idea of the cost of a step."
  (declare-ignore state1 action state2)
  1)

(defun path-cost (problem action-sequence &optional (state (problem-initial-state problem)) (cost 0))
  "Return the sum of step costs along the given action sequence."
  (if (null action-sequence) 
      cost
    (let ((next-state (result problem (first action-sequence) state)))
      (path-cost problem (rest action-sequence) next-state
		 (+ cost (step-cost problem state (first action-sequence) next-state))))))

(defmethod h-cost ((problem problem) state) 
  "The estimated cost from state to a goal for this problem.  
  If you don't overestimate, then A* will always find optimal solutions.
  The default estimate is always 0, which certainly doesn't overestimate."
  (declare (ignore state))
  0)

;;; The ability to generate a single random successor,
;;; rather than all successors at once, is important for
;;; local search algorithms in domains with large state 
;;; representations and/or many successors.
;;; For a specific domain, you need define only the random-action mrthod.

(defmethod random-successor ((problem problem) state)
  "Return (a . s) for a random legal action a and outcome s."
  (let ((action (random-action problem state)))
    (cons action (result problem action state))))

(defmethod random-successor-state ((problem problem) state)
  "Return the outcome s of a random legal action."
  (result problem (random-action problem state) state))

(defmethod random-action ((problem problem) state)
  "Return a random legal action in state; typically this
   method must be defined specially for each domain."
  (random-element (actions problem state)))




;;; Hash keys for states and actions.
;;; States are hashed in the graph search algorithms; both states and actions
;;; are hashed in the enumerated-problem class. Two states or actions represented
;;; by complex data structures may not be EQUALP if the representation
;;; is not canonical, so we must define hash keys for them. 
;;; For example, moves in backgammon can be written in any permutation
;;; and still be the "same" move. However, this situation is rare.
;;; In most cases, the state or action representation serves as its own hash key.

(defmethod state-hash-key ((problem problem) state)
  "Key to be used to hash the state; identical states must have EQUAL keys.
   Default is the state itself, i.e., assume a canonical representation."
  state)

(defmethod action-hash-key ((problem problem) action)
  "Key to be used to hash actions; identical actions must have EQUAL keys."
  action)



