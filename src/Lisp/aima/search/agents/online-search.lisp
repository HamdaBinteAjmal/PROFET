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
;;;; Agents for online search

;;; Online search agents perceive just the current state s. Their knowledge of the
;;; state space is limited to:
;;;   (actions-fn s) which returns a list of actions allowed in s
;;;   (step-cost-fn s a s2) which returns the cost of doing a to get from s to s2
;;;   (goal-test-fn s) which returns true iff s is a goal state
;;; Note in particular that the agent CANNOT access the successors of a state.
;;; If it could, it could use that ability to construct the entire state space
;;; computationally, rather than actually visiting the states. Therefore,
;;; the generic agent builds a "map" -- stored in the table result(a,s) -- that
;;; records the state reached by each action executed in each state.
;;; Note also that the cost of an action can only be accessed if the agent knows 
;;; where the action leads. Hence, all value updates are done *after* the
;;; action has taken place.

(defun new-online-dfs-agent (&key problem)
  "Returns an online search agent that does depth-first search of the environment.
   Follows any unexplored edge from the current state;
   if none, backtracks the last unbacktracked edge traversal. [2e p 126]"
  (make-agent
   :program (let ((actions-fn #'(lambda (state) (actions problem state)))
		  (goal-test-fn #'(lambda (s) (goal-test problem s)))
		  (result (make-hash-table :test #'equalp))
		  (unexplored (make-hash-table :test #'equalp))    ;;; untried in s2
		  (unbacktracked (make-hash-table :test #'equalp)) ;;; s to reach s2
		  (s nil)  ;;; previous state
		  (a nil)) ;;; previous action
	      (defun online-dfs-agent (s2)
		(if (funcall goal-test-fn s2) (RETURN-FROM online-dfs-agent :stop))
		(if (not (in-hash-table? s2 unexplored))
		    (setf (gethash s2 unexplored) (funcall actions-fn s2)))
		(when s 
		  (setf (gethash (list a s) result) s2)
		  (push s (gethash s2 unbacktracked)))
		(if (null-hash-table-entry? s2 unexplored)
		    (if (null-hash-table-entry? s2 unbacktracked)
			(RETURN-FROM online-dfs-agent :stop)
		      (let ((s3 (pop (gethash s2 unbacktracked))))
			(setf a (find-if #'(lambda (b) 
					     (equalp (gethash (list b s2) result) s3))
					 (funcall actions-fn s2)))))
		  (setf a (pop (gethash s2 unexplored))))
		(setf s s2)
		(if (null a) :stop a))
	      #'online-dfs-agent)))





(defun new-random-walk-agent (&key problem)
  "Returns an online search agent that selects an action at random,
   first from among any unexplored actions, otherwise from all actions. [2e p 126]"
  (make-agent
   :program (let ((actions-fn #'(lambda (state) (actions problem state)))
		  (goal-test-fn #'(lambda (s) (goal-test problem s)))
		  (result (make-hash-table :test #'equalp))
		  (s nil)  ;;; previous state
		  (a nil)) ;;; previous action
	      (defun random-walk-agent (s2)
		(if (funcall goal-test-fn s2) (RETURN-FROM random-walk-agent :stop))
		(when s 
		  (setf (gethash (list a s) result) s2))
		(let* ((actions (funcall actions-fn s2))
		       (choices (or (remove-if #'(lambda (b) (gethash (list b s2) result)) actions) actions)))
		  (setf a (if choices (random-element choices) :stop)))
		(setf s s2)
		a)
	      #'random-walk-agent)))

;;; This version of LRTA* uses costs of states, stored in table H. With state costs, 
;;; we need one-step lookahead to select actions, so we need to know the graph.
;;; Because the selected action may have an unknown outcome that turns out
;;; to be a state already visited, the update rule for H(s) computes the
;;; minimum of its successor values (plus step costs) *after* the transition to s2.
;;; So if we go from bad A to new B, and then find ourselves back at A,
;;; that doesn't mean that B is bad too (as long as it has other unexplored successors).

(defun new-lrta*-agent (&key problem)
  "Returns an online search agent that runs the LRTA* algorithm. [2e p 128]"
  (make-agent
   :program (let ((actions-fn #'(lambda (state) (actions problem state)))
		  (step-cost-fn #'(lambda (s1 action s2) (step-cost problem s1 action s2)))
		  (goal-test-fn #'(lambda (s) (goal-test problem s)))
		  (h-cost-fn #'(lambda (s) (h-cost problem s)))
		  (result (make-hash-table :test #'equalp))
		  (H (make-hash-table :test #'equalp))
		  (s nil)  ;;; previous state
		  (a nil)) ;;; previous action
	      (defun lrta*-agent (s2)
		(if (funcall goal-test-fn s2) (RETURN-FROM lrta*-agent :stop))
		(if (not (in-hash-table? s2 H)) (setf (gethash s2 H) (funcall h-cost-fn s2)))
		(when s 
		  (setf (gethash (list a s) result) s2)
		  (setf (gethash s H) 
			(apply #'min (mapcar #'(lambda (b) 
						 (lrta*-cost s b (gethash (list b s) result)
							     H step-cost-fn h-cost-fn))
					     (funcall actions-fn s)))))
		(setf a (the-smallest-random-tie
			 #'(lambda (b) (lrta*-cost s2 b (gethash (list b s2) result) 
						   H step-cost-fn h-cost-fn))
			 (funcall actions-fn s2)))
		(setf s s2)
		a)
	      #'lrta*-agent)))
    
(defun lrta*-cost (s a s2 H step-cost-fn h-cost-fn)
  "Returns a new cost estimate for s given that action a goes to s2."
  (if (null s2) (funcall h-cost-fn s)
    (+ (funcall step-cost-fn s a s2) (gethash s2 H))))




