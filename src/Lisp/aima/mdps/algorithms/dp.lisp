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

;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: dp.lisp

;;;; Basic dynamic programming routines for MDPs (Markov decision processes)

(defun value-iteration (mdp &optional (epsilon 0.000001)
			    &aux (states (states mdp))
			         (U (alist->hash-table
				     (mapcar #'(lambda (s) (cons s 0.0)) states)))
			         (U2 (copy-hash-table U))
			         delta
				 (gamma (mdp-gamma mdp))
				 (min-delta (if (= gamma 1.0) epsilon
					      (/ (* epsilon (- 1 gamma)) gamma))))
  "Returns the utility function (a hash table) for states in mdp. [2e p 621]"
  (loop do
    (setq delta 0)
    (rotatef U U2) ;;; switch contents; then we will overwrite U2
    (loop for s in states do
      (setf (gethash s U2)
	    (let ((actions (actions mdp s)))
	      (if actions (apply #'max
				 (mapcar #'(lambda (a) (one-step-backup s a U mdp))
					 actions))
		(if (eql (mdp-reward-type mdp) 's) (s-reward mdp s) 0.0d0))))
      (setq delta (max delta (abs (- (gethash s U2) (gethash s U))))))
    until (< delta min-delta))
  U)

(defun one-step-backup (s a U mdp)
  "Returns the expected utility of doing a in s given outcome utilities U."
  (loop for (s2 . p) in (results mdp a s) sum
	(* p (+ (reward mdp s a s2)
		(* (mdp-gamma mdp) (gethash s2 U))))))
  

(defun policy-iteration (mdp &aux (states (states mdp))
			          (U (alist->hash-table
				      (mapcar #'(lambda (s) (cons s 0.0)) states)))
				  (P (alist->hash-table
				      (mapcar #'(lambda (s) (cons s (random-element (actions mdp s))))
					      states)))
				  unchanged?)
  "Returns a policy (a hash table) for states in mdp. [2e p 624]
   Notice that this can fail if the initial random policy 
   is improper, i.e., if some states never lead to a terminal state."
  (loop do
    (setq U (policy-evaluation P U mdp))
    (setq unchanged? t)
    (loop for s in states do
      (let ((actions (actions mdp s)))
	(when (and actions (> (apply #'max
				     (mapcar #'(lambda (a) (one-step-backup s a U mdp))
					     actions))
			      (one-step-backup s (gethash s P) U mdp)))
	  (setf (gethash s P)
		(the-biggest #'(lambda (a) (one-step-backup s a U mdp))
			     (actions mdp s)))
	  (setf unchanged? nil))))
    until unchanged?)
  P)

(defun policy-evaluation (P U mdp 
			  &optional (epsilon 0.000001)
			  &aux (states (states mdp))
			       (U2 (copy-hash-table U))
			       delta
			       (gamma (mdp-gamma mdp))
			       (min-delta (if (= gamma 1.0) epsilon
					    (/ (* epsilon (- 1 gamma)) gamma))))
  "Returns the utility function (a hash table) for a fixed policy. [2e p 621]"
  (loop do
    (setq delta 0)
    (rotatef U U2) ;;; switch contents; then we will overwrite U2
    (loop for s in states do
      (setf (gethash s U2) 
	    (if (gethash s P) (one-step-backup s (gethash s P) U mdp)
	      (if (eql (mdp-reward-type mdp) 's) (s-reward mdp s) 0.0d0)))
      (setq delta (max delta (abs (- (gethash s U2) (gethash s U))))))
    until (< delta min-delta))
  U)



;;;; Methods for extracting policies from value functions and making choices

(defun value-iteration-policy (mdp)
  "Returns the optimal policy for mdp using value iteration."
  (optimal-policy (value-iteration mdp) mdp))

(defun optimal-policy (U mdp &aux (P (make-hash-table :test #'equal)))
  "Returns an optimal deterministic policy using one-step lookahead on U."
  (loop for s in (states mdp) do
    (setf (gethash s P) (the-biggest #'(lambda (a) (one-step-backup s a U mdp))
				      (actions mdp s))))
  P)

(defmethod policy-choice ((policy hash-table) state)
  "Return an action for state according to policy."
  (gethash state policy))


