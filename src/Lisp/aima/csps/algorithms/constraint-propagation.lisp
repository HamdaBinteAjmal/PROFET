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

;;;; constraint-propagation.lisp

;;; The general idea, as explained beginning on [2e p 144], is to perform
;;; inference to deduce additional restrictions on values of variables,
;;; beyond those given in the current assignment. An example of this
;;; is the Maintaining Arc Consistency (MAC) algorithm, which applies
;;; arc consistency checking prior to each branch in the backtracking tree.

;;; The implementation illustrates a more general class of algorithms in which
;;; inference methods are applied prior to branching. (Simply call the inference
;;; methods where MAC calls AC-3.) The inference methods
;;; record their deductions (in all cases, removal of inconsistent values
;;; from variables' current domains) on the "deleted" field of
;;; the current state, just like add-assignment, and restore them after
;;; branching is complete. Thus, at each level of the tree, *two* lists are
;;; added to the "deleted" field.

(defun MAC (csp 
	    &optional (state (initial-csp-state csp)) 
	    &key (select-unassigned-variable #'next-variable)
	    (order-domain-values #'default-order))
  "Maintaining Arc Consistency [2e p 146]. Given a CSP, return a solution or :failure."
  (if (assignment-complete? state) (return-from MAC (current-assignment state)))
  ;; prepare a list for values deleted by constraint propagation method(s)
  (push nil (csp-state-deleted state)) 
  ;; now call propagation method (AC-3), which records deletions on csp-state-deleted
  (AC-3 state csp) 
  ;; now branch on remaining possible values of chosen variable
  (let* ((var (funcall select-unassigned-variable (variables csp) state csp))
	 (values (funcall order-domain-values (current-legal-values var state csp) var state csp)))
    (unless (null values) (incf *guesses* (1- (length values))))
    (loop for value in values do
      (setf state (add-assignment var value state csp))
      (let ((result (MAC csp state 
			 :select-unassigned-variable select-unassigned-variable
			 :order-domain-values order-domain-values)))
	(if (not (eq result :failure)) (return-from MAC result)))
      (setf state (delete-assignment var value state csp)))
    (restore-deleted-values state))
  :failure)

(defun AC-3 (state csp)
  "Modifies state to enforce arc consistency given current assignment. 
   Similar to algorithm on [2e p 146], but operates on state not csp."
  (let ((queue (all-constraint-arcs csp)))
    (loop while queue do
      (destructuring-bind (X_i X_j constraint) (pop queue)
	(if (remove-inconsistent-values X_i X_j constraint state csp)
	    (loop for X_k in (neighbors X_i csp) do
	      (pushnew (list X_k X_i (constraint-on X_k X_i csp)) queue :test #'equal)))))))

(defun remove-inconsistent-values (X_i X_j constraint state csp)
  "Remove inconsistent values for X_i from state; return true iff any removed."
  (let ((removed nil)
	(X_j-values (current-legal-values X_j state csp)))
    (loop for x in (current-legal-values X_i state csp) do
      (when (notany #'(lambda (y) (assignment-satisfies-constraint? `((,X_i . ,x) (,X_j . ,y)) constraint))
		  X_j-values)
	(delete-and-record-legal-value X_i x state)
	(setq removed t)))
    removed))
	
(defun all-constraint-arcs (csp)
  (mapcan #'(lambda (constraint)
	      (let ((variables (constraint-variables constraint)))
		(unless (= (length variables) 2) (error "CSP is not binary -- AC-3"))
		(list (list (first variables) (second variables) constraint)
		      (list (second variables) (first variables) constraint))))
	  (constraints csp)))