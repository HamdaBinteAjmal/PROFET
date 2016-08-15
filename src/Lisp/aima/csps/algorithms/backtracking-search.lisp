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

;;;; CSP state 

;;; The basic information needed for the backtracking algorithm is
;;; the current assignment and list of unassigned variables.
;;; It is also helpful to keep track of which remaining values
;;; are legal for each unassigned variable, so that this is not recomputed
;;; when branching on a variable. Such information is also essential
;;; for efficient implementation of the MRV heuristic.
;;; The current-domains slot holds this information.
;;; We also keep the deleted values on the "deleted" slot so they can
;;; be restored when we backtrack. (The remaining values for assigned
;;; variables are kept too, so they are available when backtracking.)


(defstruct csp-state
  (assignment nil)  ;;; Association list of (var . value) pairs
  unassigned        ;;; List of unassigned variables
  current-domains   ;;; Association list of (var . (value_1 ... value_d)) pairs
  (deleted nil)  ;;; List of lists of (var . value) pairs, most recent list first
  )

(defmethod copy-state ((state csp-state))
  (make-csp-state :assignment (copy-alist (csp-state-assignment state))
		  :unassigned (copy-list (csp-state-unassigned state))
		  :current-domains (mapcar #'(lambda (v.l) (cons (car v.l) (copy-list (cdr v.l))))
					   (csp-state-current-domains state))
		  :deleted (copy-list (csp-state-deleted state))))

(defun initial-csp-state (csp &optional (assignment nil))
  "Return an initial state for the CSP; by default, has no variables assigned.
   If an initial assignment is provided, we have to process it to create the full state."
  (let ((state (make-csp-state :assignment nil
					 :unassigned (copy-list (variables csp))
					 :current-domains (copy-tree (variable-domains csp)))))
    (loop for (var . value) in assignment do (add-assignment var value state csp))
    state))

(defmethod assignment-complete? ((state csp-state))
  "Return t iff every variable has been assigned."
  (null (csp-state-unassigned state)))

(defmethod state-solves-csp? ((state csp-state) (csp csp))
  "Returns true iff current assignment satisfies all constraints in csp."
  (every #'(lambda (constraint) (state-satisfies-constraint? state constraint))
	 (constraints csp)))

(defmethod current-assignment ((state csp-state)) 
  "Return the current assignment in a form appropriate for a solution."
  (csp-state-assignment state))

(defmethod current-value (var (state csp-state))
  "Return the current assigned value of var, or nil."
  (afind var (csp-state-assignment state) :test #'equal))

(defmethod current-legal-values (var (state csp-state) csp)
  "Return all the allowable values for var."
  (declare (ignore csp))
  (cdr (assoc var (csp-state-current-domains state) :test #'equal)))

(defmethod delete-and-record-legal-value (var value (state csp-state))
  "Delete value from the current domain of var in state."
  (deletef value (cdr (assoc var (csp-state-current-domains state) :test #'equal)))
  (pushnew (cons var value) (first (csp-state-deleted state)) :test #'equal))

(defmethod add-legal-value (var value (state csp-state))
  "Add value to the current domain of var in state."
  (pushnew value (cdr (assoc var (csp-state-current-domains state) :test #'equal))
	   :test #'equal))

(defmethod current-assigned-variables ((state csp-state) csp)
  "Return the variables in csp that are not assigned in state."
  (declare (ignore csp))
  (mapcar #'car (csp-state-assignment state)))

(defmethod current-unassigned-variables ((state csp-state) csp)
  "Return the variables in csp that are not assigned in state."
  (declare (ignore csp))
  (csp-state-unassigned state))

(defmethod state-satisfies-constraint? ((state csp-state) (constraint constraint))
  "Returns true iff constraint is satisfied by current assignment in state."
  (assignment-satisfies-constraint? (current-assignment state) constraint))

(defmethod assignment-satisfies-constraint? (assignment (constraint constraint))
  "Returns true iff constraint is satisfied by assignment."
  (let* ((vars (constraint-variables constraint))
	 (values (mapcar #'(lambda (var) (afind var assignment :test #'equal)) vars)))
    (or (member nil values) (allowed? vars values constraint))))


(defmethod add-assignment (var value (state csp-state) csp)
  "Destructively modify the current state to add var=value."
  (push (cons var value) (csp-state-assignment state))
  (deletef var (csp-state-unassigned state) :test #'equal)
  (delete-inconsistent-values state var value csp))

(defmethod delete-assignment (var value (state csp-state) csp)
  "Destructively modify the current state to delete var=value."
  (declare (ignore value csp))
  (deletef var (csp-state-assignment state) :key #'car)
  (push var (csp-state-unassigned state))
  (restore-deleted-values state))

(defmethod replace-assignment (var value (state csp-state))
  "Destructively modify the current state to set var=value.
   Does not modify current domains - used only in local search."
  (setf (cdr (assoc var (csp-state-assignment state) :test #'equal)) value))

(defmethod delete-inconsistent-values ((state csp-state) var value csp
				       &aux (assignment (current-assignment state)))
  "Modify and return state by deleting values of variables connected to var
   that are inconsistent with var=value."
  (declare (ignore value))
  (push nil (csp-state-deleted state))   ;  prepare a new list of deleted values
  (loop for constraint in (variable-constraints var csp) do
    (let* ((vars (constraint-variables constraint))
	   (unassigned-vars (remove-if-not #'(lambda (v) (member v (csp-state-unassigned state) :test #'equalp))
					   vars)))
      (when (length=1 unassigned-vars)   ; Constraints must be OK if zero or >1 unassigned
	(let ((var2 (first unassigned-vars)))
	  (loop for value2 in (current-legal-values var2 state csp) do
	    (unless (assignment-satisfies-constraint? (acons var2 value2 assignment) constraint)
	      (delete-and-record-legal-value var2 value2 state)))))))
  state)

(defmethod restore-deleted-values ((state csp-state))
  "Modify and return state by restoring values of variables connected to var
   that were found to be inconsistent with var=value."
  (let ((deleted (pop (csp-state-deleted state))))
    (loop for (var2 . value2) in deleted do
      (add-legal-value var2 value2 state)))
  state)

(defmethod count-values-deleted-by (var value (state csp-state) csp &aux count)
  "Return the number of values deleted when var is assigned to value."
  (add-assignment var value state csp)
  (setq count (length (first (csp-state-deleted state))))
  (delete-assignment var value state csp)
  count)



;;;; General backtracking algorithm

;;; Specify keyword arguments for variable and value ordering heuristics.
;;; [Note: this version differs slightly from [2e p 142]: here we apply the
;;; domain ordering function to the legal values only, rather than all values.]

;;; The only *required* methods for applying backtracking to a CSP are
;;;     (variable-domain var csp) returns a list of initially legal values
;;;     (consistent? var value state csp)
;;; and these are defined already for generic CSPs.
;;; The generic implementation of "consistent?" and "variable-degree" also require
;;;     (variable-constraints var csp) returns a list of constraints for var in csp
;;; where each constraint has a "variables" slot and supports the method
;;;     (allowed? vars values constraint)
;;; Again, variable-constraints is defined for generic CSPs
;;; and allowed? is defined for enumerated constraints.

(defvar *guesses* 0)

(defun backtracking-search (csp 
			    &optional (assignment (initial-csp-state csp)) 
			    &key (select-unassigned-variable #'next-variable)
			         (order-domain-values #'default-order))
  "Backtracking search [2e p 142]. Given a CSP, return a solution or :failure."
  (if (assignment-complete? assignment) (return-from backtracking-search (current-assignment assignment)))
  (let* ((var (funcall select-unassigned-variable (variables csp) assignment csp))
	 (values (funcall order-domain-values (current-legal-values var assignment csp) var assignment csp)))
    (unless (null values) (incf *guesses* (1- (length values))))
    (loop for value in values do
      (setf assignment (add-assignment var value assignment csp))
      (let ((result (backtracking-search csp assignment 
					 :select-unassigned-variable select-unassigned-variable
					 :order-domain-values order-domain-values)))
	(if (not (eq result :failure)) (return-from backtracking-search result)))
      (setf assignment (delete-assignment var value assignment csp)))
    :failure))

(defun forward-checking-search (csp 
			    &optional (assignment (initial-csp-state csp)) 
			    &key (select-unassigned-variable #'minimum-remaining-values)
			         (order-domain-values #'default-order))
  "Forward checking search [2e p 144]. Given a CSP, return a solution or :failure.
   Identical to backtracking except that it fails immediately if any domain is empty.
   [Note: this behavior is also achieved by backtracking with the MRV heuristic.]"
  (if (assignment-complete? assignment) 
      (return-from forward-checking-search (current-assignment assignment)))
  (if (some #'(lambda (var) (null (current-legal-values var assignment csp))) (current-unassigned-variables assignment csp))
      (return-from forward-checking-search :failure))
  (let ((var (funcall select-unassigned-variable (variables csp) assignment csp)))
    (loop for value in (funcall order-domain-values (current-legal-values var assignment csp) var assignment csp) do
      (setf assignment (add-assignment var value assignment csp))
      (let ((result (forward-checking-search csp assignment 
					 :select-unassigned-variable select-unassigned-variable
					 :order-domain-values order-domain-values)))
	(if (not (eq result :failure)) (return-from forward-checking-search result)))
      (setf assignment (delete-assignment var value assignment csp)))
    :failure))

;;;; Generic consistency check for CSPs

(defmethod consistent? (var value state (csp csp)
			&aux (assignment (current-assignment state)))
  "Return t iff var=value is legal, given the constraints, in current state."
  (every #'(lambda (constraint)
	     (assignment-satisfies-constraint? (acons var value assignment) constraint))
	 (variable-constraints var csp)))


;;;; Variable and value ordering heuristics

(defmethod next-variable (all-vars (state csp-state) csp)
  "Given the current state, return the next variable to be assigned."
  (declare (ignore all-vars csp))
  (first (csp-state-unassigned state)))

(defmethod default-order (values var state csp)
  "Return the legal values in their default order."
  (declare (ignore var state csp))
  values)

(defmethod minimum-remaining-values (all-vars (state csp-state) csp)
  "Given the current state, return the next variable to be assigned.
   Choose the variable with the fewest current legal values."
  (declare (ignore all-vars))
  (the-smallest #'(lambda (var) (length (current-legal-values var state csp)))
		(csp-state-unassigned state)))

(defmethod minimum-remaining-values+degree (all-vars (state csp-state) csp)
  "Given the current state, return the next variable to be assigned.
   Choose variables with the fewest current legal values;
   among these, prefer the largest number of constraints on unassigned variables."
  (declare (ignore all-vars))
  (the-smallest #'(lambda (var) (+ (length (current-legal-values var state csp))
				   (/ 1 (+ 1 (variable-degree var state csp)))))
		(csp-state-unassigned state)))

(defmethod variable-degree (var (state csp-state) csp)
  "Return the number of constraints involving var that include unassigned variables."
  (count-if #'(lambda (constraint)
		(some #'(lambda (var2) (member var2 (csp-state-unassigned state)))
		     (constraint-variables constraint)))
	    (variable-constraints var csp)))

(defmethod least-constraining-value-order (values var state csp)
  "Return the legal values in ascending order of number of values ruled out."
  (sort (copy-list values) #'< :key #'(lambda (value) (count-values-deleted-by var value state csp))))




