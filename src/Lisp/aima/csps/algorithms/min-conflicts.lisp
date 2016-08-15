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

;;;; min-conflicts.lisp

(defun min-conflicts (csp &optional (max-steps 1000))
  "A form of stochastic hill-climbing for CSPs, with successors selected to minimize 
   the number of conflicts. Returns a solution or failure. [2e p 151]"
  (let ((current (random-csp-state csp)))
    (loop for i from 1 to max-steps do
      (if (state-solves-csp? current csp) 
	  (return-from min-conflicts (current-assignment current)))
      (let* ((var (random-element 
		   (remove-if-not #'(lambda (var) (conflicted? var current csp))
				  (variables csp))))
	     (value (the-smallest-random-tie
		     #'(lambda (v) (conflicts var v current csp))
		     (remove (current-value var current)
			     (variable-domain var csp)
			     :test #'equal))))
	(replace-assignment var value current)))
    (return-from min-conflicts :failure)))


(defmethod random-csp-state ((csp csp))
  "Returns a random complete assignment for a given CSP."
  (make-csp-state 
   :unassigned nil
   :assignment (mapcar #'(lambda (var) 
			   (cons var (random-element (variable-domain var csp))))
		       (variables csp))))

(defmethod conflicted? (var (state csp-state) (csp csp))
  "Returns true iff var's current value is in conflict with current state."
  (some #'(lambda (constraint) (not (state-satisfies-constraint? state constraint)))
	(variable-constraints var csp)))

(defmethod conflicts (var value (state csp-state) (csp csp)
		      &aux (assignment (current-assignment state)))
  "Returns the number of conflicts var=value would have with rest of state."
  (count-if-not #'(lambda (constraint)
		    (assignment-satisfies-constraint? (acons var value assignment) constraint))
		(variable-constraints var csp)))

