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

;;;; Constraint satisfaction problems

;;; By default, each CSP is specified by a list of variables,
;;; the domains for each variable, and a list of constraints.
;;; Each constraint must specify the variables involved.
;;; A constraint may be defined as an enumerated-constraint,
;;; in which case the allowed tuples of values for the variables 
;;; are listed explicitly and a generic "allowed?" method
;;; is used to check any particular set of values for consistency.
;;; In many cases, the explicit listing of allowed tuples is painful
;;; and expensive, so constraint types can override the
;;; "allowed?" method by defining a method that checks consistency
;;; using lisp code. (See, for example, neq-constraints in nqueens-csp.lisp.)

(defstruct csp
  variables   ;;; List of variable names - need not be atoms!
  domains     ;;; Association list of (var . (value_1 ... value_d)) pairs
  constraints ;;; List of constraints
  )

;;;; Constraints

;;; Some CSPs have explicit constraints; the simplest is the enumerated
;;; constraint, which lists all allowable tuples, but they can take any form.
;;; E.g., csps/domains/nqueens-csp.lisp has functionally defined constraints.


(defstruct constraint
  variables        ;;; The variables for which this constraint is defined
    )

(defstruct (enumerated-constraint (:include constraint))
  tuples      ;;; The allowable tuples of values for the variables in the constraint
  )

(defmethod allowed? (vars values (constraint enumerated-constraint))
  (declare (ignore vars))
  (member values (enumerated-constraint-tuples constraint) :test #'equalp))

;;;; Methods for extracting information from CSPs

;;; For some large CSPs, it may be desirable not to list variables, 
;;; domains, and constraints explicitly. For this reason, we define
;;; access methods for all of these, and require the CSP-solving algorithms
;;; to use these rather than the stored fields of the CSP.
;;; In this way, any CSP can override the access methods if needed.

(defmethod variables ((csp csp))
  "Return a list of variables in the CSP."
  (csp-variables csp))

(defmethod variable-domains ((csp csp))
  "Return an alist of (var . (value_1 ... value_d)) pairs."
  (csp-domains csp))

(defmethod constraints ((csp csp))
  "Return a list of constraints in the CSP."
  (csp-constraints csp))

(defmethod variable-domain (var (csp csp))
  "Return the list of values the variable can take on."
  (cdr (assoc var (variable-domains csp) :test #'equal)))

(defmethod variable-constraints (var (csp csp))
  "Return the list of constraints for a given variable."
  (remove-if-not #'(lambda (constraint) (member var (constraint-variables constraint) :test #'equal))
		 (csp-constraints csp)))

(defmethod constraint-on (var1 var2 (csp csp))
  "Return the constraint that applies to var1 and var2 (assumed unique)."
  (find-if #'(lambda (constraint) (member var2 (constraint-variables constraint) :test #'equal))
	   (variable-constraints var1 csp)))

(defmethod neighbors (var (csp csp))
  "Return the list of variables constrained by a given variable."
  (remove var (remove-duplicates
	       (mappend #'constraint-variables (variable-constraints var csp))
	       :test #'equal)))



