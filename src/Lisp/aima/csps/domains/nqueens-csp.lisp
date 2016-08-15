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

;;;; The N-Queens Puzzle as a Constraint Satisfaction Problem

;;; nqueens-csp uses a functionally defined consistency check
;;; that overrides the default method for an enumerated CSP.


(defstruct (nqueens-csp (:include csp) (:constructor create-nqueens-csp))
  "The nqueens CSP with functionally defined constraints."
  (n 8)   ;;; board is n x n
  )

(defun make-nqueens-csp (&key (n 8))
  (create-nqueens-csp 
   :n n :variables (iota n 1)
   :domains (mapcar #'(lambda (var) (cons var (iota n 1))) 
		    (iota n 1))
   :constraints (mapcan #'(lambda (var) 
			    (mapcar #'(lambda (var2) 
					(make-nqueens-constraint :variables (list var var2)))
				    (iota (- n var) (1+ var))))
			(iota n 1))))

(defstruct (nqueens-constraint (:include constraint))
  "All nqueens constraints are identical except for the variables constrained.
   They simply serve as a hook for the allowed? method.")

(defmethod allowed? (vars values (constraint nqueens-constraint))
  "Return true iff values are allowed for this constraint."
  (let ((col (first vars)) (col2 (second vars)) (row (first values)) (row2 (second values)))
    (and (not (= row row2))
	 (not (= (abs (- row row2)) (abs (- col col2)))))))

	       

(defun print-nqueens-assignment (assignment)
  "Print out nqueens board given an assignment."
  (let ((n (length assignment)))
    (loop for j from n downto 1 do
      (format t "~%")
      (loop for i from 1 to n do (format t (if (eql (cdr (assoc i assignment)) j) "Q " ". ")))))
  assignment)
