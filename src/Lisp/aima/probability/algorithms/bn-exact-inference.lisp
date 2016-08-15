;;; bn-exact-inference.lisp
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
;;; Exact inference algorithms:
;;;     inference by enumeration
;;;     variable elimination
;;;
;;; Each algorithm has an "ask-by-name" version that takes the *name*
;;; of the query variable and an "ask" version that takes the query
;;; variable itself.


;;;; Exact inference in general networks by enumeration [2e p 506]

(defun enumeration-ask-by-name (Xname e bn)
  "Returns an alist distribution for variable named Xname given event e."
  (let ((X (bnode-by-name Xname bn)))
    (distribution->alist (enumeration-ask X e bn) X)))

(defun enumeration-ask (X e bn)
  "Returns a vector distribution for variable X given event e. [2e p 506]"
  (let ((Q (make-distribution (discrete-bnode-arity X) 0.0))
        (e2 (copy-event e)))
    (when (node-value X e) (return-from enumeration-ask (point-distribution X e)))
    (map-values #'(lambda (x_i)
		    (set-value X e2 x_i)
		    (setf (elt Q x_i) (enumerate-all (bn-nodes bn) e2)))
		X)
    (dnormalize Q)))

(defun enumerate-all (vars e)
  "Return P(e) by summing over all uninstantiated vars."
  (if (null vars) 1.0d0
      (let* ((Y (first vars))
             (y-value (node-value Y e)))
        (if y-value (* (conditional-probability Y y-value e)
		       (enumerate-all (rest vars) e))
	  (prog1
	      (bnode-sum-over Y
		#'(lambda (y-value)
		    (set-value Y e y-value)
		    (* (conditional-probability Y y-value e)
		       (enumerate-all (rest vars) e))))
	    (unset-value Y e))))))




;;;; Exact inference in general networks by variable elimination [2e p 509]

(defstruct factor ;;; tabulated function over vars
  vars            ;;; specifies the variables on which function is defined
  table           ;;; array indexed by the variable values
  )

(defmethod print-object ((factor factor) stream)
  (format stream "f~A=~A" (mapcar #'bnode-name (factor-vars factor))
                     (factor-table factor))
  factor)

(defun elimination-ask-by-name (Xname e bn)
  "Returns an alist distribution for variable named Xname given event e."
  (let ((X (bnode-by-name Xname bn)))
    (distribution->alist (elimination-ask X e bn) X)))

;;; elimination-infer processes the variables in reverse order,
;;; storing factors for query and evidence variables and eliminating 
;;; hidden variables. At the end, only factors relating to the query
;;; variable remain.

(defun elimination-ask (X e bn)
  "Returns a vector distribution for variable X given event e. [2e p 509]"
  (when (node-value X e) (return-from elimination-ask (point-distribution X e)))
  (let ((factors nil) (vars (reverse (bn-nodes bn))))
    (loop for var in vars do
	  (push (construct-factor var e) factors)
	  (unless (or (eq var X) (node-value var e)) 
	    (setq factors (sum-out var factors)))) ;;; sum out if hidden
    (dnormalize (factor-table (pointwise-product factors)))))


(defun sum-out (var factors)
  "Returns the factor resulting from summing var out of the pointwise product
   of those factors that depend on var; appended with other independent factors."
  (let* ((dependent-factors 
	  (remove-if-not #'(lambda (factor) (member var (factor-vars factor)))
			 factors))
	 (independent-factors (list-difference factors dependent-factors)))
    (cons (sum-factors
	   (mapcar #'pointwise-product
		   (mapcar #'(lambda (value)
			       (mapcar #'(lambda (factor) 
					   (reduce-factor factor var value))
                                       dependent-factors))
                           (iota (discrete-bnode-arity var)))))
	  independent-factors)))

(defun reduce-factor (factor var value)
  "Returns a new factor for a fixed value of var.
   var is assumed to be one of factor's variables."
  (let ((dim (position var (factor-vars factor))))
    (unless dim (error "Cannot reduce ~A from f~A" 
	          (bnode-name var) (mapcar #'bnode-name (factor-vars factor))))
    (make-factor 
     :vars (remove var (factor-vars factor))
     :table (project-array (factor-table factor) (list dim) (list value)))))


(defun sum-factors (factors)
  "Returns a new factor that is the sum of a list of factors, each over the same variables."
  (cond ((null factors) (error "~%Cannot sum empty list of factors"))
        ((null (rest factors))
         (let ((factor (first factors)))
           (make-factor :vars (factor-vars factor)
                    :table (copy-array (factor-table factor)))))
        (t (let ((factor (sum-factors (rest factors))))
             (dadd-array (factor-table factor) (factor-table (first factors)))
             factor))))

;;; pointwise-product takes a list of factors and produces a "Cartesian" 
;;; or "pointwise" product. The idea is to make a new factor
;;; with variables given by the union of the variables of the input factors
;;; The table is then computed by iterating over the values of these
;;; variables and multiplying the corresponding values from the input tables.
;;; Implementation constructs an alist of values for the "new" variables and
;;; shares elements with similar alists for the existing factors.
;;; Then as we iterate over the "new" variable values, indices for
;;; the "old" tables are generated automatically. 

(defun pointwise-product (factors)
  "Returns a new factor that is the pointwise product of a list of factors."
  (let* ((vars (remove-duplicates (mappend #'factor-vars factors)))
         (dims (mapcar #'discrete-bnode-arity vars))
         (table (my-make-array dims)) 
         (result (make-factor :vars vars :table table))
         (alist (mapcar #'(lambda (v) (cons v nil)) vars))
         (old-alists (mapcar #'(lambda (factor)
                                 (mapcar #'(lambda (v) (assoc v alist))
                                         (factor-vars factor)))
                             factors)))
    (mapindices
     #'(lambda (values)
         (mapc #'(lambda (x v.x) (setf (cdr v.x) x)) values alist)
         (setf (apply #'aref table values)
               (multiply-factor-entries old-alists factors)))
     dims)
    result))

(defun multiply-factor-entries (alists factors)
  "Returns the product of the factor table entries indicated by 
   the variable assignments given in alists."
  (apply #'* (mapcar #'(lambda (alist factor)
                         (apply #'aref (factor-table factor)
				(mapcar #'cdr alist)))
                     alists factors)))


(defun construct-factor (var e)
  "Returns a new factor for var built from its CPT. Evidence variables are 
   fixed and projected out. Hence P(A|B,C=true) becomes f(A,B)."
  (let* ((all-vars (cons var (bnode-parents var)))
	 (fixed-vars (remove-if-not #'(lambda (v) (node-value v e)) all-vars))
	 (vars (list-difference all-vars fixed-vars))
	 (fixed-pos (mapcar #'(lambda (v) (position v all-vars)) fixed-vars))
	 (fixed-val (mapcar #'(lambda (v) (node-value v e)) fixed-vars))
	 (factor (make-factor :vars vars
			      :table (my-make-array 
				      (mapcar #'discrete-bnode-arity vars))))
	 (cpt (tabulated-bnode-cpt var)))
    (mapindices-project #'(lambda (i p)
			    (setf (apply #'aref (factor-table factor) p)
			      (aref (apply #'aref cpt (rest i)) (first i))))
			(cons (discrete-bnode-arity var) (array-dimensions cpt))
			fixed-pos fixed-val)
    factor))


