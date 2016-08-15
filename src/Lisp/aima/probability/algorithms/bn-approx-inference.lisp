;;; bn-approx-inference.lisp
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
;;; Approximate inference algorithms for Bayes net inference
;;;     rejection sampling
;;;     likelihood weighting
;;;     Markov chain Monte Carlo
;;;
;;; Each algorithm has an "ask-by-name" version that takes the *name*
;;; of the query variable and an "ask" version that takes the query
;;; variable itself.


;;;; Rejection sampling [2e p 513]

(defun prior-sample (bn)
  "Returns a sample event drawn according to the distribution specified by bn."
  (let ((x (alist->event nil bn)))
    (loop for bnode in (bn-nodes bn) do
      (set-value bnode x (sample bnode x)))
    x))

(defun rejection-sampling-ask-by-name (Xname e bn &optional (N 1000))
  "Returns an alist distribution for variable named Xname given event e,
   after generating N samples."
  (let ((X (bnode-by-name Xname bn)))
    (distribution->alist (rejection-sampling X e bn N) X)))

(defun rejection-sampling (X e bn N)
  "Returns a vector distribution for variable X given event e,
   after generating N samples. [2e p 513]"
  (when (node-value X e) (return-from rejection-sampling (point-distribution X e)))
  (let ((Ncounts (make-distribution (discrete-bnode-arity X) 0.0d0)))
    (loop for j from 1 to N do
	  (let ((xv (prior-sample bn)))
	    (when (consistent-events? e xv)
	      (increment-distribution Ncounts (node-value X xv) 1.0d0))))
    (dnormalize Ncounts t)))

(defun consistent-events? (e1 e2)
  "Return t iff events e1 and e2 agree (or are agnostic) on each variable value."
  (every #'(lambda (v1 v2) (or (null v1) (null v2) (equalp v1 v2))) e1 e2))



;;;; Likelihood weighting algorithm for approximate inference in Bayes nets [2e p 515]

(defun likelihood-weighting-ask-by-name (Xname e bn &optional (N 1000))
  "Returns an alist distribution for variable named Xname given event e,
   after generating N samples."
  (let ((X (bnode-by-name Xname bn)))
    (distribution->alist (likelihood-weighting X e bn N) X)))

(defun likelihood-weighting (X e bn N) 
  "Returns a vector distribution for variable X given event e,
   after generating N samples. [2e p 513]"
  (when (node-value X e) (return-from likelihood-weighting (point-distribution X e)))
  (let ((Ncounts (make-distribution (discrete-bnode-arity X) 0.0d0)) )
    (loop for j from 1 to N do
	  (multiple-value-bind (xv w) (weighted-sample bn e)
	    (increment-distribution Ncounts (node-value X xv) w)))
    (dnormalize Ncounts t)))

(defun weighted-sample (bn e &aux x_i-value)
  "Return a sampled event from bn, with e fixed, and its associated weight."
  (let ((x (copy-seq e))
	(w 1.0d0))
    (loop for X_i in (bn-nodes bn) do
      (if (setq x_i-value (node-value X_i e))
	  (setf w (* w (conditional-probability X_i x_i-value x)))
	(set-value X_i x (sample X_i x))))
    (values x w)))



;;;; Markov chain Monte Carlo for approximate inference in Bayes nets

(defun mcmc-ask-by-name (Xname e bn &optional (N 1000))
  "Returns an alist distribution for variable named Xname given event e,
   after generating N samples."
  (let ((X (bnode-by-name Xname bn)))
    (distribution->alist (mcmc X e bn N) X)))

(defun mcmc (X e bn N)
  "Returns a vector distribution for variable X given event e,
   after generating N samples. [2e p 517]"
  (when (node-value X e) (return-from mcmc (point-distribution X e)))
  (let ((Ncounts (make-distribution (discrete-bnode-arity X) 0.0d0))
	(Z (remove-if #'(lambda (Y) (node-value Y e)) (bn-nodes bn)))
	(state (copy-event e)))
    (loop for Z_i in Z do
      (set-value Z_i state (bnode-random-value Z_i)))
    (loop for j from 1 to N do
      (loop for Z_i in Z do
	(increment-distribution Ncounts (node-value X state) 1.0d0)
	(set-value Z_i state (random-from-discrete (MB-distribution Z_i state)))))
    (dnormalize Ncounts t)))

(defun MB-distribution (X_i state)
  "Return the Markov blanket distribution for X_i. [2e p 518 Eq. 14.11]"
  (dnormalize (bnode-distribution-over X_i
    #'(lambda (x_i-value)
	(set-value X_i state x_i-value)
	(* (conditional-probability X_i x_i-value state)
	   (product (bnode-children X_i)
	          #'(lambda (Y_j)
		      (conditional-probability Y_j (node-value Y_j state) state))))))))


