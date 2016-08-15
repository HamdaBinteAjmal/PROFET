;;; statistics.lisp
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
;;; Properties of data and methods for estimating probability models from data

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elementary statistical quantities: batch, running, weighted-running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mean (numbers)
  "Numerical average (mean) of a list of numbers."
  (if numbers (/ (sum numbers) (length numbers)) 0))

(defun median (numbers)
  "Returns the median of a list of numbers. If the list has even length,
   returns the average of the middle two numbers. This uses sort, so is
   not as efficient as it should be."
  (let ((sorted (sort (copy-list numbers) #'<))
	(n (length numbers)))
    (cond ((zerop n) 0)
	  ((oddp n) (nth (floor n 2) sorted))
	  (t (/ (+ (nth (1- (/ n 2)) sorted) (nth (/ n 2) sorted)) 2)))))

(defun variance (numbers &aux (n (length numbers)) (mu (mean numbers))
			      (s 0.0d0))
  (cond ((< n 2) 0.0d0)
	(t (loop for x in numbers do
		 (incf s (square (- x mu))))
	   (/ s (1- n)))))

(defun covariance (xs ys &aux (n (length xs)) (mu_x (mean xs)) (mu_y (mean ys))
			      (s 0.0d0))
  (cond ((< n 2) 0.0d0)
	(t (mapc #'(lambda (x y) (incf s (* (- x mu_x) (- y mu_y)))) xs ys)
	   (/ s (1- n)))))

;;; running mean and variance - compute for n+1 data points given n, new x.
;;; running covariance is a simple generalization of running variance.

;;; univariate case

(defun running-mean (x mu_n n)
  (if (zerop n)
      x
    (/ (+ (* n mu_n) x) (1+ n))))

(defun running-variance (x variance_n mu_n n)
  (if (zerop n) 
      0.0d0
    (+ (* (/ (1- n) n) variance_n) 
       (* (/ 1 (1+ n)) (square (- x mu_n))))))

;;; or equivalently 
;(defun running-variance (x variance_n mu_n+1 n)
;  (if (zerop n) 
;      0.0d0
;    (+ (* (/ (- n 1) n) variance_n) 
;       (* (/ 1 n) (/ (1+ n) n) (square (- x mu_n+1))))))

(defun running-covariance (x y covariance_n mu_xn mu_yn n)
  (if (zerop n) 
      0.0d0
    (+ (* (/ (1- n) n) covariance_n) 
       (* (/ 1 (1+ n)) (* (- x mu_xn) (- y mu_yn))))))

;;; Weighted updating for exponential forgetting
;;; The update for means is simple. For variance, the
;;; coefficients don't add to 1 so there is no simple variance weight.

(defun weighted-running-mean (x mu_n weight)
  (+ (* weight mu_n) (* (- 1 weight) x)))

(defun weighted-running-variance (x variance_n mu_n weight)
  (+ (* (/ (+ weight weight -1.0) weight) variance_n) 
     (* (- 1 weight) (square (- x mu_n)))))

(defun weighted-running-covariance (x y covariance_n mu_xn mu_yn weight)
  (+ (* (/ (+ weight weight -1.0) weight) covariance_n) 
     (* (- 1 weight) (* (- x mu_xn) (- y mu_yn)))))

;;; or equivalently 
;(defun weighted-running-variance (x variance_n mu_n+1
;				    &optional (weight *mu-weight*))
;    (+ (* (/ (+ weight weight -1.0) weight) variance_n) 
;       (* (/ (- 1 weight) (square weight)) (square (- x mu_n+1)))))

;;; multivariate case 
;;; x, mu_n are d x 1 arrays, covariance_n is an d x d array
;;; covariance matrix is updated in place.

(defun running-means (x mu_n n &aux (d (array-dimension x 0))
			            (mu_n+1 (make-array (list d 1))))
  (cond  ((zerop n)
	  (loop for i from 0 to (1- d) do 
		(setf (aref mu_n+1 i 0) (aref x i 0)))
	  mu_n+1)
	 (t (loop for i from 0 to (1- d) do 
		  (setf (aref mu_n+1 i 0)
			(running-mean (aref x i 0) (aref mu_n i 0) n)))
	    mu_n+1)))
	    
	      

(defun running-covariances (x covariance_n mu_n n 
			   &aux (d (array-dimension x 0)))
  (cond ((zerop n)
	 (make-array (list d d) :initial-element 0d0))
	(t (loop for i from 0 to (1- d) do 
	     (loop for j from 0 to (1- d) do 
	       (setf (aref covariance_n i j)
		     (running-covariance (aref x i 0) (aref x j 0) 
					 (aref covariance_n i j)
					 (aref mu_n i 0) (aref mu_n j 0) n))))
	   covariance_n)))

(defun weighted-running-means (x mu_n weight &aux (d (array-dimension x 0))
			            (mu_n+1 (copy-array mu_n)))
  (loop for i from 0 to (1- d) do 
    (setf (aref mu_n+1 i 0)
	  (weighted-running-mean (aref x i 0) (aref mu_n i 0) weight)))
  mu_n+1)
	      

(defun weighted-running-covariances (x covariance_n mu_n weight
			   &aux (d (array-dimension x 0)))
  (loop for i from 0 to (1- d) do 
    (loop for j from 0 to (1- d) do 
      (setf (aref covariance_n i j)
	    (weighted-running-covariance (aref x i 0) (aref x j 0) 
					 (aref covariance_n i j)
					 (aref mu_n i 0) 
					 (aref mu_n j 0) 
					 weight))))
  covariance_n)
  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Models and update routines
;;; These are probability models that are estimated from data,
;;; each keeps track of how many data points it is estimated from.
;;; [[Clearly we need to separate models from estimation processes!
;;; Probably models should be objects]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct normal-model mean variance (n 0))
(defstruct multinormal-model mean covariance (n 0))
(defstruct frequency-model freq last (n 0))

(defun normal-model-prob (x model)
  (normal x (normal-model-mean model) (sqrt (normal-model-variance model))))

(defun update-normal-model (x model &optional (weight 1.0) 
			            &aux (n (normal-model-n model))
				         (mu_n (normal-model-mean model)))
  (cond ((or (= weight 1.0) ;;; want unweighted running update
	     (< n (/ 1 (- 1 weight))))
	 (setf (normal-model-mean model)
	       (running-mean x mu_n n))
	 (setf (normal-model-variance model)
	       (running-variance x (normal-model-variance model) mu_n n)))
	(t 
	 (setf (normal-model-mean model)
	       (weighted-running-mean x mu_n weight))
	 (setf (normal-model-variance model)
	       (weighted-running-variance x (normal-model-variance model) mu_n weight))))
  (incf (normal-model-n model)))

(defun multinormal-model-prob (x model)
  (multinormal x (multinormal-model-mean model) (multinormal-model-covariance model)))

(defun update-multinormal-model (x model &optional (weight 1.0) 
			            &aux (n (multinormal-model-n model))
				         (mu_n (multinormal-model-mean model)))
  (cond ((or (= weight 1.0) ;;; want unweighted running update
	     (< n (/ 1 (- 1 weight))))
	 (setf (multinormal-model-mean model)
	       (running-means x mu_n n))
	 (setf (multinormal-model-covariance model)
	       (running-covariances x (multinormal-model-covariance model) mu_n n)))
	(t 
	 (setf (multinormal-model-mean model)
	       (weighted-running-means x mu_n weight))
	 (setf (multinormal-model-covariance model)
	       (weighted-running-covariances x (multinormal-model-covariance model) mu_n weight))))
  (incf (multinormal-model-n model)))

(defun update-frequency-model (time model &optional (weight 1.0) 
				          &aux (last (frequency-model-last model))
					       (n (frequency-model-n model))
					       (freq (frequency-model-freq model))
					       (elapsed (- time last)))
  (cond ((or (= weight 1.0) ;;; want unweighted running update
	     (< n (/ 1 (- 1 weight))))
	 (setf (frequency-model-freq model)
	       (/ 1 (running-mean elapsed (if (zerop n) 0.0d0 (/ 1 freq)) n))))
	(t 
	 (setf (frequency-model-freq model)
	       (/ 1 (weighted-running-mean elapsed (/ 1 freq) weight)))))
  (setf (frequency-model-last model) time)
  (incf (frequency-model-n model)))


