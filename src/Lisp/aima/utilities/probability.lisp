;;; probability.lisp
;;; 
 ;; PROFET Copyright 2015 (c) Data Mining and Machine Learning Group,
 ;; National University of Ireland Galway.  
 ;; This file is a part of PROFET  
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
;;; Basic definitions and utilities for representing and
;;; manipulating probability distributions.

;;; probabilityp checks if an object is a probability.
;;; Probabilities are double-floats between 0 and 1 inclusive.

(defun probabilityp (p)
  (and (numberp p) (>= p 0.0d0) (<= p 1.0d0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic operations on discrete distributions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Discrete distributions: stored as a vector of probabilities
;;; with the probabilities assumed normalized by default



(defun make-distribution (arity &optional (p (/ 1 arity)))
  "Return a new, usually uniform distribution for a discrete random variable of given arity."
  (make-sequence '(vector double-float)
		 arity
		 :initial-element (coerce p 'double-float)))



(defun list->distribution (l)
  "Converts a list of probabilities to a discrete distribution."
  (coerce (mapcar #'(lambda (x) (coerce x 'double-float)) l)
	  '(vector double-float)))


;;; edit normalize and dnormalize, by Norm, 2007-7-17, to change behavior on zero
;;; vectors.  Now returns two values, first being old return value, and only changes
;;; behavior on zero vector if uniform-if-zero? is true.  Mostly redundant code now, ugh.

(defun normalize (d &optional (uniform-if-zero? nil) &aux (sum (reduce #'+ d)))
  "Returns a new distribution that is the normalized version of the given distribution.
   Second value is T if result is uniform due to zero d."
  (if (zerop sum)
      (if uniform-if-zero? 
	  (values
	   (dmap-array #'(lambda (x) (declare (ignore x)) (/ 1.0d0 (length d))) d)
	   T)
	  (error "Attempt to normalize a zero vector - normalize"))
      (values (map '(vector double-float) #'(lambda (p) (/ p sum)) d) NIL)))

(defun dnormalize (d &optional (uniform-if-zero? nil) &aux (sum (reduce #'+ d)))
  "Normalizes the given distribution in place and returns it.
   Second value is T if result is uniform due to zero d."
  (if (zerop sum)
      (if uniform-if-zero? 
	  (values
	   (dmap-array #'(lambda (x) (declare (ignore x)) (/ 1.0d0 (length d))) d)
	   T)
	  (error "Attempt to normalize a zero vector - dnormalize"))
    (values (dmap-array #'(lambda (x) (/ x sum)) d) NIL)))

(defun increment-distribution (d v x)
  "Increments the probability for value v by x in distribution d."
  (incf (elt d v) x))

(defun multiply-distributions (d1 d2)
  "Returns the pointwise product of two discrete distributions."
  (map '(vector double-float) #'* d1 d2))

(defun zero-distribution (d)
  "Returns d with all elements set to zero."
  (loop for i from 0 to (1- (length d)) do (setf (elt d i) 0.0d0)))

(defun random-from-discrete (d &aux (s 0.0d0) (r (random 1.0d0)))
  "Returns a random index from the domain of a discrete distribution."
  (loop for i from 0 to (1- (length d)) do
    (incf s (elt d i))
    (when (<= r s) (return i))))

(defun squared-error (d1 d2)
  "Returns the sum of squared errors of two distributions."
  (reduce #'+ (map 'vector #'(lambda (p1 p2) (square (- p2 p1))) d1 d2)))

(defun squared-error-enumerated (d1 d2)
  "Returns the sum of squared errors of two enumerated distributions."
  (reduce #'+ (mapcar #'(lambda (v.p) 
			  (square (- (cdr v.p) (cdr (assoc (car v.p) d2 :test #'equal)))))
		      d1)))

(defun rms-error-enumerated (d1 d2)
  "Returns the sum of squared errors of two enumerated distributions."
  (sqrt (/ (reduce #'+ (mapcar #'(lambda (v.p) 
			  (square (- (cdr v.p) (cdr (assoc (car v.p) d2 :test #'equal)))))
		      d1))
	   (length d1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic operations on enumerated distributions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Enumerated distributions are stored as an association list of
;;; (value . probability) pairs; assumed normalized by default


(defun make-enumerated-distribution (domain &optional (p (coerce (/ 1 (length domain)) 'double-float)))
  "Return a new enumerated distribution for a discrete random variable of given domain."
  (mapcar #'(lambda (x) (cons x p)) domain))

(defun normalize-enumerated (d &aux (sum (reduce #'+ d :key #'cdr)))
  "Returns a new enumerated distribution that is normalized."
  (mapcar #'(lambda (x.p) (cons (car x.p) (/ (cdr x.p) sum))) d))

(defun dnormalize-enumerated (d &aux (sum (reduce #'+ d :key #'cdr)))
  "Normalizes the given enumerated distribution in place and returns it."
  (loop for x.p in d do (setf (cdr x.p) (/ (cdr x.p) sum)))
  d)

(defun random-from-enumerated (d &aux (s 0.0d0) (r (random 1.0d0)))
  "Returns a random value from an enumerated (alist) distribution."
  (loop for (x . p) in d do
    (incf s p)
    (when (<= r s) (return x))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic operations on continuous distributions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *1-over-root-two-pi* (/ 1.0d0 (sqrt (* 2 pi))))

(defun normal-0-1 (x)
  ;Put in test for large x to avoid underflow error
  (if (> (abs x) 10)
      0.0d0
      (* *1-over-root-two-pi*
	 (exp (- (/ (* x x) 2))))))


(defun normal (x mu sigma &aux exponent)
  ;Put in test for large x to avoid underflow error
  (if (zerop sigma)
      (if (= x mu) most-positive-single-float 0d0)
    (if (> (setf exponent (/ (expt (- x mu) 2) (* 2 (expt sigma 2)))) 50)
	0d0
      (/ (* *1-over-root-two-pi* (exp (- exponent))) sigma))))

(defun sigmoid (x beta)
  (/ 1 (+ 1 (exp (- (* 2 beta x))))))

;;; multivariate normal distribution
;;; inputs are d x 1 mean and d x d covariance matrix
;;; requires basic-matrix.lisp 

(defun multinormal (x mu covariance &aux (d (array-dimension x 0))
              		                 (x-minus-mu (subtract-matrix x mu))
					 (det (determinant covariance)))
  (if (zerop det)
      (if (zerop-matrix x-minus-mu) most-positive-single-float 0d0)
    (* (expt *1-over-root-two-pi* d)
       (/ 1 (sqrt det))
       (exp (* (- (/ 1 2))
	       (aref
		(multiply-matrix
		 (multiply-matrix
		  (transpose-matrix x-minus-mu)
		  (invert-matrix covariance nil))
		 x-minus-mu)
		0 0))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tabulated functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct table       ;a lookup-table of dimension 1 for a given 
                       ;function of 1 variable; eventually provide a 
                       ;default value/function for each dimension exceeded
  function              ;function stored in the table
  interval             ;step size for each dimension
  lower-bound          ;lower bound of each dimension
  upper-bound          ;upper bound of each dimension
  data                  ;array containing function values
  (initialized? nil)    ;initialization flag
  initializer          ;function to initialize the table
  )

(defvar *Phi*)

(defvar *Phi-inverse*)

(defun initialize-Phi-table (&aux (x1 0.0) y1 y2)
  "Computes integral of Normal-0-1 using trapezium rule, for now"
  (print "Initializing Phi table")
  (setf (aref (table-data *Phi*) 0) 0.5)
  (loop for i from 0 to (- (array-dimension (table-data *Phi*) 0) 2) do
    (setf y1 (normal-0-1 x1))
    (setf y2 (normal-0-1 (+ x1 (table-interval *Phi*))))
    (setf (aref (table-data *Phi*) (1+ i))
	  (+ (aref (table-data *Phi*) i) 
	     (* (table-interval *Phi*)
		(/ (+ y1 y2) 2.0))))
    (setf x1 (+ x1 (table-interval *Phi*))))
  (setf (table-initialized? *Phi*) t)
   (print "Initializing Phi table exit")
  )

(defun initialize-Phi-inverse-table (&aux (just-bigger-x 0.0) just-bigger-y)
  "Computes inverse of Phi-integral"
  (print "Initializing Phi-inverse table")
  (setf (aref (table-data *Phi-inverse*) 0) -6.0)
  (setf (aref (table-data *Phi-inverse*) (round (/ 0.5 (table-interval *Phi-inverse*)))) 0)
  (setf (aref (table-data *Phi-inverse*) (round (/ 1.0 (table-interval *Phi-inverse*)))) 6.0)
  (loop for i from 0 to (- (round (/ 0.5 (table-interval *Phi-inverse*))) 2) do
    (let ((y (* (+ 501 i) (table-interval *Phi-inverse*))))
      (do ((x just-bigger-x (+ x (table-interval *Phi*))))
	  ((> (setf just-bigger-y (Phi-integral x)) y) (setf just-bigger-x x)))
      (setf (aref (table-data *Phi-inverse*) (+ 501 i))
	    (- just-bigger-x
	       (/ (* (- just-bigger-x (aref (table-data *Phi-inverse*) (+ 500 i))) (- y just-bigger-y))
		  (- (- y (table-interval *Phi-inverse*)) just-bigger-y))))))
  (loop for i from 0 to (- (round (/ 0.5 (table-interval *Phi-inverse*))) 2) do
    (setf (aref (table-data *Phi-inverse*) (- 499 i)) (- (aref (table-data *Phi-inverse*) (+ 501 i)))))
  (setf (table-initialized? *Phi-inverse*) t))

(unless (boundp '*Phi*)
  (setf *Phi* (make-table :interval 0.005 :lower-bound 0.0 :upper-bound 6.0
			  :data (make-array 
				 (list (1+ (round (/ 6.0 0.001))))
				 :initial-element 0)
			  :initializer #'initialize-Phi-table)))

(unless (boundp '*Phi-inverse*)
  (setf *Phi-inverse* 
    (make-table :interval 0.001 :lower-bound 0.0 :upper-bound 1.0
		:data (make-array 
		       (list (1+ (round (/ 1.0 0.001))))
		       :initial-element 0)
		:initializer #'initialize-Phi-inverse-table)))
  

(defun Phi-integral (x)
  "Computes the integral from -infinity to x of the normal curve N(0,1)"
  (cond ((< x 0) (- 1 (Phi-integral (- x))))
	((>= x (table-upper-bound *Phi*)) 1)
	(t (table-lookup x *Phi*))))


(defun Phi-inverse (X)  ;;; X is between 0 and 1
  (table-lookup X *Phi-inverse*))

(defun table-lookup (x       ;;; the actual input to the tabulated function
		      table   ;;; where the function is tabulated
		      &aux x1 ;;; the table point below x
		      dx ;;; x-x1 as a fraction of the interval
		      y1 ;;; the value of the integral at x1
		      y2 ;;; the value of the integral at x2
		      )
  (unless (table-initialized? table)
    (funcall (table-initializer table)))
  (multiple-value-setq (x1 dx) (floor (- x (table-lower-bound table)) (table-interval table)))
  (setf dx (/ dx (table-interval table)))
  (setf y1 (aref (table-data table) x1))
  (cond ((= dx 0) y1)
	(t (setf y2 (aref (table-data table) (1+ x1)))
	   (+ y1 (* dx (- y2 y1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definitions for mixture models
;;; [[fix so that weights are a discrete distribution, separate from the
;;; submodels which are the "domain" vector for the mixture]]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct normal-mixture-model
  distrib            ;;; alist of (component . weight) pairs
  m                  ;;; number of components
  )

(defun normal-mixture-model-weight (model j)
  (cdr (nth j (normal-mixture-model-distrib model))))

(defun normal-mixture-model-component (model j)
  (car (nth j (normal-mixture-model-distrib model))))

(defun normal-mixture-model-datum-probs (x model)
  (mapcar #'(lambda (g.w)
	      (* (cdr g.w)
		 (normal-model-prob x (car g.w))))
	  (normal-mixture-model-distrib model)))

(defun normal-mixture-model-datum-weights (x model)
  (normalize (normal-mixture-model-datum-probs x model)))

(defun normal-mixture-model-datum-classification (x model)
  (let ((classprobs (normal-mixture-model-datum-weights x model)))
    (position (apply #'max classprobs) classprobs :test #'=)))

(defun set-normal-mixture-model-weight (model j w)
  (setf (cdr (nth j (normal-mixture-model-distrib model))) w))

(defun set-normal-mixture-model-mean (model j mean)
  (setf (normal-model-mean (normal-mixture-model-component model j)) mean))

(defun set-normal-mixture-model-variance (model j variance)
  (setf (normal-model-variance (normal-mixture-model-component model j)) variance))

(defstruct multinormal-mixture-model
  distrib            ;;; alist of (component . weight) pairs
  m                  ;;; number of components
  )

(defun multinormal-mixture-model-weight (model j)
  (cdr (nth j (multinormal-mixture-model-distrib model))))

(defun multinormal-mixture-model-component (model j)
  (car (nth j (multinormal-mixture-model-distrib model))))

(defun multinormal-mixture-model-datum-probs (x model)
  (mapcar #'(lambda (g.w)
	      (* (cdr g.w)
		 (multinormal-model-prob x (car g.w))))
	  (multinormal-mixture-model-distrib model)))

(defun multinormal-mixture-model-datum-weights (x model)
  (normalize (multinormal-mixture-model-datum-probs x model)))

(defun multinormal-mixture-model-datum-classification (x model)
  (let ((classprobs (multinormal-mixture-model-datum-weights x model)))
    (position (apply #'max classprobs) classprobs :test #'=)))

(defun set-multinormal-mixture-model-weight (model j w)
  (setf (cdr (nth j (multinormal-mixture-model-distrib model))) w))

(defun set-multinormal-mixture-model-mean (model j mean)
  (setf (multinormal-model-mean (multinormal-mixture-model-component model j)) mean))

(defun set-multinormal-mixture-model-covariance (model j covariance)
  (setf (multinormal-model-covariance 
         (multinormal-mixture-model-component model j)) covariance))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Random sampling from distributions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun random-from-p (P-inverse &rest parameters)
  "To compute a random number from a parameterized probability distribution p,
   first define P(r) = \int_-\infty^r[p(x) dx]. Then if X is a uniformly-distributed
   random number in the range [0,1], r=P^-1(X) is a random number from the
   distribution p. We assume that P^-1 is available as the function P-inverse,
   which is defined to take the same parameters as the original p."
  (apply P-inverse (cons (random 1.0) parameters)))

(defun random-from-normal (mu sigma)
  (+ mu (* sigma (random-from-N-0-1))))

(defun random-from-N-0-1 ()
  (random-from-p #'Phi-inverse))

;;; From Knuth Vol.3: a formula for random-from-multinormal
;;; Let x be a vector of random variables from N(0,1)
;;; Let y, our desired output, be given by Y = Ax + B
;;; where A is lower triangular
;;; Since E(x_i) = 0, we must have B = mu.
;;; From the defn of covariance, we have AA^T = covariance
;;; The equations for d=3 can be derived from this.

(defun random-from-multinormal (mu covariance &aux (d (array-dimension mu 0))
				                   (y (make-array (array-dimensions mu))))
  (cond ((= d 3)  ;;; we have a formula for this case only
	 (let* ((x1 (random-from-N-0-1)) (x2 (random-from-N-0-1)) (x3 (random-from-N-0-1))
		(s11 (aref covariance 0 0)) (s21 (aref covariance 1 0)) (s31 (aref covariance 2 0))
		(s22 (aref covariance 1 1)) (s32 (aref covariance 2 1)) (s33 (aref covariance 2 2))
		(a11 (sqrt s11))
		(a21 (/ s21 a11))
		(a22 (sqrt (- s22 (square a21))))
		(a31 (/ s31 a11))
		(a32 (/ (- s32 (* a31 a21)) a22))
		(a33 (sqrt (- s33 (square a31) (square a32)))))
	   (setf (aref y 0 0) (+ (aref mu 0 0) (* a11 x1)))
	   (setf (aref y 1 0) (+ (aref mu 1 0) (* a21 x1) (* a22 x2)))
	   (setf (aref y 2 0) (+ (aref mu 2 0) (* a31 x1) (* a32 x2) (* a33 x3)))
	   y))
	(t (loop for i from 0 to (1- d) do
	     (setf (aref y i 0) (random-from-normal (aref mu i 0) (sqrt (aref covariance i i)))))
	   y)))

;;; Mixture models: stored as a discrete distribution whose values
;;; are themselves models. Eventually, the models will be objects with their
;;; own sampling functions. For now, assume models are Gaussian models.

(defun random-from-normal-mixture (d)
  (let* ((distrib (normal-mixture-model-distrib d))
	 (component (random-from-discrete distrib))
	 (index (position component distrib :test #'eq :key #'car)))
    (values 
     (random-from-normal (normal-model-mean component)
			 (sqrt (normal-model-variance component)))
     index)))

(defun random-from-multinormal-mixture (d)
  (let* ((distrib (multinormal-mixture-model-distrib d))
	 (component (random-from-discrete distrib))
	 (index (position component distrib :test #'eq :key #'car)))
    (values
     (random-from-multinormal (multinormal-model-mean component)
			      (multinormal-model-covariance component))
     index)))


