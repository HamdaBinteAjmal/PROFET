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

;;;; things to fix
;;;; assumption that if not discrete it's continuous (doesn't allow for vectors)
;;;; in general it'd be nice to use OO better, in particular to have multiple hierarchies
;;;; (bnode, continuous-bnode, discrete-bnode, typeless-bnode, and overlapping that
;;;; a distinction between bnodes that contain code and those that are just parameterized,
;;;; and also a distinction between regular nodes and those that can't take evidence
;;;; (they have no conditional-probability function) )

;;; bayes-nets.lisp

;;; Provides basic definitions, access and constructor functions
;;; for Bayesian networks.

;;; Also defines access functions for events - sequences of
;;; values corresponding to the variables in a network.
;;; All evidence is handled as events; unknown variables in an event
;;; have value nil.

;;; Finally, provides I/O functions for creating, displaying, 
;;; saving, and reloading BNs.

;;; (declaim (optimize (speed 3)))

;;;CE 07-02-2011 Added scale-factor as optional parameter on sample method for linear gaussian and truncated guassian nodes
;;;CE 19-10-2011 Added uniform-bnodes and associated methods
(setq *print-circle* t)


;;; Bayes net type is bn; currently just contains the list of nodes.

(defstruct bn        ;;; Bayesian network
  nodes              ;;; a list of nodes; should be in topological order
  )

;;; bnode is the generic class of Bayes net nodes.
;;; Its subtypes are 
;;;    typeless-bnode     (norm 9/2008) - any type at all, supplies its own function,
;;;                       uses arbitrary-continuous and arbitrary-discrete nodes to fetch
;;;                       its value(s)
;;;    tabulated-bnode        discrete node with discrete parents, enumerated CPT
;;;    deterministic-bnode    continuous node, deterministic function of parents
;;;    arbitrary-continuous-bnode  like deterministic-bnode, but random functions are OK. norm 6/4/2007
;;;    arbitrary-discrete-bnode  like arbitrary-continuous-bnode, but discrete. norm 6/4/2007
;;;    noisy-or               discrete (Boolean) node, discrete (Boolean) parents
;;;    linear-gaussian-bnode  continuous node, Gaussian distribution with fixed standard deviation
;;;                                and with mean equal to a linear function of parents
;;;    probit-bnode           discrete (Boolean) node, probit distribution on linear function of parents
;;;    truncated-gaussian-bnode 
;;;    uniform-bnode
;;;
;;; Each node's conditional distribution is accessed by the sample and conditional-probability methods,
;;; whose implementation depends on the node type.

;;; FOR A NEW TYPE, you need ...
;;; a defstruct below
;;; sample and conditional-probability functions for BN evaluation
;;; create-node-interactively and valid-bnode-typep will need the new type added
;;; create-bnode-conditional-distribution and (maybe) add-bnode-details for editing
;;; display-distribution and create-event-entry
;;; copy-event will need to change if copy-seq isn't good enough (it's not deep)
;;; ALSO NEED TO ADD, IN EDIT-NETS.LISP:
;;; entry in batch-create-node
;;; add to clear-compiled functions if it's a function node



(defstruct bnode
  name               ;;; a symbol, the name of the corresponding variable
  index              ;;; an integer, the index of the node in the network
  parents            ;;; list, parent nodes of this node
  children           ;;; list, child nodes of this node
  discrete-parent-indexes     ;;; ce 
  continuous-parent-indexes     ;;; ce
  X  ;; ce 17/11/2010 for DNBC
  Y
  )

;;; Typeless bnodes are useful for functions that return multiple values.
;;; Use arbitrary-discrete and arbitrary-continuous nodes to fetch results.
;;; Sampling from one is an error.

(defstruct (typeless-bnode (:include bnode))
  function			   ;;; child is this function of parents
  (compiled-fn NIL))

(defmethod sample ((bnode typeless-bnode) event &optional (scale-factor 1))
  "Call bnode's associated function with parents' values."
  (unless (typeless-bnode-compiled-fn bnode)
    (setf (typeless-bnode-compiled-fn bnode) 
          (compile NIL (typeless-bnode-function bnode))))
  (apply (typeless-bnode-compiled-fn bnode) (parent-values bnode event)))

(defmethod conditional-probability ((bnode typeless-bnode) value event)
  "Returns the probability that bnode=value given the parent values in event."
  (error "conditional-probability invoked on typeless-bnode ~A" (bnode-name bnode)))


(defstruct (discrete-bnode (:include bnode))
  arity              ;;; number of possible values
  value-names        ;;; a list of symbols, the names of the values
  )

(defun discrete-parents (bnode) (remove-if-not #'discrete-bnode-p (bnode-parents bnode)))
(defun continuous-parents (bnode) (remove-if #'discrete-bnode-p (bnode-parents bnode)))


;;;; Tabulated nodes have an explicit table for the conditional distribution

(defstruct (tabulated-bnode (:include discrete-bnode))
  cpt		     ;;; array containing the conditional distribution
  )

(defmethod sample ((bnode tabulated-bnode) event &optional (scale-factor 1))
  "Sample the value of bnode given parent values as specified in event."
  (random-from-discrete (conditional-distribution bnode (parent-values bnode event))))

(defmethod conditional-probability ((bnode tabulated-bnode) value event)
  "Returns the probability that bnode=value given the parent values in event."
  (conditional-probability-lookup bnode value (parent-values bnode event)))

(defmethod conditional-probability-lookup ((bnode tabulated-bnode) value parent-values)
  "Return P(Bnode=value|parent-values)."
  (elt (conditional-distribution bnode parent-values) value))

(defmethod conditional-distribution ((bnode tabulated-bnode) parent-values)
  (apply #'aref (tabulated-bnode-cpt bnode) parent-values))


;;; A deterministic node is sampled simply by applying the function
;;; to the parent values -- there is no uncertainty involved.
;;; The conditional-probability of any specific value is 1 if the value is correct,
;;; 0 otherwise. The deterministic function itself is specific to
;;; each node instance, and must take the parent values as arguments.

(defstruct (deterministic-bnode (:include bnode))
  function			   ;;; child is this function of parents
  compiled-fn
  ) 

(defmethod sample ((bnode deterministic-bnode) event &optional (scale-factor 1) )
  "Sample the value of bnode given parent values as specified in event."
  (unless (deterministic-bnode-compiled-fn bnode)
    (setf (deterministic-bnode-compiled-fn bnode) 
          (compile NIL (deterministic-bnode-function bnode))))
  (apply (deterministic-bnode-compiled-fn bnode) (parent-values bnode event))      
  )

(defmethod conditional-probability ((bnode deterministic-bnode) value event)
  "Returns the probability that bnode=value given the parent values in event."
  (format t "~&WARNING: conditional-probability invoked on deterministic-bnode ~A~%"
          (bnode-name bnode))
  (if (equalp (sample bnode event) value) 1.0d0 0.0d0))


;;; arbitrary-continuous-bnode -- like deterministic-bnode, except random functions 
;;; are OK within it. 
;;; Author: norm 2007-06-04

(defstruct (arbitrary-continuous-bnode (:include bnode))
  function         ;; child is this function of parents
  (compiled-fn NIL) ;; obvious?
  (cache NIL))     ;; not used

(defmethod sample ((bnode arbitrary-continuous-bnode) event &optional (scale-factor 1) )
  "Sample the value of bnode given parent values as specified in event."
  (unless (arbitrary-continuous-bnode-compiled-fn bnode)
    (setf (arbitrary-continuous-bnode-compiled-fn bnode) 
          (compile NIL (arbitrary-continuous-bnode-function bnode))))
  (apply (arbitrary-continuous-bnode-compiled-fn bnode) (parent-values bnode event))
  )

(defmethod conditional-probability ((bnode arbitrary-continuous-bnode) value event)
  "Returns the probability that bnode=value given the parent values in event."
  (format t "~&WARNING: conditional-probability invoked on arbitrary-continuous-bnode ~A~%"
          (bnode-name bnode))
  (if (equalp (sample bnode event) value) 1.0d0 0.0d0))

;;; arbitrary-discrete-bnode
;;; Author: norm 2007-06-04

(defstruct (arbitrary-discrete-bnode (:include discrete-bnode))
  function         ;; child is this function of parents
  (compiled-fn NIL)
  (cache NIL))     ;; not used

(defmethod sample ((bnode arbitrary-discrete-bnode) event &optional (scale-factor 1))
  "Sample the value of bnode given parent values as specified in event."
  ;; (format t "~A " (bnode-name bnode))
  (unless (arbitrary-discrete-bnode-compiled-fn bnode)
    (setf (arbitrary-discrete-bnode-compiled-fn bnode) 
          (compile NIL (arbitrary-discrete-bnode-function bnode))))
  (apply (arbitrary-discrete-bnode-compiled-fn bnode) (parent-values bnode event)))

(defmethod conditional-probability ((bnode arbitrary-discrete-bnode) value event)
  "Returns the probability that bnode=value given the parent values in event."
  (format t "~&WARNING: conditional-probability invoked on arbitrary-discrete-bnode ~A~%"
          (bnode-name bnode))
  (if (equalp (sample bnode event) value) 1.0d0 0.0d0))



;;; Noisy-OR nodes: see [2e p 500]
;;; Boolean node with Boolean parents.
;;; Coefficient q_i for parent i gives the probability
;;; that the node is false when just parent i is true and all others
;;; are false. In general, node is false if all true parents
;;; fail to make the node true, and failure probabilities
;;; are independent.

(defstruct (noisy-or-bnode (:include discrete-bnode 
                             (arity 2)
                             (value-names '(false true))))
  coefficients   	     ;;; list of (q_1 ... q_k) for k parents
  )



;;; Linear Gaussian nodes: can have both discrete and continuous parents.
;;; For each setting of the discrete parents, the child distribution is a Gaussian
;;; whose mean is a linear function of the continuous parent values and whose
;;; standard deviation is fixed (sigma). Hence the node needs to know
;;;   1) The coefficients of the parents and the offset in the linear function
;;;      If the coefficients are (a_1 ... a_k), the offset is b, and the parents
;;;      are x_1 ... x_k, the linear function is a_1x_1 + ... + a_kx_k + b
;;;   2) The standard deviation of the Gaussian.
;;; All these are stored in tables indexed by discrete parent values.
;;; Functions for sampling and computing the conditional-probability are given;
;;; the main thing we need is the linear part, which is easy.

(defstruct (linear-gaussian-bnode (:include bnode))
  coefficients-table	     ;;; array of lists (a_1 ... a_k) for k continuous parents
  offset-table
  sigma-table
  ;;  below, which I was hoping would speed things up, breaks too much.
  ;;  (offset-table #(0.0d0) :type (simple-array double-float *))    ;;; array of constants b
  ;;  (sigma-table #(0.0d0)  :type (simple-array double-float *))	 ;;; array of sd's
  )

(defmethod linear-gaussian-mean (bnode event)
  "Returns the mean for bnode, a linear function of the parent values in event."
  (let ((dvalues (discrete-parent-values bnode event))
        (cvalues (continuous-parent-values bnode event)))
    (+ (apply #'aref (linear-gaussian-bnode-offset-table bnode) dvalues)
       (apply #'+ (mapcar #'* (apply #'aref (linear-gaussian-bnode-coefficients-table bnode) 
                                     dvalues)
                          cvalues)))))

(defmethod sample ((bnode linear-gaussian-bnode) event &optional (scale-factor 1))
  "Sample the value of bnode given parent values as specified in event."
  ;; Scale-factor added by CE for adaptive particle filtering
  (let ((dvalues (discrete-parent-values bnode event)))
    (random-from-normal (linear-gaussian-mean bnode event)
                        (/ (apply #'aref (linear-gaussian-bnode-sigma-table bnode) dvalues) scale-factor))
    ))

(defmethod conditional-probability ((bnode linear-gaussian-bnode) value event)
  "Returns the probability that bnode=value given the parent values in event."
  (let ((dvalues (discrete-parent-values bnode event)))
    (normal value (linear-gaussian-mean bnode event) 
            (apply #'aref (linear-gaussian-bnode-sigma-table bnode) dvalues))))



;;; Truncated Gaussian nodes: can have both discrete and continuous parents.
;;; As per Linear Gaussian node except it has 2 additional fields for upper and lower limit

(defstruct (truncated-gaussian-bnode (:include linear-gaussian-bnode))
  upper-limit-table    
  lower-limit-table             
  )

(defmethod truncated-gaussian-mean ((bnode truncated-gaussian-bnode) event)
  "Returns the mean for bnode, a linear function of the parent values in event."
  (let* ((dvalues (discrete-parent-values bnode event))
         (meanvalue(linear-gaussian-mean bnode event))
         (lower-limit (apply #'aref (truncated-gaussian-bnode-lower-limit-table bnode) dvalues))
         (upper-limit (apply #'aref (truncated-gaussian-bnode-upper-limit-table bnode) dvalues)))
    (if lower-limit 
        (if (< meanvalue lower-limit)
            (error "~%~A has a mean value of ~A. This is below the lower limit ~A "  (bnode-name bnode) meanvalue lower-limit)))
    (if upper-limit
        (if (> meanvalue upper-limit)
            (error "~%~A has a mean value of ~A. This is above the upper limit ~A "  (bnode-name bnode) meanvalue upper-limit)))
    meanvalue))

(defmethod sample ((bnode truncated-gaussian-bnode) event &optional (scale-factor 1) )
  "Sample the value of bnode given parent values as specified in event."
  (let ((dvalues (discrete-parent-values bnode event))
        (no-of-tries 0))
    (loop
     ;;Only try for a sample 20 times. Otherwise we could end up in an endless loop looking for valid samples
     (when (> no-of-tries 50) (error "Cannot find a sample within range for ~A  Scale Factor ~A" (bnode-name bnode) scale-factor))
     (let ((sample-value (random-from-normal (truncated-gaussian-mean bnode event)
                                             (/ (apply #'aref (truncated-gaussian-bnode-sigma-table bnode) dvalues)scale-factor)))
           (lower-limit (apply #'aref (truncated-gaussian-bnode-lower-limit-table bnode) dvalues))
           (upper-limit (apply #'aref (truncated-gaussian-bnode-upper-limit-table bnode) dvalues)))
       
       (if lower-limit   ;; Does a lower limit exist?
           (if (>= sample-value lower-limit) ;; Check if sample is above lower limit
               (if upper-limit  ;; Does an upper limit also exist?
                   (if (<= sample-value upper-limit) (Return sample-value)) ;; Within upper and lower limit
                   (Return sample-value) ;;Above lower limit and no upper limit
                   ))
           ;; No lower limit. Check upper limit
           (if upper-limit
               (if (<= sample-value upper-limit) (Return sample-value)) ;; Below upper limit and no lower limit
               (Return sample-value)) ;; No upper or lower limit so sample must be between limits.
           )
       (incf no-of-tries)))))


(defmethod conditional-probability ((bnode truncated-gaussian-bnode) value event)
  "Returns the probability that bnode=value given the parent values in event."
  (let ((dvalues (discrete-parent-values bnode event)))
    (normal value (truncated-gaussian-mean bnode event) 
            (apply #'aref (truncated-gaussian-bnode-sigma-table bnode) dvalues))))

;; ce uniform-bnodes start
;; Uniform-bnodes

(defstruct (uniform-bnode (:include bnode))
  a-lower-table ;;lower bound
  b-upper-table  ;;upper bound          
  )

(defmethod sample ((bnode uniform-bnode) event &optional (scale-factor 1) )
  "Sample the value of bnode
   If only discrete parents sample from uniform distribution
   If continuous parent which is also a uniform distribution just use parent value" 
  (let ((dvalues (discrete-parent-values bnode event))
        (cvalue (continuous-parent-values bnode event)))
    (if (zerop (length cvalue))
        (progn ;; First time slice so sample from distribution
          (let* (
                 (a  (apply #'aref (uniform-bnode-a-lower-table bnode) dvalues))
                 (b  (apply #'aref (uniform-bnode-b-upper-table bnode) dvalues))
                 )         
            
            (+  (random (float (- b a))) a)
            )
          )
        (first cvalue) ;;Just return parent value
        )
    )
  )


(defmethod conditional-probability ((bnode uniform-bnode) value event)
  "Returns the probability that bnode=value given the parent values in event."
  (let* ((dvalues (discrete-parent-values bnode event))
         (a  (apply #'aref (uniform-bnode-a-lower-table bnode) dvalues))
         (b  (apply #'aref (uniform-bnode-b-upper-table bnode) dvalues))
         )  
    (print a) (print b) (print value)
    (if ( AND (>= value a ) (<= value b ))      
        (/ 1 (- b a))
        0
        )
    )
  )

;; ce uniform-bnodes end


;;; Probit nodes: the node is Boolean, with values (false true), so we
;;; assume the node is very likely to take on its second value when the
;;; linear function of the parent values is high. For each setting of
;;; the discrete parents, the distribution is
;;; defined by the coefficients of the linear function and by the mean
;;; and standard deviation of the Gaussian from which the cumulative
;;; probit distribution is constructed.

(defstruct (probit-bnode (:include discrete-bnode 
                           (arity 2)
                           (value-names '(false true))))
  coefficients-table	     ;;; a list (a_1 ... a_k) for k continuous parents
  mu-table                   ;;; mean for the Gaussian on which probit is based
  sigma-table		     ;;; standard deviation for the Gaussian
  )

(defun probit-linear (bnode event)
  "Returns the specified linear combination of parent values given in event."
  (let ((dvalues (discrete-parent-values bnode event))
        (cvalues (continuous-parent-values bnode event)))
    (apply #'+ (mapcar #'* (apply #'aref (probit-bnode-coefficients-table bnode) dvalues)
                       cvalues))))

(defun probit (x mu sigma)
  "Returns the integral of the normal distribution N(mu,sigma^2) up to x."
  (Phi-integral (/ (- x mu) sigma)))

(defmethod sample ((bnode probit-bnode) event &optional (scale-factor 1) )
  "Sample the value of bnode given parent values as specified in event."
  (let ((p (conditional-probability bnode 1 event)))
    (random-from-discrete (list->distribution (list (- 1 p) p)))))

(defmethod conditional-probability ((bnode probit-bnode) value event)
  "Returns the probability that bnode=value given the parent values in event."
  (let* ((dvalues (discrete-parent-values bnode event))
         (p (probit (probit-linear bnode event)
                    (apply #'aref (probit-bnode-mu-table bnode) dvalues)
                    (apply #'aref (probit-bnode-sigma-table bnode) dvalues))))
    (if (zerop value) (- 1 p) p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; General functions for working with bnodes

(defun map-values (f bnode)
  "Calls f with each possible value of bnode (assumed discrete)."
  (if (discrete-bnode-p bnode)
      (loop for i from 0 to (1- (discrete-bnode-arity bnode)) do (funcall f i))
      (error "~%~A is not a discrete node -- map-values" (bnode-name bnode))))

(defun bnode-distribution-over (bnode f)
  "Returns a distribution constructed by calling f with each possible value of bnode."
  (let* ((n (discrete-bnode-arity bnode)) (result (make-distribution n 0.0d0)))
    (loop for i from 0 to (1- n) do
          (setf (elt result i) (funcall f i)))
    result))

(defun bnode-sum-over (bnode f)
  "Returns the sum of f applied to all values of bnode."
  (let ((sum 0.0d0))
    (loop for i from 0 to (1- (discrete-bnode-arity bnode)) do
          (incf sum (funcall f i)))
    sum))

(defun bnode-random-value (bnode)
  "Returns a value selected uniformly at random."
  (random (discrete-bnode-arity bnode)))

(defun point-distribution (bnode event &aux d)
  "Returns a distribution with probability 1 for value."
  (setq d (make-distribution (discrete-bnode-arity bnode) 0.0d0))
  (setf (elt d (node-value bnode event)) 1.0d0)
  d)

(defun bnode-by-name (name bn)
  "Returns the actual node with the given name."
  (or (find name (bn-nodes bn) :key #'bnode-name :test #'eq)
      (error "Node ~A does not exist in belief net" name)))

(defun distribution->alist (d bnode &key (use-nils NIL))
  "Converts a vector distribution to alist form with actual value names,
   replacing values with NIL if use-nils is set."
  (if use-nils
      (mapcar (lambda (name) (cons name NIL)) (discrete-bnode-value-names bnode))
      (map 'list #'cons (discrete-bnode-value-names bnode) d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The following functions deal with events, which are vectors
;;; with entries giving the value (or value index) of the corresponding variable.


(defun alist->event (alist bn &aux (event (make-empty-event bn)))
  "Returns an event given an association list of (variable . value) pairs."
  (loop for (x . v) in alist do
        (let ((bnode (bnode-by-name x bn)))
          (if (discrete-bnode-p bnode)
              (setf (elt event (bnode-index bnode))
                    (position v (discrete-bnode-value-names bnode) :test #'eq))
              (setf (elt event (bnode-index bnode)) v))))
  event)

(defun event->alist (event bn)
  "Returns an association list of (variable . value) pairs given an event ."
  (remove-if #'null 
             (map 'list #'(lambda (vindex bnode)
                            (and vindex 
                                 (cons (bnode-name bnode)
                                       (if (discrete-bnode-p bnode)
                                           (nth vindex (discrete-bnode-value-names bnode))
                                           vindex))))
                  event (bn-nodes bn))))

(defun make-empty-event (bn) 
  "Return an event for the variables of bn with no values specified."
  (make-sequence 'vector (length (bn-nodes bn)) :initial-element nil))

(defun node-value (bnode event)
  "Returns the value of the node in event (nil if none)."
  (elt event (bnode-index bnode)))

(defun set-value (bnode event value)
  "Sets the value of bnode in event."
  (setf (elt event (bnode-index bnode)) value))

(defun unset-value (bnode event)
  "Unsets the value of bnode in event."
  (set-value bnode event nil))

(defun parent-values (bnode event)
  "Returns a list of the parent values of node in event."
  (mapcar #'(lambda (parent) (elt event (bnode-index parent)))
          (bnode-parents bnode)))

(defun discrete-parent-values (bnode event)
  "Returns a list of the discrete parent values of node in event."
  (mapcar #'(lambda (parent) (elt event (bnode-index parent)))
          (discrete-parents bnode)))
;;Ce changes possible performance improvement
;;(mapcar #'(lambda (parentindex) (svref event parentindex))
;;        (bnode-discrete-parent-indexes bnode))

;; )

(defun continuous-parent-values (bnode event)
  "Returns a list of the continuous parent values of node in event."
  (mapcar #'(lambda (parent) (elt event (bnode-index parent)))
          (continuous-parents bnode)))
;; CE changes possible performance improvement
;;  (mapcar #'(lambda (parentindex) (elt event parentindex))
;;	  (bnode-continuous-parent-indexes bnode)))



(defun copy-event (event) (copy-seq event))

(defun hide-vars (event predicate bn)
  "Return an event with no values for nodes satisfying predicate."
  (let ((e (copy-event event)))
    (loop for node in (bn-nodes bn) do 
          (when (funcall predicate node)
            (set-value node e nil)))
    e))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Functions for creating Bayes nets interactively

(defun create-bayes-net ()
  "Create a BN interactively and return it."
  (create-indices
   (create-distributions
    (sort-bayes-net
     (create-arcs
      (make-bn :nodes (create-nodes)))))))

(defun create-indices (bn &aux (nodes (bn-nodes bn)))
  "Sets the indices of each node; indices are integers from 0 to n-1."
  (loop for i from 0 to (1- (length nodes)) do 
        (setf (bnode-index (nth i nodes)) i))
  bn)

(defun create-nodes (&aux (nodes nil) (node t))
  "Returns a list of BN nodes created interactively."
  (loop until (null node) do
        (setf node (create-node-interactively))
        (when node (push node nodes)))
  nodes)

(defun create-node-interactively ()
  "Create and return a BN node. First determines the type and then calls
   the appropriate specialized creation function."
  (format t "~%*****************Creating New Node*******************~%")
  (let ((name (query-user (format nil "~%What is the node's name (nil if none)?  ")
                          #'atom
                          (format nil "~%Name must be an atom (unquoted)"))))
    (when name
      (let* ((type (query-user (format nil "~%Type? (Tabulated, deterministic, arbitrary-continuous, arbitrary-discrete, noisy-or, linear-gaussian, truncated-guassian, probit, uniform, or typeless.)~%    ")
                               #'valid-bnode-typep
                               (format nil "~%Unknown type -- try again.  ")))
             (bnode (case type
                          (typeless (make-typeless-bnode :name name))
                          (tabulated (make-tabulated-bnode :name name))
                          (deterministic (make-deterministic-bnode :name name))
                          (arbitrary-continuous (make-arbitrary-continuous-bnode :name name))
                          (arbitrary-discrete (make-arbitrary-discrete-bnode :name name))
                          (noisy-or (make-noisy-or-bnode :name name))
                          (linear-gaussian (make-linear-gaussian-bnode :name name))
                          (truncated-gaussian (make-truncated-gaussian-bnode :name name))
                          (uniform (make-uniform-bnode :name name))
                          (probit (make-probit-bnode :name name))
                          (otherwise (error "Unknown type -- ~A" type) nil))))
        (add-bnode-details bnode)
        bnode))))

(defun valid-bnode-typep (x)
  "Returns t iff x (an atom) is a recognized BN node type."
  (member x '('typeless tabulated deterministic arbitrary-continuous arbitrary-discrete
                        noisy-or linear-gaussian truncated-gaussian probit uniform)))

(defmethod add-bnode-details ((bnode bnode))
  "Ask user for additional information to define node; by default, nothing for now.")

;; Should I be able to combine these functions?  Only if I go with more OO, I think.
(defmethod add-bnode-details ((bnode arbitrary-discrete-bnode))
  "For arbitrary discrete nodes we need the value names."
  (setf (discrete-bnode-value-names bnode) (create-value-names-interactively))
  (setf (discrete-bnode-arity bnode) (length (discrete-bnode-value-names bnode))))

(defmethod add-bnode-details ((bnode tabulated-bnode))
  "For tabulated nodes we need the value names."
  (setf (discrete-bnode-value-names bnode) (create-value-names-interactively))
  (setf (discrete-bnode-arity bnode) (length (discrete-bnode-value-names bnode))))



(defun create-value-names-interactively ()
  "Returns a list of value names (distinct atoms) entered by the user."
  (query-user (format nil "~%Enter list of values  ")
              #'(lambda (x) (and (listp x) (every #'atom x) 
                                 (equal x (remove-duplicates x))))
              (format nil "~%Must be a list of distinct atoms, e.g., (false true)")))

(defun create-arcs (bn)
  "Choose parents for each node interactively, return the resulting BN."
  (format t "~%************Creating Bayes net arcs*****************~%")
  (delete-arcs bn)
  (loop for bnode in (bn-nodes bn) do
        (create-arcs-for-node bnode bn))
  bn)

(defun create-arcs-for-node (bnode bn)
  "Choose parents for bnode (a BN node) interactively, return the node."
  (let ((names (remove (bnode-name bnode) (mapcar #'bnode-name (bn-nodes bn)))))
    (setf (bnode-parents bnode)
          (mapcar 
           #'(lambda (name) (bnode-by-name name bn))
           (query-user (format nil "~%Enter list of parent node names for ~A  "
                               (bnode-name bnode))
                       #'(lambda (x) (and (listp x) (subsetp x names)
                                          (equal x (remove-duplicates x))))
                       (format nil "~%Must be a list of node names chosen from ~A"
                               names)))))
  (loop for parent in (bnode-parents bnode) do
        (push bnode (bnode-children parent))))

(defun delete-arcs (bn)
  "Delete all arcs in the BN and return it."
  (loop for bnode in (bn-nodes bn) do
        (setf (bnode-parents bnode) nil
              (bnode-children bnode) nil)))

(defun create-distributions (bn)
  "Create a distribution for each node in a BN, return the resulting BN."
  (loop for bnode in (bn-nodes bn) do
        (create-node-distribution bnode))
  bn)

(defun create-node-distribution (bnode &aux (type (type-of bnode)))
  "Create a distribution for a BN node; returned value is unspecified. 
   First determines the type and then calls the appropriate specialized
   creation function."
  (format t "~%****Creating distribution for node ~A of type ~A****"
          (bnode-name bnode) type)
  (create-bnode-conditional-distribution bnode))

(defmethod create-bnode-conditional-distribution ((bnode tabulated-bnode))
  "Create and fill in table of CPT entries for tabulated BN node."
  (let* ((parents (bnode-parents bnode))
         (dimensions (mapcar #'discrete-bnode-arity parents))
         (indices (make-list (length dimensions))))
    (setf (tabulated-bnode-cpt bnode) (my-make-array dimensions :element-type '(vector double-float)))
    (fill-tables bnode parents dimensions indices indices 2)))

(defun fill-tables (bnode parents dimensions indices rest-indices indent)
  "Recursively enumerate parent value combinations, asking for distributions for child node
   conditioned on the parent node values specified by the current array indices.
   Specific table entries are filled in by the appropriate fill-table-entry mehod for bnode."
  (cond (dimensions 
         (loop for i from 0 to (1- (first dimensions)) do
               (setf (first rest-indices) i)
               (format t "~%~A Given ~A = ~A" 
                       (spaces indent)
                       (bnode-name (first parents))
                       (nth i (discrete-bnode-value-names (first parents))))
               (fill-tables bnode (rest parents) (rest dimensions) 
                            indices (rest rest-indices) (+ indent 2))))
    (t ; have now conditioned on everything
       (fill-table-entry bnode indices indent))))

(defmethod fill-table-entry ((bnode tabulated-bnode) indices indent)
  "Fill in the distribution for bnode conditioned on parent values in indices."
  (setf (apply #'aref (tabulated-bnode-cpt bnode) indices)
        (list->distribution 
         (query-user 
          (format nil "~%~A   Enter probabilities for ~A = ~A~%~A   "
                  (spaces indent)
                  (bnode-name bnode)
                  (discrete-bnode-value-names bnode)
                  (spaces indent))
          #'(lambda (x) (and (listp x) (every #'probabilityp x)))
          (format nil "~%Must be a list of numbers in [0,1]")))))

(defmethod create-bnode-conditional-distribution ((bnode deterministic-bnode))
  "Create the function defining a deterministic node's dependence on its parents.
   Functional expressions may be typed in using any of the following forms:
      sqrt                   a simple symbol naming a function
      (lambda (x) (sqrt x))  a lambda expression
   We disallow #'sqrt because it may cause problems with saving/reloading.
   All places where the function is used can accommodate the symbolic form."
  (let* ((parent-names (mapcar #'bnode-name (bnode-parents bnode)))
         (fx (query-user 
              (format nil "~%Enter a function with arguments ~A:~%" parent-names)
              #'functional-expressionp
              (format nil "~%Must be a function name or a lambda-expression, unquoted"))))
    (setf (deterministic-bnode-function bnode) fx)
    ))

(defmethod create-bnode-conditional-distribution ((bnode typeless-bnode))
  "See above function -- basically just the same as for deterministic-bnode"
  (let* ((parent-names (mapcar #'bnode-name (bnode-parents bnode)))
         (fx (query-user 
              (format nil "~%Enter a function with arguments ~A:~%" parent-names)
              #'functional-expressionp
              (format nil "~%Must be a function name or a lambda-expression, unquoted"))))
    (setf (typeless-bnode-function bnode) fx)))

(defmethod create-bnode-conditional-distribution ((bnode arbitrary-continuous-bnode))
  "See above function -- basically just the same as for deterministic-bnode"
  (let* ((parent-names (mapcar #'bnode-name (bnode-parents bnode)))
         (fx (query-user 
              (format nil "~%Enter a function with arguments ~A:~%" parent-names)
              #'functional-expressionp
              (format nil "~%Must be a function name or a lambda-expression, unquoted"))))
    (setf (arbitrary-continuous-bnode-function bnode) fx)))

(defmethod create-bnode-conditional-distribution ((bnode arbitrary-discrete-bnode))
  "See above function -- basically just the same as for deterministic-bnode"
  (let* ((parent-names (mapcar #'bnode-name (bnode-parents bnode)))
         (fx (query-user 
              (format nil "~%Enter a function with arguments ~A and possible values ~A:~%" 
                      parent-names (discrete-bnode-value-names bnode))
              #'functional-expressionp
              (format nil "~%Must be a function name or a lambda-expression, unquoted"))))
    (setf (arbitrary-discrete-bnode-function bnode) fx)))

(defun functional-expressionp (fx)
  "Return t iff fx is of the appropriate form for naming a function."
  (or (and (symbolp fx) (fboundp fx))
      (and (listp fx) (eq (first fx) 'lambda))))

(defmethod create-bnode-conditional-distribution ((bnode noisy-or-bnode))
  "Create a noisy-or distribution: asks for the coefficients for
   the independent probability that each parent, when true, fails to
   cause the node to be true."
  (let* ((parent-names (mapcar #'bnode-name (bnode-parents bnode)))
         (n (length parent-names)))
    (setf (noisy-or-bnode-coefficients bnode)
          (if (zerop n) nil
              (query-user (format nil "~%Enter list of ~A coefficients for parents ~A~%For each parent, give the probability that the parent,~%when true, FAILS to cause this node to be true   " n parent-names)
                          #'(lambda (x) (and (listp x) (every #'probabilityp x)))
                          (format nil "~%Must be a list of ~A probabilities" n))))))



(defmethod create-bnode-conditional-distribution ((bnode linear-gaussian-bnode))
  "Create and fill in tables of coefficients, offsets, and sigmas for a linear-Gaussian bnode."
  (let* ((dparents (discrete-parents bnode))
         (dimensions (mapcar #'discrete-bnode-arity dparents))
         (indices (make-list (length dimensions))))
    (setf (linear-gaussian-bnode-coefficients-table bnode) (my-make-array dimensions))
    (setf (linear-gaussian-bnode-offset-table bnode) (my-make-array dimensions))
    (setf (linear-gaussian-bnode-sigma-table bnode) (my-make-array dimensions))
    (fill-tables bnode dparents dimensions indices indices 2)))

(defmethod fill-table-entry ((bnode linear-gaussian-bnode) indices indent)
  "Fill in the linear-Gaussian function for bnode conditioned on parent values in indices."
  (let* ((cparents (continuous-parents bnode))
         (cparent-names (mapcar #'bnode-name cparents))
         (n (length cparent-names)))
    (setf (apply #'aref (linear-gaussian-bnode-coefficients-table bnode) indices)
          (if (zerop n) nil
              (query-user (format nil "~%~A   Enter list of ~A coefficients for continuous parents ~A   " 
                                  (spaces indent) n cparent-names)
                          #'(lambda (x) (and (listp x) (every #'numberp x) (= (length x) n)))
                          (format nil "~%~A   Must be a list of ~A numbers" (spaces indent) n))))
    (setf (apply #'aref (linear-gaussian-bnode-offset-table bnode) indices)
          (query-user (format nil "~%~A   Enter constant term for linear function   " (spaces indent))
                      #'numberp
                      (format nil "~%~A   Must be a single number" (spaces indent))))
    (setf (apply #'aref (linear-gaussian-bnode-sigma-table bnode) indices)
          (query-user (format nil "~%~A   Enter standard deviation for Gaussian   " (spaces indent))
                      #'(lambda (x) (and (numberp x) (plusp x)))
                      (format nil "~%~A   Must be a single positive number" (spaces indent))))))


(defmethod create-bnode-conditional-distribution ((bnode truncated-gaussian-bnode))
  "Create and fill in tables of coefficients, offsets, and sigmas for a linear-Gaussian bnode."
  (let* ((dparents (discrete-parents bnode))
         (dimensions (mapcar #'discrete-bnode-arity dparents))
         (indices (make-list (length dimensions))))
    (setf (truncated-gaussian-bnode-coefficients-table bnode) (my-make-array dimensions))
    (setf (truncated-gaussian-bnode-offset-table bnode) (my-make-array dimensions))
    (setf (truncated-gaussian-bnode-sigma-table bnode) (my-make-array dimensions))
    (setf (truncated-gaussian-bnode-lower-limit-table bnode) (my-make-array dimensions))
    (setf (truncated-gaussian-bnode-upper-limit-table bnode) (my-make-array dimensions))
    (fill-tables bnode dparents dimensions indices indices 2)))

(defmethod fill-table-entry ((bnode truncated-gaussian-bnode) indices indent)
  "Fill in the  truncated-Gaussian function for bnode conditioned on parent values in indices."
  (let* ((cparents (continuous-parents bnode))
         (cparent-names (mapcar #'bnode-name cparents))
         (n (length cparent-names)))
    (setf (apply #'aref (truncated-gaussian-bnode-coefficients-table bnode) indices)
          (if (zerop n) nil
              (query-user (format nil "~%~A   Enter list of ~A coefficients for continuous parents ~A   " 
                                  (spaces indent) n cparent-names)
                          #'(lambda (x) (and (listp x) (every #'numberp x) (= (length x) n)))
                          (format nil "~%~A   Must be a list of ~A numbers" (spaces indent) n))))
    (setf (apply #'aref (truncated-gaussian-bnode-offset-table bnode) indices)
          (query-user (format nil "~%~A   Enter constant term for linear function   " (spaces indent))
                      #'numberp
                      (format nil "~%~A   Must be a single number" (spaces indent))))
    (setf (apply #'aref (truncated-gaussian-bnode-sigma-table bnode) indices)
          (query-user (format nil "~%~A   Enter standard deviation for Gaussian   " (spaces indent))
                      #'(lambda (x) (and (numberp x) (plusp x)))
                      (format nil "~%~A   Must be a single positive number" (spaces indent))))
    (setf (apply #'aref (truncated-gaussian-bnode-lower-limit-table bnode) indices)
          (query-user (format nil "~%~A   Enter a lower limit for Truncated Gaussian  (Enter 'nil' if no limit exists)  " (spaces indent))
                      #'(lambda (x) (OR (numberp x) (string-equal x nil)))                  
                      (format nil "~%~A   Must be a single number or nil" (spaces indent))))  
    (setf (apply #'aref (truncated-gaussian-bnode-upper-limit-table bnode) indices)
          (query-user (format nil "~%~A   Enter an upper limit for Truncated Gaussian  (Enter 'nil' if no limit exists)  " (spaces indent))
                      #'(lambda (x) (OR (numberp x) (string-equal x nil)))                  
                      (format nil "~%~A   Must be a single number or nil" (spaces indent))))))

(defmethod create-bnode-conditional-distribution ((bnode uniform-bnode))
  "Create and fill lower and upper bound of bnode."
  (let* ((dparents (discrete-parents bnode))
         (dimensions (mapcar #'discrete-bnode-arity dparents))
         (indices (make-list (length dimensions))))
    (setf (uniform-bnode-a-lower-table bnode) (my-make-array dimensions))
    (setf (uniform-bnode-b-upper-table bnode) (my-make-array dimensions))
    (fill-tables bnode dparents dimensions indices indices 2))
  )

(defmethod fill-table-entry ((bnode uniform-bnode) indices indent)
  "Fill in the uniform distribution for bnode conditioned on parent values in indices.
   If first slice there are no continuous parents so jsut need distribution
   If second slice and filling transition probability just copy parent values."
  (let* ((cparents (continuous-parents bnode))
         (n (length cparents)))
    
    (if (zerop n)
        (progn ;; no continuous parent => zero slice
          (setf (apply #'aref (uniform-bnode-a-lower-table bnode) indices)
                (query-user (format nil "~%   Enter a, the lower bound for the Uniform Distribution" )
                            #'(lambda (x) (numberp x) )
                            (format nil "~%   Must be a single number" )))
          (setf (apply #'aref (uniform-bnode-b-upper-table bnode) indices)
                (query-user (format nil "~%   Enter b, the upper bound for the Uniform Distribution" )
                            #'(lambda (x) (OR (numberp x) (string-equal x nil)))
                            (format nil "~%   Must be a single number greater than ~%~A"  
                                    (apply #'aref (uniform-bnode-a-lower-table bnode) indices)))
                )
          )
        (progn ;;continuous parent => second slice use parent values
          (setf (apply #'aref (uniform-bnode-a-lower-table bnode) indices)   
                (apply #'aref (uniform-bnode-a-lower-table (first cparents)) indices)
                )
          (setf (apply #'aref (uniform-bnode-b-upper-table bnode) indices) 
                (apply #'aref (uniform-bnode-b-upper-table (first cparents)) indices)
                )
          )
        )
    )
  )


(defmethod create-bnode-conditional-distribution ((bnode probit-bnode))
  "Create and fill in tables of coefficients, means, and sigmas for a probit bnode."
  (let* ((dparents (discrete-parents bnode))
         (dimensions (mapcar #'discrete-bnode-arity dparents))
         (indices (make-list (length dimensions))))
    (setf (probit-bnode-coefficients-table bnode) (my-make-array dimensions))
    (setf (probit-bnode-mu-table bnode) (my-make-array dimensions))
    (setf (probit-bnode-sigma-table bnode) (my-make-array dimensions))
    (fill-tables bnode dparents dimensions indices indices 2)))

(defmethod fill-table-entry ((bnode probit-bnode) indices indent)
  "Fill in the probit function for bnode conditioned on parent values in indices."
  (let* ((cparents (continuous-parents bnode))
         (cparent-names (mapcar #'bnode-name cparents))
         (n (length cparent-names)))
    (setf (apply #'aref (probit-bnode-coefficients-table bnode) indices)
          (if (zerop n) nil
              (query-user (format nil "~%~A   Enter list of ~A coefficients for continuous parents ~A   " (spaces indent) n cparent-names)
                          #'(lambda (x) (and (listp x) (every #'numberp x) (= (length x) n)))
                          (format nil "~%~A   Must be a list of ~A numbers" (spaces indent) n))))
    (setf (apply #'aref (probit-bnode-mu-table bnode) indices)
          (query-user (format nil "~%~A   Enter mean of the Gaussian on which the probit is based   " (spaces indent))
                      #'numberp
                      (format nil "~%~A   Must be a single number" (spaces indent))))
    (setf (apply #'aref (probit-bnode-sigma-table bnode) indices)
          (query-user (format nil "~%~A   Enter standard deviation for Gaussian   " (spaces indent))
                      #'(lambda (x) (and (numberp x) (plusp x)))
                      (format nil "~%~A   Must be a single positive number" (spaces indent))))))

(defun sort-bayes-net (bn)
  "Return BN with its nodes sorted topologically according to parent links."
  (setf (bn-nodes bn) (topological-BN-sort (bn-nodes bn)))
  bn)

(defun topological-BN-sort (nodes &optional (predecessors nil))
  "Return list of nodes in topological order."
  (if (null nodes)
      predecessors
      (let ((next-layer (remove-if-not #'(lambda (bnode)
                                           (every #'(lambda (parent)
                                                      (not (member parent nodes)))
                                                  (bnode-parents bnode)))
                                       nodes)))
        (topological-BN-sort (remove-if #'(lambda (bnode)
                                            (member bnode next-layer :test #'eq))
                                        nodes)
                             (append predecessors next-layer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functions for displaying Bayes nets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun display-bayes-net (bn &optional (display-distributions? t))
  "Returns a pretty-printed version of the network and, optionally, its distributions."
  (let (display-accum)
    (loop for bnode in (bn-nodes bn) do
          (setf display-accum 
                (format NIL "~A~A~%~%" display-accum
                        (display-node bnode display-distributions?))))
    display-accum))

(defun display-node (bnode &optional (display-distribution? t)
                           &aux (type (type-of bnode)))
  "Display a node and, optionally, its distribution, according to node type."
  (format NIL "Node: ~A~%Parents: ~A~%Type: ~A~A"
          (bnode-name bnode) (mapcar #'bnode-name (bnode-parents bnode)) type
          (if display-distribution? (display-distribution bnode) "")))

(defgeneric display-distribution (bnode))

(defmethod display-distribution ((bnode tabulated-bnode))
  "Display tabulated distribution for a BN node."
  (let* ((name (bnode-name bnode))
         (value-names (discrete-bnode-value-names bnode))
         (parent-names (mapcar #'bnode-name (bnode-parents bnode)))
         (parent-value-names (mapcar #'discrete-bnode-value-names (bnode-parents bnode)))
         (condition-width (max 12 (* 18 (length parent-names))))
         (distribution-width (* 9 (length value-names)))
         )
    (format NIL "~%P(~A|~{~A,~}~A)~%~v<~A= ~>|~{ ~8A~}~%~v,,,'-A~A" 
            name (butlast parent-names) (car (last parent-names))
            condition-width name value-names
            (+ condition-width 1 distribution-width) ""
            (list-tables bnode condition-width parent-names parent-value-names 
                         (array-dimensions (tabulated-bnode-cpt bnode))))))

(defun list-tables (bnode condition-width parent-names parent-value-names dimensions)
  "Recursively enumerate and print the information in tables."
  (let ((display-accum ""))
    (mapindices
     #'(lambda (indices)
         (let ((eqns 
                (mapcar #'(lambda (parent-name value-name) (list parent-name value-name))
                        parent-names (mapcar #'(lambda (i value-names) (nth i value-names))
                                             indices parent-value-names))))
           (setf display-accum
                 (format NIL "~A~%~A~A"
                         display-accum
                         (if eqns
                             (format NIL "~v<~:{~8@A=~9@<~A,~>~}~?~>"
                                     condition-width (butlast eqns) "~8@A=~9@<~A~>" 
                                     (car (last eqns)))
                             (format NIL "~vA" condition-width ""))
                         (list-table-entry bnode indices condition-width)))))
     dimensions)
    display-accum))

(defmethod list-table-entry ((bnode tabulated-bnode) indices condition-width)
  "Display the distribution for a tabulated bnode given specific parent values."
  (declare (ignore condition-width))
  (format NIL "|~{ ~8,6F~}" (coerce (apply #'aref (tabulated-bnode-cpt bnode) indices) 'list)))

(defmethod display-distribution ((bnode typeless-bnode))
  "Display function definition for a determinstic BN node."
  (let* (;;(name (bnode-name bnode))
                 ;;;;(parent-names (mapcar #'bnode-name (bnode-parents bnode)))
                 (fx (typeless-bnode-function bnode)))
    (format NIL "~%Function: ~A" fx)))

(defmethod display-distribution ((bnode deterministic-bnode))
  "Display function definition for a determinstic BN node."
  (let* (;;(name (bnode-name bnode))
                 ;;;;(parent-names (mapcar #'bnode-name (bnode-parents bnode)))
                 (fx (deterministic-bnode-function bnode))
                 )
    (format NIL "~%Function: ~A " fx )) 
  )

(defmethod display-distribution ((bnode arbitrary-continuous-bnode))
  "Display function definition for an arbitrary continuous BN node."
  (let* (;;(name (bnode-name bnode))
                 ;;(parent-names (mapcar #'bnode-name (bnode-parents bnode)))
                 (fx (arbitrary-continuous-bnode-function bnode)))
    (format NIL "~%Function: ~A" fx)))

(defmethod display-distribution ((bnode arbitrary-discrete-bnode))
  "Display function definition for an arbitrary discrete BN node."
  (let* (;;(name (bnode-name bnode))
                 ;;(parent-names (mapcar #'bnode-name (bnode-parents bnode)))
                 (fx (arbitrary-discrete-bnode-function bnode)))
    (format NIL "~%Function: ~A" fx)))

(defmethod display-distribution ((bnode noisy-or-bnode))
  "Display coefficient information for a noisy-or BN node."
  (let* ((coefficients (noisy-or-bnode-coefficients bnode)))
    (format NIL "~%Noisy-OR coefficients are ~A" coefficients)))

(defmethod display-distribution ((bnode linear-gaussian-bnode))
  "Display parameters for a linear-Gaussian BN node."
  (let* ((name (bnode-name bnode))
         (parents (bnode-parents bnode))
         (parent-names (mapcar #'bnode-name parents)) 
         (dparents (discrete-parents bnode))
         (dparent-names (mapcar #'bnode-name dparents)) 
         (dparent-value-names (mapcar #'discrete-bnode-value-names dparents))
         (condition-width (max 12 (* 18 (length dparent-names)))))
    (format NIL "~%P(~A|~{~A,~}~A)~%~v,,,'-A~A" 
            name (butlast parent-names) (car (last parent-names))
            (+ condition-width 2) ""
            (list-tables bnode condition-width dparent-names dparent-value-names 
                         (mapcar #'discrete-bnode-arity dparents)))))


(defmethod list-table-entry ((bnode linear-gaussian-bnode) indices condition-width)
  "Display the parameters of a linear-Gaussian model given specific
   values for the discrete parents."
  (let* ((cparents (continuous-parents bnode))
         (cparent-names (mapcar #'bnode-name cparents))
         (coefficients (apply #'aref (linear-gaussian-bnode-coefficients-table bnode) indices))
         (offset (apply #'aref (linear-gaussian-bnode-offset-table bnode) indices))
         (sigma (apply #'aref (linear-gaussian-bnode-sigma-table bnode) indices)))
    (format NIL "| ~~ Gaussian with mean ~A + ~A.~A~%~vA|    and standard deviation ~A" 
            offset coefficients cparent-names
            condition-width ""
            sigma)))

(defmethod display-distribution ((bnode truncated-gaussian-bnode))
  "Display parameters for a truncated-Gaussian BN node."
  (let* ((name (bnode-name bnode))
         (parents (bnode-parents bnode))
         (parent-names (mapcar #'bnode-name parents)) 
         (dparents (discrete-parents bnode))
         (dparent-names (mapcar #'bnode-name dparents)) 
         (dparent-value-names (mapcar #'discrete-bnode-value-names dparents))
         (condition-width (max 12 (* 18 (length dparent-names)))))
    (format NIL "~%P(~A|~{~A,~}~A)~%~v,,,'-A~A" 
            name (butlast parent-names) (car (last parent-names))
            (+ condition-width 2) ""
            (list-tables bnode condition-width dparent-names dparent-value-names 
                         (mapcar #'discrete-bnode-arity dparents)))))


(defmethod list-table-entry ((bnode truncated-gaussian-bnode) indices condition-width)
  "Display the parameters of a truncated-Gaussian model given specific
   values for the discrete parents."
  (let* ((cparents (continuous-parents bnode))
         (cparent-names (mapcar #'bnode-name cparents))
         (coefficients (apply #'aref (truncated-gaussian-bnode-coefficients-table bnode) indices))
         (offset (apply #'aref (truncated-gaussian-bnode-offset-table bnode) indices))
         (sigma (apply #'aref (truncated-gaussian-bnode-sigma-table bnode) indices))
         (lower-limit (apply #'aref (truncated-gaussian-bnode-lower-limit-table bnode) indices))
         (upper-limit (apply #'aref (truncated-gaussian-bnode-upper-limit-table bnode) indices)))
    (format NIL "| ~~ Truncated Gaussian with mean ~A + ~A.~A~%~vA|    and standard deviation ~A ~%~vA|    Lower Limit ~A Upper Limit ~A" 
            offset coefficients cparent-names
            condition-width ""
            sigma condition-width "" lower-limit upper-limit)
    
    ;;(format NIL "~% | ~~ Node Lower Limit ~A Node Upper Limit ~A" 
    ;;       lower-limit upper-limit)
    ))

(defmethod display-distribution ((bnode uniform-bnode))
  "Display Uniform Distribution."
  (let* ((name (bnode-name bnode))
         (parents (bnode-parents bnode))
         (parent-names (mapcar #'bnode-name parents)) 
         (dparents (discrete-parents bnode))
         (dparent-names (mapcar #'bnode-name dparents)) 
         (dparent-value-names (mapcar #'discrete-bnode-value-names dparents))
         (condition-width (max 12 (* 18 (length dparent-names)))))
    (format NIL "~%P(~A|~{~A,~}~A)~%~v,,,'-A~A" 
            name (butlast parent-names) (car (last parent-names))
            (+ condition-width 2) ""
            (list-tables bnode condition-width dparent-names dparent-value-names 
                         (mapcar #'discrete-bnode-arity dparents)))))

(defmethod list-table-entry ((bnode uniform-bnode) indices condition-width)
  "Display the parameters of a truncated-Gaussian model given specific
   values for the discrete parents."
  
  (format NIL "~% Uniform Distribution [~A, ~A] " (apply #'aref (uniform-bnode-a-lower-table bnode) indices)
          (apply #'aref (uniform-bnode-b-upper-table bnode) indices))
  
  )


(defmethod display-distribution ((bnode probit-bnode))
  "Display parameters for a probit BN node."
  (let* ((name (bnode-name bnode))
         (parents (bnode-parents bnode))
         (parent-names (mapcar #'bnode-name parents)) 
         (dparents (discrete-parents bnode))
         (dparent-names (mapcar #'bnode-name dparents)) 
         (dparent-value-names (mapcar #'discrete-bnode-value-names dparents))
         (condition-width (max 12 (* 18 (length dparent-names)))))
    (format NIL "~%P(~A|~{~A,~}~A)~%~v,,,'-A~A"
            name (butlast parent-names) (car (last parent-names))
            (+ condition-width 2) ""
            (list-tables bnode condition-width dparent-names dparent-value-names 
                         (mapcar #'discrete-bnode-arity dparents)))))


(defmethod list-table-entry ((bnode probit-bnode) indices condition-width)
  "Display the parameters of a probit model given specific
   values for the discrete parents."
  (let* ((cparents (continuous-parents bnode))
         (cparent-names (mapcar #'bnode-name cparents))
         (coefficients (apply #'aref (probit-bnode-coefficients-table bnode) indices))
         (mu (apply #'aref (probit-bnode-mu-table bnode) indices))
         (sigma (apply #'aref (probit-bnode-sigma-table bnode) indices)))
    (format NIL 
            "| ~~ probit function of ~A.~A~%~vA|    with mean ~A and standard deviation ~A" 
            coefficients cparent-names
            condition-width ""
            mu sigma)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Functions for creating events

(defun create-event (bn &aux (bnode-name t) value-name
                        (alist nil))
  "Creates and returns an event by querying the user."
  (loop until (null bnode-name) do 
        (multiple-value-setq (bnode-name value-name) (create-event-entry bn))
        (when bnode-name (push (cons bnode-name value-name) alist)))
  (alist->event alist bn))

(defun create-event-entry (bn)
  ;; I'm leaving typeless-bnode out of this right now because I don't think I need it;
  ;; it should never have evidence set.
  "Return the name of the node whose value is to be set, and the value.
   Discrete nodes need a value name, continuous nodes just a number."
  (let* ((bnode-names (mapcar #'bnode-name (bn-nodes bn)))
         (bnode-name 
          (query-user (format nil "~%Name of node to set (nil if none)?  ")
                      #'(lambda (x) (or (null x) (member x bnode-names)))
                      (format nil "~%Must be one of ~A or nil" bnode-names))))
    (if bnode-name
        (let* ((bnode (bnode-by-name bnode-name bn))
               (value-names (discrete-bnode-value-names bnode)))
          (case (type-of bnode)
                ((tabulated-bnode probit-bnode arbitrary-discrete-bnode)
                 (values bnode-name
                         (query-user (format nil "~%Value to set it to?  ")
                                     #'(lambda (x) (member x value-names))
                                     (format nil "~%Must be one of ~A" value-names))))
                ((linear-gaussian-bnode deterministic-bnode arbitrary-continuous-bnode truncated-gaussian-bnode)
                 (values bnode-name
                         (query-user (format nil "~%Value to set it to?  ")
                                     #'numberp
                                     (format nil "~%Must be a number"))))))
        (values nil nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Functions for saving and loading Bayes nets

(defun save-bn (bn file &aux (save-circle *print-circle*))
  "Writes a BN to file, such that it can be restored using load-bn."
  (setq *print-circle* t)
  (clear-compiled-functions bn)
  (with-open-file (s file :direction :output :if-does-not-exist :create 
                     :if-exists :supersede)
                  (pprint bn s))
  (setq *print-circle* save-circle)
  t)

(defun load-bn (file)
  "Returns a BN read in from file."
  (with-open-file (s file :direction :input) (read s)))

