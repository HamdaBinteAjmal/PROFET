;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: iterative.lisp
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
;;;; Local Search Algorithms

;;; These functions differ slightly from the algorithms given in AIMA2e.
;;; 1) They check for states with no successors, which are local minima by definition.
;;; 2) They attempt to *minimize* a cost function on states (usually h).
;;; 3) They take a stopping criterion as argument, to allow for variants on the basic version.

(defstruct local-node
  "Nodes for local search - contain only state and cost, no parent or action."
  state
  cost)

(defun local-expand (node problem)
  "Return a list of nodes reachable from the current node."
  (mapcar #'(lambda (s) (make-local-node :state s :cost (cost problem s))) 
	  (successor-states problem (local-node-state node))))

(defmethod cost ((problem problem) state)
  "The default cost function is the h-cost, which may also work for
   non-local search algorithms."
  (h-cost problem state))


;;;; Hill climbing and variants

(defun hill-climbing (problem 
		      &optional (stopping-criterion #'minimum-or-flat)
		      &aux current neighbor)
  "Search by picking the least-cost successor according to an evaluation. [2e p 112]
  Stops according to stopping-criterion."
  (setf current (make-local-node :state (problem-initial-state problem) 
				 :cost (cost problem (problem-initial-state problem))))
  (loop
    (let ((successors (local-expand current problem)))
       (when (null successors) (RETURN current))
       (setf neighbor (the-smallest-random-tie #'local-node-cost successors))
       (when (funcall stopping-criterion (local-node-cost current) (local-node-cost neighbor))
	 (RETURN current))
       (setf current neighbor))))

(defun minimum-or-flat (current-h next-h)
  "Stop when the next state is no better than the current."
  (>= next-h current-h))

(defun local-minimum (current-h next-h)
  "Stop when the next state is worse than the current."
  (> next-h current-h))

(defun make-minimum-or-flat-n-times (n)
  "Return a function that stops when no improvement is made n times in a row."
  (let ((times-in-a-row 0))
    #'(lambda (current-h next-h)
	(cond ((> next-h current-h) t)
	      ((< next-h current-h)
	       (setf times-in-a-row 0)
	       nil)
	      ((>= (incf times-in-a-row) n))))))


;;;; Random-restart hill-climbing

(defun random-restart-hill-climbing (problem-generator
			      &optional (k 10)
			                (stopping-criterion #'minimum-or-flat))
  "Random-restart hill-climbing repeatedly calls hill-climbing
  PROBLEM-GENERATOR should return a problem with a random initial state.
  We look at k different initial states, and keep the best solution found.
  This version stops immediately if a goal is found."
  (let ((best (make-local-node :state nil :cost infinity)))
    (loop for i from 1 to k do
	 (let* ((problem (funcall problem-generator))
		(current (hill-climbing problem stopping-criterion)))
	     (when (goal-test problem (local-node-state current))
	       (RETURN-FROM random-restart-hill-climbing current))
	     (when (< (local-node-cost current) (local-node-cost best))
	       (setf best current))))
    best))


;;;; Simulated annealing


(defun simulated-annealing (problem &optional (schedule (make-exp-schedule))
				    &aux current best next temp delta_E)
  "See [2e p 116]; but we return the best result found, rather than the final result.
   Note also that we minimize costs rather than maximizing values."
  (setf current (make-local-node :state (problem-initial-state problem) 
				 :cost (cost problem (problem-initial-state problem)))
	best current)
  (loop for time from 1 to infinity do
	(setf temp (funcall schedule time))
	(when (< (local-node-cost current) (local-node-cost best)) 
	  (setf best current))
	(when (or (= temp 0) (goal-test problem (local-node-state best)))
	  (RETURN best))
	(setf next (let ((state (random-successor-state problem (local-node-state current))))
		     (make-local-node :state state :cost (cost problem state))))
	(setf delta_E (- (local-node-cost next) (local-node-cost current)))
	(when (or (< delta_E 0.0) ; < because we are minimizing costs
		  (< (random 1.0) (exp (/ (- delta_E) temp))))
	  (setf current next))))

(defun make-exp-schedule (&key (k 20) (lambda 0.005) (limit 100))
  "Return an exponential temperature schedule function with time limit.
   The k parameter determines the starting temperature; 
   this should be >> typical delta_E. The lambda parameter
   determines the decay rate; the half-life is about 1/lambda."
  #'(lambda (time) (if (< time limit)
		       (* k (exp (- (* lambda time))))
		     0)))


;;; Local beam search

(defun local-beam-search (problem-generator
			  &optional (k 10)
			            (stopping-criterion #'best-minimum-or-flat)
			  &aux best)
  "Run k hill-climbing searches, keeping the best k successors from all 
   k searches combined. Stop according to stopping-criterion, which
   is applied to the current and next state-lists."
  (let ((current-k nil) problem state)
    (loop for i from 1 to k do
      (setf problem (funcall problem-generator) state (problem-initial-state problem))
      (push (make-local-node :state state :cost (cost problem state))
	    current-k))
    (setf current-k (sort current-k #'< :key #'local-node-cost)
	  best (first current-k))
    (loop
     (when (goal-test problem (local-node-state best))
       (RETURN best))
     (when (< (local-node-cost (first current-k)) (local-node-cost best))
       (setf best (first current-k)))
     (let ((next-k
	    (subseq
	     (sort 
	      (delete-duplicates
	       (mapcan #'(lambda (node) (local-expand node problem)) current-k)
	       :test #'equalp
	       :key #'(lambda (node) (state-hash-key problem (local-node-state node))))
	      #'< :key #'local-node-cost)
	     0 k)))
       (when (or (null next-k)
		 (funcall stopping-criterion current-k next-k))
	 (RETURN (first current-k)))
       (setf current-k next-k)))))

(defun best-minimum-or-flat (current-k next-k)
  "For local-beam-search. Stop when the best next-k state is 
   no better than the best current-k."
  (>= (local-node-cost (first next-k)) (local-node-cost (first current-k))))

(defun best-local-minimum (current-k next-k)
  "For local-beam-search. Stop when the best next-k state is 
   worse than the best current-k."
  (>  (local-node-cost (first next-k)) (local-node-cost (first current-k))))

(defun make-best-minimum-or-flat-n-times (n)
  "Return a function that stops when no improvement is made N times in a row.
   Assumes that best-k lists are stored as (state . value) pairs."
  (let ((times-in-a-row 0))
    #'(lambda (current-k next-k)
	(cond ((> (local-node-cost (first next-k)) (local-node-cost (first current-k)))
	       t)
	      ((< (local-node-cost (first next-k)) (local-node-cost (first current-k)))
	       (setf times-in-a-row 0)
	       nil)
	      ((>= (incf times-in-a-row) n))))))

;;;; Genetic algorithms

;;; To apply the genetic algorithm to a search problem, call
;;; genetic-search with a random problem generator.
;;; The following methods are required:
;;;     GA-encode creates a string representation of the initial state.
;;;     GA-decode extracts a state from the final string found.
;;;     GA-alphabet returns a list of charatcers.
;;;     fitness returns a positive real number.

(defun genetic-search (problem-generator &optional (population-size 100) (steps 100)
					 &aux (population nil)
					      (sample-problem (funcall problem-generator)))
  "Wrapper for genetic algorithm: generates and encode a population of start states;
   runs genetic algorithm; decodes resulting best individual. As with other local
   search algorithms, returns a local search node with cost (not fitness!). 
   Suitable for use with complete-state problem formulations."
  (loop for i from 1 to population-size do
    (let ((problem (funcall problem-generator)))
      (push (GA-encode problem (problem-initial-state problem)) population)))
  (let* ((best-individual (genetic-algorithm population 
					     #'(lambda (individual) (fitness sample-problem individual))
					     (GA-alphabet sample-problem) steps))
	 (best-state (GA-decode sample-problem best-individual)))
    (make-local-node :state best-state :cost (cost sample-problem best-state))))

(defun genetic-algorithm (population fitness-fn alphabet &optional (steps 100) &key (mutation-probability 0.1))
  "Returns a high-fitness individuals after a specified number of generations. [2e p 119]"
  (loop for step from 1 to steps do
    (let ((new-population nil)
	  (population-fitness-distribution
	   (dnormalize-enumerated 
	    (mapcar #'(lambda (x) (cons x (funcall fitness-fn x))) population))))
      (loop for i from 1 to (length population) do
	(let* ((x (random-from-enumerated population-fitness-distribution))
	       (y (random-from-enumerated population-fitness-distribution))
	       (child (reproduce x y)))
	  (when (< (random 1.0) mutation-probability)
	    (setf child (mutate child alphabet)))
	  (push child new-population)))
      (setf population new-population)))
  (the-biggest fitness-fn population))

(defun reproduce (x y)
  "Return an individual obtained by crossover on x and y."
  (let* ((n (length x))
	 (c (1+ (random n))))
    (concatenate 'vector (subseq x 0 c) (subseq y c n))))

(defun mutate (individual alphabet)
  "Return an individual mutated at a randomly selected point."
  (let ((c (random (length individual))))
    (setf (elt individual c) (random-element alphabet))
    individual))

