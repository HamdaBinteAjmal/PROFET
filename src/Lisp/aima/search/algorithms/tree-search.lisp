;; -*- Mode: Lisp -*-
;;; 
 ;; PROFET Copyright 2015 (c) Data Mining and Machine Learning Group,
 ;; National University of Ireland Galway.  
 ;; This file is a part of AIMA Framwork
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
;;; search/algorithms/tree-search.lisp
;;; Simple search algorithms for trees (or graphs treated thereas)

;;; Here we define the generic TREE-SEARCH function, which views the
;;; state space as a tree (i.e., doesn't check for repeated states).

(defun tree-search (problem fringe &aux node)
  "Expand nodes according to the specification of PROBLEM until we find
  a solution or run out of nodes to expand.  The FRINGE is a particular
  queue type (see utilities/queue.lisp), initially empty, whose insertion 
  order defines the behavior of the search algorithm. 
   Returns a solution or :failure. [2e p 72]"
  (setf fringe (insert (create-start-node problem) fringe))
  (loop (if (empty? fringe) (RETURN :failure))
	(setf node (remove-first fringe))
	(if (goal-test problem (node-state node)) 
	    (RETURN (solution node)))
	(setf fringe (insert-all (expand node problem) fringe))))

;;; Standard uninformed tree search algorithms are defined by calling the
;;; generic tree-search algorithm with the right queueing function.

(defun breadth-first-tree-search (problem)
  "Search the shallowest nodes in the search tree first. 
   Returns a solution or :failure. [2e p 73]"
  (tree-search problem (make-FIFO-queue nil)))

(defun uniform-cost-tree-search (problem)
  "Uniform-cost search expands nodes in order of g-cost. 
   Returns a solution or :failure. [2e p 75]"
  (tree-search problem (make-priority-queue nil #'node-g-cost)))

(defun depth-first-tree-search (problem)
  "Search the deepest nodes in the search tree first. 
   Returns a solution or :failure. [2e p 75]"
  (tree-search problem (make-LIFO-queue nil)))

(defun recursive-dfs (problem &optional (node (create-start-node problem)))
  "Recursive depth-first search. Returns a solution or :failure"
  (cond ((goal-test problem (node-state node)) (solution node))
	(t (loop for successor in (expand node problem) do
	     (let ((result (recursive-dfs problem successor)))
	       (if (not (eq result :failure)) (return result))))
	   :failure)))

(defun depth-limited-search (problem &optional (limit infinity)
                                               (node (create-start-node problem)))
  "Search depth-first, but only up to LIMIT branches deep in the tree. 
   Returns a solution, :failure, or :cutoff. [2e p 77]"
  (let ((cutoff-occurred? nil))
    (cond ((goal-test problem (node-state node)) (solution node))
	  ((>= (node-depth node) limit) :cut-off)
	  (t (loop for successor in (expand node problem) do
	       (let ((result (depth-limited-search problem limit successor)))
		 (cond ((eq result :cut-off) (setf cutoff-occurred? t))
		       ((not (eq result :failure)) (RETURN-FROM depth-limited-search result)))))
	     (if cutoff-occurred? :cut-off :failure)))))

(defun iterative-deepening-search (problem)
  "Do a series of depth-limited searches, increasing depth each time. 
   Returns a solution or :failure. [2e p 78]"
  (loop for depth from 0 to infinity do
    (let ((result (depth-limited-search problem depth)))
      (if (not (eq result :cut-off)) (RETURN result)))))

;;;; Search Algorithms That Use Heuristic Information

(defun best-first-tree-search (problem eval-fn)
  "Search the nodes with the best evaluation first. [2e p 94]"
  (tree-search problem (make-priority-queue nil eval-fn)))

(defun greedy-best-first-tree-search (problem)
  "Best-first search using H (heuristic distance to goal). [2e p 95]"
  (best-first-tree-search problem #'node-h-cost))

(defun a*-tree-search (problem)
  "Best-first search using estimated total cost, or (f = g + h). [2e p 97]"
  (best-first-tree-search problem #'node-f-cost))

;;;; Search Algorithms That Use Heuristic Information and Limited Memory

(defun ida* (problem)
  "Iterative Deepening A* [2e p 101]."
  ;; The main loop does a series of f-cost-bounded depth-first
  ;; searches until a solution is found. After each search, the f-cost
  ;; bound is increased to the smallest f-cost value found that
  ;; exceeds the previous bound.  
  (let* ((root (create-start-node problem))
	 (f-limit (node-f-cost root))
	 solution)
    (loop (multiple-value-setq (solution f-limit)
	    (f-limited-search root problem f-limit))
	(if (listp solution) (RETURN solution))
	(if (= f-limit infinity) (RETURN nil)))))

(defun f-limited-search (node problem f-limit)
  "Return a solution and a new f-cost limit."
  (let ((next-f-limit infinity))
    (cond ((> (node-f-cost node) f-limit) (values :cutoff (node-f-cost node)))
	  ((goal-test problem (node-state node)) (values (solution node) f-limit))
	  (t (loop for successor in (expand node problem) do
	       (multiple-value-bind (solution new-f-limit)
		 (f-limited-search successor problem f-limit)
		 (if (listp solution)
		     (RETURN-FROM f-limited-search (values solution f-limit)))
		 (setf next-f-limit (min next-f-limit new-f-limit))))
	     (values :failure next-f-limit)))))
	 

(defun recursive-best-first-search (problem &optional (node (create-start-node problem))
		                                      (f-limit infinity)
					    &aux successors best alternative)
  "Recursive Best-First Search [2e p 102]."
  (if (goal-test problem (node-state node)) (RETURN-FROM recursive-best-first-search (values (solution node) 0)))
  (setf successors (expand node problem))
  (if (null successors) (RETURN-FROM recursive-best-first-search (values :failure infinity)))
  (loop for s in successors do
    (setf (node-f-cost s) (max (+ (node-g-cost s) (node-h-cost s))
			       (node-f-cost node))))
  (loop
    (setf best (the-smallest #'node-f-cost successors))
    (if (> (node-f-cost best) f-limit) (RETURN-FROM recursive-best-first-search (values :failure (node-f-cost best))))
    (setf alternative 
	  (if (length>1 successors)
	      (apply #'min (mapcar #'node-f-cost (remove best successors)))
	    infinity))
    (multiple-value-bind (result f)
			 (recursive-best-first-search problem best (min f-limit alternative))
      (if (not (eq result :failure)) (RETURN-FROM recursive-best-first-search (values result 0)))
      (setf (node-f-cost best) f))))

