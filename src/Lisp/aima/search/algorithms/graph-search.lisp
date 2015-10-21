;; -*- Mode: Lisp -*-


;;;; Simple Search Algorithms for Graphs

;;; Here we define the GRAPH-SEARCH function, and then a set of
;;; search functions that follow specific search strategies.  
;;; We assume that the first path found to a repeated state
;;; is the best, and new paths are discarded. This can be
;;; suboptimal for inconsistent heuristics (even if admissible).

(defun graph-search (problem fringe &aux node)
  "Like tree-search, but new nodes are discarded if previously generated. [2e p 83]"
  (let ((closed (make-hash-table :test #'equalp)))
    (setf fringe (insert (create-start-node problem) fringe))
    (loop (if (empty? fringe) (RETURN :failure))
	  (setf node (remove-first fringe))
	  (if (goal-test problem (node-state node)) (RETURN (solution node)))
	  (unless (gethash (state-hash-key problem (node-state node)) closed)
	      (setf (gethash (state-hash-key problem (node-state node)) closed) (node-state node))
	      (setf fringe (insert-all (expand node problem) fringe))))))

(defun breadth-first-graph-search (problem)
  "Search the shallowest nodes in the search tree first. [2e p 73]"
  (graph-search problem (make-FIFO-queue nil)))

(defun uniform-cost-graph-search (problem)
  "Uniform-cost search expands nodes in order of g-cost. [2e p 75]"
  (graph-search problem (make-priority-queue nil #'node-g-cost)))

(defun depth-first-graph-search (problem)
  "Search the deepest nodes in the search tree first. [2e p 75]"
  (graph-search problem (make-LIFO-queue nil)))

;;;; Search Algorithms That Use Heuristic Information

(defun best-first-graph-search (problem eval-fn)
  "Search the nodes with the best evaluation first. [2e p 94]"
  (graph-search problem (make-priority-queue nil eval-fn)))

(defun greedy-best-first-graph-search (problem)
  "Best-first search using h(n) (heuristic distance to goal). [2e p 95]"
  (best-first-graph-search problem #'node-h-cost))

(defun a*-graph-search (problem)
  "Best-first search using estimated total cost, or f = g + h. [2e p 97]"
  (best-first-graph-search problem #'node-f-cost))

