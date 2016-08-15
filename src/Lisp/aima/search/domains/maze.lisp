;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: tsp.lisp
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
;;;; Grid maze problems

;;; Each state is simply a pair of integers denoting the coordinates of a maze square.
;;; By default, (0 0) is the start square and the goal is the opposite corner.
;;; Actions (Up, Down, Left, Right) are stored in a hash table indexed by state.
;;; They have the usual effect except at the
;;; boundaries of the grid. "Walls" are made by removing actions.


(defstruct (maze-problem (:include enumerated-problem))
  xsize ysize)

;;; Use default goal test: state in (enumerated-problem-goal-states problem)
;;; Use default step-cost = 1
;;; Use default actions from hash table

(defmethod result ((problem maze-problem) action state)
  "Return the state resulting from executing action in state."
  (let ((outcome (copy-xy state))
	(xsize (maze-problem-xsize problem))
	(ysize (maze-problem-ysize problem)))
    (case action
	  (Up (when (< (elt outcome 1) (1- ysize)) (incf (elt outcome 1))))
	  (Down (when (> (elt outcome 1) 0) (decf (elt outcome 1))))
	  (Left (when (> (elt outcome 0) 0) (decf (elt outcome 0))))
	  (Right (when (< (elt outcome 0) (1- xsize)) (incf (elt outcome 0)))))
    outcome))

(defmethod step-cost ((problem maze-problem) state1 action state2)
  "1 per move."
  (declare-ignore state1 action state2)
  1)

(defmethod h-cost ((problem maze-problem) state)
  "Addmissible heuristic: Manhattan distance assumes no barriers."
  (x+y-distance state (first (enumerated-problem-goal-states problem))))

;;;; Simple maze problem [2e p 124]

(defvar *simple-maze-problem*)

(setq *simple-maze-problem*
  (make-maze-problem
   :xsize 3 :ysize 3
   :initial-state (@ 0 0)
   :goal-states (list (@ 2 2))
   :actions (alist->hash-table
	     `((,(@ 0 0) . (Up Right))
	       (,(@ 0 1) . (Down))
	       (,(@ 0 2) . (Right))
	       (,(@ 1 0) . (Up Left Right))
	       (,(@ 1 1) . (Up Down))
	       (,(@ 1 2) . (Down Left))
	       (,(@ 2 0) . (Up Left))
	       (,(@ 2 1) . (Up Down))
	       (,(@ 2 2) . (Down))))))

(defmethod print-object ((maze maze-problem) stream)
  (let ((start (problem-initial-state maze))
	(goal (first (enumerated-problem-goal-states maze)))
	(xsize (maze-problem-xsize maze))
	(ysize (maze-problem-ysize maze))
	(actions (maze-problem-actions maze)))
    (loop for y from (1- ysize) downto 0 do
	  (format stream "~%")
	  (loop for x from 0 to (1- xsize) do
		(format stream "*")
		(format stream (if (member 'up (gethash (@ x y) actions)) "   " "---")))
	  (format stream "*~%")
	  (loop for x from 0 to (1- xsize) do
		(format stream (if (member 'Left (gethash (@ x y) actions)) " " "|"))
		(format stream (if (equal start (@ x y)) " S "
			    (if (equal goal (@ x y)) " G " "   "))))
	  (format stream "|"))
    (format stream "~%")
    (loop for x from 0 to (1- xsize) do
	  (format stream "*---"))
    (format stream "*~%")))



    