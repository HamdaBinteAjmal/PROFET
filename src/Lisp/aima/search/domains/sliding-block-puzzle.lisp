;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
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
;;;; The 8-Puzzle Problem and Other Sliding-Block Puzzles

;;; Definitions and heuristics for the 8-puzzle, 15-puzzle, etc.
;;; Calling (make-tile-puzzle :n n) defines an "n^2-1"-puzzle instance
;;; on an n x n board with a random initial configuration. 

(defstruct (sliding-block-puzzle (:include enumerated-goals-problem) 
			   (:constructor create-sliding-block-puzzle))
  n              ;;; The size of the puzzle (n x n)
  inverse-goal   ;;; Inverse lookup for tiles in goal state
  )

(defun make-sliding-block-puzzle 
       (&key (n 3) 
	     (initial-state nil) 
	     (goal-states (list (make-sliding-block-puzzle-goal-state n))))
  "Create and return a sliding-block-puzzle problem. n specifies the size,
   which defaults to 3x3. If the initial state is not supplied,
   a solvable one is generated randomly. If no goal state is supplied,
   we use the standard one with tiles in numerical order."
  (let ((problem (create-sliding-block-puzzle 
		  :n n
		  :initial-state initial-state
		  :goal-states goal-states
		  :inverse-goal (sliding-block-puzzle-invert (first goal-states)))))
    (unless initial-state
      (setf (problem-initial-state problem)
	    (random-sliding-block-puzzle-initial-state n (first goal-states) problem)))
    problem))

;;; A state is defined by an n x n array containing the tiles (integers).
;;; We include the location of the blank as an xy pair (two-element list)
(defstruct sliding-block-puzzle-state
  array blank)

(defmethod actions ((problem sliding-block-puzzle) state)
  "Return a list of legal actions."
  (let* ((blank (sliding-block-puzzle-state-blank state))
	 (x (xy-x blank)) (y (xy-y blank))
	 (n (sliding-block-puzzle-n problem))
	 (actions nil))
    (when (> x 0) (push 'left actions))
    (when (> y 0) (push 'down actions))
    (when (< x (- n 1)) (push 'right actions))
    (when (< y (- n 1)) (push 'up actions))
    actions))

(defmethod copy-state ((state sliding-block-puzzle-state))
  (make-sliding-block-puzzle-state 
   :array (copy-array (sliding-block-puzzle-state-array state))
   :blank (copy-xy (sliding-block-puzzle-state-blank state))))

(defmethod result ((problem sliding-block-puzzle) action state)
  "Return the new state that results from doing this action."
  (let* ((outcome (copy-state state))
	 (old-blank (sliding-block-puzzle-state-blank state))
	 (new-blank (sliding-block-puzzle-state-blank outcome))
	 (array (sliding-block-puzzle-state-array outcome)))
    (case action 
      (left  (decf (xy-x new-blank)))
      (right (incf (xy-x new-blank)))
      (up    (incf (xy-y new-blank)))
      (down  (decf (xy-y new-blank))))
    (setf (apply #'aref array old-blank) (apply #'aref array new-blank))
    (setf (apply #'aref array new-blank) 0)
    outcome))

(defmethod h-cost ((problem sliding-block-puzzle) state)
  (manhattan-distance problem state))

(defun manhattan-distance (problem state)
  "Manhattan, or sum of city block distances.  This is h_2 on [2e p. 106]."
  (let ((sum 0)
	(n (sliding-block-puzzle-n problem))
	(array (sliding-block-puzzle-state-array state))
	(inverse-goal (sliding-block-puzzle-inverse-goal problem)))
    (loop for x from 0 to (- n 1) do
      (loop for y from 0 to (- n 1) do
	(let ((tile (aref array x y)))
	  (unless (= tile 0)
	    (incf sum (x+y-distance 
		       (list x y)
		       (aref inverse-goal tile)))))))
    sum))

(defmethod state-hash-key ((problem sliding-block-puzzle) state)
  "Returns a unique integer key for each state by treating
   the array as a number in base n^2 with n^2 digits."
  (let ((sum 0) (power 1)
	(n (sliding-block-puzzle-n problem))
	(array (sliding-block-puzzle-state-array state)))
    (loop for x from 0 to (- n 1) do
      (loop for y from 0 to (- n 1) do
	(incf sum (* power (aref array x y)))
	(setf power (* power n n))))
    sum))
	   
(defun sliding-block-puzzle-invert (state)
  "Return a one-dimensional array, indexed by tile, giving the
   location of that tile in the state."
  (let* ((array (sliding-block-puzzle-state-array state))
	 (n (array-dimension array 0))
	 (inverse (make-array (* n n))))
    (loop for x from 0 to (- n 1) do
      (loop for y from 0 to (- n 1) do
	(setf (aref inverse (aref array x y)) (list x y))))
    inverse))

(defun random-sliding-block-puzzle-initial-state (n state problem)
  "Return a random state of the n^2-1 puzzle. This is done by
   applying a long sequence of random moves to the goal state."
  (loop for i from 1 to (* 10 n n) do
       (setf state 
	     (result problem (random-element (actions problem state)) state)))
  state)

(defun make-sliding-block-puzzle-goal-state (n)
  "Return the standard goal state with 0 in the top left and
   tiles in increasing order."
  (let ((goal (make-sliding-block-puzzle-state
	    :blank (list 0 (- n 1)) :array (make-array (list n n)))))
    (loop for tile from 0 to (- (* n n) 1) do
      (setf (aref (sliding-block-puzzle-state-array goal)
		  (mod tile n) (floor (- (* n n) 1 tile) n)) 
	    tile))
    goal))

(defmethod print-object ((state sliding-block-puzzle-state) stream)
  "Print the state in tabular format and return it."
  (let* ((array (sliding-block-puzzle-state-array state))
	 (n (array-dimension array 0))
	 (digits (1+ (floor (log (- (* n n) 1) 10)))))
    (loop for y from (- n 1) downto 0 do
      (format stream "~%") 	  
      (loop for x from 0 to (- n 1) do	  
        (let ((tile (aref array x y)))
	  (if (= tile 0)
	      (format stream "~V@T." digits)
	    (format stream " ~VD" digits tile)))))
    (format stream "~%"))
  state) 	  


;;;; Alternative Heuristic Function

(defun misplaced-tiles (problem state)
  "Number of misplaced tiles.  This is h_1 on [2e p. 106]."
  (let ((sum 0)
        (n (sliding-block-puzzle-n problem))
        (array (sliding-block-puzzle-state-array state))
        (goal-array (sliding-block-puzzle-state-array (first (sliding-block-puzzle-goal-states problem)))))
    (loop for x from 0 to (- n 1) do
      (loop for y from 0 to (- n 1) do
        (let ((tile (aref array x y)))
          (unless (or (= tile 0) (= tile (aref goal-array x y)))
            (incf sum)))))
    sum))

;;;; Example problem instance

(defparameter *Figure-3.4-8puzzle-problem* 
  (make-sliding-block-puzzle 
   :initial-state (make-sliding-block-puzzle-state
		   :array #2A((8 5 7) (3 0 2) (1 6 4)) :blank '(1 1))))


