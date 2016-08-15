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
;;; n-queens as a search problem. 
;;; We give both an incremental formulation [2e p 66] 
;;; and a complete-state formulation [2e p 110-111].
;;; We also provide the methods required for applying
;;; genetic algorithms to the complete-state formulation.

;;;; Incremental formulation: add one queen at a time, avoiding illegal choices.

(defstruct (nqueens-incremental-problem 
	    (:include problem) (:constructor create-nqueens-incremental-problem))
  n  ;;; the number of queens (n x n board)
  )

(defun make-nqueens-incremental-problem (&key (n 8))
  "Returns an nqueens problem instance with an empty board. In general, 
   a state is an n-element vector of queen positions, one per column.
   Positions (row numbers) are 1...n."
  (create-nqueens-incremental-problem 
   :n n :initial-state (make-sequence 'vector n :initial-element nil)))

(defmethod copy-state ((state vector)) (copy-seq state))

(defmethod goal-test ((problem nqueens-incremental-problem) state)
  "Return true if all queens have been placed, i.e., last queen is non-nil."
  (elt state (1- (nqueens-incremental-problem-n problem))))

(defmethod actions ((problem nqueens-incremental-problem) state)
  "Generate the possible moves from an nqueens-incremental state.
   A move is simply the row position of the queen in the next column."
  (let ((n (length state))
	(next-col (1+ (position nil state)))
	(actions nil))
    ;;; For each possible square, check if attacked by previously placed queens
    (loop for row from 1 to n do
      (unless (some #'(lambda (col)
			(let ((q (elt state (1- col))))
			  (or (= q row) (= (- next-col col) (abs (- q row))))))
		    (iota (1- next-col) 1))
	(push row actions)))
    actions))

(defmethod result ((problem nqueens-incremental-problem) action state)
  (let ((outcome (copy-state state)))
    (setf (elt outcome (position nil state)) action)
    outcome))
      
(defmethod h-cost ((problem nqueens-incremental-problem) state)
  "Number of unfilled columns."
  (let ((next-col (position nil state)))
    (if next-col (- (nqueens-incremental-problem-n problem) next-col) 0)))

(defun print-nqueens-state (state)
  "Print out nqueens board state."
  (let ((n (length state)))
    (loop for j from n downto 1 do
      (format t "~%")
      (loop for i from 1 to n do (format t (if (= (elt state (1- i)) j) "Q " ". ")))))
  state)


;;;; Complete-state formulation: start with all queens on
;;;; the board, pick any queen and move it in its column.

(defstruct (nqueens-complete-problem
	    (:include problem) (:constructor create-nqueens-complete-problem))
  n             ;;; the number of queens (n x n board)
  )

(defun make-nqueens-complete-problem (&key (n 8))
  "Returns an nqueens problem instance with all n queens placed
   randomly, one per column."
  (create-nqueens-complete-problem 
   :n n :initial-state (random-nqueens-complete-state n)))

(defmethod goal-test ((problem nqueens-complete-problem) state)
  (zerop (h-cost problem state)))

(defmethod actions ((problem nqueens-complete-problem) state)
  "Generate the possible moves from a complete nqueens state.
   A move is simply the column and the new row for that queen."
  (let ((n (length state))
	(actions nil))
    ;;; For each column, generate all other rows but the current one
    (loop for col from 1 to n do
      (let ((q (elt state (1- col))))
        (loop for row from 1 to n do
          (unless (= row q) (push (list col row) actions)))))
    actions))

(defmethod random-action ((problem nqueens-complete-problem) state)
  (let* ((n (length state))
	 (col (1+ (random n))))
    (list col (random-element (delete (aref state (1- col)) (iota n 1))))))

(defmethod result ((problem nqueens-complete-problem) action state)
  "Return a new state with the specified queen moved to the specified square."
  (let ((outcome (copy-state state)))
    (setf (elt outcome (1- (first action))) (second action))
    outcome))
      
(defmethod h-cost ((problem nqueens-complete-problem) state)
  "Number of pairs of queens attacking each other."
  (let ((n (length state))
	(sum 0))
    (loop for i from 1 to (- n 1) do
      (loop for j from (+ i 1) to n do
	(let ((delta (- (aref state (1- i)) (aref state (1- j)))))
	(when (or (= delta 0) (= (abs delta) (- j i)))
	  (incf sum)))))
    sum))

(defun random-nqueens-complete-state (n)
  "Return a random complete state with n queens, one per column."
  (let ((state (make-sequence 'vector n)))
    (loop for i from 1 to n do
       (setf (elt state (1- i)) (1+ (random n))))
    state))



;;;; Methods for genetic algorithms applied to complete-state nqueens

(defmethod GA-encode ((problem nqueens-complete-problem) state)
  "Encode state as a sequence - already in that form."
  state)

(defmethod GA-decode ((problem nqueens-complete-problem) individual)
  "Decode state from sequence - already in that form."
  individual)

(defmethod GA-alphabet ((problem nqueens-complete-problem))
  "Return the list of characters used in sequence form - 1...n."
  (iota (nqueens-complete-problem-n problem) 1))

(defmethod fitness ((problem nqueens-complete-problem) individual)
  "Return the number of non-attacks between queens."
  (let ((n (length individual)))
    (- (/ (* n (- n 1)) 2) (h-cost problem individual))))
