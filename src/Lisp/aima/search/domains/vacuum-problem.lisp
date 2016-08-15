;;; File: search/domains/vacuum-problem.lisp
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
;;;; The deterministic vacuum world expressed as a search problem [2e p 65]

(defstruct (vacuum-problem (:include problem) (:constructor create-vacuum-problem))
  (xsize 2) (ysize 1)   ;;; World is xsize x ysize squares, defaults to 2 x 1
  )

(defstruct (vacuum-state)
  location                     ; xy agent location 
  dirt                         ; array  of t/nil for dirt locations
  (dirt-amount 0)              ; number of dirty squares remaining
  )

(defmethod copy-state ((state vacuum-state))
  (make-vacuum-state :location (copy-list (vacuum-state-location state))
		     :dirt (copy-array (vacuum-state-dirt state))
		     :dirt-amount (vacuum-state-dirt-amount state)))


;;;;  Vacuum problem generator

(defun make-vacuum-problem (&key (xsize 2) (ysize 1) (location (@ 0 0)) (dirt nil) (dirt-probability 0.5))
  "Create a Vacuum World problem."
  (create-vacuum-problem
   :xsize xsize :ysize ysize
   :initial-state      (make-vacuum-initial-state xsize ysize location dirt dirt-probability)
   ))

(defun make-vacuum-initial-state (xsize ysize location dirt dirt-probability)
  (let ((state (make-vacuum-state :location location)))
    (unless dirt
      (setf dirt (make-array (list xsize ysize) :initial-element nil))
      (dmap-array #'(lambda (x) (declare (ignore x)) (> dirt-probability (random 1.0)))
		  dirt))
    (setf (vacuum-state-dirt state) dirt)
    (mapindices #'(lambda (xy) (when (apply #'aref dirt xy) (incf (vacuum-state-dirt-amount state))))
		(list xsize ysize))
    state))

;;;; Vacuum domain functions

(defmethod actions ((problem vacuum-problem) state)
  (declare (ignore state))
  (append (when (> (vacuum-problem-xsize problem) 1) '(Left Right))
	  (when (> (vacuum-problem-ysize problem) 1) '(Up Down))
	  '(Suck NoOp)))

(defmethod result ((problem vacuum-problem) action state)
  (let* ((outcome (copy-state state))
	 (location (vacuum-state-location outcome))
	 (dirt (vacuum-state-dirt outcome))
	 (xsize (vacuum-problem-xsize problem))
	 (ysize (vacuum-problem-ysize problem)))
    (case action 
	  (Up (when (< (elt location 1) (1- ysize)) (incf (elt location 1))))
	  (Down (when (> (elt location 1) 0) (decf (elt location 1))))
	  (Left (when (> (elt location 0) 0) (decf (elt location 0))))
	  (Right (when (< (elt location 0) (1- xsize)) (incf (elt location 0))))
	  (Suck (when (apply #'aref dirt location)
		  (setf (apply #'aref dirt location) nil)
		  (decf (vacuum-state-dirt-amount outcome))))
	  (NoOp))
    outcome))

(defmethod goal-test ((problem vacuum-problem) state)
  "Is this a goal state?"
  (zerop (vacuum-state-dirt-amount state)))

(defmethod h-cost ((problem vacuum-problem) state) 
  "Cost to finish clean-up is at least twice the number of dirty squares
   (moves + sucks) minus 1 if we need not move."
  (let ((dirt-amount (vacuum-state-dirt-amount state)))
    (if (= dirt-amount 0) 0
      (- (* 2 dirt-amount)
	 (if (apply #'aref (vacuum-state-dirt state)
		    (vacuum-state-location state))
	     1 0)))))

;;; step-cost is inherited: 1 per step.