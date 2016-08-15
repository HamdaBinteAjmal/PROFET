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

;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-

;;;; The Vacuum World [2e p 34]

(defstruct (vacuum-world (:include environment (history (make-no-history))) (:constructor create-vacuum-world))
  (xsize 2)         ;;; dimension in the x-direction (i.e., number of columns)
  (ysize 1)         ;;; dimension in the y-direction (i.e., number of rows)
  )

(defstruct vacuum-world-state
  dirt (dirt-amount 0) location)

;;;; Initializing the world - default method is random placement of dirt

(defun make-vacuum-world (&key (xsize 2) (ysize 1) (agents nil) (max-steps 1000)
			       (location (@ (random xsize) (random ysize))) 
			       (dirt nil) (dirt-probability 0.5))
  "Create a Vacuum World problem."
  (let ((env (create-vacuum-world :xsize xsize :ysize ysize :agents agents :max-steps max-steps))
	(state (make-vacuum-world-state :location location)))
    (unless dirt
      (setf dirt (make-array (list xsize ysize) :initial-element nil))
      (dmap-array #'(lambda (x) (declare (ignore x)) (> dirt-probability (random 1.0)))
		  dirt))
    (setf (vacuum-world-state-dirt state) dirt)
    (mapindices #'(lambda (xy) (when (apply #'aref dirt xy) (incf (vacuum-world-state-dirt-amount state))))
		(list xsize ysize))
    (setf (environment-state env) state)
    env))



;;;; Defining the generic functions

(defmethod get-percept ((env vacuum-world) agent)
  "Percept is a two-element sequence: location and Clean/Dirty."
  (declare (ignore agent))
  (let* ((state (vacuum-world-state env))
	 (location (vacuum-world-state-location state)))
    (list (if (and (= (vacuum-world-xsize env) 2) (= (vacuum-world-ysize env) 1))
	      (if (equal location (@ 0 0)) 'A 'B) ;;; for compatibility with book example
	    location)
	  (if (apply #'aref (vacuum-world-state-dirt state) location) 'Dirty 'Clean))))

;;; update-history is inherited directly for no-history type

;;; update-state is inherited directly - calls agent actions as generic functions

(defmethod performance-measure ((env vacuum-world) agent history state previous-score)
  "As defined on [2e p 36]: one point for each clean square at each time step."
  (declare (ignore agent history))
  (+ previous-score (- (* (vacuum-world-xsize env) (vacuum-world-ysize env))
		       (vacuum-world-state-dirt-amount state))))

(defmethod legal-actions ((env vacuum-world))
  (append (when (> (vacuum-world-xsize env) 1) '(Left Right))
	  (when (> (vacuum-world-ysize env) 1) '(Up Down))
	  '(Suck NoOp)))

(defmethod left ((env vacuum-world) agent-body)
  (declare (ignore agent-body))
  (let* ((state (vacuum-world-state env))
	 (location (vacuum-world-state-location state)))
    (when (> (elt location 0) 0) (decf (elt location 0)))))

(defmethod right ((env vacuum-world) agent-body)
  (declare (ignore agent-body))
  (let* ((state (vacuum-world-state env))
	 (location (vacuum-world-state-location state)))
    (when (< (elt location 0) (1- (vacuum-world-xsize env))) 
      (incf (elt location 0)))))

(defmethod down ((env vacuum-world) agent-body)
  (declare (ignore agent-body))
  (let* ((state (vacuum-world-state env))
	 (location (vacuum-world-state-location state)))
    (when (> (elt location 1) 0) (decf (elt location 1)))))

(defmethod up ((env vacuum-world) agent-body)
  (declare (ignore agent-body))
  (let* ((state (vacuum-world-state env))
	 (location (vacuum-world-state-location state)))
    (when (< (elt location 1) (1- (vacuum-world-ysize env))) (incf (elt location 1)))))

(defmethod suck ((env vacuum-world) agent-body)
  (declare (ignore agent-body))
  (let* ((state (vacuum-world-state env))
	(location (vacuum-world-state-location state))
	(dirt (vacuum-world-state-dirt state)))
    (when (apply #'aref dirt location)
      (setf (apply #'aref dirt location) nil)
      (decf (vacuum-world-state-dirt-amount state)))))

(defmethod noop ((env environment) agent-body)
  (declare (ignore agent-body)))


  

