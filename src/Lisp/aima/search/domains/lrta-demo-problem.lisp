;;;; The one-dimensional environment for illustrating LRTA* [2e p 128]
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
;;; States are points on the integer line, actions 
;;; move left and right by adding or substracting 1.


(defstruct (one-dimensional-problem (:include enumerated-goals-problem))
  h-cost     ;;; hash table, indexed by state
  n          ;;; number of states - ordered left to right, 1 to n
  )

(defmethod actions ((problem one-dimensional-problem) state)
  (let ((n (one-dimensional-problem-n problem)))
    (append (when (< state n) '(+1))
	    (when (> state 1) '(-1)))))

(defmethod result ((problem one-dimensional-problem) action state)
  (+ state action))

(defmethod h-cost ((problem one-dimensional-problem) state)
  (gethash state (one-dimensional-problem-h-cost problem)))

(defvar *lrta-demo-problem*)

(setf *lrta-demo-problem*
      (make-one-dimensional-problem
       :n 10
       :initial-state 4
       :goal-states '(10)
       :h-cost (alist->hash-table
		'((1 . 9)
		  (2 . 8)
		  (3 . 9)
		  (4 . 2)
		  (5 . 2)
		  (6 . 4)
		  (7 . 3)
		  (8 . 2)
		  (9 . 1)
		  (10 . 0)))))

		  