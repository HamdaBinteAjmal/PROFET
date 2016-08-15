;;;; Definitions for Enumerated Problems (Tabulated States, Actions, Etc.)
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
;;; An enumerated-goals problem is one in which the goal
;;; is defined by a list of states. This is sufficiently
;;; common that it merits its own class.

(defstruct (enumerated-goals-problem (:include problem))
  goal-states   ;;; list of states that satisfy goal test
  )

(defmethod goal-test ((problem enumerated-goals-problem) state)
  (member state (enumerated-goals-problem-goal-states problem)
	  :test #'(lambda (s1 s2)
		    (equalp (state-hash-key problem s1)
			    (state-hash-key problem s2)))))


;;; An enumerated problem is one in which all states, actions, and
;;; costs are enumerated explicitly and stored in hash tables.
;;; Examples include route-finding problems and mazes.
;;; For such problems, we need only create the tables -- the
;;; methods are the same for all such problems, except possibly for
;;; state and action hash keys. A problem with enumerated states but
;;; constant step costs may want to override the step-cost method
;;; instead of building a large table of 1s. Generally speaking,
;;; the h-cost method will be defined by a function rather than a table.

(defstruct (enumerated-problem (:include enumerated-goals-problem))
  actions       ;;; hash table, indexed by state, of legal actions
  result        ;;; hash table, indexed by (state action), of action results
  step-cost     ;;; hash table, indexed by (state action state)
  )

(defmethod actions ((problem enumerated-problem) state)
  (gethash (state-hash-key problem state) 
	   (enumerated-problem-actions problem)))

(defmethod result ((problem enumerated-problem) action state)
  (gethash (list (state-hash-key problem state)
		 (action-hash-key problem action)) 
	   (enumerated-problem-result problem)))

(defmethod step-cost ((problem enumerated-problem) state1 action state2)
  (gethash (list (state-hash-key problem state1)
		 (action-hash-key problem action)
		 (state-hash-key problem state2)) 
	   (enumerated-problem-step-cost problem)))


