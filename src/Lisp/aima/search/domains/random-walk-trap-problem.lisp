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

(defstruct (random-walk-trap-problem (:include problem (initial-state 1)))
  "The random walk trap environment expressed as a search problem. [2e p 127]"
  (n 11)   ;;; Number of states - must be odd. Numbered from left to right, 1 to n.
  )

;;;; Random-Walk-Trap domain functions

(defmethod actions ((problem random-walk-trap-problem) state)
  "Return a list of actions in current state."
  (if (oddp state)  ;;; in top row of states
      (let ((n (random-walk-trap-problem-n problem)))
	(append (when (< state n) '(+2))
		(when (> state 1) '(-1 -2))))
    '(-1)))

(defmethod result ((problem random-walk-trap-problem) action state)
  (+ state action))

(defmethod goal-test ((problem random-walk-trap-problem) state)
  "Is this a goal state?"
  (= state (random-walk-trap-problem-n problem)))

