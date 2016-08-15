;;; search/domains/poproblems.lisp
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
;;; A partially observable problem is a problem in which the state
;;; is not directly observed, but is accessible only through a 
;;; deterministic get-percept method.


(defstruct (poproblem (:include problem)))

(defmethod get-percept ((problem poproblem) state)
  "Given the current true state, return the percept received by the agent.
   By default, the percept is just the state (the observable case).
   This method should be specialized for any particular poproblem."
  state)

