;;; search/domains/poproblem-env.lisp
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
;;; Code for defining environments based on partially observable problems.
;;; In these environments, the agent's percept is obtained by applying the
;;; get-percept method of the underlying poproblem.

(defstruct (poproblem-environment (:include problem-environment))
  "An environment in which to solve problems.  The state of the environment
  is one of the states from the problem, starting with the initial state.")

(defmethod get-percept ((env poproblem-environment) agent)
  "Percept is generated using the percept function of the poproblem."
  (declare-ignore agent)
  (get-percept (problem-environment-problem env) (environment-state env)))

(defun poproblem->environment (poproblem &key (agents (list (new-random-problem-solving-agent :problem poproblem))))
  "Convert a partially observable problem into an environment, with a random agent."
  (make-poproblem-environment :agents agents :problem poproblem))

