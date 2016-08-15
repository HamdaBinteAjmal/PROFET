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
;;; File: agents/test.lisp -*- Mode: Lisp; -*-

(deftest agents
  "Test agents in the vacuum and wumpus worlds." 
  "Here is how to run an environment, in this case the vacuum world."

  ((run-environment (make-vacuum-world :agents (list (make-reflex-vacuum-agent)) :max-steps 10))
   (>= (agent-score (first (environment-agents *))) 18))

  )
