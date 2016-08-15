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

(deftest mdps
  "Test code for MDPs."

  ((setq agent (make-mdp-agent :mdp *4x3-mdp*
			       :algorithm 'value-iteration-policy)))
  ((setq env (make-mdp-environment :mdp *4x3-mdp* 
				   :agents (list agent))))
  "Now run the agent in the environment. With luck, reaches the (4 3) terminal square."
  ((run-environment env))
  )
