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

(defun new-expectimax-cutoff-mdp-agent (&key mdp Uhat (k 1) (algorithm #'expectimax-cutoff-decision))
  "Return an agent that uses an expectimax algorithm for MDPs, which searches to depth k 
   (including through chance nodes) and applies Uhat to leaves."
  (make-agent
   :program (let ()
	      (defun expectimax-cutoff-mdp-agent (percept) 
		(funcall algorithm mdp (mdp-percept-state percept) Uhat k))
	      #'expectimax-cutoff-mdp-agent)))

