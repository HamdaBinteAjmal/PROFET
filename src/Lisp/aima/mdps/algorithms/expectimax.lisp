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

(defun expectimax-cutoff-decision (mdp s Uhat limit)
  "Return the best action, according to backed-up evaluation 
   after searching to depth limit and applying Uhat to the leaves."
  (let ((actions (actions mdp s)))
    (if actions 
	(the-biggest #'(lambda (a) (expected-cutoff-value mdp a s Uhat (- limit 1)))
		     actions)
      nil)))

(defun expected-cutoff-value (mdp a s Uhat limit)
  "Return the expected value of doing a in s, based on lookahead to depth limit."
  (loop for (s2 . p) in (results mdp a s) sum 
    (* p (+ (reward mdp s a s2)
	    (* (mdp-gamma mdp) 
	       (max-cutoff-value mdp s2 Uhat limit))))))


(defun max-cutoff-value (mdp s Uhat limit)
  "Return the expected value of state s, based on lookahead to depth limit."
  (cond ((terminal? mdp s) (if (eq (mdp-reward-type mdp) 's) (s-reward mdp s) 0))
	((<= limit 0) (funcall Uhat s))
	(t (apply #'max (mapcar #'(lambda (a) (expected-cutoff-value mdp a s Uhat (- limit 1)))
				(actions mdp s))))))

