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

;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- File: .lisp

;;;; Definitions for Markov decision processes (MDPs).

;;; An MDP is defined by initial state, actions, results, rewards, and
;;; (possibly) distinguished terminal states. 
;;; The results model is a function from states and actions
;;; to a probability distribution over the next state, which is represented by
;;; an enumerated distribution ((s0 . p0) (s1 . p1) ...).
;;; Terminal states are distinguished by the function terminal-test.
;;; MDPs can have one of three reward types (s, sa, sas); for s,
;;; the reward is received in the current state, but for sa and sas
;;; the reward is received in the next state.

(defstruct mdp
  (initial-state (required)) ; A state in the domain
  (gamma 1.0)                ; Discount factor, 0 < gamma <= 1
  (reward-type 'sas)         ; sas = R(s,a,s'); sa = R(s,a); s = R(s)
  )

(defmethod state-hash-key ((mdp mdp) state)
  state)

(defmethod action-hash-key ((mdp mdp) action)
  action)

(defmethod reward ((mdp mdp) state1 &optional action state2)
  (case (mdp-reward-type mdp)
    (sas (sas-reward mdp state1 action state2))
    (sa (sa-reward mdp state1 action))
    (s (s-reward mdp state1))))

(defstruct (mdp-percept (:type list))
  "A percept gives the current state, the reward received, and whether it is
  a terminal state."
  state reward terminalp)

(defmethod print-object ((mdp mdp) stream)
  "Print the MDP; could be specialized to print
   more information if, e.g., the initial state can vary."
  (format stream "#<~A>" (type-of mdp)))


;;;; Definitions for enumerated MDPs

;;; Enumerated MDPs have states stored in a list and
;;; actions, results, rewards are stored in a hash table.
;;; The type of the reward table depends on the reward-type of the MDP.

(defstruct (enumerated-mdp (:include mdp))
  states
  (actions (make-hash-table :test #'equal))
  (results (make-hash-table :test #'equal))
  (reward (make-hash-table :test #'equal))
  (terminal-states nil))

(defmethod states ((mdp enumerated-mdp))
  (enumerated-mdp-states mdp))

(defmethod actions ((mdp enumerated-mdp) state)
  (gethash (state-hash-key mdp state) 
	   (enumerated-mdp-actions mdp)))

(defmethod results ((mdp enumerated-mdp) action state)
  (gethash (list (state-hash-key mdp state)
		 (action-hash-key mdp action)) 
	   (enumerated-mdp-results mdp)))

(defmethod sas-reward ((mdp enumerated-mdp) state1 action state2)
  (gethash (list (state-hash-key mdp state1)
		 (action-hash-key mdp action)
		 (state-hash-key mdp state2)) 
	   (enumerated-mdp-reward mdp)))

(defmethod sa-reward ((mdp enumerated-mdp) state1 action)
  (gethash (list (state-hash-key mdp state1)
		 (action-hash-key mdp action))
	   (enumerated-mdp-reward mdp)))

(defmethod s-reward ((mdp enumerated-mdp) state)
  (declare (ignore action state2))
  (gethash (state-hash-key mdp state)
	   (enumerated-mdp-reward mdp)))

(defmethod terminal? ((mdp enumerated-mdp) s)
  "Returns t iff s is terminal in mdp."
  (truth (member s (enumerated-mdp-terminal-states mdp) 
		 :test #'(lambda (s1 s2) 
			   (equalp (state-hash-key mdp s1) (state-hash-key mdp s2))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Might need these for RL agents

(defmethod set-mdp-results ((mdp enumerated-mdp) s a alist)
  "Set the results alist for a specific state and action"
  (setf (gethash (list s a) (enumerated-mdp-results mdp))
	alist))

(defmethod add-mdp-transition ((mdp enumerated-mdp) s a s2)
  "Add a new outcome to the transition model for a specific state and action"
  (push (cons s2 0.0d0) (gethash (list s a) (enumerated-mdp-results mdp))))

(defmethod set-mdp-transition-probability ((mdp enumerated-mdp) s a s2 p)
  "Set the transition probability from s to s2 when a is done."
  (let ((s2.p (assoc s2 (results mdp s a))))
    (if s2.p (setf (cdr s2.p) p)
      (error "Transition ~A ~A ~A does not exist" s a s2))))

(defmethod mdp-state-action-outcomes ((mdp enumerated-mdp) s a)
  "Returns a list of states reachable from s via a"
  (mapcar #'car (results mdp a s)))

