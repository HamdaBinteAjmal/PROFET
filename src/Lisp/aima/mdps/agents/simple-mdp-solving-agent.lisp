;;;; A simple policy agent for Markov decision processes (MDPs).

(defun new-simple-mdp-solving-agent (&key mdp (algorithm #'value-iteration-policy))
  "Given an MDP algorithm, return an agent that computes a policy,
   executes it, then stops."
  (make-agent
   :program (let ((policy nil))
	      (defun simple-mdp-solving-agent (percept) 
		(when (null policy)
		  (setf policy (funcall algorithm mdp)))
		(policy-choice policy (mdp-percept-state percept)))
	      #'simple-mdp-solving-agent)))




