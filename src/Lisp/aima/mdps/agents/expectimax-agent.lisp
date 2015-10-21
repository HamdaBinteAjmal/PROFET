(defun new-expectimax-cutoff-mdp-agent (&key mdp Uhat (k 1) (algorithm #'expectimax-cutoff-decision))
  "Return an agent that uses an expectimax algorithm for MDPs, which searches to depth k 
   (including through chance nodes) and applies Uhat to leaves."
  (make-agent
   :program (let ()
	      (defun expectimax-cutoff-mdp-agent (percept) 
		(funcall algorithm mdp (mdp-percept-state percept) Uhat k))
	      #'expectimax-cutoff-mdp-agent)))

