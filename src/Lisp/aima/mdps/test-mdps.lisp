(deftest mdps
  "Test code for MDPs."

  ((setq agent (make-mdp-agent :mdp *4x3-mdp*
			       :algorithm 'value-iteration-policy)))
  ((setq env (make-mdp-environment :mdp *4x3-mdp* 
				   :agents (list agent))))
  "Now run the agent in the environment. With luck, reaches the (4 3) terminal square."
  ((run-environment env))
  )
