;;; File: agents/test.lisp -*- Mode: Lisp; -*-

(deftest agents
  "Test agents in the vacuum and wumpus worlds." 
  "Here is how to run an environment, in this case the vacuum world."

  ((run-environment (make-vacuum-world :agents (list (make-reflex-vacuum-agent)) :max-steps 10))
   (>= (agent-score (first (environment-agents *))) 18))

  )
