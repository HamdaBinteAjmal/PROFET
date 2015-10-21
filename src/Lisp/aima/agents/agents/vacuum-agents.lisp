;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*- Author: Peter Norvig

;;;; Some simple agents for the vacuum world

(defstruct (random-vacuum-agent 
   (:include agent
    (program 
     #'(lambda (percept)
	 (declare (ignore percept))
	 (random-element '(right left up down suck noop))))))
  "A very stupid agent: ignore percept and choose a random action.")

(defstruct (reflex-vacuum-agent 
   (:include agent
    (program 
     #'(lambda (percept)
	 (destructuring-bind (location status) percept
	   (cond ((eq status 'Dirty) 'suck)
		 ((eq location 'A) 'Right)
		 ((eq location 'B) 'Left)
		 (t (random-element '(right left up down noop)))))))))
  "An agent for the 2x1 vacuum world. Sucks if there is dirt, 
   otherwise switches locations. [2e p 46]")


