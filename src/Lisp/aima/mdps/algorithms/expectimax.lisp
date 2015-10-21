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

